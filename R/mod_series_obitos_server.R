# R/mod_series_obitos_server.R

#' Server: Séries de Mortalidade e Morbidade Materna
#'
#' @param id Módulo id
#' @param data_list Lista retornada por load_series_data()
#' @import shiny dplyr highcharter
#' @importFrom shinyjs show hide
#' @noRd
#' @export
mod_series_obitos_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #------------------------------------------------
    # 1. Definição dos cálculos (bloco6_calcs)
    #------------------------------------------------
    bloco6_calcs <- data.frame(
      tipo                     = c("local", "referencia"),
      soma_obitos_mat_totais   = rep("sum(obitos_mat_totais)", 2),
      rmm                      = c("round(sum(obitos_mat_totais)/sum(nascidos)*100000,1)", "30"),
      prop_obitos_diretos      = rep("round(sum(obitos_mat_diretos)/sum(obitos_mat_totais)*100,1)", 2),
      prop_obitos_aborto       = rep("round(sum(obitos_mat_aborto)/sum(obitos_mat_diretos)*100,1)", 2),
      prop_obitos_hipertens    = rep("round(sum(obitos_mat_hipertensao)/sum(obitos_mat_diretos)*100,1)", 2),
      prop_obitos_hemo         = rep("round(sum(obitos_mat_hemorragia)/sum(obitos_mat_diretos)*100,1)", 2),
      prop_obitos_infec        = rep("round(sum(obitos_mat_infec_puerperal)/sum(obitos_mat_diretos)*100,1)", 2),
      stringsAsFactors = FALSE
    )

    # Cálculos incluindo incompletude
    bloco6_calcs_resumo <- bloco6_calcs %>%
      dplyr::mutate(
        prop_mif_investigado = c(
          "round((sum(obito_mif_investigado_com_ficha_sintese[ano<=2020],na.rm=TRUE)+sum(obito_mif_investigado_sem_ficha_sintese[ano<=2020],na.rm=TRUE))/sum(total_obitos_mulher_idade_fertil[ano<=2020],na.rm=TRUE)*100,1)",
          "100"
        ),
        prop_obito_materno_investigado = c(
          "round((sum(obito_materno_investigado_com_ficha_sintese[ano<=2020],na.rm=TRUE)+sum(obito_materno_investigado_sem_ficha_sintese[ano<=2020],na.rm=TRUE))/sum(total_obitos_maternos[ano<=2020],na.rm=TRUE)*100,1)",
          "100"
        )
      )

    # Ajustes de configuração
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$decimalPoint <- ","
    hcoptslang$thousandsSep <- "."
    options(highcharter.lang = hcoptslang)

    #------------------------------------------------
    # 2. UI Dinâmica de Subfiltros
    #------------------------------------------------
    output$ui_subfiltros <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "estadual" = NULL,  # São Paulo já está implícito
             "rras" = {
               # 1) pega vetor de strings, sem duplicatas
               rras_vals <- unique(data_list$rras_choices$rras)

               # 2) extrai apenas o número de cada "RRAS N"
               #remove "RRAS " e converte em inteiro
               rras_nums <- as.integer(sub("^RRAS\\s+", "", rras_vals))

               # 3) obtém a ordem crescente desses números
               ord <- order(rras_nums, na.last = TRUE)

               # 4) reordena as labels segundo 'ord'
               choices_rras <- rras_vals[ord]

               # 5) finalmente, passa para o selectizeInput
               selectizeInput(
                 ns("rras"), "RRAS:",
                 choices = choices_rras,
                 options = list(placeholder = "Selecione")
               )
             },
             "drs" = selectizeInput(
               ns("drs"), "DRS:",
               choices = sort(unique(data_list$drs_choices$drs)),
               options = list(placeholder = "Selecione")
             ),
             "regiao_saude" = selectizeInput(
               ns("regiao_saude"), "Região de Saúde:",
               choices = sort(unique(data_list$regiao_saude_choices$regiao_de_saude)),
               options = list(placeholder = "Selecione")
             ),
             "municipal" = selectizeInput(
               ns("municipio"), "Município:",
               choices = sort(data_list$municipios_choices$municipio[
                 data_list$municipios_choices$uf == "São Paulo"
               ]),
               options = list(placeholder = "Selecione")
             ),
             NULL
      )
    })

    output$ui_subfiltros_comp <- renderUI({
      req(input$comparar == "Sim", input$nivel2)

      # 1) Ordena alfabeticamente os estados
      estados_ordenados <- sort(data_list$estados_choices)

      switch(input$nivel2,

             #------ NACIONAL não precisa de subfiltro ------
             "nacional" = NULL,

             #------ REGIONAL ------
             "regional" = selectizeInput(
               ns("regiao2"), "Região do país (comparação):",
               choices = sort(unique(data_list$bloco6$regiao)),
               options = list(placeholder = "Selecione"),
               selected = character(0)
             ),

             #------ ESTADUAL ------
             "estadual" = selectizeInput(
               ns("estado2"), "Estado (comparação):",
               choices = estados_ordenados,
               options = list(placeholder = "Selecione"),
               selected = character(0)
             ),

             #------ MACRORREGIÃO (antes RRAS) ------
             "macro" = {
               # só gera a lista de macrorregiões após ter um estado2 selecionado
               fluidRow(
                 column(
                   6,
                   selectizeInput(
                     ns("estado2"), "Estado (comparação):",
                     choices = estados_ordenados,
                     options = list(placeholder = "Selecione"),
                     selected = input$estado2
                   )
                 ),
                 column(
                   6,
                   selectizeInput(
                     ns("macro2"), "Macrorregião (comparação):",
                     choices = local({
                       # 1) pega vetor de strings, sem duplicatas, só do estado escolhido
                       vals <- unique(
                         data_list$macro_r_saude_choices$macro_r_saude[
                           data_list$macro_r_saude_choices$uf == input$estado2
                         ]
                       )
                       # 2) extrai o *primeiro* número de cada string (se houver)
                       #    gsub(".*?(\\d+).*", "\\1", ...) captura os dígitos
                       nums_str <- gsub(".*?(\\d+).*", "\\1", vals)
                       nums     <- suppressWarnings(as.integer(nums_str))
                       has_num  <- !is.na(nums)

                       # 3) ordena cada grupo
                       with_num <- if (any(has_num)) {
                         vals[has_num][order(nums[has_num])]
                       } else character(0)

                       without_num <- if (any(!has_num)) {
                         sort(vals[!has_num])
                       } else character(0)

                       # 4) junta: numéricos primeiro, depois o resto
                       c(with_num, without_num)
                     }),
                     options  = list(placeholder = "Selecione"),
                     selected = character(0)
                   )
                 )
               )
             },

             #------ DRS (fixo em SP) ------
             "drs" = fluidRow(
               column(
                 6,
                 selectizeInput(
                   ns("estado2"), "Estado (comparação):",
                   choices = "São Paulo",
                   selected = "São Paulo"
                 )
               ),
               column(
                 6,
                 selectizeInput(
                   ns("drs2"), "DRS (comparação):",
                   choices = sort(unique(data_list$drs_choices$drs)),
                   options = list(placeholder = "Selecione"),
                   selected = character(0)
                 )
               )
             ),

             #------ MICRORREGIÃO (Região de Saúde comparativa) ------
             "micro" = {
               fluidRow(
                 column(
                   6,
                   selectizeInput(
                     ns("estado2"), "Estado (comparação):",
                     choices = estados_ordenados,
                     options = list(placeholder = "Selecione"),
                     selected = input$estado2
                   )
                 ),
                 column(
                   6,
                   selectizeInput(
                     ns("micro2"), "Região de saúde (comparação):",
                     choices = sort(
                       data_list$micro_r_saude_choices$r_saude[
                         data_list$micro_r_saude_choices$uf == input$estado2
                       ]
                     ),
                     options = list(placeholder = "Selecione"),
                     selected = character(0)
                   )
                 )
               )
             },

             #------ MUNICIPAL comparativo ------
             "municipal" = {
               fluidRow(
                 column(
                   6,
                   selectizeInput(
                     ns("estado2"), "Estado (comparação):",
                     choices = estados_ordenados,
                     options = list(placeholder = "Selecione"),
                     selected = input$estado2
                   )
                 ),
                 column(
                   6,
                   selectizeInput(
                     ns("municipio2"), "Município (comparação):",
                     choices = sort(
                       data_list$municipios_choices$municipio[
                         data_list$municipios_choices$uf == input$estado2
                       ]
                     ),
                     options = list(placeholder = "Selecione"),
                     selected = character(0)
                   )
                 )
               )
             },

             # fallback
             NULL
      )
    })

    # #------------------------------------------------
    # # 4. Reatividade dos filtros
    # #------------------------------------------------
    # filtros <- eventReactive(input$atualizar, {
    #   list(
    #     anos        = input$anos,
    #     nivel       = input$nivel,
    #     regiao      = input$regiao,
    #     estado      = ifelse(input$nivel == "estadual", "São Paulo", input$estado),
    #     rras        = input$rras,
    #     drs         = input$drs,
    #     regiao_saude= input$regiao_saude,
    #     municipio   = input$municipio,
    #     comparar    = input$comparar,
    #     nivel2      = input$nivel2,
    #     regiao2     = input$regiao2,
    #     estado2     = input$estado2,
    #     rras2       = input$rras2,
    #     drs2        = input$drs2,
    #     regiao_saude2 = input$regiao_saude2,
    #     municipio2  = input$municipio2,
    #     mostrar_referencia = input$mostrar_referencia
    #   )
    # }, ignoreNULL = FALSE)

    #------------------------------------------------
    # 4. Reatividade dos filtros
    #------------------------------------------------
    filtros <- eventReactive(input$atualizar, {
      list(
        anos                 = input$anos,
        nivel                = input$nivel,
        # fixamos sempre São Paulo
        estado               = "São Paulo",
        # subfiltros que realmente existem na UI:
        rras                 = input$rras,
        drs                  = input$drs,
        regiao_saude         = input$regiao_saude,
        municipio            = input$municipio,
        comparar             = input$comparar,
        nivel2               = input$nivel2,
        regiao2              = input$regiao2,
        estado2              = input$estado2,
        macro2               = input$macro2,
        drs2                 = input$drs2,
        micro2               = input$micro2,
        municipio2           = input$municipio2,
        mostrar_referencia   = input$mostrar_referencia
      )
    }, ignoreNULL = FALSE)

    #------------------------------------------------
    # 5. Dados principais e de comparação
    #------------------------------------------------
    data_main <- reactive({
      req(filtros())
      df <- data_list$bloco6 %>%
        filter(ano >= filtros()$anos[1], ano <= filtros()$anos[2])

      # Filtra pela localidade principal
      df <- switch(filtros()$nivel,
                   "estadual"  = filter(df, uf == "São Paulo"),
                   "rras"      = filter(df,
                                        uf == "São Paulo" &
                                          macro_r_saude == filtros()$rras),
                   "drs"       = filter(df,
                                        uf == "São Paulo" &
                                          drs == filtros()$drs),
                   "regiao_saude" = filter(df,
                                           uf == "São Paulo" &
                                             r_saude == filtros()$regiao_saude),
                   "municipal" = filter(df,
                                        uf == "São Paulo" &
                                          municipio == filtros()$municipio),
                   df
      )

      # Agrupa e cria indicadores
      df %>%
        group_by(ano) %>%
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros())
    })

    data_comp <- reactive({
      req(filtros(), filtros()$comparar == "Sim")
      df <- data_list$bloco6 %>%
        filter(ano >= filtros()$anos[1], ano <= filtros()$anos[2])

      # Filtra pela localidade de comparação
      df <- switch(filtros()$nivel2,
                   "nacional"  = df,
                   "regional"  = filter(df, regiao == filtros()$regiao2),
                   "estadual"  = filter(df, uf == filtros()$estado2),
                   "macro"     = filter(df,
                                        uf == filtros()$estado2 &
                                          macro_r_saude == filtros()$macro2),
                   "drs"       = filter(df,
                                        uf == "São Paulo" &
                                          drs == filtros()$drs2),
                   "micro"     = filter(df,
                                        uf       == filtros()$estado2 &
                                          r_saude  == filtros()$micro2),
                   "municipal" = filter(df,
                                        uf == filtros()$estado2 &
                                          municipio == filtros()$municipio2),
                   df
      )

      df %>%
        group_by(ano) %>%
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), comp = TRUE)
    })

    # Dados de referência (nacional)
    data_ref <- reactive({
      req(filtros())
      data_list$bloco6 %>%
        filter(ano >= filtros()$anos[1], ano <= filtros()$anos[2]) %>%
        group_by(ano) %>%
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), referencia = TRUE)
    })

    # Corrigir RMM quando nível estadual/regional/nacional
    data_main_rmm <- reactive({
      df <- data_main()

      if (filtros()$nivel == "estadual") {
        rmm_corr <- data_list$rmm_corrigida %>%
          filter(localidade == "São Paulo",
                 ano >= filtros()$anos[1],
                 ano <= filtros()$anos[2])

        if (nrow(rmm_corr) > 0) {
          df <- df %>%
            left_join(rmm_corr %>% select(ano, RMM), by = "ano") %>%
            mutate(rmm = ifelse(!is.na(RMM) & ano <= 2022, RMM, rmm)) %>%
            select(-RMM)
        }
      }

      df
    })

    data_comp_rmm <- reactive({
      if (filtros()$comparar != "Sim") return(NULL)

      df <- data_comp()

      if (filtros()$nivel2 %in% c("estadual", "regional", "nacional") &&
          any(df$ano <= 2022)) {

        localidade_nome <- switch(filtros()$nivel2,
                                  "estadual" = filtros()$estado2,
                                  "regional" = filtros()$regiao2,
                                  "nacional" = "Brasil"
        )

        if (!is.null(localidade_nome)) {
          rmm_corr <- data_list$rmm_corrigida %>%
            filter(localidade == localidade_nome,
                   ano >= filtros()$anos[1],
                   ano <= filtros()$anos[2])

          if (nrow(rmm_corr) > 0) {
            df <- df %>%
              left_join(rmm_corr %>% select(ano, RMM), by = "ano") %>%
              mutate(rmm = ifelse(!is.na(RMM) & ano <= 2022, RMM, rmm)) %>%
              select(-RMM)
          }
        }
      }

      df
    })

    #------------------------------------------------
    # 6. Cálculo de indicadores de incompletude
    #------------------------------------------------
    data_incompletude <- reactive({
      req(filtros())

      # Filtra base_incompletude
      df_incomp <- data_list$base_incompletude %>%
        filter(ano >= filtros()$anos[1], ano <= filtros()$anos[2])

      # Aplica filtro de localidade
      df_incomp <- switch(filtros()$nivel,
                          "estadual"  = filter(df_incomp, uf == "São Paulo"),
                          "rras"      = filter(df_incomp,
                                               uf == "São Paulo" &
                                                 macro_r_saude == filtros()$rras),
                          "drs"       = filter(df_incomp,
                                               uf == "São Paulo" &
                                                 drs == filtros()$drs),
                          "regiao_saude" = filter(df_incomp,
                                                  uf == "São Paulo" &
                                                    r_saude == filtros()$regiao_saude),
                          "municipal" = filter(df_incomp,
                                               uf == "São Paulo" &
                                                 municipio == filtros()$municipio),
                          df_incomp
      )

      # Calcula indicadores de incompletude
      df_incomp <- df_incomp %>%
        group_by(ano) %>%
        summarise(
          prop_mif_investigado = round(
            (sum(obito_mif_investigado_com_ficha_sintese, na.rm = TRUE) +
               sum(obito_mif_investigado_sem_ficha_sintese, na.rm = TRUE)) /
              sum(total_obitos_mulher_idade_fertil, na.rm = TRUE) * 100, 1),
          prop_obito_materno_investigado = round(
            (sum(obito_materno_investigado_com_ficha_sintese, na.rm = TRUE) +
               sum(obito_materno_investigado_sem_ficha_sintese, na.rm = TRUE)) /
              sum(total_obitos_maternos, na.rm = TRUE) * 100, 1)
        ) %>%
        ungroup()

      # Adiciona cobertura se disponível
      if (filtros()$nivel == "municipal") {
        df_cob <- data_list$sub_registro_sim_muni %>%
          filter(
            ano >= filtros()$anos[1],
            ano <= filtros()$anos[2],
            municipio == filtros()$municipio,
            uf == "São Paulo"
          ) %>%
          select(ano, cobertura)
      } else if (filtros()$nivel == "estadual") {
        df_cob <- data_list$sub_registro_sim_uf %>%
          filter(
            ano >= filtros()$anos[1],
            ano <= filtros()$anos[2],
            localidade == "São Paulo"
          ) %>%
          select(ano, cobertura)
      } else {
        # Para RRAS, DRS e região de saúde, assume cobertura 100%
        df_cob <- data.frame(
          ano = seq(filtros()$anos[1], filtros()$anos[2]),
          cobertura = 100
        )
      }

      # Junta incompletude com cobertura
      df_final <- df_incomp %>%
        left_join(df_cob, by = "ano") %>%
        mutate(cobertura = ifelse(is.na(cobertura), 100, cobertura))

      df_final
    })

    #------------------------------------------------
    # 7. Controle de exibição dos botões de alerta
    #------------------------------------------------
    observeEvent(input$atualizar, {
      # Esconde todos os botões inicialmente
      shinyjs::hide(id = "mostrar_botao1", anim = TRUE)
      shinyjs::hide(id = "mostrar_botao2", anim = TRUE)
      shinyjs::hide(id = "mostrar_botao3", anim = TRUE)
      shinyjs::hide(id = "mostrar_botao4", anim = TRUE)

      # Verifica se deve mostrar os botões
      df_incomp <- data_incompletude()
      if (nrow(df_incomp) > 0) {
        if (any(df_incomp$prop_mif_investigado < 90, na.rm = TRUE) |
            any(df_incomp$prop_obito_materno_investigado < 100, na.rm = TRUE) |
            any(df_incomp$cobertura < 90, na.rm = TRUE)) {

          shinyjs::show(id = "mostrar_botao1", anim = TRUE)
          shinyjs::show(id = "mostrar_botao2", anim = TRUE)
          shinyjs::show(id = "mostrar_botao3", anim = TRUE)
          shinyjs::show(id = "mostrar_botao4", anim = TRUE)
        }
      }
    }, ignoreNULL = FALSE)

    # Handlers para os botões de alerta
    observeEvent(input$botao1, {
      cria_modal_incompletude(
        df = data_incompletude(),
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        cobertura = data_incompletude()$cobertura,
        base = "SIM",
        bloco = "bloco6",
        nivel = 2
      )
    })

    observeEvent(input$botao2, {
      cria_modal_incompletude(
        df = data_incompletude(),
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        cobertura = data_incompletude()$cobertura,
        base = "SIM",
        bloco = "bloco6",
        nivel = 2
      )
    })

    observeEvent(input$botao3, {
      cria_modal_incompletude(
        df = data_incompletude(),
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        cobertura = data_incompletude()$cobertura,
        base = "SIM",
        bloco = "bloco6",
        nivel = 2
      )
    })

    observeEvent(input$botao4, {
      cria_modal_incompletude(
        df = data_incompletude(),
        incompletude1 = data_incompletude()$prop_mif_investigado,
        incompletude2 = data_incompletude()$prop_obito_materno_investigado,
        cobertura = data_incompletude()$cobertura,
        base = "SIM",
        bloco = "bloco6",
        nivel = 2
      )
    })

    #------------------------------------------------
    # 8. Renderização dos gráficos
    #------------------------------------------------
    cores <- c("#2c115f", "#b73779", "#fc8961", "#000004FF", "#f1605d")

    # 8.1 Número de óbitos maternos
    output$plot_n_obitos <- highcharter::renderHighchart({
      hc <- highcharter::highchart() %>%
        highcharter::hc_add_dependency("modules/series-label.js") %>%
        highcharter::hc_plotOptions(series = list(
          label = list(enabled = TRUE),
          allowPointSelect = TRUE
        )) %>%
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = seq(filtros()$anos[1], filtros()$anos[2]),
          allowDecimals = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "Número de óbitos maternos"),
          min = 0
        ) %>%
        highcharter::hc_colors(cores)

      # Série principal
      hc <- hc %>%
        highcharter::hc_add_series(
          data = data_main(),
          type = "line",
          highcharter::hcaes(
            x = ano,
            y = soma_obitos_mat_totais,
            group = class,
            colour = class
          )
        )

      # Série de comparação
      if (filtros()$comparar == "Sim") {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_comp(),
            type = "line",
            highcharter::hcaes(
              x = ano,
              y = soma_obitos_mat_totais,
              group = class,
              colour = class
            )
          )
      }

      hc %>%
        highcharter::hc_tooltip(shared = TRUE, sort = TRUE)
    })

    # 8.2 Razão de mortalidade materna por 100k nascidos vivos
    output$plot_rmm <- highcharter::renderHighchart({
      hc <- highcharter::highchart() %>%
        highcharter::hc_add_dependency("modules/series-label.js") %>%
        highcharter::hc_plotOptions(series = list(
          label = list(enabled = TRUE),
          allowPointSelect = TRUE
        )) %>%
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = seq(filtros()$anos[1], filtros()$anos[2]),
          allowDecimals = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "Óbitos maternos por 100 mil nascidos vivos"),
          min = 0
        ) %>%
        highcharter::hc_colors(cores)

      # Série principal (com RMM corrigida se aplicável)
      hc <- hc %>%
        highcharter::hc_add_series(
          data = data_main_rmm(),
          type = "line",
          highcharter::hcaes(
            x = ano,
            y = rmm,
            group = class,
            colour = class
          )
        )

      # Série de comparação (com RMM corrigida se aplicável)
      if (filtros()$comparar == "Sim") {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_comp_rmm(),
            type = "line",
            highcharter::hcaes(
              x = ano,
              y = rmm,
              group = class,
              colour = class
            )
          )
      }

      # Linha de referência (meta ODS) - condicional
      if (filtros()$comparar != "Sim" ||
          (filtros()$comparar == "Sim" && filtros()$mostrar_referencia == "mostrar_referencia")) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_ref(),
            type = "line",
            name = "Referência (meta ODS)",
            highcharter::hcaes(x = ano, y = rmm),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
      }

      hc %>%
        highcharter::hc_tooltip(shared = TRUE, sort = TRUE)
    })

    # 8.3 % de óbitos por causas obstétricas diretas
    output$plot_pct_diretas <- highcharter::renderHighchart({
      validate(
        need(
          sum(data_main()$soma_obitos_mat_totais, na.rm = TRUE) > 0,
          "Não foram registrados óbitos maternos no período."
        )
      )
      hc <- highcharter::highchart() %>%
        highcharter::hc_add_dependency("modules/series-label.js") %>%
        highcharter::hc_plotOptions(series = list(
          label = list(enabled = TRUE),
          allowPointSelect = TRUE
        )) %>%
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = seq(filtros()$anos[1], filtros()$anos[2]),
          allowDecimals = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "%"),
          min = 0,
          max = 100
        ) %>%
        highcharter::hc_colors(cores) %>%
        highcharter::hc_add_series(
          data = data_main(),
          type = "line",
          highcharter::hcaes(
            x = ano,
            y = prop_obitos_diretos,
            group = class,
            colour = class
          )
        )

      # Adiciona comparação se aplicável
      if (filtros()$comparar == "Sim") {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_comp(),
            type = "line",
            highcharter::hcaes(
              x = ano,
              y = prop_obitos_diretos,
              group = class,
              colour = class
            )
          )
      }

      # Adiciona referência média nacional se não for nacional
      # e se mostrar_referencia estiver ativo ou comparação desativada
      if ((filtros()$comparar != "Sim" || filtros()$nivel2 != "nacional") &&
          (filtros()$comparar != "Sim" || filtros()$mostrar_referencia == "mostrar_referencia")) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_ref(),
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = prop_obitos_diretos),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
      }

      hc %>% highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    # 8.4 % de óbitos por causa específica
    output$plot_pct_especificas <- highcharter::renderHighchart({
      validate(
        need(
          sum(data_main()$soma_obitos_mat_totais, na.rm = TRUE) > 0,
          "Não foram registrados óbitos maternos no período."
        )
      )
      validate(
        need(!is.null(input$causa_especifica), "Selecione uma causa específica")
      )

      # Prepara dados segundo a causa escolhida
      df_main_aux <- data_main() %>%
        select(ano, eixo = !!sym(input$causa_especifica), class)

      hc <- highcharter::highchart() %>%
        highcharter::hc_add_dependency("modules/series-label.js") %>%
        highcharter::hc_plotOptions(series = list(
          label = list(enabled = TRUE),
          allowPointSelect = TRUE
        )) %>%
        highcharter::hc_xAxis(
          title = list(text = ""),
          categories = seq(filtros()$anos[1], filtros()$anos[2]),
          allowDecimals = FALSE
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = "%"),
          min = 0,
          max = 100
        ) %>%
        highcharter::hc_colors(cores) %>%
        highcharter::hc_add_series(
          data = df_main_aux,
          type = "line",
          highcharter::hcaes(x = ano, y = eixo, group = class, colour = class)
        )

      # Adiciona comparação se aplicável
      if (filtros()$comparar == "Sim") {
        df_comp_aux <- data_comp() %>%
          select(ano, eixo = !!sym(input$causa_especifica), class)

        hc <- hc %>%
          highcharter::hc_add_series(
            data = df_comp_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo, group = class, colour = class)
          )
      }

      # Adiciona referência média nacional se não for nacional
      # e se mostrar_referencia estiver ativo ou comparação desativada
      if ((filtros()$comparar != "Sim" || filtros()$nivel2 != "nacional") &&
          (filtros()$comparar != "Sim" || filtros()$mostrar_referencia == "mostrar_referencia")) {
        df_ref_aux <- data_ref() %>%
          select(ano, eixo = !!sym(input$causa_especifica))
        hc <- hc %>%
          highcharter::hc_add_series(
            data = df_ref_aux,
            type = "line",
            name = "Referência (média nacional)",
            highcharter::hcaes(x = ano, y = eixo),
            dashStyle = "ShortDot",
            opacity = 0.8
          )
      }

      hc %>% highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

  })
}
