# R/mod_series_obitos_server.R

#' Server: Séries de Mortalidade e Morbidade Materna
#'
#' @param id Módulo id
#' @param data_list Lista retornada por load_series_data()
#' @param data_list_obitos Lista retornada por load_obitos_data()
#' @import shiny dplyr highcharter
#' @importFrom shinyjs show hide
#' @noRd
#' @export
mod_series_obitos_server <- function(id, data_list, data_list_obitos = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title = tagList(shiny::icon("circle-question"), " Séries de Mortalidade"),
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Fechar"),
          tags$div(
            style = "font-size: 15px; line-height: 1.55;",
            tags$p(
              "O painel apresenta a evolução temporal dos óbitos maternos e da razão de mortalidade materna, ",
              "permitindo acompanhar tendências e apoiar o planejamento das ações de saúde."
            ),
            tags$hr(),
            tags$p(
              tags$b("Razão de Mortalidade Materna (RMM)"),
              br(),
              "A Razão de Mortalidade Materna representa o número de óbitos maternos em relação ao total de nascidos vivos ",
              "em determinado local e período. Esse indicador é utilizado para monitorar as condições de saúde materna e a ",
              "qualidade da atenção à gestação, parto e puerpério. Calculamos: (Número de óbitos maternos ÷ Número de nascidos vivos) × 100.000."
            ),
            tags$p(
              tags$b("Causas obstétricas diretas"),
              br(),
              "São óbitos resultantes de complicações da gestação, parto ou puerpério, incluindo situações relacionadas à assistência prestada, ",
              "intervenções, omissões ou tratamento inadequado."
            ),
            tags$ul(
              tags$li("Hemorragias"),
              tags$li("Hipertensão gestacional"),
              tags$li("Infecções relacionadas à gestação ou parto")
            ),
            tags$p(
              tags$b("Causas obstétricas indiretas"),
              br(),
              "São óbitos decorrentes de doenças preexistentes ou desenvolvidas durante a gestação, que foram agravadas pelos efeitos da gravidez."
            ),
            tags$ul(
              tags$li("Doenças cardiovasculares"),
              tags$li("Diabetes"),
              tags$li("Doenças respiratórias")
            )
          )
        )
      )
    })

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

    causas_diretas_tabela <- list(
      aborto = list(
        rotulo = "Aborto",
        coluna = "obitos_mat_aborto"
      ),
      hipertensivas = list(
        rotulo = "Hipertensivas",
        coluna = "obitos_mat_hipertensao"
      ),
      hemorragicas = list(
        rotulo = "Hemorrágicas",
        coluna = "obitos_mat_hemorragia"
      ),
      infeccao_puerperal = list(
        rotulo = "Infecção puerperal",
        coluna = "obitos_mat_infec_puerperal"
      )
    )

    # Ajustes de configuração
    opcoes_selectize_causas_indiretas <- function() {
      list(
        placeholder = "Selecione",
        render = htmlwidgets::JS(
          "{
            item: function(item, escape) {
              var label = item.label || item.text || item.value || '';
              return '<div class=\"series-obitos-select-item\" title=\"' + escape(label) + '\">' + escape(label) + '</div>';
            },
            option: function(item, escape) {
              var label = item.label || item.text || item.value || '';
              return '<div class=\"series-obitos-select-option\" title=\"' + escape(label) + '\">' + escape(label) + '</div>';
            }
          }"
        )
      )
    }

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
               # selectizeInput(
               #   ns("rras"), "RRAS:",
               #   choices = choices_rras,
               #   options = list(placeholder = "Selecione")
               # )
               shinyWidgets::pickerInput(
                 inputId = ns("rras"),
                 label = "RRAS:",
                 choices = choices_rras,
                 options = list("live-search" = TRUE)
               )
             },
             # "drs" = selectizeInput(
             #   ns("drs"), "DRS:",
             #   choices = sort(unique(data_list$drs_choices$drs)),
             #   options = list(placeholder = "Selecione")
             # )
             "drs" = shinyWidgets::pickerInput(
               inputId = ns("drs"),
               label = "DRS:",
               choices = sort(unique(data_list$drs_choices$drs)),
               options = list("live-search" = TRUE)
             ),
             # "regiao_saude" = selectizeInput(
             #   ns("regiao_saude"), "Região de Saúde:",
             #   choices = sort(unique(data_list$regiao_saude_choices$regiao_de_saude)),
             #   options = list(placeholder = "Selecione")
             # )
             "regiao_saude" = shinyWidgets::pickerInput(
               inputId = ns("regiao_saude"),
               label = "Região de Saúde:",
               choices = sort(unique(data_list$regiao_saude_choices$regiao_de_saude)),
               options = list("live-search" = TRUE)
             ),
             # "municipal" = selectizeInput(
             #   ns("municipio"), "Município:",
             #   choices = sort(data_list$municipios_choices$municipio[
             #     data_list$municipios_choices$uf == "São Paulo"
             #   ]),
             #   options = list(placeholder = "Selecione")
             # )
             "municipal" = shinyWidgets::pickerInput(
               inputId = ns("municipio"),
               label = "Município:",
               choices = sort(data_list$municipios_choices$municipio[
                 data_list$municipios_choices$uf == "São Paulo"
               ]),
               options = list("live-search" = TRUE)
             ),
             NULL
      )
    })

    indices_comparacao <- 1:2

    comparacao_sufixo <- function(indice) {
      as.character(indice + 1L)
    }

    construir_subfiltro_comparacao <- function(indice, nivel) {
      sufixo <- comparacao_sufixo(indice)

      switch(nivel,
             "estadual" = NULL,
             "rras" = {
               rras_vals <- unique(data_list$rras_choices$rras)
               rras_nums <- suppressWarnings(as.integer(sub("^RRAS\\s+", "", rras_vals)))
               choices_rras <- rras_vals[order(rras_nums, na.last = TRUE)]

               shinyWidgets::pickerInput(
                 inputId = ns(paste0("rras", sufixo)),
                 label = "RRAS (comparação):",
                 choices = choices_rras,
                 options = list("live-search" = TRUE),
                 selected = character(0)
               )
             },
             "drs" = shinyWidgets::pickerInput(
               inputId = ns(paste0("drs", sufixo)),
               label = "DRS (comparação):",
               choices = sort(unique(data_list$drs_choices$drs)),
               options = list("live-search" = TRUE),
               selected = character(0)
             ),
             "regiao_saude" = shinyWidgets::pickerInput(
               inputId = ns(paste0("regiao_saude", sufixo)),
               label = "Região de Saúde (comparação):",
               choices = sort(unique(data_list$regiao_saude_choices$regiao_de_saude)),
               options = list("live-search" = TRUE),
               selected = character(0)
             ),
             "municipal" = shinyWidgets::pickerInput(
               inputId = ns(paste0("municipio", sufixo)),
               label = "Município (comparação):",
               choices = sort(
                 data_list$municipios_choices$municipio[
                   data_list$municipios_choices$uf == "São Paulo"
                 ]
               ),
               options = list("live-search" = TRUE),
               selected = character(0)
             ),
             NULL
      )
    }

    output$ui_subfiltros_comp <- renderUI({
      req(input$comparar == "Sim", input$nivel2)
      construir_subfiltro_comparacao(1L, input$nivel2)
    })

    output$ui_subfiltros_comp2 <- renderUI({
      req(input$comparar == "Sim", input$comparar2 == "Sim", input$nivel3)
      construir_subfiltro_comparacao(2L, input$nivel3)
    })

    #------------------------------------------------
    # 4. Reatividade dos filtros
    #------------------------------------------------
    filtros <- eventReactive(input$atualizar, {
      list(
        anos                 = input$anos,
        nivel                = input$nivel,
        # fixamos sempre São Paulo
        estado               = "Estado de SP",
        # subfiltros que realmente existem na UI:
        rras                 = input$rras,
        drs                  = input$drs,
        regiao_saude         = input$regiao_saude,
        municipio            = input$municipio,
        comparar             = input$comparar,
        nivel2               = input$nivel2,
        estado2              = "Estado de SP",
        rras2                = input$rras2,
        drs2                 = input$drs2,
        regiao_saude2        = input$regiao_saude2,
        municipio2           = input$municipio2,
        comparar2            = input$comparar2,
        nivel3               = input$nivel3,
        estado3              = "Estado de SP",
        rras3                = input$rras3,
        drs3                 = input$drs3,
        regiao_saude3        = input$regiao_saude3,
        municipio3           = input$municipio3,
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

    comparacao_ativa <- function(f, indice) {
      if (is.null(f)) {
        return(FALSE)
      }

      if (indice == 1L) {
        return(identical(f$comparar, "Sim"))
      }

      if (indice == 2L) {
        return(identical(f$comparar, "Sim") &&
                 identical(f$comparar2, "Sim"))
      }

      FALSE
    }

    campo_localidade_comparacao <- function(nivel) {
      switch(nivel,
             "rras" = "rras",
             "drs" = "drs",
             "regiao_saude" = "regiao_saude",
             "municipal" = "municipio",
             NULL)
    }

    localidade_comparacao_preenchida <- function(f, indice) {
      sufixo <- comparacao_sufixo(indice)
      nivel <- f[[paste0("nivel", sufixo)]]

      if (identical(nivel, "estadual")) {
        return(TRUE)
      }

      campo <- campo_localidade_comparacao(nivel)
      if (is.null(campo)) {
        return(FALSE)
      }

      valor <- f[[paste0(campo, sufixo)]]
      !is.null(valor) && length(valor) > 0 && !is.na(valor) && nzchar(valor)
    }

    filtrar_localidade_comparacao <- function(df, f, indice) {
      sufixo <- comparacao_sufixo(indice)

      switch(f[[paste0("nivel", sufixo)]],
             "estadual"  = filter(df, uf == "São Paulo"),
             "rras"      = filter(df,
                                  uf == "São Paulo" &
                                    macro_r_saude == f[[paste0("rras", sufixo)]]),
             "drs"       = filter(df,
                                  uf == "São Paulo" &
                                    drs == f[[paste0("drs", sufixo)]]),
             "regiao_saude" = filter(df,
                                     uf == "São Paulo" &
                                       r_saude == f[[paste0("regiao_saude", sufixo)]]),
             "municipal" = filter(df,
                                  uf == "São Paulo" &
                                    municipio == f[[paste0("municipio", sufixo)]]),
             df
      )
    }

    filtros_para_comparacao <- function(f, indice) {
      sufixo <- comparacao_sufixo(indice)
      f$nivel2 <- f[[paste0("nivel", sufixo)]]
      f$estado2 <- f[[paste0("estado", sufixo)]]
      f$rras2 <- f[[paste0("rras", sufixo)]]
      f$drs2 <- f[[paste0("drs", sufixo)]]
      f$regiao_saude2 <- f[[paste0("regiao_saude", sufixo)]]
      f$municipio2 <- f[[paste0("municipio", sufixo)]]
      f
    }

    criar_data_comparacao <- function(indice) {
      reactive({
        f <- filtros()
        req(f, comparacao_ativa(f, indice), localidade_comparacao_preenchida(f, indice))

        df <- data_list$bloco6 %>%
          filter(ano >= f$anos[1], ano <= f$anos[2])

        df <- filtrar_localidade_comparacao(df, f, indice)

        df %>%
          group_by(ano) %>%
          cria_indicadores(
            df_calcs = bloco6_calcs,
            filtros = filtros_para_comparacao(f, indice),
            comp = TRUE
          )
      })
    }

    data_comp <- criar_data_comparacao(1L)
    data_comp2 <- criar_data_comparacao(2L)

    data_comparacoes <- reactive({
      f <- filtros()
      comparacoes <- list()

      if (comparacao_ativa(f, 1L)) {
        comparacoes[[length(comparacoes) + 1L]] <- list(indice = 1L, dados = data_comp())
      }

      if (comparacao_ativa(f, 2L)) {
        comparacoes[[length(comparacoes) + 1L]] <- list(indice = 2L, dados = data_comp2())
      }

      comparacoes
    })

    # Dados de referência do Estado de São Paulo
    data_ref <- reactive({
      req(filtros())
      data_list$bloco6 %>%
        filter(
          ano >= filtros()$anos[1],
          ano <= filtros()$anos[2],
          uf == "São Paulo"
        ) %>%
        group_by(ano) %>%
        cria_indicadores(df_calcs = bloco6_calcs, filtros = filtros(), referencia = TRUE)
    })

    # Mantém a RMM bruta em todas as séries
    data_main_rmm <- reactive({
      data_main()
    })

    alguma_comparacao_ativa <- function(f) {
      any(vapply(indices_comparacao, function(indice) {
        comparacao_ativa(f, indice)
      }, logical(1)))
    }

    comparacao_estadual_ativa <- reactive({
      f <- filtros()
      any(vapply(indices_comparacao, function(indice) {
        comparacao_ativa(f, indice) &&
          identical(f[[paste0("nivel", comparacao_sufixo(indice))]], "estadual")
      }, logical(1)))
    })

    mostrar_linha_referencia <- reactive({
      f <- filtros()
      !comparacao_ativa(f, 1L) ||
        identical(f$mostrar_referencia, "mostrar_referencia")
    })

    mostrar_referencia_estado <- reactive({
      if (!mostrar_linha_referencia()) {
        return(FALSE)
      }

      f <- filtros()
      if (alguma_comparacao_ativa(f)) {
        return(!(identical(f$nivel, "estadual") &&
                   comparacao_estadual_ativa()))
      }

      !identical(f$nivel, "estadual")
    })

    ocultar_serie_estadual_principal <- reactive({
      f <- filtros()
      alguma_comparacao_ativa(f) &&
        mostrar_referencia_estado() &&
        identical(f$nivel, "estadual") &&
        !comparacao_estadual_ativa()
    })

    ocultar_serie_estadual_comparacao <- function(indice) {
      f <- filtros()
      comparacao_ativa(f, indice) &&
        mostrar_referencia_estado() &&
        identical(f[[paste0("nivel", comparacao_sufixo(indice))]], "estadual") &&
        !identical(f$nivel, "estadual")
    }

    mostrar_referencia_estado_n_obitos <- reactive({
      f <- filtros()
      alguma_comparacao_ativa(f) &&
        identical(f$mostrar_referencia, "mostrar_referencia") &&
        xor(
          identical(f$nivel, "estadual"),
          comparacao_estadual_ativa()
        )
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
    # 7. Tabela detalhada
    #------------------------------------------------
    output$ui_tabela_ano <- renderUI({
      req(input$anos)
      anos <- seq(input$anos[1], input$anos[2])

      selectizeInput(
        ns("tabela_ano"),
        "Ano da tabela:",
        choices = anos,
        selected = max(anos),
        width = "100%"
      )
    })

    ano_tabela <- reactive({
      req(filtros())
      anos_disponiveis <- seq(filtros()$anos[1], filtros()$anos[2])
      ano <- suppressWarnings(as.integer(input$tabela_ano))

      if (length(ano) == 0 || is.na(ano) || !ano %in% anos_disponiveis) {
        return(max(anos_disponiveis))
      }

      ano
    })

    output$ui_tabela_causa <- renderUI({
      req(input$tabela_indicador)

      if (!identical(input$tabela_indicador, "causas_especificas")) {
        return(NULL)
      }

      tipo_causa <- input$tabela_tipo_causa
      if (is.null(tipo_causa)) {
        tipo_causa <- "diretas"
      }

      if (identical(tipo_causa, "diretas")) {
        selectizeInput(
          ns("tabela_causa_direta"),
          "Causa:",
          choices = stats::setNames(
            names(causas_diretas_tabela),
            vapply(causas_diretas_tabela, `[[`, character(1), "rotulo")
          ),
          selected = "aborto",
          width = "100%"
        )
      } else {
        causas <- causas_indiretas_top4()

        if (length(causas) == 0) {
          return(tags$div(
            class = "series-obitos-tabela-nota",
            "Não há óbitos maternos indiretos com causa informada no período selecionado."
          ))
        }

        escolhas <- choices_causas_indiretas(causas)

        tags$div(
          class = "series-obitos-causa-select",
          selectizeInput(
            ns("tabela_causa_indireta"),
            "Causa:",
            choices = escolhas,
            selected = unname(escolhas[[1]]),
            options = opcoes_selectize_causas_indiretas(),
            width = "100%"
          )
        )
      }
    })

    ordenar_localidades_tabela <- function(df) {
      df <- df %>%
        dplyr::mutate(
          rras_ordem = suppressWarnings(as.integer(gsub("\\D+", "", rras))),
          drs_ordem = tolower(trimws(drs)),
          regiao_saude_ordem = tolower(trimws(regiao_saude)),
          municipio_ordem = tolower(trimws(municipio))
        ) %>%
        dplyr::arrange(
          drs_ordem,
          is.na(rras_ordem),
          rras_ordem,
          regiao_saude_ordem,
          municipio_ordem
        )

      niveis_rras <- df %>%
        dplyr::distinct(rras, rras_ordem) %>%
        dplyr::arrange(is.na(rras_ordem), rras_ordem, rras) %>%
        dplyr::pull(rras)

      df %>%
        dplyr::mutate(rras = factor(rras, levels = niveis_rras)) %>%
        dplyr::select(
          -rras_ordem,
          -drs_ordem,
          -regiao_saude_ordem,
          -municipio_ordem
        )
    }

    locais_tabela <- reactive({
      req(filtros())

      nomes_municipios <- data_list$bloco6 %>%
        dplyr::filter(uf == "São Paulo") %>%
        dplyr::mutate(codmunres = as.numeric(codmunres)) %>%
        dplyr::group_by(codmunres) %>%
        dplyr::summarise(
          municipio_series = dplyr::first(na.omit(municipio)),
          .groups = "drop"
        )

      data_list$muni_rras_rs_drs %>%
        dplyr::transmute(
          codmunres = as.numeric(cod_ibge),
          drs = drs,
          rras = rras,
          regiao_saude = regiao_de_saude,
          municipio = municipio
        ) %>%
        dplyr::left_join(nomes_municipios, by = "codmunres") %>%
        dplyr::mutate(
          municipio = dplyr::if_else(
            !is.na(municipio_series) & nzchar(municipio_series),
            municipio_series,
            municipio
          )
        ) %>%
        dplyr::select(-municipio_series) %>%
        dplyr::distinct() %>%
        dplyr::filter(
          !is.na(drs),
          !is.na(rras),
          !is.na(regiao_saude),
          !is.na(municipio)
        ) %>%
        ordenar_localidades_tabela()
    })

    base_tabela_series_anos <- function(anos) {
      req(filtros())
      anos <- sort(unique(as.integer(anos)))
      locais <- locais_tabela() %>%
        dplyr::mutate(.ordem_local = dplyr::row_number())
      grade <- merge(
        data.frame(ano = anos),
        locais,
        by = NULL
      )

      data_list$bloco6 %>%
        dplyr::filter(
          ano %in% anos,
          uf == "São Paulo"
        ) %>%
        dplyr::mutate(
          codmunres = as.numeric(codmunres)
        ) %>%
        dplyr::group_by(ano, codmunres) %>%
        dplyr::summarise(
          obitos_mat_totais = sum(obitos_mat_totais, na.rm = TRUE),
          obitos_mat_diretos = sum(obitos_mat_diretos, na.rm = TRUE),
          obitos_mat_aborto = sum(obitos_mat_aborto, na.rm = TRUE),
          obitos_mat_hipertensao = sum(obitos_mat_hipertensao, na.rm = TRUE),
          obitos_mat_hemorragia = sum(obitos_mat_hemorragia, na.rm = TRUE),
          obitos_mat_infec_puerperal = sum(obitos_mat_infec_puerperal, na.rm = TRUE),
          nascidos = sum(nascidos, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          obitos_mat_outras_diretas = pmax(
            obitos_mat_diretos -
              obitos_mat_aborto -
              obitos_mat_hipertensao -
              obitos_mat_hemorragia -
              obitos_mat_infec_puerperal,
            0
          )
        ) %>%
        dplyr::right_join(grade, by = c("ano", "codmunres")) %>%
        dplyr::mutate(
          obitos_mat_totais = dplyr::if_else(is.na(obitos_mat_totais), 0, obitos_mat_totais),
          obitos_mat_diretos = dplyr::if_else(is.na(obitos_mat_diretos), 0, obitos_mat_diretos),
          obitos_mat_aborto = dplyr::if_else(is.na(obitos_mat_aborto), 0, obitos_mat_aborto),
          obitos_mat_hipertensao = dplyr::if_else(is.na(obitos_mat_hipertensao), 0, obitos_mat_hipertensao),
          obitos_mat_hemorragia = dplyr::if_else(is.na(obitos_mat_hemorragia), 0, obitos_mat_hemorragia),
          obitos_mat_infec_puerperal = dplyr::if_else(is.na(obitos_mat_infec_puerperal), 0, obitos_mat_infec_puerperal),
          nascidos = dplyr::if_else(is.na(nascidos), 0, nascidos),
          obitos_mat_outras_diretas = dplyr::if_else(is.na(obitos_mat_outras_diretas), 0, obitos_mat_outras_diretas)
        ) %>%
        dplyr::arrange(ano, .ordem_local) %>%
        dplyr::select(-.ordem_local)
    }

    base_tabela_series <- reactive({
      base_tabela_series_anos(ano_tabela()) %>%
        dplyr::select(-ano)
    })

    base_obitos_oficiais_sp_anos <- function(anos) {
      req(filtros())
      validate(
        need(
          !is.null(data_list_obitos) && !is.null(data_list_obitos$oficiais),
          "A base de óbitos oficiais não está disponível para montar as causas indiretas."
        )
      )

      anos <- sort(unique(as.integer(anos)))
      data_list_obitos$oficiais %>%
        dplyr::filter(
          ano %in% anos,
          uf == "SP"
        ) %>%
        dplyr::mutate(
          codmunres = as.numeric(cod_ibge),
          regiao_saude = regiao_de_saude,
          causabas_categoria = dplyr::if_else(
            is.na(causabas_categoria) | trimws(causabas_categoria) == "",
            "Sem informação",
            causabas_categoria
          )
        )
    }

    base_obitos_oficiais_sp <- reactive({
      base_obitos_oficiais_sp_anos(ano_tabela())
    })

    codigos_causas_indiretas <- function(causas) {
      causas <- as.character(causas)
      codigos <- ifelse(
        grepl("^[[:alpha:]][0-9]{2}", causas),
        toupper(sub("^([[:alpha:]][0-9]{2}).*$", "\\1", causas)),
        NA_character_
      )

      sem_codigo <- is.na(codigos) | !nzchar(codigos)
      if (any(sem_codigo)) {
        slugs <- iconv(causas[sem_codigo], from = "", to = "ASCII//TRANSLIT", sub = "")
        slugs <- tolower(gsub("[^[:alnum:]]+", "_", slugs))
        slugs <- gsub("^_+|_+$", "", slugs)
        slugs_vazios <- is.na(slugs) | !nzchar(slugs)
        slugs[slugs_vazios] <- paste0("causa_", seq_len(sum(slugs_vazios)))
        codigos[sem_codigo] <- slugs
      }

      make.unique(codigos, sep = "_")
    }

    choices_causas_indiretas <- function(causas) {
      if (length(causas) == 0) {
        return(character(0))
      }

      stats::setNames(codigos_causas_indiretas(causas), causas)
    }

    causa_indireta_por_input <- function(valor, causas) {
      if (length(causas) == 0) {
        return(NA_character_)
      }

      if (is.null(valor) || length(valor) == 0 || is.na(valor) || !nzchar(valor)) {
        return(causas[[1]])
      }

      valor <- as.character(valor[[1]])
      if (valor %in% causas) {
        return(valor)
      }

      escolhas <- choices_causas_indiretas(causas)
      indice <- match(valor, unname(escolhas))
      if (!is.na(indice)) {
        return(names(escolhas)[[indice]])
      }

      causas[[1]]
    }

    causas_indiretas_mais_frequentes <- function(anos, limite = 4) {
      base_obitos_oficiais_sp_anos(anos) %>%
        dplyr::filter(
          tipo_de_morte_materna == "Indireta",
          causabas_categoria != "Sem informação"
        ) %>%
        dplyr::group_by(causabas_categoria) %>%
        dplyr::summarise(
          obitos = sum(as.numeric(obitos), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(obitos), causabas_categoria) %>%
        utils::head(limite) %>%
        dplyr::pull(causabas_categoria)
    }

    causas_indiretas_top4 <- reactive({
      f <- filtros()
      req(f, length(f$anos) == 2, !anyNA(f$anos))
      causas_indiretas_mais_frequentes(seq(f$anos[1], f$anos[2]), 4)
    })

    causa_indireta_tabela <- reactive({
      causas <- causas_indiretas_top4()

      validate(
        need(
          length(causas) > 0,
          "Não há óbitos maternos indiretos com causa informada no período selecionado."
        )
      )

      causa_indireta_por_input(input$tabela_causa_indireta, causas)
    })

    base_tabela_indiretas_anos <- function(anos, causa_indireta) {
      anos <- sort(unique(as.integer(anos)))
      locais <- locais_tabela() %>%
        dplyr::mutate(.ordem_local = dplyr::row_number())
      grade <- merge(
        data.frame(ano = anos),
        locais,
        by = NULL
      )

      base_obitos_oficiais_sp_anos(anos) %>%
        dplyr::filter(tipo_de_morte_materna == "Indireta") %>%
        dplyr::group_by(ano, codmunres) %>%
        dplyr::summarise(
          obitos_indiretos = sum(as.numeric(obitos), na.rm = TRUE),
          obitos_causa_indireta = sum(
            ifelse(
              causabas_categoria == causa_indireta,
              as.numeric(obitos),
              0
            ),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        dplyr::right_join(
          grade,
          by = c("ano", "codmunres")
        ) %>%
        dplyr::mutate(
          obitos_indiretos = dplyr::if_else(is.na(obitos_indiretos), 0, obitos_indiretos),
          obitos_causa_indireta = dplyr::if_else(is.na(obitos_causa_indireta), 0, obitos_causa_indireta)
        ) %>%
        dplyr::arrange(ano, .ordem_local) %>%
        dplyr::select(-.ordem_local)
    }

    base_tabela_indiretas <- reactive({
      causa_indireta <- causa_indireta_tabela()

      base_tabela_indiretas_anos(ano_tabela(), causa_indireta) %>%
        dplyr::select(-ano)
    })

    base_tabela_obstetricas_indiretas_anos <- function(anos) {
      anos <- sort(unique(as.integer(anos)))
      locais <- locais_tabela() %>%
        dplyr::mutate(.ordem_local = dplyr::row_number())
      grade <- merge(
        data.frame(ano = anos),
        locais,
        by = NULL
      )

      base_obitos_oficiais_sp_anos(anos) %>%
        dplyr::group_by(ano, codmunres) %>%
        dplyr::summarise(
          obitos_maternos = sum(as.numeric(obitos), na.rm = TRUE),
          obitos_indiretos = sum(
            ifelse(
              tipo_de_morte_materna == "Indireta",
              as.numeric(obitos),
              0
            ),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        dplyr::right_join(
          grade,
          by = c("ano", "codmunres")
        ) %>%
        dplyr::mutate(
          obitos_maternos = dplyr::if_else(is.na(obitos_maternos), 0, obitos_maternos),
          obitos_indiretos = dplyr::if_else(is.na(obitos_indiretos), 0, obitos_indiretos)
        ) %>%
        dplyr::arrange(ano, .ordem_local) %>%
        dplyr::select(-.ordem_local)
    }

    base_tabela_obstetricas_indiretas <- reactive({
      base_tabela_obstetricas_indiretas_anos(ano_tabela()) %>%
        dplyr::select(-ano)
    })

    causas_indiretas_periodo <- reactive({
      f <- filtros()
      req(f, length(f$anos) == 2, !anyNA(f$anos))

      causas_indiretas_mais_frequentes(seq(f$anos[1], f$anos[2]), 4)
    })

    observeEvent(causas_indiretas_periodo(), {
      causas <- causas_indiretas_periodo()
      escolhas <- choices_causas_indiretas(causas)
      valores <- unname(escolhas)
      valor_atual <- input$causa_indireta_especifica
      selected <- if (length(valores) > 0) {
        if (!is.null(valor_atual) && length(valor_atual) > 0 && valor_atual[[1]] %in% valores) {
          valor_atual[[1]]
        } else {
          valores[[1]]
        }
      } else {
        character(0)
      }

      updateSelectizeInput(
        session,
        "causa_indireta_especifica",
        choices = escolhas,
        selected = selected,
        options = opcoes_selectize_causas_indiretas(),
        server = FALSE
      )
    }, ignoreNULL = FALSE)

    causa_indireta_grafico <- reactive({
      causas <- causas_indiretas_periodo()

      validate(
        need(
          length(causas) > 0,
          "Não há óbitos maternos indiretos com causa informada no período selecionado."
        )
      )

      causa_indireta_por_input(input$causa_indireta_especifica, causas)
    })

    normalizar_nome_localidade <- function(x) {
      x <- as.character(x)
      x <- gsub("\u00A0", " ", x, fixed = TRUE)
      x <- trimws(gsub("\\s+", " ", x))
      x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
      toupper(x)
    }

    valor_localidade_filtro <- function(f, indice = NULL, campo) {
      sufixo <- if (is.null(indice)) "" else comparacao_sufixo(indice)
      f[[paste0(campo, sufixo)]]
    }

    codmunres_por_municipio <- function(municipio) {
      if (is.null(municipio) || length(municipio) == 0 || is.na(municipio) || !nzchar(municipio)) {
        return(numeric(0))
      }

      locais <- locais_tabela()
      codigos <- locais$codmunres[locais$municipio == municipio]

      if (length(codigos) == 0) {
        codigos <- locais$codmunres[
          normalizar_nome_localidade(locais$municipio) == normalizar_nome_localidade(municipio)
        ]
      }

      unique(as.numeric(codigos))
    }

    filtrar_obitos_oficiais_localidade <- function(df, f, indice = NULL) {
      nivel <- valor_localidade_filtro(f, indice, "nivel")

      if (identical(nivel, "estadual")) {
        return(df)
      }

      if (identical(nivel, "rras")) {
        valor <- valor_localidade_filtro(f, indice, "rras")
        return(dplyr::filter(df, rras == valor))
      }

      if (identical(nivel, "drs")) {
        valor <- valor_localidade_filtro(f, indice, "drs")
        return(dplyr::filter(df, drs == valor))
      }

      if (identical(nivel, "regiao_saude")) {
        valor <- valor_localidade_filtro(f, indice, "regiao_saude")
        return(dplyr::filter(df, regiao_saude == valor))
      }

      if (identical(nivel, "municipal")) {
        codigos <- codmunres_por_municipio(valor_localidade_filtro(f, indice, "municipio"))
        return(dplyr::filter(df, codmunres %in% codigos))
      }

      df[0, , drop = FALSE]
    }

    rotulo_localidade_series <- function(f, indice = NULL) {
      nivel <- valor_localidade_filtro(f, indice, "nivel")

      rotulo <- switch(
        nivel,
        "estadual" = valor_localidade_filtro(f, indice, "estado"),
        "rras" = valor_localidade_filtro(f, indice, "rras"),
        "drs" = valor_localidade_filtro(f, indice, "drs"),
        "regiao_saude" = valor_localidade_filtro(f, indice, "regiao_saude"),
        "municipal" = valor_localidade_filtro(f, indice, "municipio"),
        NA_character_
      )

      if (is.null(rotulo) || length(rotulo) == 0 || is.na(rotulo) || !nzchar(rotulo)) {
        return(NA_character_)
      }

      as.character(rotulo[[1]])
    }

    data_prop_indiretas_localidade <- function(f, indice = NULL, referencia = FALSE) {
      anos <- seq(f$anos[1], f$anos[2])
      grade_anos <- data.frame(ano = anos)

      df <- base_obitos_oficiais_sp_anos(anos)
      if (!isTRUE(referencia)) {
        df <- filtrar_obitos_oficiais_localidade(df, f, indice)
      }

      df %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          obitos_maternos = sum(as.numeric(obitos), na.rm = TRUE),
          obitos_indiretos = sum(
            ifelse(
              tipo_de_morte_materna == "Indireta",
              as.numeric(obitos),
              0
            ),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        dplyr::right_join(grade_anos, by = "ano") %>%
        dplyr::mutate(
          obitos_maternos = dplyr::if_else(is.na(obitos_maternos), 0, obitos_maternos),
          obitos_indiretos = dplyr::if_else(is.na(obitos_indiretos), 0, obitos_indiretos),
          prop_obitos_indiretos = dplyr::if_else(
            obitos_maternos > 0,
            round(obitos_indiretos / obitos_maternos * 100, 1),
            NA_real_
          ),
          class = if (isTRUE(referencia)) {
            "Estado de SP (referência)"
          } else {
            rotulo_localidade_series(f, indice)
          }
        ) %>%
        dplyr::arrange(ano)
    }

    data_prop_indiretas_main <- reactive({
      f <- filtros()
      req(f)
      data_prop_indiretas_localidade(f)
    })

    data_prop_indiretas_ref <- reactive({
      f <- filtros()
      req(f)
      data_prop_indiretas_localidade(f, referencia = TRUE)
    })

    data_prop_indiretas_comparacoes <- reactive({
      f <- filtros()
      req(f)
      comparacoes <- list()

      for (indice in indices_comparacao) {
        if (comparacao_ativa(f, indice)) {
          req(localidade_comparacao_preenchida(f, indice))
          comparacoes[[length(comparacoes) + 1L]] <- list(
            indice = indice,
            dados = data_prop_indiretas_localidade(f, indice = indice)
          )
        }
      }

      comparacoes
    })

    data_indiretas_localidade <- function(f, causa_indireta, indice = NULL, referencia = FALSE) {
      anos <- seq(f$anos[1], f$anos[2])
      grade_anos <- data.frame(ano = anos)

      df <- base_obitos_oficiais_sp_anos(anos)
      if (!isTRUE(referencia)) {
        df <- filtrar_obitos_oficiais_localidade(df, f, indice)
      }

      df %>%
        dplyr::filter(tipo_de_morte_materna == "Indireta") %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          obitos_indiretos = sum(as.numeric(obitos), na.rm = TRUE),
          obitos_causa_indireta = sum(
            ifelse(
              causabas_categoria == causa_indireta,
              as.numeric(obitos),
              0
            ),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        dplyr::right_join(grade_anos, by = "ano") %>%
        dplyr::mutate(
          obitos_indiretos = dplyr::if_else(is.na(obitos_indiretos), 0, obitos_indiretos),
          obitos_causa_indireta = dplyr::if_else(is.na(obitos_causa_indireta), 0, obitos_causa_indireta),
          prop_obitos_indiretos_especifica = dplyr::if_else(
            obitos_indiretos > 0,
            round(obitos_causa_indireta / obitos_indiretos * 100, 1),
            NA_real_
          ),
          class = if (isTRUE(referencia)) {
            "Estado de SP (referência)"
          } else {
            rotulo_localidade_series(f, indice)
          }
        ) %>%
        dplyr::arrange(ano)
    }

    data_indiretas_main <- reactive({
      f <- filtros()
      req(f)
      data_indiretas_localidade(f, causa_indireta_grafico())
    })

    data_indiretas_ref <- reactive({
      f <- filtros()
      req(f)
      data_indiretas_localidade(f, causa_indireta_grafico(), referencia = TRUE)
    })

    data_indiretas_comparacoes <- reactive({
      f <- filtros()
      req(f)
      causa_indireta <- causa_indireta_grafico()
      comparacoes <- list()

      for (indice in indices_comparacao) {
        if (comparacao_ativa(f, indice)) {
          req(localidade_comparacao_preenchida(f, indice))
          comparacoes[[length(comparacoes) + 1L]] <- list(
            indice = indice,
            dados = data_indiretas_localidade(f, causa_indireta, indice = indice)
          )
        }
      }

      comparacoes
    })

    montar_tabela_indicador <- function(df, numerador_col, denominador_col, nome_numerador,
                                        nome_denominador, nome_valor, percentual = TRUE,
                                        fator = 1) {
      numerador <- df[[numerador_col]]
      denominador <- df[[denominador_col]]
      colunas_localidade <- c(
        if ("ano" %in% names(df)) "ano",
        "drs",
        "rras",
        "regiao_saude",
        "municipio"
      )

      dados <- df %>%
        dplyr::mutate(
          numerador = numerador,
          denominador = denominador,
          valor = dplyr::if_else(
            denominador > 0,
            numerador / denominador * fator,
            NA_real_
          )
        ) %>%
        dplyr::select(dplyr::all_of(colunas_localidade), numerador, denominador, valor)

      list(
        dados = dados,
        absoluto = FALSE,
        nome_numerador = nome_numerador,
        nome_denominador = nome_denominador,
        nome_valor = nome_valor,
        percentual = percentual,
        fator = fator
      )
    }

    dados_tabela_mortalidade <- reactive({
      req(input$tabela_indicador)

      if (identical(input$tabela_indicador, "n_obitos")) {
        df <- base_tabela_series() %>%
          dplyr::transmute(
            drs,
            rras,
            regiao_saude,
            municipio,
            valor = obitos_mat_totais
          )

        return(list(
          dados = df,
          absoluto = TRUE,
          nome_valor = "Óbitos maternos"
        ))
      }

      if (identical(input$tabela_indicador, "rmm")) {
        return(montar_tabela_indicador(
          base_tabela_series(),
          "obitos_mat_totais",
          "nascidos",
          "Óbitos maternos",
          "Nascidos vivos",
          "Razão por 100.000 nascidos vivos",
          percentual = FALSE,
          fator = 100000
        ))
      }

      if (identical(input$tabela_indicador, "prop_obstetricas")) {
        req(input$tabela_tipo_causa)

        if (identical(input$tabela_tipo_causa, "diretas")) {
          return(montar_tabela_indicador(
            base_tabela_series(),
            "obitos_mat_diretos",
            "obitos_mat_totais",
            "Óbitos maternos diretos",
            "Óbitos maternos",
            "% de óbitos por causas obstétricas diretas",
            percentual = TRUE,
            fator = 1
          ))
        }

        return(montar_tabela_indicador(
          base_tabela_obstetricas_indiretas(),
          "obitos_indiretos",
          "obitos_maternos",
          "Óbitos maternos indiretos",
          "Óbitos maternos",
          "% de óbitos por causas obstétricas indiretas",
          percentual = TRUE,
          fator = 1
        ))
      }

      req(input$tabela_tipo_causa)

      if (identical(input$tabela_tipo_causa, "diretas")) {
        causa <- input$tabela_causa_direta
        if (is.null(causa) || !causa %in% names(causas_diretas_tabela)) {
          causa <- "aborto"
        }

        info_causa <- causas_diretas_tabela[[causa]]
        montar_tabela_indicador(
          base_tabela_series(),
          info_causa$coluna,
          "obitos_mat_diretos",
          paste0("Óbitos por ", tolower(info_causa$rotulo)),
          "Óbitos maternos diretos",
          paste0("% de óbitos maternos diretos por ", tolower(info_causa$rotulo)),
          percentual = TRUE,
          fator = 1
        )
      } else {
        causa_indireta <- causa_indireta_tabela()

        montar_tabela_indicador(
          base_tabela_indiretas(),
          "obitos_causa_indireta",
          "obitos_indiretos",
          paste0("Óbitos por ", tolower(causa_indireta)),
          "Óbitos maternos indiretos",
          paste0("% de óbitos maternos indiretos por ", tolower(causa_indireta)),
          percentual = TRUE,
          fator = 1
        )
      }
    })

    anos_download_tabela_mortalidade <- reactive({
      req(filtros())
      seq(filtros()$anos[1], filtros()$anos[2])
    })

    dados_tabela_mortalidade_download <- reactive({
      req(input$tabela_indicador)
      anos <- anos_download_tabela_mortalidade()

      if (identical(input$tabela_indicador, "n_obitos")) {
        df <- base_tabela_series_anos(anos) %>%
          dplyr::transmute(
            ano,
            drs,
            rras,
            regiao_saude,
            municipio,
            valor = obitos_mat_totais
          )

        return(list(
          dados = df,
          absoluto = TRUE,
          nome_valor = "Óbitos maternos"
        ))
      }

      if (identical(input$tabela_indicador, "rmm")) {
        return(montar_tabela_indicador(
          base_tabela_series_anos(anos),
          "obitos_mat_totais",
          "nascidos",
          "Óbitos maternos",
          "Nascidos vivos",
          "Razão por 100.000 nascidos vivos",
          percentual = FALSE,
          fator = 100000
        ))
      }

      if (identical(input$tabela_indicador, "prop_obstetricas")) {
        req(input$tabela_tipo_causa)

        if (identical(input$tabela_tipo_causa, "diretas")) {
          return(montar_tabela_indicador(
            base_tabela_series_anos(anos),
            "obitos_mat_diretos",
            "obitos_mat_totais",
            "Óbitos maternos diretos",
            "Óbitos maternos",
            "% de óbitos por causas obstétricas diretas",
            percentual = TRUE,
            fator = 1
          ))
        }

        return(montar_tabela_indicador(
          base_tabela_obstetricas_indiretas_anos(anos),
          "obitos_indiretos",
          "obitos_maternos",
          "Óbitos maternos indiretos",
          "Óbitos maternos",
          "% de óbitos por causas obstétricas indiretas",
          percentual = TRUE,
          fator = 1
        ))
      }

      req(input$tabela_tipo_causa)

      if (identical(input$tabela_tipo_causa, "diretas")) {
        causa <- input$tabela_causa_direta
        if (is.null(causa) || !causa %in% names(causas_diretas_tabela)) {
          causa <- "aborto"
        }

        info_causa <- causas_diretas_tabela[[causa]]
        montar_tabela_indicador(
          base_tabela_series_anos(anos),
          info_causa$coluna,
          "obitos_mat_diretos",
          paste0("Óbitos por ", tolower(info_causa$rotulo)),
          "Óbitos maternos diretos",
          paste0("% de óbitos maternos diretos por ", tolower(info_causa$rotulo)),
          percentual = TRUE,
          fator = 1
        )
      } else {
        causa_indireta <- causa_indireta_tabela()

        montar_tabela_indicador(
          base_tabela_indiretas_anos(anos, causa_indireta),
          "obitos_causa_indireta",
          "obitos_indiretos",
          paste0("Óbitos por ", tolower(causa_indireta)),
          "Óbitos maternos indiretos",
          paste0("% de óbitos maternos indiretos por ", tolower(causa_indireta)),
          percentual = TRUE,
          fator = 1
        )
      }
    })

    slug_download_tabela_mortalidade <- function(x) {
      slug <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
      if (is.na(slug) || !nzchar(slug)) {
        slug <- x
      }
      slug <- tolower(gsub("[^[:alnum:]]+", "_", slug))
      slug <- gsub("^_+|_+$", "", slug)

      if (!nzchar(slug)) "tabela" else slug
    }

    dados_download_tabela_mortalidade <- reactive({
      info <- dados_tabela_mortalidade_download()
      df <- info$dados

      validate(need(nrow(df) > 0, "Não há dados para baixar no intervalo selecionado."))

      if (isTRUE(info$absoluto)) {
        return(df %>%
          dplyr::transmute(
            Ano = ano,
            Indicador = info$nome_valor,
            DRS = drs,
            RRAS = as.character(rras),
            `Região de Saúde` = regiao_saude,
            Município = municipio,
            !!info$nome_valor := valor
          ))
      }

      df %>%
        dplyr::transmute(
          Ano = ano,
          Indicador = info$nome_valor,
          DRS = drs,
          RRAS = as.character(rras),
          `Região de Saúde` = regiao_saude,
          Município = municipio,
          !!info$nome_numerador := numerador,
          !!info$nome_denominador := denominador,
          !!info$nome_valor := valor
        )
    })

    output$download_tabela_mortalidade_xlsx <- downloadHandler(
      filename = function() {
        info <- dados_tabela_mortalidade()
        anos <- anos_download_tabela_mortalidade()
        intervalo_anos <- paste0(min(anos), "_", max(anos))

        paste0(
          "series_mortalidade_",
          intervalo_anos,
          "_",
          slug_download_tabela_mortalidade(info$nome_valor),
          ".xlsx"
        )
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        info <- dados_tabela_mortalidade_download()
        df <- dados_download_tabela_mortalidade()

        wb <- openxlsx::createWorkbook()
        sheet_name <- "Tabela"
        header_style <- openxlsx::createStyle(
          textDecoration = "bold",
          fgFill = "#EAF0F7",
          border = "bottom"
        )

        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(
          wb,
          sheet_name,
          df,
          headerStyle = header_style,
          withFilter = TRUE
        )
        openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
        openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")

        if (nrow(df) > 0) {
          linhas_dados <- 2:(nrow(df) + 1)
          inteiro_style <- openxlsx::createStyle(numFmt = "#,##0")
          valor_style <- openxlsx::createStyle(
            numFmt = if (isTRUE(info$percentual)) "0.0%" else "0.0"
          )

          if (isTRUE(info$absoluto)) {
            inteiro_cols <- match(info$nome_valor, names(df))
          } else {
            inteiro_cols <- match(c(info$nome_numerador, info$nome_denominador), names(df))
            valor_col <- match(info$nome_valor, names(df))
            if (!is.na(valor_col)) {
              openxlsx::addStyle(
                wb,
                sheet_name,
                valor_style,
                rows = linhas_dados,
                cols = valor_col,
                gridExpand = TRUE,
                stack = TRUE
              )
            }
          }

          inteiro_cols <- inteiro_cols[!is.na(inteiro_cols)]
          if (length(inteiro_cols) > 0) {
            openxlsx::addStyle(
              wb,
              sheet_name,
              inteiro_style,
              rows = linhas_dados,
              cols = inteiro_cols,
              gridExpand = TRUE,
              stack = TRUE
            )
          }
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    outputOptions(output, "download_tabela_mortalidade_xlsx", suspendWhenHidden = FALSE)

    output$tabela_contexto_causa <- renderUI({
      req(input$tabela_indicador)

      if (!identical(input$tabela_indicador, "causas_especificas") ||
          !identical(input$tabela_tipo_causa, "indiretas")) {
        return(NULL)
      }

      NULL
    })

    agregador_indicador <- function(fator) {
      htmlwidgets::JS(
        paste0(
          "function(values, rows) {
            var numerador = 0;
            var denominador = 0;
            rows.forEach(function(row) {
              numerador += Number(row['numerador']) || 0;
              denominador += Number(row['denominador']) || 0;
            });
            if (denominador === 0) {
              return null;
            }
            return numerador / denominador * ", fator, ";
          }"
        )
      )
    }

    formatar_numero_ptbr <- function(valor) {
      if (length(valor) == 0 || is.na(valor)) {
        return("")
      }

      formatC(
        valor,
        format = "f",
        digits = 1,
        big.mark = ".",
        decimal.mark = ","
      )
    }

    output$tabela_mortalidade_detalhada <- reactable::renderReactable({
      info <- dados_tabela_mortalidade()
      df <- info$dados

      validate(need(nrow(df) > 0, "Não há dados para montar a tabela no período selecionado."))

      colunas_localidade <- list(
        drs = reactable::colDef(
          name = "DRS",
          minWidth = 150,
          aggregate = htmlwidgets::JS("function() { return '' }"),
          format = list(aggregated = reactable::colFormat(prefix = "Todas")),
          footer = "Total"
        ),
        rras = reactable::colDef(
          name = "RRAS",
          minWidth = 90,
          aggregate = htmlwidgets::JS("function() { return '' }"),
          format = list(aggregated = reactable::colFormat(prefix = "Todas"))
        ),
        regiao_saude = reactable::colDef(
          name = "Região de Saúde",
          minWidth = 170,
          aggregate = htmlwidgets::JS("function() { return '' }"),
          format = list(aggregated = reactable::colFormat(prefix = "Todas"))
        ),
        municipio = reactable::colDef(
          name = "Município",
          minWidth = 190,
          aggregate = htmlwidgets::JS("function() { return '' }"),
          format = list(aggregated = reactable::colFormat(prefix = "Todos"))
        )
      )

      if (isTRUE(info$absoluto)) {
        colunas_valores <- list(
          valor = reactable::colDef(
            name = info$nome_valor,
            aggregate = "sum",
            footer = formatar_numero_ptbr(sum(df$valor, na.rm = TRUE)),
            format = reactable::colFormat(
              digits = 1,
              separators = TRUE,
              locales = "pt-BR"
            )
          )
        )
      } else {
        numerador_total <- sum(df$numerador, na.rm = TRUE)
        denominador_total <- sum(df$denominador, na.rm = TRUE)
        valor_total <- if (denominador_total > 0) {
          numerador_total / denominador_total * info$fator
        } else {
          NA_real_
        }
        valor_footer <- if (isTRUE(info$percentual)) "" else valor_total

        colunas_valores <- list(
          numerador = reactable::colDef(
            name = info$nome_numerador,
            aggregate = "sum",
            footer = formatar_numero_ptbr(numerador_total),
            format = reactable::colFormat(
              digits = 1,
              separators = TRUE,
              locales = "pt-BR"
            )
          ),
          denominador = reactable::colDef(
            name = info$nome_denominador,
            aggregate = "sum",
            footer = formatar_numero_ptbr(denominador_total),
            format = reactable::colFormat(
              digits = 1,
              separators = TRUE,
              locales = "pt-BR"
            )
          ),
          valor = reactable::colDef(
            name = info$nome_valor,
            aggregate = agregador_indicador(info$fator),
            footer = if (identical(valor_footer, "")) {
              ""
            } else {
              formatar_numero_ptbr(valor_footer)
            },
            format = reactable::colFormat(
              digits = 1,
              separators = TRUE,
              percent = info$percentual,
              locales = "pt-BR"
            )
          )
        )
      }

      reactable::reactable(
        df,
        groupBy = c("drs", "rras", "regiao_saude"),
        columns = c(colunas_localidade, colunas_valores),
        defaultColDef = reactable::colDef(
          align = "center",
          footerStyle = list(fontWeight = "700")
        ),
        searchable = TRUE,
        sortable = TRUE,
        filterable = TRUE,
        resizable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        bordered = FALSE,
        pagination = FALSE,
        defaultExpanded = FALSE,
        height = 650,
        theme = reactable::reactableTheme(
          style = list(fontSize = "14px"),
          stripedColor = "#f8fafc",
          highlightColor = "#eef6ff",
          borderColor = "#e5e7eb",
          cellPadding = "8px 10px"
        ),
        rowStyle = htmlwidgets::JS(
          "function(rowInfo) {
            if (rowInfo.aggregated) {
              return { fontWeight: '700', background: '#f8fafc' };
            }
          }"
        )
      )
    })

    #------------------------------------------------
    # 8. Controle de exibição dos botões de alerta
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
    # 9. Renderização dos gráficos
    #------------------------------------------------
    cor_principal         <- "#0a1e3c"
    estilos_comparacao    <- list(
      list(cor = "#32a0ff", dashStyle = "Solid", marcador = "circle"),
      list(cor = "#0062cc", dashStyle = "Solid", marcador = "square")
    )
    cor_meta_ods          <- "#d71920"
    cor_referencia_estado <- "#596472"

    adicionar_serie_comparacao <- function(hc, dados, coluna_y, estilo) {
      dados_aux <- dados
      dados_aux$valor_comparacao <- dados_aux[[coluna_y]]

      hc %>%
        highcharter::hc_add_series(
          data = dados_aux,
          type = "line",
          highcharter::hcaes(x = ano, y = valor_comparacao, group = class),
          color = estilo$cor,
          dashStyle = estilo$dashStyle,
          lineWidth = 3,
          marker = list(
            enabled = TRUE,
            symbol = estilo$marcador,
            radius = 4,
            fillColor = estilo$cor,
            lineColor = "#ffffff",
            lineWidth = 1
          ),
          label = list(
            style = list(
              color = estilo$cor,
              fontWeight = "600"
            )
          ),
          states = list(
            hover = list(lineWidthPlus = 1)
          )
        )
    }

    # 8.1 Número de óbitos maternos
    output$plot_n_obitos <- highcharter::renderHighchart({
      validate(need(nrow(data_main()) > 0,
                    "Não há dados para o período/local selecionado."))

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
        )

      if (!ocultar_serie_estadual_principal()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_main(),
            type = "line",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais, group = class),
            color = cor_principal
          )
      }

      for (comparacao in data_comparacoes()) {
        validate(need(nrow(comparacao$dados) > 0, "Sem dados para a comparação selecionada."))
        if (!ocultar_serie_estadual_comparacao(comparacao$indice)) {
          hc <- adicionar_serie_comparacao(
            hc,
            comparacao$dados,
            "soma_obitos_mat_totais",
            estilos_comparacao[[comparacao$indice]]
          )
        }
      }

      if (mostrar_referencia_estado_n_obitos()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_ref(),
            type = "line",
            name = "Estado de SP (referência)",
            highcharter::hcaes(x = ano, y = soma_obitos_mat_totais),
            color = cor_referencia_estado,
            dashStyle = "ShortDot",
            lineWidth = 3,
            opacity = 1,
            zIndex = 2,
            label = list(
              enabled = TRUE,
              style = list(
                color = cor_referencia_estado,
                fontWeight = "600"
              )
            )
          )
      }

      hc %>% highcharter::hc_tooltip(shared = TRUE, sort = TRUE)
    })

    # 8.2 Razão de mortalidade materna por 100k nascidos vivos
    output$plot_rmm <- highcharter::renderHighchart({
      validate(need(nrow(data_main_rmm()) > 0,
                    "Não há dados para o período/local selecionado."))

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
        )

      hc <- hc %>%
        highcharter::hc_add_series(
          data = data_main_rmm(),
          type = "line",
          highcharter::hcaes(x = ano, y = rmm, group = class),
          color = cor_principal
        )

      for (comparacao in data_comparacoes()) {
        validate(need(nrow(comparacao$dados) > 0, "Sem dados para a comparação selecionada."))
        hc <- adicionar_serie_comparacao(
          hc,
          comparacao$dados,
          "rmm",
          estilos_comparacao[[comparacao$indice]]
        )
      }

      if (mostrar_linha_referencia()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_ref(),
            type = "line",
            name = "Referência (meta ODS)",
            highcharter::hcaes(x = ano, y = rmm),
            color = cor_meta_ods,
            dashStyle = "ShortDot",
            lineWidth = 3,
            opacity = 1,
            zIndex = 2,
            label = list(
              enabled = TRUE,
              style = list(
                color = cor_meta_ods,
                fontWeight = "600"
              )
            )
          )
      }

      hc %>% highcharter::hc_tooltip(shared = TRUE, sort = TRUE)
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
        )

      if (!ocultar_serie_estadual_principal()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_main(),
            type = "line",
            highcharter::hcaes(
              x = ano,
              y = prop_obitos_diretos,
              group = class
            ),
            color = cor_principal
          )
      }

      for (comparacao in data_comparacoes()) {
        if (!ocultar_serie_estadual_comparacao(comparacao$indice)) {
          hc <- adicionar_serie_comparacao(
            hc,
            comparacao$dados,
            "prop_obitos_diretos",
            estilos_comparacao[[comparacao$indice]]
          )
        }
      }

      if (mostrar_referencia_estado()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_ref(),
            type = "line",
            name = "Estado de SP (referência)",
            highcharter::hcaes(x = ano, y = prop_obitos_diretos),
            color = cor_referencia_estado,
            dashStyle = "ShortDot",
            lineWidth = 3,
            opacity = 1,
            zIndex = 2,
            label = list(
              enabled = TRUE,
              style = list(
                color = cor_referencia_estado,
                fontWeight = "600"
              )
            )
          )
      }

      hc %>% highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    # 8.4 % de óbitos por causas obstétricas indiretas
    output$plot_pct_indiretas <- highcharter::renderHighchart({
      validate(
        need(
          sum(data_prop_indiretas_main()$obitos_maternos, na.rm = TRUE) > 0,
          "Não foram registrados óbitos maternos no período/local selecionado."
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
        )

      if (!ocultar_serie_estadual_principal()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_prop_indiretas_main(),
            type = "line",
            highcharter::hcaes(
              x = ano,
              y = prop_obitos_indiretos,
              group = class
            ),
            color = cor_principal
          )
      }

      for (comparacao in data_prop_indiretas_comparacoes()) {
        if (!ocultar_serie_estadual_comparacao(comparacao$indice)) {
          hc <- adicionar_serie_comparacao(
            hc,
            comparacao$dados,
            "prop_obitos_indiretos",
            estilos_comparacao[[comparacao$indice]]
          )
        }
      }

      if (mostrar_referencia_estado()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_prop_indiretas_ref(),
            type = "line",
            name = "Estado de SP (referência)",
            highcharter::hcaes(x = ano, y = prop_obitos_indiretos),
            color = cor_referencia_estado,
            dashStyle = "ShortDot",
            lineWidth = 3,
            opacity = 1,
            zIndex = 2,
            label = list(
              enabled = TRUE,
              style = list(
                color = cor_referencia_estado,
                fontWeight = "600"
              )
            )
          )
      }

      hc %>% highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    # 8.5 % de óbitos por causa específica direta
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
        )

      if (!ocultar_serie_estadual_principal()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = df_main_aux,
            type = "line",
            highcharter::hcaes(x = ano, y = eixo, group = class),
            color = cor_principal
          )
      }

      for (comparacao in data_comparacoes()) {
        if (!ocultar_serie_estadual_comparacao(comparacao$indice)) {
          hc <- adicionar_serie_comparacao(
            hc,
            comparacao$dados,
            input$causa_especifica,
            estilos_comparacao[[comparacao$indice]]
          )
        }
      }

      if (mostrar_referencia_estado()) {
        df_ref_aux <- data_ref() %>%
          select(ano, eixo = !!sym(input$causa_especifica))
        hc <- hc %>%
          highcharter::hc_add_series(
            data = df_ref_aux,
            type = "line",
            name = "Estado de SP (referência)",
            highcharter::hcaes(x = ano, y = eixo),
            color = cor_referencia_estado,
            dashStyle = "ShortDot",
            lineWidth = 3,
            opacity = 1,
            zIndex = 2,
            label = list(
              enabled = TRUE,
              style = list(
                color = cor_referencia_estado,
                fontWeight = "600"
              )
            )
          )
      }

      hc %>% highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })

    # 8.6 % de óbitos por causa específica entre causas indiretas
    output$plot_pct_indiretas_especificas <- highcharter::renderHighchart({
      validate(
        need(
          sum(data_indiretas_main()$obitos_indiretos, na.rm = TRUE) > 0,
          "Não foram registrados óbitos maternos indiretos no período/local selecionado."
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
        )

      if (!ocultar_serie_estadual_principal()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_indiretas_main(),
            type = "line",
            highcharter::hcaes(
              x = ano,
              y = prop_obitos_indiretos_especifica,
              group = class
            ),
            color = cor_principal
          )
      }

      for (comparacao in data_indiretas_comparacoes()) {
        if (!ocultar_serie_estadual_comparacao(comparacao$indice)) {
          hc <- adicionar_serie_comparacao(
            hc,
            comparacao$dados,
            "prop_obitos_indiretos_especifica",
            estilos_comparacao[[comparacao$indice]]
          )
        }
      }

      if (mostrar_referencia_estado()) {
        hc <- hc %>%
          highcharter::hc_add_series(
            data = data_indiretas_ref(),
            type = "line",
            name = "Estado de SP (referência)",
            highcharter::hcaes(x = ano, y = prop_obitos_indiretos_especifica),
            color = cor_referencia_estado,
            dashStyle = "ShortDot",
            lineWidth = 3,
            opacity = 1,
            zIndex = 2,
            label = list(
              enabled = TRUE,
              style = list(
                color = cor_referencia_estado,
                fontWeight = "600"
              )
            )
          )
      }

      hc %>% highcharter::hc_tooltip(valueSuffix = "%", shared = TRUE, sort = TRUE)
    })
  })
}
