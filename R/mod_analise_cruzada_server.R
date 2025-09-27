# R/mod_analise_cruzada_server.R
#' Server: Análise Cruzada de Óbitos Maternos (São Paulo)
#'
#' @param id módulo id
#' @param data_list lista de dados de óbitos (load_obitos_data())
#' @import shiny dplyr tidyr
#' @importFrom highcharter renderHighchart hchart hcaes hc_tooltip hc_xAxis hc_yAxis
#' @importFrom gtsummary tbl_cross bold_labels set_gtsummary_theme as_gt
#' @importFrom gt render_gt
#' @noRd
#' @export
mod_analise_cruzada_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Ajusta slider de anos
    # 1) Ajusta slider de anos (robusto a NA/vazio)
    observe({
      # Remove NAs e ordena
      anos <- sort(unique(na.omit(data_list$estendido$ano)))
      # Garante que exista pelo menos 1 ano antes de continuar
      req(length(anos) > 0)

      # Pega a janela final de até 7 anos; se houver menos, usa todo o intervalo
      min_ano <- min(anos)
      max_ano <- max(anos)
      start   <- if ((max_ano - 7) >= min_ano) max_ano - 7 else min_ano

      updateSliderInput(
        session, "selectAnoAC",
        min   = min_ano,
        max   = max_ano,
        value = c(start, max_ano)
      )
    })

    # 2) UI dinâmica de filtros locais (com seleção inicial segura para evitar inputs vazios)
    output$filtros_locais_ac <- renderUI({
      req(input$selectNivelAC)
      ofic <- data_list$oficiais

      # Helper: primeiro elemento seguro (NULL se choices estiver vazio)
      safe_first <- function(x) if (length(x) > 0) x[1] else NULL

      switch(
        input$selectNivelAC,
        "ESTADUAL" = NULL,

        "RRAS" = {
          # ---- Ordenação numérica de RRAS ----
          order_rras <- function(x) {
            x <- unique(na.omit(x))
            if (!length(x)) return(x)

            # Extrai o primeiro grupo de dígitos presente em cada rótulo (se houver)
            nums_chr <- sub(".*?(\\d+).*", "\\1", x)
            nums     <- suppressWarnings(as.integer(nums_chr))
            has_num  <- !is.na(nums)

            # Com número: ordena pelo número; Sem número: mantém no fim, em ordem alfabética
            with_num     <- x[has_num][order(nums[has_num], na.last = TRUE)]
            without_num  <- sort(x[!has_num], na.last = TRUE)

            c(with_num, without_num)
          }

          # Utilitário para seleção inicial segura
          safe_first <- function(v) if (length(v) > 0) v[1] else NULL

          choices <- order_rras(ofic$rras)

          # selectInput(
          #   ns("rras_AC"), "Selecione a RRAS:",
          #   choices  = choices,
          #   selected = safe_first(choices)
          # )
          shinyWidgets::pickerInput(
            inputId = ns("rras_AC"),
            label = "Selecione a RRAS:",
            choices = choices,
            options = list("live-search" = TRUE),
            selected = safe_first(choices)
          )
        },

        "DRS" = {
          choices <- sort(unique(na.omit(ofic$drs)))
          # selectInput(
          #   ns("drs_AC"), "Selecione a DRS:",
          #   choices  = choices,
          #   selected = safe_first(choices)
          # )
          shinyWidgets::pickerInput(
            inputId = ns("drs_AC"),
            label = "Selecione a DRS:",
            choices = choices,
            options = list("live-search" = TRUE),
            selected = safe_first(choices)
          )
        },

        "REGIÃO DE SAÚDE" = {
          choices <- sort(unique(na.omit(ofic$regiao_de_saude)))
          # selectInput(
          #   ns("regiao_de_saude_AC"), "Selecione a Região de Saúde:",
          #   choices  = choices,
          #   selected = safe_first(choices)
          # )
          shinyWidgets::pickerInput(
            inputId = ns("regiao_de_saude_AC"),
            label = "Selecione a Região de Saúde:",
            choices = choices,
            options = list("live-search" = TRUE),
            selected = safe_first(choices)
          )
        },

        "MUNICIPAL" = {
          choices <- sort(unique(na.omit(ofic$municipio_sp)))
          # selectInput(
          #   ns("municipio_sp_AC"), "Selecione o Município:",
          #   choices  = choices,
          #   selected = safe_first(choices)
          # )
          shinyWidgets::pickerInput(
            inputId = ns("municipio_sp_AC"),
            label = "Selecione o Município:",
            choices = choices,
            options = list("live-search" = TRUE),
            selected = safe_first(choices)
          )
        }
      )
    })

    # 3) Atualiza choices de variáveis e demográficos (robusto a NULL)
    observe({
      # Garante que o slider já esteja definido antes de filtrar por ano
      req(!is.null(input$selectAnoAC))

      df0 <- data_list$estendido %>%
        dplyr::filter(ano >= input$selectAnoAC[1], ano <= input$selectAnoAC[2])

      # Use 'identical()' para evitar 'if' com NA/NULL e 'req()' para o subfiltro
      if (identical(input$selectNivelAC, "RRAS")) {
        req(isTruthy(input$rras_AC))
        df0 <- dplyr::filter(df0, rras == input$rras_AC)
      }
      if (identical(input$selectNivelAC, "DRS")) {
        req(isTruthy(input$drs_AC))
        df0 <- dplyr::filter(df0, drs == input$drs_AC)
      }
      if (identical(input$selectNivelAC, "REGIÃO DE SAÚDE")) {
        req(isTruthy(input$regiao_de_saude_AC))
        df0 <- dplyr::filter(df0, regiao_de_saude == input$regiao_de_saude_AC)
      }
      if (identical(input$selectNivelAC, "MUNICIPAL")) {
        req(isTruthy(input$municipio_sp_AC))
        df0 <- dplyr::filter(df0, municipio_sp == input$municipio_sp_AC)
      }

      # Conjunto de variáveis disponível para cruzamentos
      variaveis <- c(
        "Ano do óbito"            = "ano",
        "Raça/cor"                = "racacor",
        "Estado civil"            = "est_civil",
        "Escolaridade"            = "escolaridade",
        "Local de ocorrência"     = "local_ocorrencia_obito",
        "Óbito em idade fértil"   = "obito_em_idade_fertil",
        "Tipo de morte materna"   = "tipo_de_morte_materna",
        "Período do óbito"        = "periodo_do_obito",
        "Assistência médica"      = "assistencia_med",
        "Necrópsia"               = "necropsia",
        "Capítulo CID-10"         = "capitulo_cid10",
        "Investigação por CMM"    = "investigacao_cmm"
      )

      # Esconder chaves territoriais quando não for ESTADUAL,
      # remova PELOS VALORES (não pelos nomes):
      if (!identical(input$selectNivelAC, "ESTADUAL")) {
        remover_vals <- c("regiao","uf","rras","drs","regiao_de_saude","municipio_sp")
        variaveis <- variaveis[!variaveis %in% remover_vals]
      }

      # Atualiza selects de linha/coluna (seleções padrão seguras)
      updateSelectInput(
        session, "selectVarLinhaAC",
        choices  = sort(variaveis),
        selected = if ("racacor" %in% variaveis) "racacor" else variaveis[[1]]
      )
      updateSelectInput(
        session, "selectVarColunaAC",
        choices  = sort(variaveis),
        selected = if ("ano" %in% variaveis) "ano" else variaveis[[1]]
      )

      # Atualiza filtros demográficos (ok receber vetores vazios)
      updateCheckboxGroupInput(
        session, "selectRacaAC",
        choices  = sort(unique(na.omit(df0$racacor))),
        selected = sort(unique(na.omit(df0$racacor)))
      )
      updateCheckboxGroupInput(
        session, "selectCausasAC",
        choices  = sort(unique(na.omit(df0$tipo_de_morte_materna))),
        selected = sort(unique(na.omit(df0$tipo_de_morte_materna)))
      )

      periodos <- unique(df0$periodo_do_obito)
      periodos <- ifelse(periodos %in% c("Inconsistente","Período inconsistente"),
                         "Período inconsistente", periodos)
      periodos <- sort(unique(na.omit(periodos)))
      updateCheckboxGroupInput(
        session, "selectPeriodoAC",
        choices  = periodos,
        selected = periodos
      )

      invest_choices <- c(
        "Investigado por Comitê de Morte Materna"     = "Sim",
        "Não investigado por Comitê de Morte Materna" = "Não",
        "Sem informação"                              = "Sem informação"
      )
      inv <- unique(df0$investigacao_cmm)
      inv[is.na(inv)] <- "Sem informação"
      sel_choices <- invest_choices[invest_choices %in% inv]

      updateCheckboxGroupInput(
        session, "selectInvestigacaoAC",
        choices  = sel_choices,
        selected = sel_choices
      )
    })

    # 4) Reativo principal (à prova de inputs vazios)
    dados_ac <- reactive({
      req(input$selectVarLinhaAC, input$selectVarColunaAC, input$selectAnoAC, input$selectIdadeAC)

      df <- data_list$estendido %>%
        dplyr::filter(
          ano   >= input$selectAnoAC[1], ano   <= input$selectAnoAC[2],
          idade >= input$selectIdadeAC[1], idade <= input$selectIdadeAC[2],
          racacor               %in% input$selectRacaAC,
          tipo_de_morte_materna %in% input$selectCausasAC
        ) %>%
        dplyr::mutate(
          periodo_do_obito = dplyr::if_else(
            periodo_do_obito %in% c("Inconsistente","Período inconsistente"),
            "Período inconsistente", periodo_do_obito
          ),
          investigacao_cmm = dplyr::if_else(is.na(investigacao_cmm), "Sem informação", investigacao_cmm)
        ) %>%
        dplyr::filter(
          periodo_do_obito %in% input$selectPeriodoAC,
          investigacao_cmm %in% input$selectInvestigacaoAC
        )

      # Filtros por nível com guarda de input
      if (identical(input$selectNivelAC, "RRAS")) {
        req(isTruthy(input$rras_AC))
        df <- dplyr::filter(df, rras == input$rras_AC)
      }
      if (identical(input$selectNivelAC, "DRS")) {
        req(isTruthy(input$drs_AC))
        df <- dplyr::filter(df, drs == input$drs_AC)
      }
      if (identical(input$selectNivelAC, "REGIÃO DE SAÚDE")) {
        req(isTruthy(input$regiao_de_saude_AC))
        df <- dplyr::filter(df, regiao_de_saude == input$regiao_de_saude_AC)
      }
      if (identical(input$selectNivelAC, "MUNICIPAL")) {
        req(isTruthy(input$municipio_sp_AC))
        df <- dplyr::filter(df, municipio_sp == input$municipio_sp_AC)
      }

      # Se ficar vazio, devolve um tibble compatível para o validate do gráfico
      if (nrow(df) == 0L) {
        return(tibble::tibble(
          variavel_linha = character(0),
          variavel_coluna = character(0),
          obitos = integer(0)
        ))
      }

      df %>%
        dplyr::mutate(obitos = 1L) %>%
        dplyr::select(
          variavel_linha  = dplyr::all_of(input$selectVarLinhaAC),
          variavel_coluna = dplyr::all_of(input$selectVarColunaAC),
          obitos
        ) %>%
        dplyr::group_by(variavel_linha, variavel_coluna) %>%
        dplyr::summarise(obitos = sum(obitos), .groups = "drop") %>%
        dplyr::mutate(
          variavel_linha  = as.character(variavel_linha),
          variavel_coluna = as.character(variavel_coluna)
        )
    })

    # 5) Gráfico cruzado
    output$grafico_ac <- renderHighchart({
      df <- dados_ac()
      validate(need(nrow(df) > 0, "Não existem registros para estes filtros."))
      n_cats <- length(unique(df$variavel_coluna))
      pal    <- colorRampPalette(c("#bdd5ea", "#0A1E3C"))(n_cats)
      highcharter::hchart(
        df, type = "column",
        highcharter::hcaes(
          x     = variavel_linha,
          y     = obitos,
          group = variavel_coluna
        )
      ) %>%
        highcharter::hc_colors(pal) %>%
        highcharter::hc_tooltip(valueSuffix = " óbitos") %>%
        highcharter::hc_xAxis(title = list(text = "")) %>%
        highcharter::hc_yAxis(title = list(text = "Óbitos"))
    })

    # 6) Tabela cruzada (com validação de vazio e guardas por nível)
    output$tabela_ac <- gt::render_gt({
      req(input$selectVarLinhaAC, input$selectVarColunaAC, input$selectAnoAC, input$selectIdadeAC)

      df_full <- data_list$estendido %>%
        dplyr::filter(
          ano   >= input$selectAnoAC[1], ano   <= input$selectAnoAC[2],
          idade >= input$selectIdadeAC[1], idade <= input$selectIdadeAC[2],
          racacor               %in% input$selectRacaAC,
          tipo_de_morte_materna %in% input$selectCausasAC
        ) %>%
        dplyr::mutate(
          periodo_do_obito = dplyr::if_else(
            periodo_do_obito %in% c("Inconsistente","Período inconsistente"),
            "Período inconsistente", periodo_do_obito
          ),
          investigacao_cmm = dplyr::if_else(is.na(investigacao_cmm), "Sem informação", investigacao_cmm)
        ) %>%
        dplyr::filter(
          periodo_do_obito %in% input$selectPeriodoAC,
          investigacao_cmm %in% input$selectInvestigacaoAC
        )

      if (identical(input$selectNivelAC, "RRAS")) {
        req(isTruthy(input$rras_AC))
        df_full <- dplyr::filter(df_full, rras == input$rras_AC)
      }
      if (identical(input$selectNivelAC, "DRS")) {
        req(isTruthy(input$drs_AC))
        df_full <- dplyr::filter(df_full, drs == input$drs_AC)
      }
      if (identical(input$selectNivelAC, "REGIÃO DE SAÚDE")) {
        req(isTruthy(input$regiao_de_saude_AC))
        df_full <- dplyr::filter(df_full, regiao_de_saude == input$regiao_de_saude_AC)
      }
      if (identical(input$selectNivelAC, "MUNICIPAL")) {
        req(isTruthy(input$municipio_sp_AC))
        df_full <- dplyr::filter(df_full, municipio_sp == input$municipio_sp_AC)
      }

      validate(need(nrow(df_full) > 0, "Não existem registros para estes filtros."))

      labels <- c(
        "Região do país"        = "regiao",
        "Ano do óbito"          = "ano",
        "Raça/cor"              = "racacor",
        "Estado civil"          = "est_civil",
        "Escolaridade"          = "escolaridade",
        "UF de residência"      = "uf",
        "Local ocorrência"      = "local_ocorrencia_obito",
        "Óbito em idade fértil" = "obito_em_idade_fertil",
        "Tipo de morte materna" = "tipo_de_morte_materna",
        "Período do óbito"      = "periodo_do_obito",
        "Assistência médica"    = "assistencia_med",
        "Necrópsia"             = "necropsia",
        "Capítulo CID-10"       = "capitulo_cid10",
        "Investigação por CMM"  = "investigacao_cmm"
      )

      label_list <- list()
      label_list[[ input$selectVarLinhaAC  ]] <- names(labels)[labels == input$selectVarLinhaAC]
      label_list[[ input$selectVarColunaAC ]] <- names(labels)[labels == input$selectVarColunaAC]

      gtsummary::set_gtsummary_theme(list(
        "style_number-arg:big.mark"     = ".",
        "style_number-arg:decimal.mark" = ","
      ))

      gtsummary::tbl_cross(
        df_full,
        row     = input$selectVarLinhaAC,
        col     = input$selectVarColunaAC,
        percent = input$selectPercentualAC,
        label   = label_list
      ) |>
        gtsummary::bold_labels() |>
        gtsummary::as_gt()
    })
  })
}
