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
    observe({
      anos <- sort(unique(data_list$estendido$ano))
      updateSliderInput(session, "selectAnoAC",
                        min   = min(anos),
                        max   = max(anos),
                        value = c(max(anos)-7, max(anos)))
    })

    # 2) UI dinâmica de filtros locais
    output$filtros_locais_ac <- renderUI({
      req(input$selectNivelAC)
      switch(input$selectNivelAC,
             "ESTADUAL"        = NULL,
             "RRAS"            = selectInput(ns("rras_AC"), "Selecione a RRAS:",
                                             choices = sort(unique(data_list$oficiais$rras))),
             "DRS"             = selectInput(ns("drs_AC"),  "Selecione a DRS:",
                                             choices = sort(unique(data_list$oficiais$drs))),
             "REGIÃO DE SAÚDE" = selectInput(ns("regiao_de_saude_AC"),
                                             "Selecione a Região de Saúde:",
                                             choices = sort(unique(data_list$oficiais$regiao_de_saude))),
             "MUNICIPAL"       = selectInput(ns("municipio_sp_AC"), "Selecione o Município:",
                                             choices = sort(unique(data_list$oficiais$municipio_sp)))
      )
    })

    # 3) Atualiza choices de variáveis e demográficos
    observe({
      df0 <- data_list$estendido %>%
        filter(ano >= input$selectAnoAC[1], ano <= input$selectAnoAC[2])

      if (input$selectNivelAC == "RRAS") {
        req(input$rras_AC); df0 <- filter(df0, rras == input$rras_AC)
      }
      if (input$selectNivelAC == "DRS") {
        req(input$drs_AC);  df0 <- filter(df0, drs == input$drs_AC)
      }
      if (input$selectNivelAC == "REGIÃO DE SAÚDE") {
        req(input$regiao_de_saude_AC)
        df0 <- filter(df0, regiao_de_saude == input$regiao_de_saude_AC)
      }
      if (input$selectNivelAC == "MUNICIPAL") {
        req(input$municipio_sp_AC)
        df0 <- filter(df0, municipio_sp == input$municipio_sp_AC)
      }

      variaveis <- c(
        "Ano do óbito"          = "ano",
        "Raça/cor"              = "racacor",
        "Estado civil"          = "est_civil",
        "Escolaridade"          = "escolaridade",
        "Local de ocorrência"      = "local_ocorrencia_obito",
        "Óbito em idade fértil" = "obito_em_idade_fertil",
        "Tipo de morte materna" = "tipo_de_morte_materna",
        "Período do óbito"      = "periodo_do_obito",
        "Assistência médica"    = "assistencia_med",
        "Necrópsia"             = "necropsia",
        "Capítulo CID-10"       = "capitulo_cid10",
        "Investigação por CMM"  = "investigacao_cmm"
      )

      if (input$selectNivelAC != "ESTADUAL") {
        removidos <- c("regiao","uf","rras","drs","regiao_de_saude","municipio_sp")
        variaveis <- variaveis[! names(variaveis) %in% removidos]
      }

      updateSelectInput(session, "selectVarLinhaAC",
                        choices = sort(variaveis), selected = "racacor")
      updateSelectInput(session, "selectVarColunaAC",
                        choices = sort(variaveis), selected = "ano")

      updateCheckboxGroupInput(session, "selectRacaAC",
                               choices  = sort(unique(df0$racacor)),
                               selected = sort(unique(df0$racacor))
      )
      updateCheckboxGroupInput(session, "selectCausasAC",
                               choices  = sort(unique(df0$tipo_de_morte_materna)),
                               selected = sort(unique(df0$tipo_de_morte_materna))
      )
      periodos <- unique(df0$periodo_do_obito)
      periodos <- ifelse(periodos %in% c("Inconsistente","Período inconsistente"),
                         "Período inconsistente", periodos)
      updateCheckboxGroupInput(session, "selectPeriodoAC",
                               choices  = sort(unique(periodos)),
                               selected = sort(unique(periodos))
      )
      invest_choices <- c(
        "Investigado por Comitê de Morte Materna"    = "Sim",
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

    # 4) Reativo principal
    dados_ac <- reactive({
      req(input$selectVarLinhaAC, input$selectVarColunaAC)

      df <- data_list$estendido %>%
        filter(
          ano >= input$selectAnoAC[1], ano <= input$selectAnoAC[2],
          idade >= input$selectIdadeAC[1], idade <= input$selectIdadeAC[2],
          racacor                %in% input$selectRacaAC,
          tipo_de_morte_materna  %in% input$selectCausasAC
        ) %>%
        mutate(
          periodo_do_obito = ifelse(
            periodo_do_obito %in% c("Inconsistente","Período inconsistente"),
            "Período inconsistente", periodo_do_obito
          ),
          investigacao_cmm = ifelse(
            is.na(investigacao_cmm), "Sem informação", investigacao_cmm
          )
        ) %>%
        filter(
          periodo_do_obito %in% input$selectPeriodoAC,
          investigacao_cmm %in% input$selectInvestigacaoAC
        )

      if (input$selectNivelAC == "RRAS") {
        df <- filter(df, rras == input$rras_AC)
      }
      if (input$selectNivelAC == "DRS") {
        df <- filter(df, drs == input$drs_AC)
      }
      if (input$selectNivelAC == "REGIÃO DE SAÚDE") {
        df <- filter(df, regiao_de_saude == input$regiao_de_saude_AC)
      }
      if (input$selectNivelAC == "MUNICIPAL") {
        df <- filter(df, municipio_sp == input$municipio_sp_AC)
      }

      df %>%
        mutate(obitos = 1L) %>%
        select(
          variavel_linha  = all_of(input$selectVarLinhaAC),
          variavel_coluna = all_of(input$selectVarColunaAC),
          obitos
        ) %>%
        group_by(variavel_linha, variavel_coluna) %>%
        summarise(obitos = sum(obitos), .groups = "drop") %>%
        mutate(
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

    # 6) Tabela cruzada
    output$tabela_ac <- render_gt({
      req(input$selectVarLinhaAC, input$selectVarColunaAC)

      df_full <- data_list$estendido %>%
        filter(
          ano >= input$selectAnoAC[1], ano <= input$selectAnoAC[2],
          idade >= input$selectIdadeAC[1], idade <= input$selectIdadeAC[2],
          racacor                %in% input$selectRacaAC,
          tipo_de_morte_materna  %in% input$selectCausasAC
        ) %>%
        mutate(
          periodo_do_obito = ifelse(
            periodo_do_obito %in% c("Inconsistente","Período inconsistente"),
            "Período inconsistente", periodo_do_obito
          ),
          investigacao_cmm = ifelse(
            is.na(investigacao_cmm), "Sem informação", investigacao_cmm
          )
        ) %>%
        filter(
          periodo_do_obito %in% input$selectPeriodoAC,
          investigacao_cmm %in% input$selectInvestigacaoAC
        )

      if (input$selectNivelAC == "RRAS") {
        df_full <- filter(df_full, rras == input$rras_AC)
      }
      if (input$selectNivelAC == "DRS") {
        df_full <- filter(df_full, drs == input$drs_AC)
      }
      if (input$selectNivelAC == "REGIÃO DE SAÚDE") {
        df_full <- filter(df_full, regiao_de_saude == input$regiao_de_saude_AC)
      }
      if (input$selectNivelAC == "MUNICIPAL") {
        df_full <- filter(df_full, municipio_sp == input$municipio_sp_AC)
      }

      # Correção: cria lista de labels com nomes dinâmicos
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

      # aqui fazemos a nomeação via [[ ]] para aceitar nomes vindos dos inputs
      label_list <- list()
      label_list[[ input$selectVarLinhaAC  ]] <- names(labels)[labels == input$selectVarLinhaAC]
      label_list[[ input$selectVarColunaAC ]] <- names(labels)[labels == input$selectVarColunaAC]  # :contentReference[oaicite:2]{index=2}

      set_gtsummary_theme(list(
        "style_number-arg:big.mark"     = ".",
        "style_number-arg:decimal.mark" = ","
      ))

      tbl_cross(
        df_full,
        row     = input$selectVarLinhaAC,
        col     = input$selectVarColunaAC,
        percent = input$selectPercentualAC,
        label   = label_list
      ) %>%
        bold_labels() %>%
        as_gt()
    })
  })
}
