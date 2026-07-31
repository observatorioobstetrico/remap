#' Server: Acesso e Qualidade do Pré-Natal
#' @noRd
mod_indicadores_prenatal_acesso_server <- function(id, data_list) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    dados <- data_list$dados
    anos_exibidos <- 2022:2025

    opcoes_nivel <- function(nivel) {
      switch(
        nivel,
        "ESTADUAL" = c("Estado de São Paulo"),
        "DRS" = data_list$drs_choices,
        "RRAS" = data_list$rras_choices,
        "REGIAO" = data_list$regiao_saude_choices,
        "MUNICIPAL" = data_list$municipios_sp_choices,
        character()
      )
    }

    rotulo_nivel <- function(nivel) {
      switch(
        nivel,
        "ESTADUAL" = "Estado",
        "DRS" = "DRS",
        "RRAS" = "RRAS",
        "REGIAO" = "Região de Saúde",
        "MUNICIPAL" = "Município",
        nivel
      )
    }

    escolha_valida <- function(valor, escolhas) {
      length(valor) == 1L &&
        !is.na(valor) &&
        nzchar(valor) &&
        valor %in% escolhas
    }

    output$local_ui <- shiny::renderUI({
      shiny::req(input$nivel)
      escolhas <- opcoes_nivel(input$nivel)

      shinyWidgets::pickerInput(
        inputId = ns("local"),
        label = "Local de análise:",
        choices = escolhas,
        selected = if (length(escolhas)) escolhas[[1]] else NULL,
        options = list(
          "live-search" = length(escolhas) > 10L,
          "size" = 10
        )
      )
    })

    output$comparacao_ui <- shiny::renderUI({
      shiny::req(input$nivel_comparacao)

      if (identical(input$nivel_comparacao, "NENHUM")) {
        return(
          tags$div(
            class = "prenatal-comparison-disabled",
            tags$label(`for` = ns("comparacoes_placeholder"), "Comparar com:"),
            tags$div(
              id = ns("comparacoes_placeholder"),
              class = "form-control",
              "Selecione um nível"
            )
          )
        )
      }

      escolhas <- opcoes_nivel(input$nivel_comparacao)
      shinyWidgets::pickerInput(
        inputId = ns("comparacoes"),
        label = "Comparar com:",
        choices = escolhas,
        selected = NULL,
        multiple = TRUE,
        options = list(
          "live-search" = length(escolhas) > 10L,
          "actions-box" = FALSE,
          "selected-text-format" = "count > 2",
          "count-selected-text" = "{0} territórios selecionados",
          "none-selected-text" = "Selecione até 4 territórios",
          "max-options" = 4,
          "max-options-text" = "Selecione no máximo 4 territórios",
          "size" = 10
        )
      )
    })

    recortar_territorio <- function(df, nivel, local) {
      switch(
        nivel,
        "ESTADUAL" = df,
        "DRS" = dplyr::filter(df, .data$drs == local),
        "RRAS" = dplyr::filter(df, .data$rras == local),
        "REGIAO" = dplyr::filter(df, .data$regiao_de_saude == local),
        "MUNICIPAL" = dplyr::filter(df, .data$municipio_sp == local),
        df[0, , drop = FALSE]
      )
    }

    montar_serie <- function(nivel, local, serie_id, localidade) {
      agregado <- recortar_territorio(dados, nivel, local) %>%
        dplyr::filter(.data$ano %in% anos_exibidos) %>%
        dplyr::group_by(.data$ano) %>%
        dplyr::summarise(
          consultas_1_3 = sum(.data$consultas_1_3, na.rm = TRUE),
          consultas_4_5 = sum(.data$consultas_4_5, na.rm = TRUE),
          consultas_6_mais = sum(.data$consultas_6_mais, na.rm = TRUE),
          primeira_consulta = sum(.data$primeira_consulta, na.rm = TRUE),
          primeira_ate_12_semana = sum(.data$primeira_ate_12_semana, na.rm = TRUE),
          exames_ate_20_semana = sum(.data$exames_ate_20_semana, na.rm = TRUE),
          .groups = "drop"
        )

      tibble::tibble(ano = anos_exibidos) %>%
        dplyr::left_join(agregado, by = "ano") %>%
        dplyr::mutate(
          serie_id = serie_id,
          localidade = localidade,
          primeira_12_pct = dplyr::if_else(
            !is.na(.data$primeira_consulta) & .data$primeira_consulta > 0,
            .data$primeira_ate_12_semana / .data$primeira_consulta * 100,
            NA_real_
          ),
          exames_20_pct = dplyr::if_else(
            !is.na(.data$primeira_consulta) & .data$primeira_consulta > 0,
            .data$exames_ate_20_semana / .data$primeira_consulta * 100,
            NA_real_
          )
        )
    }

    serie_indicadores <- shiny::reactive({
      shiny::validate(
        shiny::need(nrow(dados) > 0L, "Os dados de pré-natal ainda não foram carregados.")
      )
      shiny::req(input$nivel)

      escolhas_principais <- opcoes_nivel(input$nivel)
      shiny::req(escolha_valida(input$local, escolhas_principais), cancelOutput = TRUE)

      especificacoes <- list(list(
        nivel = input$nivel,
        local = input$local,
        serie_id = paste(input$nivel, input$local, sep = "::"),
        localidade = if (identical(input$nivel, "ESTADUAL")) {
          "Estado de São Paulo"
        } else {
          paste0(rotulo_nivel(input$nivel), ": ", input$local)
        }
      ))

      nivel_comparacao <- input$nivel_comparacao
      if (!is.null(nivel_comparacao) && !identical(nivel_comparacao, "NENHUM")) {
        escolhas_comparacao <- opcoes_nivel(nivel_comparacao)
        locais_comparacao <- intersect(as.character(input$comparacoes), as.character(escolhas_comparacao))

        if (length(locais_comparacao)) {
          especificacoes <- c(
            especificacoes,
            lapply(locais_comparacao, function(local) {
              list(
                nivel = nivel_comparacao,
                local = local,
                serie_id = paste(nivel_comparacao, local, sep = "::"),
                localidade = if (identical(nivel_comparacao, "ESTADUAL")) {
                  "Estado de São Paulo"
                } else {
                  paste0(rotulo_nivel(nivel_comparacao), ": ", local)
                }
              )
            })
          )
        }
      }

      ids <- vapply(especificacoes, `[[`, character(1), "serie_id")
      especificacoes <- especificacoes[!duplicated(ids)]

      dplyr::bind_rows(lapply(especificacoes, function(especificacao) {
        montar_serie(
          nivel = especificacao$nivel,
          local = especificacao$local,
          serie_id = especificacao$serie_id,
          localidade = especificacao$localidade
        )
      }))
    })

    criar_grafico <- function(df, coluna, percentual = FALSE) {
      cores <- c("#0a1e3c", "#32a0ff", "#0062cc", "#596472", "#d71920")
      series <- unique(df$serie_id)

      grafico <- highcharter::highchart() %>%
        highcharter::hc_chart(
          type = "line",
          backgroundColor = "transparent",
          spacing = c(12, 10, 8, 6)
        ) %>%
        highcharter::hc_title(text = NULL) %>%
        highcharter::hc_xAxis(
          categories = as.character(anos_exibidos),
          title = list(text = "Ano", style = list(color = "#596472")),
          lineColor = "#cfd8e3",
          tickColor = "#cfd8e3",
          labels = list(style = list(color = "#384656", fontSize = "12px")),
          plotBands = list(list(
            from = 2.5,
            to = 3.5,
            color = "rgba(10, 30, 60, 0.055)",
            label = list(
              text = "Preliminar",
              align = "center",
              y = 12,
              style = list(color = "#596472", fontSize = "10px")
            )
          ))
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = if (percentual) 100 else NULL,
          tickInterval = if (percentual) 25 else NULL,
          title = list(
            text = if (percentual) "Percentual (%)" else "Número de gestantes",
            style = list(color = "#596472")
          ),
          labels = list(
            format = if (percentual) "{value}%" else "{value:,.0f}",
            style = list(color = "#384656", fontSize = "11px")
          ),
          gridLineColor = "#e9edf2"
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          valueDecimals = if (percentual) 1 else 0,
          valueSuffix = if (percentual) "%" else ""
        ) %>%
        highcharter::hc_plotOptions(
          series = list(connectNulls = FALSE),
          line = list(
            marker = list(enabled = TRUE, radius = 4, symbol = "circle"),
            lineWidth = 3
          )
        ) %>%
        highcharter::hc_legend(
          enabled = length(series) > 1L,
          align = "center",
          verticalAlign = "bottom",
          layout = "horizontal",
          itemStyle = list(color = "#384656", fontSize = "11px", fontWeight = "normal")
        ) %>%
        highcharter::hc_exporting(enabled = TRUE) %>%
        highcharter::hc_credits(enabled = FALSE)

      for (i in seq_along(series)) {
        valores <- dplyr::filter(df, .data$serie_id == series[[i]]) %>%
          dplyr::arrange(.data$ano)
        valores_ordenados <- valores[[coluna]][match(anos_exibidos, valores$ano)]

        grafico <- grafico %>%
          highcharter::hc_add_series(
            id = series[[i]],
            name = valores$localidade[[1]],
            data = as.numeric(valores_ordenados),
            color = cores[((i - 1L) %% length(cores)) + 1L],
            zIndex = if (i == 1L) 3 else 2
          )
      }

      grafico
    }

    output$grafico_consultas <- highcharter::renderHighchart({
      faixa_consultas <- shiny::req(input$faixa_consultas)
      shiny::validate(
        shiny::need(
          faixa_consultas %in% c("consultas_1_3", "consultas_4_5", "consultas_6_mais"),
          "Selecione uma quantidade de atendimentos."
        )
      )
      criar_grafico(serie_indicadores(), faixa_consultas)
    })
    output$grafico_primeira_12 <- highcharter::renderHighchart({
      criar_grafico(serie_indicadores(), "primeira_12_pct", percentual = TRUE)
    })
    output$grafico_exames_20 <- highcharter::renderHighchart({
      criar_grafico(serie_indicadores(), "exames_20_pct", percentual = TRUE)
    })
  })
}
