# R/mod_anomalias_server.R
#' Server: Anomalias Congênitas (SP)
#' @noRd
mod_anomalias_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      anos_disp <- sort(unique(data_list$sinasc$ano[data_list$sinasc$ano >= 2001]))
      updateSliderInput(session, "anos",
                        min=min(anos_disp), max=max(anos_disp),
                        value=c(min(anos_disp), max(anos_disp)))
    })

    output$filtros_locais <- renderUI({
      req(input$nivel)
      # switch(
      #   input$nivel,
      #   "ESTADUAL" = span(class = "text-muted", ""),
      #   "RRAS"     = selectInput(
      #     ns("rras"), "Selecione a RRAS:",
      #     choices  = data_list$rras_choices,
      #     selected = data_list$rras_choices[1]
      #   ),
      #   "DRS"      = selectInput(
      #     ns("drs"), "Selecione a DRS:",
      #     choices  = data_list$drs_choices,
      #     selected = data_list$drs_choices[1]
      #   ),
      #   "REGIÃO DE SAÚDE" = selectInput(
      #     ns("regiao_de_saude"), "Selecione a Região de Saúde:",
      #     choices  = data_list$regiao_saude_choices,
      #     selected = data_list$regiao_saude_choices[1]
      #   ),
      #   "MUNICIPAL" = selectInput(
      #     ns("municipio_sp"), "Selecione o Município:",
      #     choices  = data_list$municipios_sp_choices,
      #     selected = data_list$municipios_sp_choices[1]
      #   )
      # )
      switch(
        input$nivel,
        "ESTADUAL" = span(class = "text-muted", ""),

        "RRAS" = shinyWidgets::pickerInput(
          inputId = ns("rras"),
          label = "Selecione a RRAS:",
          choices  = data_list$rras_choices,
          options = list("live-search" = TRUE),
          selected = data_list$rras_choices[1]
        ),

        "DRS" = shinyWidgets::pickerInput(
          inputId = ns("drs"),
          label = "Selecione a DRS:",
          choices  = data_list$drs_choices,
          options = list("live-search" = TRUE),
          selected = data_list$drs_choices[1]
        ),

        "REGIÃO DE SAÚDE" = shinyWidgets::pickerInput(
          inputId = ns("regiao_de_saude"),
          label = "Selecione a Região de Saúde:",
          choices  = data_list$regiao_saude_choices,
          options = list("live-search" = TRUE),
          selected = data_list$regiao_saude_choices[1]
        ),

        "MUNICIPAL" = shinyWidgets::pickerInput(
          inputId = ns("municipio_sp"),
          label = "Selecione o Município:",
          choices  = data_list$municipios_sp_choices,
          options = list("live-search" = TRUE),
          selected = data_list$municipios_sp_choices[1]
        )
      )
    })

    filtra_local <- function(df) {
      switch(
        input$nivel,
        "ESTADUAL"        = df,
        "RRAS"            = { req(length(input$rras) > 0); dplyr::filter(df, rras == input$rras) },
        "DRS"             = { req(length(input$drs) > 0); dplyr::filter(df, drs == input$drs) },
        "REGIÃO DE SAÚDE" = { req(length(input$regiao_de_saude) > 0); dplyr::filter(df, regiao_de_saude == input$regiao_de_saude) },
        "MUNICIPAL"       = { req(length(input$municipio_sp) > 0); dplyr::filter(df, municipio_sp == input$municipio_sp) }
      )
    }

    output$tabela_an <- reactable::renderReactable({
      df_tab <- data_list$sinasc %>%
        dplyr::filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        filtra_local() %>%
        dplyr::mutate(anomalia = dplyr::if_else(ano < 2001, NA_real_, as.numeric(anomalia))) %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          nascidos  = sum(total_nascidos,    na.rm = TRUE),
          faltantes = sum(faltante_anomalia, na.rm = TRUE),
          n_anom    = sum(anomalia,          na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        dplyr::mutate(`% anomalias` = paste0(
          format(round(n_anom / pmax(nascidos - faltantes, 1) * 100, 2),
                 big.mark=".", decimal.mark=",", scientific=FALSE), "%"))

      validate(need(nrow(df_tab) > 0, "Sem registros para os filtros selecionados."))
      reactable::reactable(
        df_tab,
        defaultColDef = reactable::colDef(align = "center"),
        defaultSorted = "ano", defaultSortOrder = "desc",
        columns = list(
          ano           = reactable::colDef(name = "Ano"),
          nascidos      = reactable::colDef(name = "N° de nascimentos"),
          faltantes     = reactable::colDef(name = "N° sem informação"),
          n_anom        = reactable::colDef(name = "N° anomalias"),
          `% anomalias` = reactable::colDef(name = "% anomalias")
        ),
        highlight = TRUE, bordered = TRUE, pagination = FALSE
      )
    }) %>% bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos, cache = "app")

    output$grafico_an <- plotly::renderPlotly({
      df_plot <- data_list$sinasc %>%
        dplyr::filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        filtra_local() %>%
        dplyr::mutate(anomalia = dplyr::if_else(ano < 2001, NA_real_, as.numeric(anomalia))) %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          nascidos  = sum(total_nascidos,    na.rm = TRUE),
          faltantes = sum(faltante_anomalia, na.rm = TRUE),
          n_anom    = sum(anomalia,          na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        dplyr::mutate(pct = n_anom / pmax(nascidos - faltantes, 1))

      validate(need(nrow(df_plot) > 0, "Sem dados para exibir o gráfico."))

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = ano, y = pct,
          text = paste0(
            "Ano: ", ano, "<br>",
            "Nascimentos: ", format(nascidos, big.mark=".", decimal.mark=",", scientific=FALSE), "<br>",
            "Sem informação: ", format(faltantes, big.mark=".", decimal.mark=",", scientific=FALSE), "<br>",
            "N° anomalias: ", format(n_anom, big.mark=".", decimal.mark=",", scientific=FALSE), "<br>",
            "% anomalias: ", format(round(pct*100, 2), big.mark=".", decimal.mark=","), "%"
          )
        )
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(x = "Ano", y = "% de anomalias") +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background  = ggplot2::element_rect(fill = "white", color = NA),
          panel.grid.major = ggplot2::element_line(color = "grey90"),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(size=8, angle=90, vjust=0.5, hjust=1),
          axis.text.y      = ggplot2::element_text(size=10)
        ) +
        ggplot2::scale_x_continuous(breaks = df_plot$ano) +
        ggplot2::scale_y_continuous(labels = function(x) format(x*100, big.mark=".", decimal.mark=",", scientific=FALSE))

      plotly::ggplotly(p, tooltip = "text") %>% plotly::layout(hovermode = "x unified")
    }) %>% bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos, cache = "app")
  })
}
