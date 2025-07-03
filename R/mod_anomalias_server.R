# R/mod_anomalias_server.R
#' Server: Anomalias Congênitas
#' @noRd
mod_anomalias_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Atualiza slider apenas com anos ≥2001
    observe({
      anos_disp <- sort(unique(data_list$sinasc$ano[data_list$sinasc$ano >= 2001]))
      updateSliderInput(
        session, "anos",
        min   = min(anos_disp),
        max   = max(anos_disp),
        value = c(min(anos_disp), max(anos_disp))
      )
    })

    # 2) UI de filtros locais
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "Nacional"  = NULL,
             "Estadual"  = selectInput(
               ns("estado"), "Selecione o Estado:",
               choices = sort(unique(data_list$sinasc$uf))
             ),
             "Municipal" = tagList(
               selectInput(
                 ns("estado_mun"), "Selecione o Estado:",
                 choices = sort(unique(data_list$sinasc$uf))
               ),
               selectizeInput(
                 ns("municipio"), "Selecione o Município:",
                 choices = NULL
               )
             )
      )
    })

    # 3) Popula municípios para Municipal
    observeEvent(input$estado_mun, {
      choices <- data_list$tabela_aux_municipios %>%
        filter(uf == input$estado_mun) %>%
        pull(municipio) %>%
        unique() %>%
        sort()
      updateSelectizeInput(session, "municipio", choices = choices)
    })

    # 4) Renderiza tabela com Nascidos, Sem informação, N° anomalias e % anomalias
    output$tabela_an <- reactable::renderReactable({
      df_tab <- data_list$sinasc %>%
        filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        {
          if (input$nivel == "Estadual") {
            filter(., uf == input$estado)
          } else if (input$nivel == "Municipal") {
            filter(., uf == input$estado_mun, municipio == input$municipio)
          } else {
            .
          }
        } %>%
        # marca anomalias só a partir de 2001
        mutate(
          anomalia = ifelse(ano < 2001, NA, as.numeric(anomalia)),
          total    = total_nascidos - faltante_anomalia
        ) %>%
        group_by(ano) %>%
        summarise(
          nascidos     = sum(total_nascidos,    na.rm = TRUE),
          faltantes    = sum(faltante_anomalia, na.rm = TRUE),
          n_anom       = sum(anomalia,          na.rm = TRUE),
          .groups      = "drop"
        ) %>%
        # formata % anomalias com vírgula
        mutate(
          `% anomalias` = paste0(
            format(
              round(n_anom / (nascidos - faltantes) * 100, 2),
              big.mark     = ".",   # separador de milhar
              decimal.mark = ",",   # separador decimal
              scientific   = FALSE
            ),
            "%"
          )
        )

      validate(need(nrow(df_tab) > 0, "Sem registros para os filtros selecionados."))

      reactable::reactable(
        df_tab,
        defaultColDef = reactable::colDef(align = "center"),
        defaultSorted   = "ano",
        defaultSortOrder= "desc",
        columns = list(
          ano           = reactable::colDef(name = "Ano"),
          nascidos      = reactable::colDef(name = "N° de nascimentos"),
          faltantes     = reactable::colDef(name = "N° sem informação"),
          n_anom        = reactable::colDef(name = "N° anomalias"),
          `% anomalias` = reactable::colDef(name = "% anomalias")
        ),
        highlight  = TRUE,
        bordered   = TRUE,
        pagination = FALSE
      )
    })

    # 5) Renderiza gráfico interativo de % anomalias
    output$grafico_an <- plotly::renderPlotly({
      df_plot <- data_list$sinasc %>%
        filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        {
          if (input$nivel == "Estadual") {
            filter(., uf == input$estado)
          } else if (input$nivel == "Municipal") {
            filter(., uf == input$estado_mun, municipio == input$municipio)
          } else {
            .
          }
        } %>%
        mutate(
          anomalia = ifelse(ano < 2001, NA, as.numeric(anomalia)),
          total    = total_nascidos - faltante_anomalia
        ) %>%
        group_by(ano) %>%
        summarise(
          nascidos  = sum(total_nascidos,    na.rm = TRUE),
          faltantes = sum(faltante_anomalia, na.rm = TRUE),
          n_anom    = sum(anomalia,          na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(pct = n_anom / (nascidos - faltantes))

      validate(need(nrow(df_plot) > 0, "Sem dados para exibir o gráfico com esses filtros."))

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x    = ano,
          y    = pct,
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
        ggplot2::scale_y_continuous(
          labels = function(x) format(x*100, big.mark=".", decimal.mark=",", scientific=FALSE)
        )

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified")
    })

  })
}
