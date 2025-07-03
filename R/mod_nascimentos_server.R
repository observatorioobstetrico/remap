# R/mod_nascimentos_server.R
#' Server: Nascimentos
#' @noRd
mod_nascimentos_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Atualiza slider de anos
    observe({
      anos_disp <- sort(unique(data_list$sinasc$ano))
      updateSliderInput(
        session, "anos",
        min   = min(anos_disp),
        max   = max(anos_disp),
        value = c(min(anos_disp), max(anos_disp))
      )
    })

    # 2) UI local conforme nível
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "Nacional"  = NULL,
             "Estadual"  = selectInput(
               ns("estado"), "Selecione o Estado:",
               choices = sort(unique(data_list$tabela_aux_municipios$uf))
             ),
             "Municipal" = tagList(
               selectInput(
                 ns("estado_mun"), "Selecione o Estado:",
                 choices = sort(unique(data_list$tabela_aux_municipios$uf))
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
      municipios <- data_list$tabela_aux_municipios %>%
        filter(uf == input$estado_mun) %>%
        pull(municipio) %>%
        unique() %>%
        sort()
      updateSelectizeInput(session, "municipio", choices = municipios)
    })

    # 4) Renderiza tabela com colunas centralizadas
    output$tabela_nascimentos <- reactable::renderReactable({
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
        group_by(ano) %>%
        summarise(nascidos = sum(nascidos, na.rm = TRUE), .groups = "drop")

      validate(need(nrow(df_tab) > 0, "Não existem registros para os filtros selecionados."))

      reactable::reactable(
        df_tab,
        defaultColDef = reactable::colDef(align = "center"),
        defaultSorted   = "ano",
        defaultSortOrder= "desc",
        columns = list(
          ano      = reactable::colDef(name = "Ano"),
          nascidos = reactable::colDef(name = "N° de nascimentos")
        ),
        highlight  = TRUE,
        bordered   = TRUE,
        pagination = FALSE
      )
    })

    # 5) Renderiza gráfico interativo com hover e formatação completa no eixo Y
    output$grafico_nascimentos <- plotly::renderPlotly({
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
        group_by(ano) %>%
        summarise(nascidos = sum(nascidos, na.rm = TRUE), .groups = "drop")

      validate(need(nrow(df_plot) > 0, "Não há dados para exibir o gráfico com esses filtros."))

      # Monta ggplot com tooltip text
      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = ano,
          y = nascidos,
          text = paste0(
            "Ano: ", ano, "<br>",
            "Nascidos: ",
            format(nascidos, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          )
        )
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(x = "Ano", y = "N° de nascimentos") +
        # fundo todo branco e grid leve
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background  = ggplot2::element_rect(fill = "white", color = NA),
          panel.grid.major = ggplot2::element_line(color = "grey90"),
          panel.grid.minor = ggplot2::element_blank(),
          # ticks do eixo X na vertical e fonte menor
          axis.text.x      = ggplot2::element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
          # ticks do eixo Y com fonte padrão
          axis.text.y      = ggplot2::element_text(size = 10)
        ) +
        ggplot2::scale_x_continuous(breaks = df_plot$ano)

      # Escala Y sem abreviações, mostrando valores completos
      p <- p + ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      )

      # Converte em plotly para hover
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified")
    })
  })
}
