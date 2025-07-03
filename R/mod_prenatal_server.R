# R/mod_prenatal_server.R
#' Server: Consultas de Pré-natal
#' @noRd
mod_prenatal_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dataset de consultas
    req(data_list$premat_cons)
    df_cons <- data_list$premat_cons

    # Atualiza slider de anos
    observe({
      anos_disp <- sort(unique(df_cons$ano))
      updateSliderInput(session, "anos",
                        min = min(anos_disp), max = max(anos_disp),
                        value = c(min(anos_disp), max(anos_disp)))
    })

    # Filtros locais
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "Nacional"  = NULL,
             "Estadual"  = selectInput(ns("estado"), "Selecione o Estado:",
                                       choices = sort(unique(df_cons$uf))),
             "Municipal" = tagList(
               selectInput(ns("estado_mun"), "Selecione o Estado:",
                           choices = sort(unique(df_cons$uf))),
               selectizeInput(ns("municipio"), "Selecione o Município:", choices = NULL)
             )
      )
    })

    # Popula municípios
    observeEvent(input$estado_mun, {
      muni <- data_list$tabela_aux_municipios %>%
        filter(uf == input$estado_mun) %>% pull(municipio) %>% unique() %>% sort()
      updateSelectizeInput(session, "municipio", choices = muni)
    })

    # Renderiza tabela
    output$tabela_cpn <- reactable::renderReactable({
      df_tab <- df_cons %>%
        filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        {
          if (input$nivel == "Estadual") filter(., uf == input$estado)
          else if (input$nivel == "Municipal")
            filter(., uf == input$estado_mun, municipio == input$municipio)
          else .
        } %>%
        group_by(ano) %>% summarise(
          nascidos     = sum(nascidos,           na.rm = TRUE),
          faltantes    = sum(faltante_consulta,  na.rm = TRUE),
          nenhuma      = sum(nenhuma_consulta,   na.rm = TRUE),
          uma_ate_seis = sum(consulta1,          na.rm = TRUE),
          sete_ou_mais = sum(consulta4,          na.rm = TRUE),
          .groups      = "drop"
        ) %>%
        mutate(
          `% nenhuma` = paste0(format(round(nenhuma      / (nascidos - faltantes) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%"),
          `% 1 a 6`   = paste0(format(round(uma_ate_seis / (nascidos - faltantes) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%"),
          `% 7+`      = paste0(format(round(sete_ou_mais / (nascidos - faltantes) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%")
        ) %>% arrange(desc(ano))

      validate(need(nrow(df_tab) > 0, "Sem registros para os filtros selecionados."))

      reactable::reactable(
        df_tab,
        defaultColDef    = reactable::colDef(align = "center"),
        defaultSorted    = "ano",
        defaultSortOrder = "desc",
        columns = list(
          ano          = reactable::colDef(name = "Ano"),
          nascidos     = reactable::colDef(name = "N° de nascimentos"),
          faltantes    = reactable::colDef(name = "N° sem informação"),
          nenhuma      = reactable::colDef(name = "N° nenhuma consulta"),
          uma_ate_seis = reactable::colDef(name = "N° 1 a 6 consultas"),
          sete_ou_mais = reactable::colDef(name = "N° 7+ consultas"),
          `% nenhuma`  = reactable::colDef(name = "% nenhuma consulta"),
          `% 1 a 6`    = reactable::colDef(name = "% 1 a 6 consultas"),
          `% 7+`       = reactable::colDef(name = "% 7+ consultas")
        ),
        highlight  = TRUE,
        bordered   = TRUE,
        pagination = FALSE
      )
    })

    # Renderiza gráfico único com hover apenas de porcentagem
    output$grafico_cpn <- plotly::renderPlotly({
      df_plot <- df_cons %>%
        filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        {
          if (input$nivel == "Estadual") filter(., uf == input$estado)
          else if (input$nivel == "Municipal")
            filter(., uf == input$estado_mun, municipio == input$municipio)
          else .
        } %>%
        group_by(ano) %>% summarise(
          nascidos     = sum(nascidos,          na.rm = TRUE),
          faltantes    = sum(faltante_consulta, na.rm = TRUE),
          nenhuma      = sum(nenhuma_consulta,  na.rm = TRUE),
          uma_ate_seis = sum(consulta1,         na.rm = TRUE),
          sete_ou_mais = sum(consulta4,         na.rm = TRUE),
          .groups      = "drop"
        )

      validate(need(nrow(df_plot) > 0, "Sem dados para exibir o gráfico."))

      y_col <- switch(input$serie,
                      "nenhuma" = "nenhuma",
                      "1_6"     = "uma_ate_seis",
                      "7plus"   = "sete_ou_mais")

      df_plot <- df_plot %>% mutate(pct = .data[[y_col]] / (nascidos - faltantes))

      # Gráfico ggplot com texto de tooltip formatado como porcentagem
      p <- ggplot2::ggplot(df_plot,
                           ggplot2::aes(
                             x = ano,
                             y = pct,
                             text = paste0(
                               format(round(pct * 100, 2), big.mark = ".", decimal.mark = ","),
                               "%"
                             )
                           )
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(
          x = "Ano",
          y = switch(
            input$serie,
            "nenhuma" = "% de nenhuma consulta",
            "1_6"     = "% de 1 a 6 consultas",
            "7plus"   = "% de 7+ consultas"
          )
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "grey90"),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1)
        ) +
        ggplot2::scale_x_continuous(breaks = df_plot$ano) +
        ggplot2::scale_y_continuous(
          labels = function(x) format(x * 100, big.mark = ".", decimal.mark = ",", scientific = FALSE)
        )

      # Plotly exibindo apenas o tooltip de texto (porcentagem)
      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(
          hovermode = "x unified",
          margin    = list(t = 40, b = 40, l = 40, r = 20)
        )
    })
  })
}
