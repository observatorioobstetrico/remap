# R/mod_prenatal_server.R
#' Server: Consultas de Pré-natal (SP)
#' @noRd
mod_prenatal_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df_cons <- data_list$premat_cons
    req(df_cons)

    observe({
      anos_disp <- sort(unique(df_cons$ano))
      updateSliderInput(session, "anos",
                        min=min(anos_disp), max=max(anos_disp),
                        value=c(min(anos_disp), max(anos_disp)))
    })

    output$filtros_locais <- renderUI({
      req(input$nivel)
      # switch(
      #   input$nivel,
      #   "ESTADUAL" = span(class = "text-muted", ""),
      #   "RRAS"     = selectInput(ns("rras"), "Selecione a RRAS:",
      #                            choices = data_list$rras_choices,
      #                            selected = data_list$rras_choices[1]),
      #   "DRS"      = selectInput(ns("drs"), "Selecione a DRS:",
      #                            choices = data_list$drs_choices,
      #                            selected = data_list$drs_choices[1]),
      #   "REGIÃO DE SAÚDE" = selectInput(ns("regiao_de_saude"), "Selecione a Região de Saúde:",
      #                                   choices = data_list$regiao_saude_choices,
      #                                   selected = data_list$regiao_saude_choices[1]),
      #   "MUNICIPAL" = selectInput(ns("municipio_sp"), "Selecione o Município:",
      #                             choices = data_list$municipios_sp_choices,
      #                             selected = data_list$municipios_sp_choices[1])
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

    output$tabela_cpn <- reactable::renderReactable({
      df_tab <- df_cons %>%
        dplyr::filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        filtra_local() %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          nascidos     = sum(nascidos,          na.rm = TRUE),
          faltantes    = sum(faltante_consulta, na.rm = TRUE),
          nenhuma      = sum(nenhuma_consulta,  na.rm = TRUE),
          uma_ate_seis = sum(consulta1,         na.rm = TRUE),
          sete_ou_mais = sum(consulta4,         na.rm = TRUE),
          .groups      = "drop"
        ) %>%
        dplyr::mutate(
          `% nenhuma` = paste0(format(round(nenhuma      / pmax(nascidos - faltantes, 1) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%"),
          `% 1 a 6`   = paste0(format(round(uma_ate_seis / pmax(nascidos - faltantes, 1) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%"),
          `% 7+`      = paste0(format(round(sete_ou_mais / pmax(nascidos - faltantes, 1) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%")
        ) %>% dplyr::arrange(dplyr::desc(ano))

      validate(need(nrow(df_tab) > 0, "Sem registros para os filtros selecionados."))
      reactable::reactable(
        df_tab,
        defaultColDef    = reactable::colDef(align = "center"),
        defaultSorted    = "ano", defaultSortOrder = "desc",
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
        highlight = TRUE, bordered = TRUE, pagination = FALSE
      )
    }) %>% bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos, cache = "app")

    output$grafico_cpn <- plotly::renderPlotly({
      df_plot <- df_cons %>%
        dplyr::filter(ano >= input$anos[1], ano <= input$anos[2]) %>%
        filtra_local() %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(
          nascidos     = sum(nascidos,          na.rm = TRUE),
          faltantes    = sum(faltante_consulta, na.rm = TRUE),
          nenhuma      = sum(nenhuma_consulta,  na.rm = TRUE),
          uma_ate_seis = sum(consulta1,         na.rm = TRUE),
          sete_ou_mais = sum(consulta4,         na.rm = TRUE),
          .groups      = "drop"
        )

      y_col <- switch(input$serie,
                      "nenhuma" = "nenhuma",
                      "1_6"     = "uma_ate_seis",
                      "7plus"   = "sete_ou_mais")

      df_plot <- df_plot %>% dplyr::mutate(pct = .data[[y_col]] / pmax(nascidos - faltantes, 1))

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(x = ano, y = pct,
                     text = paste0(format(round(pct*100, 2), big.mark=".", decimal.mark=","), "%"))
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(
          x = "Ano",
          y = switch(input$serie,
                     "nenhuma" = "% de nenhuma consulta",
                     "1_6"     = "% de 1 a 6 consultas",
                     "7plus"   = "% de 7+ consultas")
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(color = "grey90"),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1)
        ) +
        ggplot2::scale_x_continuous(breaks = df_plot$ano) +
        ggplot2::scale_y_continuous(labels = function(x) format(x*100, big.mark=".", decimal.mark=",", scientific=FALSE))

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified", margin = list(t = 40, b = 40, l = 40, r = 20))
    }) %>% bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos, input$serie, cache = "app")
  })
}
