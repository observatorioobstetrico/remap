# R/mod_robson_server.R
#' Server: Classificação de Robson (SP)
#' @noRd
mod_robson_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      anos_disp <- sort(unique(data_list$robson$ano))
      anos_disp <- anos_disp[anos_disp >= 2014]
      updateNumericInput(session, "ano",
                         min = min(anos_disp), max = max(anos_disp),
                         value = max(anos_disp))
    })

    observeEvent(input$btn_robson_modal, {
      showModal(tagList(
        tags$style(HTML("
          .modal-dialog { width: 90vw !important; max-width: 90vw !important; }
          .modal-body { max-height: 90vh; overflow-y: auto; padding: 0 !important; }
        ")),
        modalDialog(
          title     = "Diagrama da Classificação de Robson",
          easyClose = TRUE,
          footer    = modalButton("Fechar"),
          tags$div(class = "modal-body",
                   style = "display:flex; justify-content:center; align-items:flex-start;",
                   tags$img(src = "www/robson.png", style = "width:100%; height:auto;"))
        )
      ))
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

    output$tabela_robson <- reactable::renderReactable({
      req(input$ano)
      df <- data_list$robson %>% dplyr::filter(ano == input$ano) %>% filtra_local()

      faltantes <- sum(df$nascidos[df$grupo_robson_aux == "faltante"], na.rm = TRUE)

      tab <- df %>%
        dplyr::group_by(grupo_robson_aux) %>%
        dplyr::summarise(nascidos = sum(nascidos), .groups = "drop") %>%
        dplyr::mutate(
          grupo_robson_aux = factor(grupo_robson_aux, levels = c(as.character(1:10), "faltante")),
          pct = dplyr::if_else(grupo_robson_aux == "faltante", NA_real_,
                               (nascidos / pmax(sum(nascidos) - faltantes, 1)) * 100)
        ) %>% dplyr::arrange(grupo_robson_aux)

      validate(need(nrow(tab) > 0, "Sem registros para os filtros selecionados."))
      reactable::reactable(
        tab,
        columns = list(
          grupo_robson_aux = reactable::colDef(name = "Grupo de Robson"),
          nascidos         = reactable::colDef(name = "N° de nascimentos"),
          pct              = reactable::colDef(
            name = "% de Robson",
            cell = function(value) {
              if (is.na(value)) return(NA)
              paste0(formatC(value, format = "f", digits = 2, decimal.mark = ",", big.mark = "."), "%")
            }
          )
        ),
        defaultColDef = reactable::colDef(align = "center", sortable = TRUE),
        highlight = TRUE, bordered = TRUE, pagination = FALSE
      )
    }) %>% bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$ano, cache = "app")

    output$grafico_robson <- plotly::renderPlotly({
      req(input$ano)
      df <- data_list$robson %>% dplyr::filter(ano == input$ano) %>% filtra_local()

      faltantes <- sum(df$nascidos[df$grupo_robson_aux == "faltante"], na.rm = TRUE)
      plot_df <- df %>%
        dplyr::group_by(grupo_robson_aux) %>%
        dplyr::summarise(nascidos = sum(nascidos), .groups = "drop") %>%
        dplyr::mutate(
          grupo_robson_aux = factor(grupo_robson_aux, levels = c(as.character(1:10), "faltante")),
          pct = dplyr::if_else(grupo_robson_aux == "faltante", NA_real_,
                               (nascidos / pmax(sum(nascidos) - faltantes, 1)) * 100)
        ) %>%
        dplyr::filter(!is.na(pct))

      validate(need(nrow(plot_df) > 0, "Sem dados para exibir o gráfico."))

      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = grupo_robson_aux, y = pct,
          text = paste0("% de Robson: ", formatC(pct, format="f", digits=2, decimal.mark=",", big.mark="."), "%")
        )
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(x = "Grupo de Robson", y = "% de Robson") +
        ggplot2::theme_linedraw()

      plotly::ggplotly(p, tooltip = "text") %>% plotly::layout(hovermode = "x unified")
    }) %>% bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$ano, cache = "app")
  })
}
