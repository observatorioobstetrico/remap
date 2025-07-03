# R/mod_robson_server.R
#' Server: Classificação de Robson – modal com scroll vertical
#' @noRd
mod_robson_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Inicializa o input de ano (dados >= 2014)
    observe({
      anos_disp <- sort(unique(data_list$robson$ano))
      anos_disp <- anos_disp[anos_disp >= 2014]
      updateNumericInput(
        session, "ano",
        min   = min(anos_disp),
        max   = max(anos_disp),
        value = max(anos_disp)
      )
    })

    # 2) Exibe modal com scroll vertical para diagrama ocupar grande tamanho
    observeEvent(input$btn_robson_modal, {
      showModal(
        tagList(
          # CSS customizado: diálogo largo e body com scroll
          tags$style(HTML("
            .modal-dialog {
              width: 90vw !important;
              max-width: 90vw !important;
            }
            .modal-body {
              max-height: 90vh;
              overflow-y: auto;
              padding: 0 !important;
            }
          ")),
          modalDialog(
            title     = "Diagrama da Classificação de Robson",
            easyClose = TRUE,
            footer    = modalButton("Fechar"),
            # Container para scroll vertical e centralização
            tags$div(
              class = "modal-body",
              style = "display: flex; justify-content: center; align-items: flex-start;",
              tags$img(
                src   = "www/robson.png",
                style = "width: 100%; height: auto;"
              )
            )
          )
        )
      )
    })

    # 3) UI dinâmica de filtros locais
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "Nacional"  = NULL,
             "Estadual"  = selectInput(
               ns("estado"), "Selecione o Estado:",
               choices = sort(unique(data_list$robson$uf))
             ),
             "Municipal" = tagList(
               selectInput(
                 ns("estado_mun"), "Selecione o Estado:",
                 choices = sort(unique(data_list$robson$uf))
               ),
               selectizeInput(
                 ns("municipio"), "Selecione o Município:",
                 choices = NULL
               )
             )
      )
    })

    # 4) Popula municípios quando o nível for Municipal
    observeEvent(input$estado_mun, {
      mun_choices <- data_list$tabela_aux_municipios %>%
        dplyr::filter(uf == input$estado_mun) %>%
        dplyr::pull(municipio) %>%
        unique() %>%
        sort()
      updateSelectizeInput(session, "municipio", choices = mun_choices)
    })

    # 5) Renderiza tabela de Classificação de Robson
    output$tabela_robson <- reactable::renderReactable({
      req(input$ano)

      df <- data_list$robson %>%
        dplyr::filter(ano == input$ano) %>%
        {
          if (input$nivel == "Estadual") {
            dplyr::filter(., uf == input$estado)
          } else if (input$nivel == "Municipal") {
            dplyr::filter(., uf == input$estado_mun, municipio == input$municipio)
          } else {
            .
          }
        }

      faltantes <- sum(df$nascidos[df$grupo_robson_aux == "faltante"], na.rm = TRUE)

      tab <- df %>%
        dplyr::group_by(grupo_robson_aux) %>%
        dplyr::summarise(nascidos = sum(nascidos), .groups = "drop") %>%
        dplyr::mutate(
          grupo_robson_aux = factor(
            grupo_robson_aux,
            levels = c(as.character(1:10), "faltante")
          ),
          pct = ifelse(
            grupo_robson_aux == "faltante",
            NA,
            (nascidos / (sum(nascidos) - faltantes)) * 100
          )
        ) %>%
        dplyr::arrange(grupo_robson_aux)

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
              paste0(
                formatC(
                  value,
                  format = "f",
                  digits = 2,
                  decimal.mark = ",",
                  big.mark = "."
                ),
                "%"
              )
            }
          )
        ),
        defaultColDef = reactable::colDef(
          align    = "center",
          sortable = TRUE
        ),
        highlight  = TRUE,
        bordered   = TRUE,
        pagination = FALSE
      )
    })

    # 6) Renderiza gráfico interativo de % de Robson
    output$grafico_robson <- plotly::renderPlotly({
      req(input$ano)

      df <- data_list$robson %>%
        dplyr::filter(ano == input$ano) %>%
        {
          if (input$nivel == "Estadual") {
            dplyr::filter(., uf == input$estado)
          } else if (input$nivel == "Municipal") {
            dplyr::filter(., uf == input$estado_mun, municipio == input$municipio)
          } else {
            .
          }
        }

      faltantes <- sum(df$nascidos[df$grupo_robson_aux == "faltante"], na.rm = TRUE)
      plot_df <- df %>%
        dplyr::group_by(grupo_robson_aux) %>%
        dplyr::summarise(nascidos = sum(nascidos), .groups = "drop") %>%
        dplyr::mutate(
          grupo_robson_aux = factor(
            grupo_robson_aux,
            levels = c(as.character(1:10), "faltante")
          ),
          pct = ifelse(
            grupo_robson_aux == "faltante", NA,
            (nascidos / (sum(nascidos) - faltantes)) * 100
          )
        ) %>%
        dplyr::filter(!is.na(pct))

      validate(need(nrow(plot_df) > 0, "Sem dados para exibir o gráfico."))

      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = grupo_robson_aux,
          y = pct,
          text = paste0(
            "% de Robson: ",
            formatC(
              pct,
              format = "f",
              digits = 2,
              decimal.mark = ",",
              big.mark = "."
            ),
            "%"
          )
        )
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(x = "Grupo de Robson", y = "% de Robson") +
        ggplot2::theme_linedraw()

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified")
    })
  })
}
