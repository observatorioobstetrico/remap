# R/mod_nascimentos_server.R
#' Server: Indicadores Obstétricos → Nascimentos (SP)
#' @noRd
mod_nascimentos_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    library(dplyr)

    # 1) Header auxiliar
    output$ultima_atualizacao <- renderUI({
      if (!is.null(data_list$data_por_extenso)) {
        tags$p(
          style = "margin:0; font-size:13px; color:#555;",
          paste0("Última atualização: ", as.character(data_list$data_por_extenso))
        )
      }
    })

    # 2) Inicialização de anos
    observe({
      anos_disp <- sort(unique(na.omit(as.integer(data_list$sinasc$ano))))
      validate(need(length(anos_disp) > 0, "Não há anos disponíveis na base SINASC (SP)."))
      updateSliderInput(
        session, "anos",
        min   = min(anos_disp),
        max   = max(anos_disp),
        value = c(min(anos_disp), max(anos_disp))
      )
    })

    # 3) UI local dependente do nível
    output$filtros_locais <- renderUI({
      req(input$nivel)

      # helpers de escolhas com fallback
      get_rras <- function() {
        x <- data_list$rras_choices
        if (is.null(x) || !length(x)) {
          x <- sort(unique(na.omit(data_list$sinasc$rras)))
        }
        x
      }
      get_drs <- function() {
        x <- data_list$drs_choices
        if (is.null(x) || !length(x)) {
          x <- sort(unique(na.omit(data_list$sinasc$drs)))
        }
        x
      }
      get_rs <- function() {
        x <- data_list$regiao_saude_choices
        if (is.null(x) || !length(x)) {
          x <- sort(unique(na.omit(data_list$sinasc$regiao_de_saude)))
        }
        x
      }
      get_muni <- function() {
        x <- data_list$municipios_sp_choices
        if (is.null(x) || !length(x)) {
          x <- data_list$sinasc |>
            dplyr::mutate(municipio_sp = dplyr::if_else(!is.na(.data$municipio_sp), .data$municipio_sp, .data$municipio)) |>
            dplyr::filter(!is.na(.data$municipio_sp)) |>
            dplyr::distinct(.data$municipio_sp) |>
            dplyr::arrange(.data$municipio_sp) |>
            dplyr::pull(.data$municipio_sp)
        }
        x
      }

      switch(
        input$nivel,
        "ESTADUAL" = tagList(
          tags$p(HTML(""), style = "margin-bottom:0;")
        ),
        "RRAS" = {
          ch <- get_rras()
          # selectInput(ns("rras"), "Selecione a RRAS:", choices = ch, selected = ch[1])
          shinyWidgets::pickerInput(
            inputId = ns("rras"),
            label = "Selecione a RRAS:",
            choices = ch,
            options = list("live-search" = TRUE),
            selected = ch[1]
          )
        },
        "DRS" = {
          ch <- get_drs()
          # selectInput(ns("drs"), "Selecione a DRS:", choices = ch, selected = ch[1])
          shinyWidgets::pickerInput(
            inputId = ns("drs"),
            label = "Selecione a DRS:",
            choices = ch,
            options = list("live-search" = TRUE),
            selected = ch[1]
          )
        },
        "REGIÃO DE SAÚDE" = {
          ch <- get_rs()
          # selectInput(ns("regiao_de_saude"), "Selecione a Região de Saúde:", choices = ch, selected = ch[1])
          shinyWidgets::pickerInput(
            inputId = ns("regiao_de_saude"),
            label = "Selecione a Região de Saúde:",
            choices = ch,
            options = list("live-search" = TRUE),
            selected = ch[1]
          )
        },
        "MUNICIPAL" = {
          ch <- get_muni()
          # selectizeInput(
          #   ns("municipio_sp"), "Selecione o Município:",
          #   choices  = ch, selected = ch[1],
          #   options  = list(placeholder = "Digite para buscar...")
          # )
          shinyWidgets::pickerInput(
            inputId = ns("municipio_sp"),
            label = "Selecione o Município:",
            choices = ch,
            options = list("live-search" = TRUE)
          )
        }
      )
    })

    # 4) Reativo de filtragem (com cache)
    dados_filtrados <- reactive({
      req(input$anos)
      df <- data_list$sinasc %>%
        filter(!is.na(ano), ano >= input$anos[1], ano <= input$anos[2])

      validate(need(nrow(df) > 0, "Não existem registros para o intervalo de anos escolhido."))

      if (identical(input$nivel, "RRAS")) {
        req(shiny::isTruthy(input$rras))
        df <- df %>% dplyr::filter(.data$rras == input$rras)
      } else if (identical(input$nivel, "DRS")) {
        req(shiny::isTruthy(input$drs))
        df <- df %>% dplyr::filter(.data$drs == input$drs)
      } else if (identical(input$nivel, "REGIÃO DE SAÚDE")) {
        req(shiny::isTruthy(input$regiao_de_saude))
        df <- df %>% dplyr::filter(.data$regiao_de_saude == input$regiao_de_saude)
      } else if (identical(input$nivel, "MUNICIPAL")) {
        req(shiny::isTruthy(input$municipio_sp))
        df <- df %>%
          dplyr::mutate(municipio_alvo = dplyr::if_else(!is.na(.data$municipio_sp), .data$municipio_sp, .data$municipio)) %>%
          dplyr::filter(.data$municipio_alvo == input$municipio_sp)
      }
      df
    }) %>%
      bindCache(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos, cache = "app")

    # 5) Tabela (com cache)
    output$tabela_nascimentos <- reactable::renderReactable({
      library(reactable)
      df_tab <- dados_filtrados() %>%
        group_by(ano) %>%
        summarise(nascidos = sum(as.numeric(nascidos), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(ano))
      validate(need(nrow(df_tab) > 0, "Não existem registros para os filtros selecionados."))
      reactable::reactable(
        df_tab,
        defaultColDef    = reactable::colDef(align = "center"),
        defaultSorted    = "ano",
        defaultSortOrder = "desc",
        columns = list(
          ano      = reactable::colDef(name = "Ano"),
          nascidos = reactable::colDef(
            name = "Nº de nascimentos",
            format = reactable::colFormat(separators = TRUE, locales = "pt-BR")
          )
        ),
        searchable = TRUE, sortable = TRUE, filterable = FALSE,
        highlight  = TRUE, bordered   = TRUE, pagination = FALSE
      )
    }) %>% bindCache(dados_filtrados(), cache = "app")

    # 6) Gráfico (com cache)
    output$grafico_nascimentos <- plotly::renderPlotly({
      library(ggplot2); library(plotly)
      df_plot <- dados_filtrados() %>%
        group_by(ano) %>%
        summarise(nascidos = sum(as.numeric(nascidos), na.rm = TRUE), .groups = "drop") %>%
        arrange(ano)
      validate(need(nrow(df_plot) > 0, "Não há dados para exibir o gráfico com esses filtros."))

      rotulo_local <- switch(
        input$nivel,
        "ESTADUAL" = "São Paulo",
        "RRAS"     = req(input$rras),
        "DRS"      = req(input$drs),
        "REGIÃO DE SAÚDE" = req(input$regiao_de_saude),
        "MUNICIPAL"= req(input$municipio_sp),
        "São Paulo"
      )

      p <- ggplot(
        df_plot,
        aes(
          x = ano,
          y = nascidos,
          text = paste0(
            "Local: ", rotulo_local, "<br>",
            "Ano: ", ano, "<br>",
            "Nascidos: ",
            format(nascidos, big.mark = ".", decimal.mark = ",", scientific = FALSE)
          )
        )
      ) +
        geom_col(fill = "#37399a", color = "black") +
        labs(x = "Ano", y = "Nº de nascimentos") +
        scale_x_continuous(breaks = df_plot$ano) +
        scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background  = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank(),
          axis.text.x      = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y      = element_text(size = 10)
        )

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified")
    }) %>% bindCache(dados_filtrados(), cache = "app")
  })
}
