#' Server: Partos Prematuros (SP) — versão blindada contra inputs "vazios"
#' @noRd
mod_prematuros_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -----------------------------------------------------------------------
    # Utilitário %||% (fallback simples)
    `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

    # Garante que as colunas esperadas existem (evita erros em filter/index)
    ensure_cols <- function(df, cols) {
      miss <- setdiff(cols, names(df))
      for (nm in miss) df[[nm]] <- NA_character_
      df
    }

    # -----------------------------------------------------------------------
    # 0) Base única + salvaguardas de tipo/colunas
    #    Espera-se: ano, total_nascidos, premat, faltante_premat,
    #               rras, drs, regiao_de_saude, municipio_sp
    # -----------------------------------------------------------------------
    df_base <- data_list$sinasc
    shiny::req(!is.null(df_base), nrow(df_base) > 0)

    df_base <- df_base %>%
      dplyr::mutate(
        ano               = suppressWarnings(as.integer(.data$ano)),
        total_nascidos    = dplyr::coalesce(suppressWarnings(as.numeric(.data$total_nascidos)),
                                            suppressWarnings(as.numeric(.data$nascidos))),
        premat            = dplyr::coalesce(suppressWarnings(as.numeric(.data$premat)), 0),
        faltante_premat   = dplyr::coalesce(suppressWarnings(as.numeric(.data$faltante_premat)), 0),
        rras              = trimws(.data$rras),
        drs               = trimws(.data$drs),
        regiao_de_saude   = trimws(.data$regiao_de_saude),
        municipio_sp      = trimws(.data$municipio_sp)
      ) %>%
      ensure_cols(c("rras","drs","regiao_de_saude","municipio_sp"))

    # -----------------------------------------------------------------------
    # 1) Slider de anos dinâmico
    # -----------------------------------------------------------------------
    observe({
      anos_disp <- sort(unique(stats::na.omit(df_base$ano)))
      shiny::req(length(anos_disp) > 0)
      updateSliderInput(
        session, "anos",
        min   = min(anos_disp),
        max   = max(anos_disp),
        value = c(min(anos_disp), max(anos_disp))
      )
    })

    # -----------------------------------------------------------------------
    # 2) UI de filtros locais
    # -----------------------------------------------------------------------
    output$filtros_locais <- shiny::renderUI({
      shiny::req(input$nivel)
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

    # -----------------------------------------------------------------------
    # 2.1) (Recomendado) Ao trocar o nível, limpar seleções incompatíveis
    #      (selected = character(0) é suportado oficialmente)
    # -----------------------------------------------------------------------
    observeEvent(input$nivel, {
      if (input$nivel != "RRAS")
        updateSelectInput(session, "rras", selected = character(0))
      if (input$nivel != "DRS")
        updateSelectInput(session, "drs", selected = character(0))
      if (input$nivel != "REGIÃO DE SAÚDE")
        updateSelectInput(session, "regiao_de_saude", selected = character(0))
      if (input$nivel != "MUNICIPAL")
        updateSelectInput(session, "municipio_sp", selected = character(0))
    }, ignoreInit = TRUE)

    # -----------------------------------------------------------------------
    # 3) Filtro local 100% seguro (base R + %in%)
    #    - Nunca usa '==' com vetores possivelmente vazios.
    #    - Quando faltam seleções exigidas, retorna 0 linhas; os renderizadores
    #      exibem mensagem via validate(need(...)).
    # -----------------------------------------------------------------------
    filtra_local <- function(df) {
      lvl <- input$nivel %||% "ESTADUAL"

      if (lvl == "ESTADUAL") {
        return(df)
      }

      if (lvl == "RRAS") {
        val <- input$rras %||% character(0)
        return(df[df$rras %in% val, , drop = FALSE])
      }

      if (lvl == "DRS") {
        val <- input$drs %||% character(0)
        return(df[df$drs %in% val, , drop = FALSE])
      }

      if (lvl == "REGIÃO DE SAÚDE") {
        val <- input$regiao_de_saude %||% character(0)
        return(df[df$regiao_de_saude %in% val, , drop = FALSE])
      }

      if (lvl == "MUNICIPAL") {
        val <- input$municipio_sp %||% character(0)
        return(df[df$municipio_sp %in% val, , drop = FALSE])
      }

      # caso 'nivel' venha diferente do esperado:
      df[0, , drop = FALSE]
    }

    # -----------------------------------------------------------------------
    # 4) Tabela
    # -----------------------------------------------------------------------
    output$tabela_pp <- reactable::renderReactable({
      shiny::req(shiny::isTruthy(input$nivel))
      shiny::req(!is.null(input$anos), length(input$anos) == 2)

      df_tab <- df_base
      # Corte temporal
      df_tab <- df_tab[df_tab$ano >= input$anos[1] & df_tab$ano <= input$anos[2], , drop = FALSE]
      # Filtro geográfico (seguro)
      df_tab <- filtra_local(df_tab)

      # Agregação
      df_tab <- df_tab %>%
        dplyr::group_by(.data$ano) %>%
        dplyr::summarise(
          nascidos   = sum(.data$total_nascidos,  na.rm = TRUE),
          faltantes  = sum(.data$faltante_premat, na.rm = TRUE),
          prematuros = sum(.data$premat,          na.rm = TRUE),
          .groups    = "drop"
        ) %>%
        dplyr::mutate(`% prematuros` = paste0(
          format(round(.data$prematuros / pmax(.data$nascidos - .data$faltantes, 1) * 100, 2),
                 big.mark = ".", decimal.mark = ",", scientific = FALSE), "%")
        ) %>%
        dplyr::arrange(dplyr::desc(.data$ano))

      validate(need(nrow(df_tab) > 0, ""))

      reactable::reactable(
        df_tab,
        defaultColDef    = reactable::colDef(align = "center"),
        defaultSorted    = "ano",
        defaultSortOrder = "desc",
        columns = list(
          ano            = reactable::colDef(name = "Ano"),
          nascidos       = reactable::colDef(name = "N° de nascimentos"),
          faltantes      = reactable::colDef(name = "N° sem informação"),
          prematuros     = reactable::colDef(name = "N° prematuros"),
          `% prematuros` = reactable::colDef(name = "% prematuros")
        ),
        highlight = TRUE, bordered = TRUE, pagination = FALSE
      )
    }) %>%
      bindCache(
        input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos,
        cache = "app"
      )

    # -----------------------------------------------------------------------
    # 5) Gráfico
    # -----------------------------------------------------------------------
    output$grafico_pp <- plotly::renderPlotly({
      shiny::req(shiny::isTruthy(input$nivel))
      shiny::req(!is.null(input$anos), length(input$anos) == 2)

      df_plot <- df_base
      df_plot <- df_plot[df_plot$ano >= input$anos[1] & df_plot$ano <= input$anos[2], , drop = FALSE]
      df_plot <- filtra_local(df_plot)

      df_plot <- df_plot %>%
        dplyr::group_by(.data$ano) %>%
        dplyr::summarise(
          nascidos   = sum(.data$total_nascidos,  na.rm = TRUE),
          faltantes  = sum(.data$faltante_premat, na.rm = TRUE),
          prematuros = sum(.data$premat,          na.rm = TRUE),
          .groups    = "drop"
        ) %>%
        dplyr::mutate(pct = .data$prematuros / pmax(.data$nascidos - .data$faltantes, 1))

      validate(need(nrow(df_plot) > 0, ""))

      p <- ggplot2::ggplot(
        df_plot,
        ggplot2::aes(
          x = .data$ano, y = .data$pct,
          text = paste0(
            "Ano: ", .data$ano, "<br>",
            "Nascidos: ",  format(.data$nascidos,  big.mark=".", decimal.mark=",", scientific=FALSE), "<br>",
            "Sem informação: ", format(.data$faltantes, big.mark=".", decimal.mark=",", scientific=FALSE), "<br>",
            "Prematuros: ", format(.data$prematuros, big.mark=".", decimal.mark=",", scientific=FALSE), "<br>",
            "% prematuros: ", format(round(.data$pct*100, 2), big.mark=".", decimal.mark=","), "%"
          )
        )
      ) +
        ggplot2::geom_col(fill = "#37399a", color = "black") +
        ggplot2::labs(x = "Ano", y = "% de prematuros") +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background  = ggplot2::element_rect(fill = "white", color = NA),
          panel.grid.major = ggplot2::element_line(color = "grey90"),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
          axis.text.y      = ggplot2::element_text(size = 10)
        ) +
        ggplot2::scale_x_continuous(breaks = df_plot$ano) +
        # Evita named vectors -> silencia aviso do jsonlite
        ggplot2::scale_y_continuous(labels = function(x) unname(
          format(x*100, big.mark=".", decimal.mark=",", scientific=FALSE)
        ))

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified")
    }) %>%
      bindCache(
        input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp, input$anos,
        cache = "app"
      )
  })
}
