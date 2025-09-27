# R/mod_robson_cesarias_server.R
#' Server: Robson & Cesáreas (SP)
#' @noRd
mod_robson_cesareas_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Semeia limites iniciais UMA ÚNICA VEZ (não depende de input$anos)
    observeEvent(TRUE, {
      anos_total <- sort(unique(data_list$robson_cesarea$ano))
      anos_total <- anos_total[anos_total >= 2014]

      updateNumericInput(
        session, "fixedAno",
        value = max(anos_total),
        min   = min(anos_total),
        max   = max(anos_total)
      )

      updateSliderInput(
        session, "anos",
        min = min(anos_total),
        max = max(anos_total),
        value = c(min(anos_total), max(anos_total)),
        step = 1
      )
    }, once = TRUE)  # <- roda só uma vez.

    # 2) Ao trocar nível/local, recalcula limites mas preserva o intervalo escolhido (sem depender de input$anos)
    observeEvent(
      list(input$nivel, input$rras, input$drs, input$regiao_de_saude, input$municipio_sp),
      {
        df_loc <- filtra_local(data_list$robson_cesarea)
        anos_disp <- sort(unique(df_loc$ano))
        anos_disp <- anos_disp[anos_disp >= 2014]
        if (length(anos_disp) == 0) return(NULL)

        new_min <- min(anos_disp); new_max <- max(anos_disp)
        cur <- isolate(input$anos)  # não cria dependência reativa.
        if (is.null(cur)) cur <- c(new_min, new_max)
        new_val <- c(max(new_min, cur[1]), min(new_max, cur[2]))

        updateSliderInput(session, "anos", min = new_min, max = new_max, value = new_val, step = 1)
        updateNumericInput(session, "fixedAno",
                           min = new_min, max = new_max,
                           value = min(max(new_val[2], new_min), new_max))
      },
      ignoreInit = TRUE
    )

    output$filtros_locais <- renderUI({
      req(input$nivel)
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

    # --- CORREÇÃO: filtra_local robusto a inputs vazios e usando %in% (mantém lógica de seleção) ---
    filtra_local <- function(df) {
      niv <- input$nivel
      if (is.null(niv)) return(df[0, , drop = FALSE])

      switch(
        niv,
        "ESTADUAL" = df,

        "RRAS" = {
          val <- input$rras
          if (is.null(val) || length(val) == 0) return(df[0, , drop = FALSE])
          dplyr::filter(df, rras %in% val)
        },

        "DRS" = {
          val <- input$drs
          if (is.null(val) || length(val) == 0) return(df[0, , drop = FALSE])
          dplyr::filter(df, drs %in% val)
        },

        "REGIÃO DE SAÚDE" = {
          val <- input$regiao_de_saude
          if (is.null(val) || length(val) == 0) return(df[0, , drop = FALSE])
          dplyr::filter(df, regiao_de_saude %in% val)
        },

        "MUNICIPAL" = {
          val <- input$municipio_sp
          if (is.null(val) || length(val) == 0) return(df[0, , drop = FALSE])
          dplyr::filter(df, municipio_sp %in% val)
        }
      )
    }
    # --- FIM DA CORREÇÃO ---

    # 3) Base filtrada: ano único para "Todos"; intervalo p/ grupo específico
    base_filtrada <- reactive({
      df <- data_list$robson_cesarea %>% filtra_local()
      if (identical(input$grupo, "Todos")) {
        req(input$fixedAno)
        dplyr::filter(df, ano == input$fixedAno)
      } else {
        req(input$anos)
        dplyr::filter(
          df,
          ano >= input$anos[1], ano <= input$anos[2],
          grupo_robson_aux == input$grupo
        )
      }
    }) %>% bindCache(
      input$nivel, input$rras, input$drs, input$regiao_de_saude,
      input$municipio_sp, input$grupo, input$fixedAno, input$anos,
      cache = "app"
    )

    # 4) Tabela
    output$tabela_rc <- reactable::renderReactable({
      df <- base_filtrada()

      if (identical(input$grupo, "Todos")) {
        rob <- df %>% dplyr::group_by(grupo_robson_aux) %>%
          dplyr::summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% dplyr::group_by(grupo_robson_aux, tipo_parto) %>%
          dplyr::summarise(n = sum(nascidos), .groups="drop") %>%
          tidyr::pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- dplyr::select(ces, -"vaginal")
        rob_ces <- dplyr::left_join(rob, ces, by = "grupo_robson_aux")
      } else {
        rob <- df %>% dplyr::group_by(ano) %>%
          dplyr::summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% dplyr::group_by(ano, tipo_parto) %>%
          dplyr::summarise(n = sum(nascidos), .groups="drop") %>%
          tidyr::pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- dplyr::select(ces, -"vaginal")
        rob_ces <- dplyr::left_join(rob, ces, by = "ano")
      }

      # Garante colunas
      if (!"faltante" %in% names(rob_ces)) rob_ces$faltante <- 0
      if (!"cesarea"  %in% names(rob_ces)) rob_ces$cesarea  <- 0

      rob_ces <- rob_ces %>%
        dplyr::mutate(`% cesáreas` =
                        paste0(format(round(cesarea / pmax(nascidos - faltante, 1) * 100, 2),
                                      big.mark=".", decimal.mark=","), "%"))

      if ("grupo_robson_aux" %in% names(rob_ces)) {
        ord_levels <- rob_ces$grupo_robson_aux %>% unique() %>%
          setdiff("faltante") %>% as.numeric() %>% sort() %>% as.character()
        ord_levels <- c(ord_levels, "faltante")
        rob_ces <- rob_ces %>%
          dplyr::mutate(grupo_robson_aux = factor(grupo_robson_aux, levels = ord_levels)) %>%
          dplyr::arrange(grupo_robson_aux)
      }

      reactable::reactable(
        rob_ces,
        defaultColDef = reactable::colDef(align = "center"),
        columns = list(
          grupo_robson_aux = reactable::colDef(
            name = if (identical(input$grupo,"Todos")) "Grupo de Robson" else "Ano"
          ),
          nascidos         = reactable::colDef(name = "N° de nascimentos"),
          cesarea          = reactable::colDef(name = "N° de cesáreas"),
          faltante         = reactable::colDef(name = "N° sem informação"),
          `% cesáreas`     = reactable::colDef(name = "% cesáreas")
        ),
        highlight = TRUE, bordered = TRUE, pagination = FALSE
      )
    }) %>% bindCache(base_filtrada(), cache = "app")

    # 5) Gráfico
    output$grafico_rc <- plotly::renderPlotly({
      df <- base_filtrada()

      if (identical(input$grupo, "Todos")) {
        rob <- df %>% dplyr::group_by(grupo_robson_aux) %>%
          dplyr::summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% dplyr::group_by(grupo_robson_aux, tipo_parto) %>%
          dplyr::summarise(n = sum(nascidos), .groups="drop") %>%
          tidyr::pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- dplyr::select(ces, -"vaginal")
        rob_ces <- dplyr::left_join(rob, ces, by = "grupo_robson_aux")

        if (!"faltante" %in% names(rob_ces)) rob_ces$faltante <- 0
        if (!"cesarea"  %in% names(rob_ces)) rob_ces$cesarea  <- 0

        ord_levels <- rob_ces$grupo_robson_aux %>% unique() %>%
          setdiff("faltante") %>% as.numeric() %>% sort() %>% as.character()
        ord_levels <- c(ord_levels, "faltante")

        plot_df <- rob_ces %>%
          dplyr::mutate(
            pct = (cesarea / pmax(nascidos - faltante, 1)) * 100,
            grupo_robson_aux = factor(grupo_robson_aux, levels = ord_levels)
          ) %>% dplyr::filter(grupo_robson_aux != "faltante")

        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(
            x = grupo_robson_aux, y = pct,
            text = paste0(
              "Grupo: ", grupo_robson_aux, "<br>",
              "% cesáreas: ", formatC(pct, format="f", digits=2, decimal.mark=",", big.mark="."), "%"
            )
          )
        ) +
          ggplot2::geom_col(fill = "#37399a", color = "black") +
          ggplot2::labs(x = "Grupo de Robson", y = "% de cesáreas") +
          ggplot2::theme_linedraw()

      } else {
        rob <- df %>% dplyr::group_by(ano) %>%
          dplyr::summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% dplyr::group_by(ano, tipo_parto) %>%
          dplyr::summarise(n = sum(nascidos), .groups="drop") %>%
          tidyr::pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- dplyr::select(ces, -"vaginal")
        rob_ces <- dplyr::left_join(rob, ces, by = "ano")

        if (!"faltante" %in% names(rob_ces)) rob_ces$faltante <- 0
        if (!"cesarea"  %in% names(rob_ces)) rob_ces$cesarea  <- 0

        plot_df <- rob_ces %>%
          dplyr::mutate(pct = (cesarea / pmax(nascidos - faltante, 1)) * 100)

        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(
            x = as.factor(ano), y = pct,
            text = paste0("Ano: ", ano, "<br>",
                          "% cesáreas: ", formatC(pct, format="f", digits=2, decimal.mark=",", big.mark="."), "%")
          )
        ) +
          ggplot2::geom_col(fill = "#37399a", color = "black") +
          ggplot2::labs(x = "Ano", y = "% de cesáreas") +
          ggplot2::theme_linedraw()
      }

      plotly::ggplotly(p, tooltip = "text") %>% plotly::layout(hovermode = "x unified")
    }) %>% bindCache(base_filtrada(), cache = "app")
  })
}
