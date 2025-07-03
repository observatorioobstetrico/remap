# R/mod_robson_cesareas_server.R
#' Server: Robson & Cesáreas
#'
#' @param id módulo id
#' @param data_list lista de dados (load_indicadores_data())
#' @import shiny
#' @importFrom dplyr filter group_by summarise left_join arrange
#' @importFrom tidyr pivot_wider
#' @import reactable
#' @import plotly
#' @noRd
#' @export
mod_robson_cesareas_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Inicializa o numericInput de ano (dados >= 2014)
    observe({
      anos_disp <- sort(unique(data_list$robson_cesarea$ano))
      anos_disp <- anos_disp[anos_disp >= 2014]
      updateNumericInput(
        session, "fixedAno",
        value = max(anos_disp),
        min   = min(anos_disp),
        max   = max(anos_disp)
      )
    })

    # 2) UI dinâmica de filtros locais
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "Nacional"  = NULL,
             "Estadual"  = selectInput(
               ns("estado"), "Selecione o Estado:",
               choices = sort(unique(data_list$robson_cesarea$uf))
             ),
             "Municipal" = tagList(
               selectInput(
                 ns("estado_mun"), "Selecione o Estado:",
                 choices = sort(unique(data_list$robson_cesarea$uf))
               ),
               selectizeInput(
                 ns("municipio"), "Selecione o Município:",
                 choices = NULL
               )
             )
      )
    })

    # 3) Popula municípios quando Estado é escolhido
    observeEvent(input$estado_mun, {
      choices <- data_list$tabela_aux_municipios %>%
        filter(uf == input$estado_mun) %>%
        pull(municipio) %>%
        unique() %>%
        sort()
      updateSelectizeInput(session, "municipio", choices = choices)
    })

    # 4) Base filtrada: aplica filtros de nível, grupo e ANO fixo
    base_filtrada <- reactive({
      req(input$fixedAno)
      df <- data_list$robson_cesarea

      # filtro de local
      df <- switch(input$nivel,
                   "Estadual"  = filter(df, uf == input$estado),
                   "Municipal" = filter(df, uf == input$estado_mun, municipio == input$municipio),
                   df
      )

      # filtro de grupo e ano
      if (input$grupo == "Todos") {
        filter(df, ano == input$fixedAno)
      } else {
        filter(
          df,
          grupo_robson_aux == input$grupo,
          ano == input$fixedAno
        )
      }
    })

    # 5) Renderiza tabela (mesmo código teste)
    output$tabela_rc <- reactable::renderReactable({
      df <- base_filtrada()

      # sumarização conforme grupo
      if (input$grupo == "Todos") {
        rob <- df %>% group_by(grupo_robson_aux) %>% summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% group_by(grupo_robson_aux, tipo_parto) %>% summarise(n = sum(nascidos), .groups="drop") %>%
          pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- select(ces, -"vaginal")
        rob_ces <- left_join(rob, ces, by = "grupo_robson_aux")
      } else {
        # para grupo específico, a soma por ano único
        rob <- df %>% group_by(ano) %>% summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% group_by(ano, tipo_parto) %>% summarise(n = sum(nascidos), .groups="drop") %>%
          pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- select(ces, -"vaginal")
        rob_ces <- left_join(rob, ces, by = "ano")
      }

      # calcula faltante, cesarea e % cesáreas
      rob_ces <- rob_ces %>%
        mutate(
          faltante = if ("faltante" %in% names(.)) faltante else 0,
          cesarea  = if ("cesarea" %in% names(.)) cesarea else 0,
          `% cesáreas` = paste0(
            format(
              round(cesarea / (nascidos - faltante) * 100, 2),
              big.mark=".", decimal.mark=","
            ), "%"
          )
        )

      # ordena fator para exibir "faltante" ao final
      ord_levels <- rob_ces$grupo_robson_aux %>%
        unique() %>%
        setdiff("faltante") %>%
        as.numeric() %>%
        sort() %>%
        as.character()
      ord_levels <- c(ord_levels, "faltante")

      rob_ces <- rob_ces %>%
        mutate(grupo_robson_aux = factor(grupo_robson_aux, levels = ord_levels)) %>%
        arrange(grupo_robson_aux)

      reactable::reactable(
        rob_ces,
        defaultColDef = reactable::colDef(align = "center"),
        columns = list(
          grupo_robson_aux = reactable::colDef(name = if (input$grupo=="Todos") "Grupo de Robson" else "Ano"),
          nascidos         = reactable::colDef(name = "N° de nascimentos"),
          cesarea          = reactable::colDef(name = "N° de cesáreas"),
          faltante         = reactable::colDef(name = "N° sem informação"),
          `% cesáreas`     = reactable::colDef(name = "% cesáreas")
        ),
        highlight  = TRUE,
        bordered   = TRUE,
        pagination = FALSE
      )
    })

    # 6) Renderiza gráfico (mesmo código teste)
    output$grafico_rc <- plotly::renderPlotly({
      df <- base_filtrada()

      if (input$grupo == "Todos") {
        rob <- df %>% group_by(grupo_robson_aux) %>% summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% group_by(grupo_robson_aux, tipo_parto) %>% summarise(n = sum(nascidos), .groups="drop") %>%
          pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- select(ces, -"vaginal")
        rob_ces <- left_join(rob, ces, by = "grupo_robson_aux") %>%
          mutate(
            faltante = if ("faltante" %in% names(.)) faltante else 0,
            cesarea  = if ("cesarea" %in% names(.)) cesarea else 0
          )
        ord_levels <- rob_ces$grupo_robson_aux %>%
          unique() %>%
          setdiff("faltante") %>%
          as.numeric() %>%
          sort() %>%
          as.character()
        ord_levels <- c(ord_levels, "faltante")

        plot_df <- rob_ces %>%
          mutate(
            pct = (cesarea / (nascidos - faltante)) * 100,
            grupo_robson_aux = factor(grupo_robson_aux, levels = ord_levels)
          ) %>%
          filter(grupo_robson_aux != "faltante")

        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(
            x = grupo_robson_aux,
            y = pct,
            text = paste0(
              "Grupo: ", grupo_robson_aux, "<br>",
              "% cesáreas: ",
              formatC(pct, format="f", digits=2, decimal.mark=",", big.mark="."),
              "%"
            )
          )
        ) +
          ggplot2::geom_col(fill = "#37399a", color = "black") +
          ggplot2::labs(x = "Grupo de Robson", y = "% de cesáreas") +
          ggplot2::theme_linedraw()
      } else {
        rob <- df %>% group_by(ano) %>% summarise(nascidos = sum(nascidos), .groups="drop")
        ces <- df %>% group_by(ano, tipo_parto) %>% summarise(n = sum(nascidos), .groups="drop") %>%
          pivot_wider(names_from = tipo_parto, values_from = n, values_fill = 0)
        if ("vaginal" %in% names(ces)) ces <- select(ces, -"vaginal")
        rob_ces <- left_join(rob, ces, by = "ano") %>%
          mutate(
            faltante = if ("faltante" %in% names(.)) faltante else 0,
            cesarea  = if ("cesarea" %in% names(.)) cesarea else 0
          )

        plot_df <- rob_ces %>%
          mutate(pct = (cesarea / (nascidos - faltante)) * 100)

        p <- ggplot2::ggplot(
          plot_df,
          ggplot2::aes(
            x = as.factor(ano),
            y = pct,
            text = paste0(
              "Ano: ", ano, "<br>",
              "% cesáreas: ",
              formatC(pct, format="f", digits=2, decimal.mark=",", big.mark="."),
              "%"
            )
          )
        ) +
          ggplot2::geom_col(fill = "#37399a", color = "black") +
          ggplot2::labs(x = "Ano", y = "% de cesáreas") +
          ggplot2::theme_linedraw()
      }

      plotly::ggplotly(p, tooltip = "text") %>%
        plotly::layout(hovermode = "x unified")
    })
  })
}
