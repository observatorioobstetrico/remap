# R/mod_robson_cesareas_ui.R
#' UI: Robson & Cesáreas (SP)
#' @noRd
mod_robson_cesareas_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(12, tags$div(class = "panel-title-custom", "Robson & Cesáreas"))
    ),
    fluidRow(
      column(
        width = 4,
        bs4Dash::box(
          title = "Filtros", status = "primary", solidHeader = TRUE,
          width = NULL, height = "109vh",
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",
            tags$p(style = "font-size:16px; font-style:italic;",
                   "Obs: os dados de 2025 são preliminares."),
            hr(),
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices  = c("ESTADUAL","RRAS","DRS","REGIÃO DE SAÚDE","MUNICIPAL"),
              selected = "ESTADUAL"
            ),
            uiOutput(ns("filtros_locais")), hr(),

            tags$h5(class = "section-header", "Selecione o grupo de Robson:"),
            selectInput(ns("grupo"), NULL, choices = c("Todos", as.character(1:10)), selected = "Todos"),
            hr(),

            # Ano único quando "Todos"
            conditionalPanel(
              condition = sprintf("input['%s'] === 'Todos'", ns("grupo")),
              tags$h5(class = "section-header", "Selecione o ano:"),
              numericInput(ns("fixedAno"), NULL, value = NA, min = 2014, max = 2025, step = 1)
            ),

            # Intervalo quando grupo específico
            conditionalPanel(
              condition = sprintf("input['%s'] !== 'Todos'", ns("grupo")),
              tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
              sliderInput(ns("anos"), NULL,
                          min = 2014, max = 2025,
                          value = c(2014, 2025),
                          step = 1, sep = "")
            ),
            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações adicionais:"),

            tags$p(HTML(
              "Este menu é similar à análise do menu <code>Classificação de Robson</code>, com
              a diferença que informações sobre <code>parto cesárea</code> foram acrescentadas."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <code>porcentagem de partos cesáreas dentro de um grupo
                Robson de interesse</code> é dada por:<br/>
                $$\\%\\,\\text{cesáreas}=
                \\frac{N^{\\text{o}}\\,\\text{partos cesáreas}}
                {(N^{\\text{o}}\\,\\text{nascimentos}-N^{\\text{o}}\\,\\text{sem informação})}
                \\times 100.$$"
              ))
            )
          )
        )
      ),
      column(
        width = 8,
        bs4Dash::box(
          title = "Tabela Robson & Cesáreas", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              reactable::reactableOutput(ns("tabela_rc"), height = "100%"))
        ),
        bs4Dash::box(
          title = "Gráfico Robson e Cesáreas", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              plotly::plotlyOutput(ns("grafico_rc"), height = "100%"))
        )
      )
    )
  )
}
