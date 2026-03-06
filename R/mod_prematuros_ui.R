# R/mod_prematuros_ui.R
#' UI: Partos Prematuros (SP)
#' @noRd
mod_prematuros_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(12, tags$div(class = "panel-title-custom", "Partos Prematuros"))
    ),

    fluidRow(
      # Filtros
      column(
        width = 4,
        bs4Dash::box(
          title = "Filtros", status = "primary", solidHeader = TRUE,
          width = NULL, height = "109vh",
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",
            tags$p(style = "font-size:16px;font-style:italic;",
                   "Obs: os dados de 2025 são preliminares."),
            hr(),

            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices  = c("ESTADUAL","RRAS","DRS","REGIÃO DE SAÚDE","MUNICIPAL"),
              selected = "ESTADUAL"
            ),
            uiOutput(ns("filtros_locais")),
            hr(),

            tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
            sliderInput(ns("anos"), NULL, min = 1996, max = 2025,
                        value = c(1996, 2025), sep = "", step = 1),
            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações adicionais:"),

            tags$p(HTML(
              "Um parto é considerado prematuro quando a idade gestacional é menor
              do que <code>37 semanas</code>, ou seja, se a variável <code>GESTACAO</code> da base de dados
              do SINASC for <code>1 (menos de 22 semanas)</code>, <code>2 (22 a 27 semanas)</code>, <code>3 (28 a
              31 semanas)</code>, <code>4 (32 a 36 semanas)</code>."
            )),
            tags$p(HTML(
              "Não são consideradas na <code>% prematuros</code> os casos sem informação se a
              gestação é prematura, ou seja, se <code>GESTACAO = 9 (ignorada)</code>."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <code>porcentagem de prematuros</code> é dada por:<br/>
                $$\\%\\,\\text{prematuros}=
                \\frac{N^{\\text{o}}\\,\\text{partos prematuros}}
                {(N^{\\text{o}}\\,\\text{nascimentos}-N^{\\text{o}}\\,\\text{sem informação})}
                \\times 100.$$"
              ))
            )

          )
        )
      ),

      # Conteúdo
      column(
        width = 8,
        bs4Dash::box(
          title = "Tabela Partos Prematuros", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              reactable::reactableOutput(ns("tabela_pp"), height = "100%"))
        ),
        bs4Dash::box(
          title = "Gráfico % Partos Prematuros", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              plotly::plotlyOutput(ns("grafico_pp"), height = "100%"))
        )
      )
    )
  )
}
