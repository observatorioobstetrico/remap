# R/mod_prematuros_ui.R
#' UI: Partos Prematuros
#' @noRd
mod_prematuros_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Partos Prematuros")
      )
    ),

    # Único row: filtros à esquerda, conteúdo (tabela + gráfico) à direita
    fluidRow(
      # Coluna de filtros — altura total aproximada
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          height      = "109vh",     # ocupa toda a altura (ajuste conforme necessidade)
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",
            tags$p(
              style = "font-size:19px;font-style:italic;",
              "Obs: dados de 2024 preliminares."
            ),
            hr(),
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices = c("Nacional", "Estadual", "Municipal")
            ),
            uiOutput(ns("filtros_locais")),
            hr(),
            tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
            sliderInput(
              ns("anos"), NULL,
              min   = 1996,
              max   = 2024,
              value = c(1996, 2024),
              sep   = "",
              step  = 1
            ),
            hr(),

            # Textos explicativos e fórmula
            tags$p(
              HTML(
                "<strong>Definição:</strong> Um parto é considerado prematuro quando a idade gestacional é menor do que 37 semanas, ou seja, se a variável <code>GESTACAO</code> da base do <code>SINASC</code> for:<br/>
                &nbsp;&nbsp;• 1 (menos de 22 semanas)<br/>
                &nbsp;&nbsp;• 2 (22 a 27 semanas)<br/>
                &nbsp;&nbsp;• 3 (28 a 31 semanas)<br/>
                &nbsp;&nbsp;• 4 (32 a 36 semanas).<br/>
                Casos com <code>GESTACAO = 9</code> (“ignorada”) não são considerados no cálculo de % prematuros."
              )
            ),
            withMathJax(
              tags$p(
                HTML(
                  "<strong>Fórmula:</strong><br/>
                  $$\\%\\ \\mathrm{prematuros} \\;=\\;
                  \\frac{\\mathrm{N^\\circ\\ partos\\ prematuros}}
                        {(\\mathrm{N^\\circ\\ nascimentos\\;–\\;N^\\circ\\ sem\\ informação})}
                  \\times 100$$"
                )
              )
            )
          )
        )
      ),

      # Coluna de conteúdo: empilha tabela e gráfico
      column(
        width = 8,
        # Box da tabela — metade da altura
        bs4Dash::box(
          title       = "Tabela Partos Prematuros",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            reactable::reactableOutput(ns("tabela_pp"), height = "100%")
          )
        ),
        # Box do gráfico — outra metade da altura
        bs4Dash::box(
          title       = "Gráfico % Partos Prematuros",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            plotly::plotlyOutput(ns("grafico_pp"), height = "100%")
          )
        )
      )
    )
  )
}
