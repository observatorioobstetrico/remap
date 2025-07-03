# R/mod_anomalias_ui.R
#' UI: Anomalias Congênitas
#' @noRd
mod_anomalias_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Anomalias Congênitas")
      )
    ),

    # Filtros à esquerda / conteúdo à direita
    fluidRow(
      # Coluna de filtros
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          height      = "109vh",
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",
            tags$p(
              style = "font-size:19px;font-style:italic;",
              "Obs: os dados de 2024 são preliminares."
            ),
            hr(),

            # Nível de análise
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices = c("Nacional", "Estadual", "Municipal")
            ),
            uiOutput(ns("filtros_locais")),
            hr(),

            # Intervalo de anos
            tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
            sliderInput(
              ns("anos"), NULL,
              min   = 2001,
              max   = 2024,
              value = c(2001, 2024),
              sep   = "",
              step  = 1
            ),
            hr(),

            # Definição e fórmula
            tags$p(
              HTML(
                "É importante ressaltar que essa variável do SINASC possui informações somente a partir do ano de 2001. Por esse motivo, não é possível fazer filtros em que são considerados anos anteriores."
              )
            ),
            tags$p(
              HTML(
                "<strong>Definição:</strong> Considera-se anomalia congênita quando <code>def_anomalia = 'Sim'</code> na base SINASC.
                Casos com <code>def_anomalia = 'Ignorado'</code> são considerados como dados faltantes."
              )
            ),
            withMathJax(
              tags$p(
                HTML(
                  "<strong>Fórmula:</strong><br/>
                  $$\\%\\ \\mathrm{anomalias} \\;=\\;
                  \\frac{\\mathrm{N^\\circ\\ anomalias\\ congênitas}}
                        {(\\mathrm{N^\\circ\\ nascimentos} - \\mathrm{N^\\circ\\ sem\\ informação})}
                  \\times 100$$"
                )
              )
            )
          )
        )
      ),

      # Coluna de conteúdo: tabela + gráfico
      column(
        width = 8,
        # Tabela (50% da altura)
        bs4Dash::box(
          title       = "Tabela Anomalias Congênitas",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            reactable::reactableOutput(ns("tabela_an"), height = "100%")
          )
        ),
        # Gráfico (50% da altura)
        bs4Dash::box(
          title       = "Gráfico % Anomalias Congênitas",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            plotly::plotlyOutput(ns("grafico_an"), height = "100%")
          )
        )
      )
    )
  )
}
