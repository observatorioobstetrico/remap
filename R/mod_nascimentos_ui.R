# R/mod_nascimentos_ui.R
#' UI: Nascimentos
#' @noRd
mod_nascimentos_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Nascimentos")
      )
    ),

    # Único row: filtros à esquerda, conteúdo (tabela + gráfico) à direita
    fluidRow(
      # Coluna de filtros — altura total de viewport
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          height      = "109vh",               # ocupa 100% da altura da tela
          div(
            style = "overflow-y:auto; padding:10px; height: 100%;",
            tags$p(
              style = "font-size:19px;font-style:italic;",
              "Obs: os dados de 2024 são preliminares."
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
            )
          )
        )
      ),

      # Coluna de conteúdo: empilha tabela e gráfico
      column(
        width = 8,
        # Box da tabela — metade da altura
        bs4Dash::box(
          title       = "Tabela Nascimentos",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            reactable::reactableOutput(ns("tabela_nascimentos"), height = "100%")
          )
        ),
        # Box do gráfico — outra metade da altura
        bs4Dash::box(
          title       = "Gráfico Nascimentos",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            plotly::plotlyOutput(ns("grafico_nascimentos"), height = "100%")
          )
        )
      )
    )
  )
}
