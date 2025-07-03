# R/mod_cesarias_ui.R
#' UI: Partos Cesáreas
#' @noRd
mod_cesarias_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class="panel-title-custom","Partos Cesáreas")
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
          height      = "109vh",
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",

            # Observação
            tags$p(
              style="font-size:19px;font-style:italic;",
              "Obs: dados de 2024 preliminares."
            ),
            hr(),

            # Nível de análise
            tags$h5(class="section-header","Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices = c("Nacional","Estadual","Municipal")
            ),
            uiOutput(ns("filtros_locais")),
            hr(),

            # Intervalo de anos
            tags$h5(class="section-header","Selecione o intervalo de anos:"),
            sliderInput(
              ns("anos"), NULL,
              min   = 1996,
              max   = 2024,
              value = c(1996,2024),
              sep   = "",
              step  = 1
            ),
            hr(),

            # Texto explicativo e fórmula
            tags$p(
              HTML(
                "<strong>Definição:</strong> Parto cesárea é identificado se a variável <code>PARTO = 2</code> na base do SINASC. Casos com <code>PARTO = 9</code> (Ignorado) não são considerados no cálculo de % cesáreas."
              )
            ),
            withMathJax(
              tags$p(
                HTML(
                  "<strong>Fórmula:</strong><br/>
                  $$\\%\\ \\mathrm{cesáreas} \\;=\\;
                  \\frac{\\mathrm{N^\\circ\\ partos\\ cesáreas}}
                        {(\\mathrm{N^\\circ\\ nascimentos\\;–\\;N^\\circ\\ sem\\ informação})}
                  \\times100$$"
                )
              )
            )
          )
        )
      ),

      # Coluna de conteúdo: tabela + gráfico
      column(
        width = 8,

        # Box da tabela — metade da altura
        bs4Dash::box(
          title       = "Tabela Partos Cesáreas",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            reactable::reactableOutput(ns("tabela_pc"), height = "100%")
          )
        ),

        # Box do gráfico — outra metade da altura
        bs4Dash::box(
          title       = "Gráfico % Partos Cesáreas",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            plotly::plotlyOutput(ns("grafico_pc"), height = "100%")
          )
        )
      )
    )
  )
}
