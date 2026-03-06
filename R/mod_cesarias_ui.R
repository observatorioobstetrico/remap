# R/mod_cesarias_ui.R
#' UI: Partos Cesáreas (SP)
#' @noRd
mod_cesarias_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(12, tags$div(class="panel-title-custom","Partos Cesáreas"))
    ),
    fluidRow(
      column(
        width = 4,
        bs4Dash::box(
          title = "Filtros", status = "primary", solidHeader = TRUE,
          width = NULL, height = "109vh",
          div(
            style="overflow-y:auto; padding:10px; height:100%;",
            tags$p(style="font-size:16px;font-style:italic;",
                   "Obs: os dados de 2025 são preliminares."),
            hr(),
            tags$h5(class="section-header","Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices  = c("ESTADUAL","RRAS","DRS","REGIÃO DE SAÚDE","MUNICIPAL"),
              selected = "ESTADUAL"
            ),
            uiOutput(ns("filtros_locais")),
            hr(),
            tags$h5(class="section-header","Selecione o intervalo de anos:"),
            sliderInput(ns("anos"), NULL, min=1996, max=2025,
                        value=c(1996,2025), sep="", step=1),
            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações adicionais:"),

            tags$p(HTML(
              "Parto cesárea é identificado se a variável <code>PARTO = 2 (Cesárea)</code> na base do <code>SINASC</code>."
            )),
            tags$p(HTML(
              "Não são consideradas na <code>% cesáreas</code> os casos sem informação do tipo de parto, ou seja, se <code>PARTO = 9 (Ignorado)</code>."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <code>porcentagem de partos cesáreas</code> é dada por:<br/>
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
          title = "Tabela Partos Cesáreas", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              reactable::reactableOutput(ns("tabela_pc"), height = "100%"))
        ),
        bs4Dash::box(
          title = "Gráfico % Partos Cesáreas", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              plotly::plotlyOutput(ns("grafico_pc"), height = "100%"))
        )
      )
    )
  )
}
