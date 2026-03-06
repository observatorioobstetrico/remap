# R/mod_anomalias_ui.R
#' UI: Anomalias Congênitas (SP)
#' @noRd
mod_anomalias_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(12, tags$div(class = "panel-title-custom", "Anomalias Congênitas"))
    ),
    fluidRow(
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
            sliderInput(ns("anos"), NULL, min = 2001, max = 2025,
                        value = c(2001, 2025), sep = "", step = 1),
            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações adicionais:"),

            tags$p(HTML(
              "É importante ressaltar que essa variável do <code>SINASC</code> possui informações
              somente a partir do ano de 2001. Por esse motivo, não é possível fazer
              filtros em que são considerados anos anteriores."
            )),
            tags$p(HTML(
              "Um caso é considerado como anomalia congênita se <code>def_anomalia = Sim</code>
              na base de dados do <code>SINASC</code>. E um caso de anomalia congênita considerado
              como dado faltante é se <code>def_anomalia = Ignorado</code>."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <code>porcentagem de anomalias</code> é dada por:<br/>
                $$\\%\\,\\text{anomalias}=
                \\frac{N^{\\text{o}}\\,\\text{anomalias congênitas}}
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
          title = "Tabela Anomalias Congênitas", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              reactable::reactableOutput(ns("tabela_an"), height = "100%"))
        ),
        bs4Dash::box(
          title = "Gráfico % Anomalias Congênitas", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto; position:relative;",
              plotly::plotlyOutput(ns("grafico_an"), height = "100%"))
        )
      )
    )
  )
}
