# R/mod_prenatal_ui.R
#' UI: Consultas de Pré-natal (SP)
#' @noRd
mod_prenatal_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$style(HTML("
      .custom-radio .btn {
        background-color:#264061; border-color:#264061; color:#fff;
        display:inline-flex; align-items:center; justify-content:center;
      }
      .custom-radio .btn.active,.custom-radio .btn:active,.custom-radio .btn:focus{
        background-color:#1f2b4d; border-color:#1f2b4d;
      }
    ")),
    fluidRow(
      column(12, tags$div(class = "panel-title-custom", "Consultas de Pré-natal"))
    ),
    fluidRow(
      column(
        4,
        bs4Dash::box(
          title = "Filtros", status = "primary", solidHeader = TRUE,
          width = NULL, height = "122vh",
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",
            tags$p(style="font-size:16px;font-style:italic;",
                   "Obs: os dados de 2024 são preliminares."),
            hr(),
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices  = c("ESTADUAL","RRAS","DRS","REGIÃO DE SAÚDE","MUNICIPAL"),
              selected = "ESTADUAL"
            ),
            uiOutput(ns("filtros_locais")), hr(),
            tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
            sliderInput(ns("anos"), NULL, min = 1996, max = 2024,
                        value = c(1996, 2024), sep = "", step = 1),
            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações adicionais:"),

            tags$p(HTML(
              "A variável <code>CONSULTAS</code> da base do <code>SINASC</code> é dividida em 5 classes,
              sendo: <code>1 - nenhuma consulta</code>, <code>2 - 1 a 3 consultas</code>, <code>3 - 4 a 6 consultas</code>,
              <code>4 - 7 ou mais consultas</code> e <code>9 - ignorado (sem informação sobre número
              de consultas)</code>. Como os dados das classes 2 e 3 têm comportamento
              semelhante, eles foram agrupados em uma única classe."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <code>porcentagem de prematuros</code> é dada por:<br/>
                $$\\%\\{\\}\\,\\text{consultas}=
                \\frac{N^{\\text{o}}\\,\\{\\}\\text{consultas}}
                {(N^{\\text{o}}\\,\\text{nascimentos}-N^{\\text{o}}\\,\\text{sem informação})},$$"
              ))
            ),
            withMathJax(
              tags$p(HTML("em que \\(\\{\\}\\) deve ser substituído pela categoria <code>nenhuma
                          consulta</code>, <code>1 a 6 consultas</code> ou <code>7+ consultas</code>,
                          a depender da análise de interesse."))
            )
          )
        )
      ),
      column(
        8,
        bs4Dash::box(
          title = "Tabela Consultas Pré-natal", status = "info",
          solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow-y:auto;",
              reactable::reactableOutput(ns("tabela_cpn"), height = "100%"))
        ),
        bs4Dash::box(
          title = tagList(
            tags$div(style = "font-weight: 600;", "Gráficos Consultas Pré-natal"),
            hr(),
            tags$div(
              class = "custom-radio",
              style = "display:flex; justify-content:flex-end; gap:.5rem; margin-top:.25rem;",
              shinyWidgets::radioGroupButtons(
                inputId = ns("serie"), label = NULL,
                choices = c("Nenhuma consulta" = "nenhuma",
                            "1 a 6 consultas" = "1_6",
                            "7+ consultas"    = "7plus"),
                justified = FALSE, status = "primary", size = "xs"
              )
            )
          ),
          status = "info", solidHeader = TRUE, width = NULL, height = "50vh",
          div(style = "height:100%; overflow:hidden;",
              plotly::plotlyOutput(ns("grafico_cpn"), height = "100%"))
        )
      )
    )
  )
}
