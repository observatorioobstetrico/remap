# R/mod_analise_cruzada_ui.R
#' UI: Análise Cruzada de Óbitos Maternos (São Paulo)
#'
#' @param id módulo id
#' @import shiny bs4Dash
#' @noRd
#' @export
mod_analise_cruzada_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             tags$div(class="panel-title-custom", "Análise Cruzada")
      )
    ),
    fluidRow(
      # coluna de filtros
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          div(
            style = "max-height: 168vh; overflow-y: auto; padding-right: 10px;",

            tags$p(style = "font-size 16px; font-style: italic;",
                   "Obs: os dados de 2025 são preliminares."),

            hr(),

            #---- Temporalidade e localidade ----
            tags$h5(class = "section-header", "Temporalidade e localidade"),

            sliderInput(
              ns("selectAnoAC"),
              "Selecione o(s) ano(s) de análise:",
              min   = 1996, max = 2025,
              value = c(2017, 2025),
              sep   = ""
            ),

            selectInput(
              ns("selectNivelAC"),
              "Selecione o nível de análise:",
              choices = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
              selected = "ESTADUAL"
            ),

            uiOutput(ns("filtros_locais_ac")),

            hr(),

            #---- Variáveis de interesse ----
            tags$h5(class = "section-header", "Variáveis de interesse"),

            selectInput(
              ns("selectVarLinhaAC"),
              "Selecione a variável da linha:",
              choices = NULL
            ),
            selectInput(
              ns("selectVarColunaAC"),
              "Selecione a variável da coluna:",
              choices = NULL
            ),

            radioButtons(
              ns("selectPercentualAC"),
              "Selecione o tipo de percentual da tabela:",
              choices = c("Por coluna"="column", "Por linha"="row", "Por célula"="cell"),
              selected = "column"
            ),

            hr(),

            #---- Características da gestante ou puérpera ----
            tags$h5(class = "section-header", "Características da gestante ou puérpera"),

            sliderInput(
              ns("selectIdadeAC"),
              "Selecione a faixa etária:",
              min = 0, max = 99, value = c(10,49)
            ),
            checkboxGroupInput(
              ns("selectRacaAC"),
              "Selecione a raça/cor:",
              choices = NULL
            ),
            checkboxGroupInput(
              ns("selectCausasAC"),
              "Selecione os tipos de causas obstétricas:",
              choices = NULL
            ),
            checkboxGroupInput(
              ns("selectPeriodoAC"),
              "Selecione os períodos de óbito:",
              choices = NULL
            ),
            checkboxGroupInput(
              ns("selectInvestigacaoAC"),
              "Selecione o tipo de investigação do óbito:",
              choices = NULL
            )
          )
        )
      ),

      # coluna de saída: gráfico + tabela
      column(
        width = 8,
        fluidRow(
          bs4Dash::box(
            width = 12, title = "Gráfico de Óbitos",
            highcharter::highchartOutput(ns("grafico_ac"), height = "100%")
          )
        ),
        fluidRow(
          bs4Dash::box(
            width = 12, title = "Tabela Cruzada",
            tags$div(
              style = "height = 100vh; overflow-y: auto; position: relative;",
              gt::gt_output(ns("tabela_ac"))
            )
          )
        )
      )
    )
  )
}
