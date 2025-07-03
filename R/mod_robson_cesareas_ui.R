# R/mod_robson_cesareas_ui.R
#' UI: Robson & Cesáreas
#'
#' @param id módulo id
#' @import shiny
#' @importFrom bs4Dash box
#' @import reactable
#' @import plotly
#' @noRd
#' @export
mod_robson_cesareas_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Robson & Cesáreas")
      )
    ),

    # Layout: filtros à esquerda, conteúdo à direita
    fluidRow(
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
              style = "font-size:16px; font-style:italic;",
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

            # Grupo de Robson
            tags$h5(class = "section-header", "Selecione o grupo de Robson:"),
            selectInput(
              ns("grupo"), NULL,
              choices = c("Todos", as.character(1:10)),
              selected = "Todos"
            ),
            hr(),

            # Seleção de ano (sempre numericInput)
            tags$h5(class = "section-header", "Selecione o ano:"),
            numericInput(
              ns("fixedAno"), NULL,
              value = NA,  # será definido pelo server
              min   = 2014, max = 2024, step = 1
            )
          )
        )
      ),

      # Conteúdo: tabela + gráfico
      column(
        width = 8,
        bs4Dash::box(
          title       = "Tabela Robson & Cesáreas",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            reactable::reactableOutput(ns("tabela_rc"), height = "100%")
          )
        ),
        bs4Dash::box(
          title       = "Gráfico % de Cesáreas",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            plotly::plotlyOutput(ns("grafico_rc"), height = "100%")
          )
        )
      )
    )
  )
}
