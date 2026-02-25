#' UI do módulo de Estabelecimentos de Referência
#'
#' @param id Identificador do módulo
#' @import shiny
#' @import bs4Dash
#' @importFrom reactable reactableOutput
#' @noRd
mod_estabelecimentos_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    # Título da página
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom panel-title-with-help",
          tags$span("Estabelecimentos de Referência"),
          shiny::actionButton(
            inputId = ns("help_btn"),
            label   = NULL,
            icon    = shiny::icon("circle-question"),
            class   = "btn-help-toggle",
            `aria-label` = "Ajuda"
          )
        )
      )
    ),

    # Filtros
    fluidRow(
      column(
        width = 4,
        tags$div(
          style = "margin-bottom: 10px;",
          tags$label("Selecione o nível de análise:"),
          selectInput(
            ns("nivel_selection"),
            label = NULL,
            choices  = c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
            selected = "RRAS"
          )
        )
      ),

      # Secundário (sempre existe; em DRS/MUNICIPAL é o "sp_detail")
      column(
        width = 4,
        uiOutput(ns("secondary_filter_ui"))
      ),

      # Terciário (aparece em DRS/MUNICIPAL; RRAS/REGIÃO retorna NULL)
      column(
        width = 4,
        uiOutput(ns("tertiary_filter_ui"))
      )
    ),

    br(),

    # Área das tabelas (layout será renderizado pelo server)
    uiOutput(ns("tables_ui"))
  )
}
