# R/mod_rras_aps2_ui.R
#' RRAS APS 2 UI
#'
#' @param id Module id
#' @importFrom magrittr %>%
#'
#' @export
mod_rras_aps2_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom",
          "Atenção Primária à Saúde 2"
        ),
        tags$p("", style = "font-size: 20px; font-weight: bold; text-align: center;")
      )
    ),
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("nivel_selection"),
          label = "Selecione o nível de análise:",
          choices = c(
            "ESTADO DE SP" = "ESTADUAL",
            "DRS" = "DRS",
            "RRAS" = "RRAS",
            "REGIÃO DE SAÚDE" = "REGIÃO DE SAÚDE",
            "MUNICIPAL" = "MUNICIPAL"
          ),
          selected = "ESTADUAL"
        )
      ),
      column(
        width = 4,
        conditionalPanel(
          condition = sprintf("input['%s'] == 'DRS'", ns("nivel_selection")),
          selectInput(
            inputId = ns("analisar_sp"),
            label = "Especificar a cidade de São Paulo?",
            choices = c("NÃO", "SIM"),
            selected = "NÃO"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'MUNICIPAL'", ns("nivel_selection")),
          selectInput(
            inputId = ns("analisar_muni_sp"),
            label = "Especificar a cidade de São Paulo?",
            choices = c("NÃO", "SIM"),
            selected = "NÃO"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'DRS' && input['%s'] != 'MUNICIPAL'", ns("nivel_selection"), ns("nivel_selection")),
          tags$div(style = "height: 68px;")
        )
      ),
      column(
        width = 4,
        uiOutput(ns("secondary_filter_ui"))
      )
    ),
    br(),
    uiOutput(ns("summary_boxes_ui")),
    br(),
    uiOutput(ns("aps_graph_tabs"))
  )
}
