# R/mod_rras_aps_ui.R
#' RRAS APS UI
#'
#' @param id Module id
#' @importFrom magrittr %>%
#'
#' @export
mod_rras_aps_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$script(
      htmltools::HTML(
        "
        (function() {
          if (window.apsPlotResizeHandlerRegistered) return;
          window.apsPlotResizeHandlerRegistered = true;

          Shiny.addCustomMessageHandler('aps-resize-plot', function(message) {
            var plot = document.getElementById(message.id);
            if (!plot || !message.height) return;

            plot.style.height = message.height + 'px';
            window.requestAnimationFrame(function() {
              if (window.Plotly && plot.classList.contains('js-plotly-plot')) {
                window.Plotly.Plots.resize(plot);
              }
            });
          });
        })();
        "
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom",
          tags$span("Cobertura Assistencial")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "obitos-page-description aps-page-description",
          tags$p(
            "Este painel apresenta indicadores relacionados à cobertura da atenção à saúde, permitindo acompanhar o acesso da população aos serviços de Atenção Primária à Saúde e à saúde suplementar."
          )
        )
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
