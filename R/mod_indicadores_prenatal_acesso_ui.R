#' UI: Acesso e Qualidade do Pré-Natal
#' @noRd
mod_indicadores_prenatal_acesso_ui <- function(id) {
  ns <- shiny::NS(id)

  indicador_card <- function(title, subtitle, output_id, control = NULL) {
    tags$div(
      class = "prenatal-indicator-card-host",
      bs4Dash::bs4Card(
        title = title,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = NULL,
        if (!is.null(control)) control,
        if (!is.null(subtitle)) tags$p(class = "prenatal-indicator-subtitle", subtitle),
        highcharter::highchartOutput(ns(output_id), height = "330px")
      )
    )
  }

  tagList(
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom",
          tags$span("Acesso e Qualidade do Pré-Natal")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "obitos-page-description aps-page-description",
          tags$p(
            "Este painel apresenta a evolução anual dos indicadores de acesso ao pré-natal e permite comparar territórios e níveis de análise."
          )
        )
      )
    ),
    fluidRow(
      class = "prenatal-filter-row",
      column(
        width = 3,
        selectInput(
          inputId = ns("nivel"),
          label = "Selecione o nível de análise:",
          choices = c(
            "ESTADO DE SP" = "ESTADUAL",
            "DRS" = "DRS",
            "RRAS" = "RRAS",
            "REGIÃO DE SAÚDE" = "REGIAO",
            "MUNICIPAL" = "MUNICIPAL"
          ),
          selected = "ESTADUAL"
        )
      ),
      column(width = 3, uiOutput(ns("local_ui"))),
      column(
        width = 3,
        selectInput(
          inputId = ns("nivel_comparacao"),
          label = "Nível de comparação:",
          choices = c(
            "SEM COMPARAÇÃO" = "NENHUM",
            "ESTADO DE SP" = "ESTADUAL",
            "DRS" = "DRS",
            "RRAS" = "RRAS",
            "REGIÃO DE SAÚDE" = "REGIAO",
            "MUNICIPAL" = "MUNICIPAL"
          ),
          selected = "NENHUM"
        )
      ),
      column(width = 3, uiOutput(ns("comparacao_ui")))
    ),
    tags$div(
      class = "prenatal-series-note",
      shiny::icon("circle-info"),
      tags$span("Série anual de 2022 a 2025. Os dados de 2025 são preliminares.")
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "estab-tabs-prenatal aps2-graph-tabs",
          bs4Dash::tabBox(
            id = ns("aba"),
            title = NULL,
            side = "left",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            type = "tabs",
            selected = "Acesso",
            shiny::tabPanel(
              title = "Acesso",
              fluidRow(
                class = "prenatal-indicator-row",
                column(
                  width = 4,
                  class = "prenatal-indicator-col",
                  indicador_card(
                    "Número de consultas de pré-natal",
                    subtitle = NULL,
                    output_id = "grafico_consultas",
                    control = selectizeInput(
                      inputId = ns("faixa_consultas"),
                      label = NULL,
                      choices = c(
                        "1 a 3 atendimentos" = "consultas_1_3",
                        "4 a 5 atendimentos" = "consultas_4_5",
                        "6 ou mais atendimentos" = "consultas_6_mais"
                      ),
                      selected = "consultas_1_3",
                      width = "100%"
                    )
                  )
                ),
                column(
                  width = 4,
                  class = "prenatal-indicator-col",
                  indicador_card(
                    "Primeira consulta até a 12ª semana (%)",
                    "Proporção de primeiras consultas realizadas até a 12ª semana.",
                    "grafico_primeira_12"
                  )
                ),
                column(
                  width = 4,
                  class = "prenatal-indicator-col",
                  indicador_card(
                    "Avaliação de exames até a 20ª semana (%)",
                    "Proporção de gestantes com exames avaliados até a 20ª semana.",
                    "grafico_exames_20"
                  )
                )
              )
            ),
            shiny::tabPanel(
              title = "Qualidade",
              tags$div(class = "prenatal-quality-empty", `aria-hidden` = "true")
            )
          )
        )
      )
    )
  )
}
