# R/mod_obitos_nao_considerados_ui.R
#' UI: Óbitos de Gestantes e Puérperas Não Considerados (São Paulo)
#'
#' @param id módulo id
#' @import shiny
#' @importFrom bs4Dash box
#' @importFrom reactable reactableOutput
#' @noRd
#' @export
mod_obitos_nao_considerados_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom panel-title-with-help estab-title-with-actions",
          tags$span(class = "obitos-title-text", "Óbitos não classificados como morte materna"),
          tags$div(
            class = "estab-title-actions",
            shiny::downloadLink(
              ns("download_NM_xlsx"),
              shiny::icon("download"),
              class = "btn-help-toggle btn-estab-download-toggle",
              `aria-label` = "Baixar tabela em xlsx",
              title = "Baixar tabela em xlsx"
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "obitos-page-description",
          tags$p(
            "Este painel apresenta os óbitos de mulheres ocorridos durante a gestação, parto ou puerpério ",
            "(até 42 dias após o parto ou até um ano), que ",
            tags$b("não foram classificados como morte materna"),
            ", segundo os critérios do Ministério da Saúde."
          ),
          tags$p(
            "Em geral, incluem óbitos decorrentes de causas não relacionadas diretamente à gestação, como ",
            "acidentes, violências ou outras causas externas."
          ),
          tags$p(
            "A variável “Investigação por Comitê de Mortalidade Materna” indica se o caso foi analisado ",
            "por um comitê responsável pela investigação desses óbitos."
          ),
          tags$p(
            tags$b(
              "Para consultar a fonte e a definição detalhada das informações, acesse a seção Documentação ",
              "dos Indicadores, disponível no menu lateral."
            )
          )
        )
      )
    ),
    fluidRow(
      # Filtros
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          div(
            style = "max-height: 100vh; overflow-y: auto; padding-right: 10px;",
            tags$p(style = "font-size: 16px; font-style: italic;",
                   "Obs: os dados de 2025 são preliminares."),

            hr(),

            #---- Temporalidade e Localidade ----
            tags$h5(class = "section-header", "Temporalidade e localidade"),
            numericInput(ns("ano"), "Selecione o ano de análise:", value = NA),
            selectInput(
              ns("nivel"), "Selecione o nível de análise:",
              choices  = c("ESTADO DE SP" = "ESTADUAL", "DRS" = "DRS", "RRAS" = "RRAS", "REGIÃO DE SAÚDE" = "REGIÃO DE SAÚDE", "MUNICIPAL" = "MUNICIPAL"),
              selected = "ESTADUAL"
            ),
            uiOutput(ns("filtros_locais")),

            hr(),

            #---- Características da Gestante ou Puérpera ----
            tags$h5(class = "section-header", "Características da gestante ou puérpera"),

            # >>> AJUSTADO: rádio "Sim/Não" para Sem informação
            radioButtons(
              ns("mostrar_sem_info"),
              "Exibir dados 'Sem informação' em Capítulo/Categoria CID10?",
              choices  = c("Sim", "Não"),
              selected = "Sim",
              inline   = FALSE
            ),

            sliderInput(ns("idade"), "Selecione a faixa etária:", min = 0, max = 99, value = c(10, 49)),
            checkboxGroupInput(ns("raca"), "Selecione a raça/cor:", choices = NULL),
            checkboxGroupInput(ns("periodo"), "Selecione os períodos de óbito:", choices = NULL),
            checkboxGroupInput(
              ns("investigacao"), "Selecione o tipo de investigação do óbito:", choices = NULL
            ),
            checkboxGroupInput(
              ns("externos"), "Excluir óbitos por causas externas?",
              choices  = "Excluir óbitos por causas externas",
              selected = NULL
            )
          )
        )
      ),
      # Tabela
      column(
        width = 8,
        bs4Dash::box(
          title       = "Tabela de óbitos não classificados como morte materna",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          tags$style(HTML("
            .reactable .rt-td { font-weight: normal !important; }
            .reactable .rt-thead .rt-th { font-weight: bold !important; font-size: 16px !important;}
            .reactable .rt-tfoot .rt-td,
            .reactable .rt-tr.-footer .rt-td { font-weight: bold !important; }
          ")),
          tags$style(HTML("
             .reactable .rt-table {position: relative;}
             .reactable .rt-thead {position: sticky; top: 0; z-index: 2; background: white;}
             .reactable .rt-tr.-footer {position: sticky; bottom: 0; z-index: 2; background: white;}
          ")),
          div(
            style = "height: 100vh; overflow-y: auto; position: relative;",
            reactable::reactableOutput(ns("tabela_nao"), height = "100%")
          )
        )
      )
    )
  )
}
