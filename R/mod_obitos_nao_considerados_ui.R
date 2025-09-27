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
        tags$div(class = "panel-title-custom",
                 "Óbitos de Gestantes e Puérperas Não Considerados")
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
                   "Obs: os dados de 2024 são preliminares."),

            hr(),

            #---- Temporalidade e Localidade ----
            tags$h5(class = "section-header", "Temporalidade e localidade"),
            numericInput(ns("ano"), "Selecione o ano de análise:", value = NA),
            selectInput(
              ns("nivel"), "Selecione o nível de análise:",
              choices  = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
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
            ),

            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações e opções adicionais"),

            selectInput(ns("download_choice"),
                        "Deseja fazer o download dessa tabela?",
                        choices = c("Não", "Sim"), selected = "Não"),
            uiOutput(ns("download_ui")),
            #---- Texto Descritivo ----
            tags$p(HTML("Essa tabela contém todos os óbitos de mulheres que ocorreram durante a gravidez, parto ou puerpério (tanto no período de até 42 dias após o parto, quanto no período de 43 dias a menos de um ano) mas que não são considerados como óbitos maternos pela definição Ministério da Saúde. O local de registro dos óbitos é referente ao município de residência da falecida.")),
            tags$p(HTML("É considerado que um óbito de gestantes ou puérperas ocorreu por causas externas se a causa básica da morte pertencer ao capítulo “XX. Causas externas de morbidade e mortalidade”, da <code>CID10</code>.")),
            tags$p(HTML("A coluna “Investigação por CMM” indica se os óbitos foram, ou não, investigados por um Comitê de Morte Materna. É considerado que um óbito foi investigado por um Comitê de Morte Materna se o valor da variável <code>FONTEINV</code>, do <code>SIM</code>, for <code>1</code>."))
          )
        )
      ),
      # Tabela
      column(
        width = 8,
        bs4Dash::box(
          title       = "Tabela de Óbitos Não Considerados",
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
