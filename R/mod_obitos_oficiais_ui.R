# R/mod_obitos_oficiais_ui.R
#' UI: Óbitos Maternos Oficiais (São Paulo)
#'
#' @param id módulo id
#' @import shiny
#' @importFrom bs4Dash box
#' @importFrom reactable reactableOutput
#' @noRd
#' @export
mod_obitos_oficiais_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título da tela
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Óbitos Maternos Oficiais")
      )
    ),

    # Layout principal: filtros e tabela
    fluidRow(
      # Coluna de filtros
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

            #---- Temporalidade e localidade ----
            tags$h5(class = "section-header", "Temporalidade e localidade"),
            numericInput(ns("ano"), "Selecione o ano de análise:", value = NA),

            selectInput(
              ns("nivel"), "Selecione o nível de análise:",
              choices = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
              selected = "ESTADUAL"
            ),

            uiOutput(ns("filtros_locais")),

            hr(),

            #---- Características da gestante ou puérpera ----
            tags$h5(class = "section-header", "Características da gestante ou puérpera"),

            # >>> AJUSTADO: rádio "Sim/Não" para Sem informação
            radioButtons(
              ns("mostrar_sem_info"),
              "Exibir dados 'Sem informação' em Capítulo/Categoria CID10?",
              choices  = c("Sim", "Não"),
              selected = "Sim",
              inline   = FALSE
            ),

            sliderInput(ns("idade"), "Selecione a faixa etária:",
                        min = 0, max = 99, value = c(10, 49)),

            checkboxGroupInput(ns("raca"),
                               "Selecione a raça/cor:",
                               choices = NULL),

            checkboxGroupInput(ns("causas"),
                               "Selecione os tipos de causas obstétricas:",
                               choices = NULL),

            checkboxGroupInput(ns("periodo"),
                               "Selecione os períodos de óbito:",
                               choices = NULL),

            checkboxGroupInput(
              ns("investigacao"), "Selecione o tipo de investigação do óbito:",
              choices = c(
                "Investigado por Comitê de Morte Materna"   = "Sim",
                "Não investigado por Comitê de Morte Materna" = "Não",
                "Sem informação"                             = "Sem informação"
              ),
              selected = c("Sim","Não","Sem informação")
            ),

            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações e opções adicionais"),

            selectInput(ns("download_choice"), "Deseja fazer o download dessa tabela?",
                        choices = c("Não","Sim"), selected = "Não"),
            uiOutput(ns("download_ui")),

            #---- Texto descritivo ----
            tags$p(HTML("Essa tabela contém o número total de óbitos maternos contabilizados pelo Ministério da Saúde. O local de registro dos óbitos é referente ao município de residência da falecida.")),
            tags$p(HTML("Um óbito de gestantes ou puérperas é considerado como um óbito materno quando a categoria da CID10 referente à causa de morte é uma das categorias definidas pelo Ministério da Saúde.")),
            tags$p(HTML("É considerado que um óbito materno ocorreu durante a gravidez, parto ou aborto quando os valores das variáveis <code>OBITOGRAV</code> e <code>OBITOPUERP</code>, da base de dados do <code>SIM</code>, são, respectivamente, <code>1 e 3</code> ou <code>1 e 9</code>.")),
            tags$p(HTML("Óbitos maternos que ocorreram durante o puerpério, até 42 dias após o parto, são aqueles em que os valores das variáveis <code>OBITOGRAV</code> e <code>OBITOPUERP</code> são, respectivamente, <code>2 e 1</code> ou <code>9 e 1</code>, enquanto os óbitos que ocorreram durante o puerpério, entre 43 dias e menos de um ano após o parto, são aqueles em que os valores dessas variáveis são, respectivamente, <code>2 e 2</code> ou <code>9 e 2</code>.")),
            tags$p(HTML("Óbitos maternos que não ocorreram durante a gravidez ou puerpério são aqueles em que os valores de <code>OBITOGRAV</code> e <code>OBITOPUERP</code> são, respectivamente, <code>2 e 3</code>, <code>2 e 9</code> ou <code>9 e 3</code>.")),
            tags$p(HTML("Óbitos maternos cujo período de ocorrência é não informado ou ignorado são aqueles em que os valores de <code>OBITOGRAV</code> e <code>OBITOPUERP</code> são, respectivamente, <code>9 e 9</code>.")),
            tags$p(HTML("Por fim, óbitos maternos cujo período de ocorrência é inconsistente são aqueles em que os valores de <code>OBITOGRAV</code> e <code>OBITOPUERP</code> são, respectivamente, <code>1 e 1</code> ou <code>1 e 2</code>.")),
            tags$p(HTML("A coluna “Investigação por CMM” indica se os óbitos foram, ou não, investigados por um Comitê de Morte Materna. É considerado que um óbito foi investigado por um Comitê de Morte Materna se o valor da variável <code>FONTEINV</code>, do <code>SIM</code>, for <code>1</code>."))
          )
        )
      ),
      # Tabela
      column(
        width = 8,
        bs4Dash::box(
          title       = "Tabela de Óbitos Maternos Oficiais",
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
            reactable::reactableOutput(ns("tabela_oficiais"), height = "100%")
          )
        )
      )
    )
  )
}
