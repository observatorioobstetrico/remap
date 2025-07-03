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

            tags$p(style = "font-size: 19px; font-style: italic;",
                   "Obs.: os dados de 2024 são preliminares."),

            hr(),

            #---- Temporalidade e localidade ----
            tags$h5(class = "section-header", "Temporalidade e localidade"),
            # Ano
            numericInput(ns("ano"), "Selecione o ano de análise:", value = NA),

            # Nível
            selectInput(
              ns("nivel"), "Selecione o nível de análise:",
              choices = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
              selected = "ESTADUAL"
            ),

            # Localidade fixa/dinâmica
            uiOutput(ns("filtros_locais")),

            hr(),

            #---- Características da gestante ou puérpera ----
            tags$h5(class = "section-header", "Características da gestante ou puérpera"),

            # Idade
            sliderInput(ns("idade"), "Selecione a faixa etária:",
                        min = 0, max = 99, value = c(10, 49)),

            # Raça/Cor
            checkboxGroupInput(ns("raca"),
                               "Selecione a raça/cor:",
                               choices = NULL),

            # Causas
            checkboxGroupInput(ns("causas"),
                               "Selecione os tipos de causas obstétricas:",
                               choices = NULL),

            # Período
            checkboxGroupInput(ns("periodo"),
                               "Selecione os períodos de óbito:",
                               choices = NULL),

            # Investigação
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

            #---- Download ----
            selectInput(ns("download_choice"), "Deseja fazer o download dessa tabela?",
                        choices = c("Não","Sim"), selected = "Não"),
            uiOutput(ns("download_ui")),

            #---- Texto descritivo ----
            tags$p("Essa tabela contém o número total de óbitos maternos contabilizados pelo Ministério da Saúde. O local de registro dos óbitos é referente ao município de residência da falecida."),
            tags$p("Um óbito de gestantes ou puérperas é considerado como um óbito materno quando a categoria da CID10 referente à causa de morte é uma das categorias definidas pelo Ministério da Saúde."),
            tags$p("É considerado que um óbito materno ocorreu durante a gravidez, parto ou aborto quando os valores das variáveis OBITOGRAV e OBITOPUERP, da base de dados do SIM, são, respectivamente, 1 e 3 ou 1 e 9."),
            tags$p("Óbitos maternos que ocorreram durante o puerpério, até 42 dias após o parto, são aqueles em que os valores das variáveis OBITOGRAV e OBITOPUERP são, respectivamente, 2 e 1 ou 9 e 1, enquanto os óbitos que ocorreram durante o puerpério, entre 43 dias e menos de um ano após o parto, são aqueles em que os valores dessas variáveis são, respectivamente, 2 e 2 ou 9 e 2."),
            tags$p("Óbitos maternos que não ocorreram durante a gravidez ou puerpério são aqueles em que os valores de OBITOGRAV e OBITOPUERP são, respectivamente, 2 e 3, 2 e 9 ou 9 e 3."),
            tags$p("Óbitos maternos cujo período de ocorrência é não informado ou ignorado são aqueles em que os valores de OBITOGRAV e OBITOPUERP são, respectivamente, 9 e 9."),
            tags$p("Por fim, óbitos maternos cujo período de ocorrência é inconsistente são aqueles em que os valores de OBITOGRAV e OBITOPUERP são, respectivamente, 1 e 1 ou 1 e 2."),
            tags$p("A coluna “Investigação por CMM” indica se os óbitos foram, ou não, investigados por um Comitê de Morte Materna. É considerado que um óbito foi investigado por um Comitê de Morte Materna se o valor da variável FONTEINV, do SIM, for 1.")
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
          # --- sobrescreve estilos de fonte ---
          tags$style(HTML("
            .reactable .rt-td { font-weight: normal !important; }
            .reactable .rt-thead .rt-th { font-weight: bold !important; font-size: 16px !important;}
            .reactable .rt-tfoot .rt-td,
            .reactable .rt-tr.-footer .rt-td { font-weight: bold !important; }
          ")),

          # CSS para tornar header e footer sticky
          tags$style(HTML("
             .reactable .rt-table {position: relative;}
             .reactable .rt-thead {position: sticky; top: 0; z-index: 2; background: white;}
             .reactable .rt-tr.-footer {position: sticky; bottom: 0; z-index: 2; background: white;}
          ")),
          # Contêiner com scroll vertical e altura fixa
          div(
            style = "height: 100vh; overflow-y: auto; position: relative;",
            reactable::reactableOutput(ns("tabela_oficiais"), height = "100%")
          )
        )
      )
    )
  )
}
