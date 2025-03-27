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
    # Título da tela de Atenção Primária à Saúde
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom",
          "Atenção Primária à Saúde"
        ),
        tags$p("", style = "font-size: 20px; font-weight: bold; text-align: center;")
      )
    ),
    # Filtros de seleção (mantidos no topo)
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("nivel_selection"),
          label = "Selecione o nível de análise:",
          choices = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
          selected = "ESTADUAL"
        )
      ),
      column(
        width = 4,
        # Se o nível for DRS, exibe o select de análise de SP; senão, exibe um placeholder para manter o layout
        conditionalPanel(
          condition = sprintf("input['%s'] == 'DRS'", ns("nivel_selection")),
          selectInput(
            inputId = ns("analisar_sp"),
            label = "Analisar a cidade de São Paulo?",
            choices = c("NÃO", "SIM"),
            selected = "NÃO"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'DRS'", ns("nivel_selection")),
          tags$div(style = "height: 68px;")  # ajuste a altura conforme necessário
        )
      ),
      column(
        width = 4,
        uiOutput(ns("secondary_filter_ui"))
      )
    ),
    br(),
    # Caixas de totais (dispostas horizontalmente em uma única linha)
    fluidRow(
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_1")))),
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_3")))),
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_4")))),
      column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_2"))))
    ),
    br(),
    # Caixas extras para nível MUNICIPAL
    conditionalPanel(
      condition = sprintf("input['%s'] == 'MUNICIPAL'", ns("nivel_selection")),
      fluidRow(
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_2")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
      )
    ),
    # Bloco para os gráficos quando nível não for MUNICIPAL
    conditionalPanel(
      condition = sprintf("input['%s'] != 'MUNICIPAL'", ns("nivel_selection")),
      # Primeira linha: os três primeiros gráficos
      fluidRow(
        column(width = 4, uiOutput(ns("card_plot_nascidos_vivos"))),
        column(width = 4, uiOutput(ns("card_plot_ubs"))),
        column(width = 4, uiOutput(ns("card_plot_gestantes_susdependentes")))
      ),
      br(),
      # Se o nível for ESTADUAL, exibe o gráfico único centralizado
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ESTADUAL'", ns("nivel_selection")),
        fluidRow(
          column(width = 6, offset = 3, uiOutput(ns("card_plot_nascidos_susdependentes_estadual")))
        )
      ),
      # Bloco para RRAS, DRS e REGIÃO DE SAÚDE com tratamento especial para RRAS 6 ou para São Paulo
      conditionalPanel(
        condition = sprintf("(input['%s'] == 'RRAS' && input['%s'] == 'RRAS 6') || (input['%s'] == 'DRS' && input['%s'] == 'SIM') || (input['%s'] == 'REGIÃO DE SAÚDE' && input['%s'] == 'SÃO PAULO')",
                            ns("nivel_selection"), ns("secondary_filter"),
                            ns("nivel_selection"), ns("analisar_sp"),
                            ns("nivel_selection"), ns("secondary_filter")),
        fluidRow(
          column(width = 4, uiOutput(ns("card_plot_nascidos_susdependentes_rras6"))),
          column(width = 4, uiOutput(ns("card_plot_cobertura_ans_rras6"))),
          column(width = 4, uiOutput(ns("card_plot_cobertura_ab_rras6")))
        )
      ),
      conditionalPanel(
        condition = sprintf("((input['%s'] == 'RRAS' && input['%s'] != 'RRAS 6') || (input['%s'] == 'DRS' && input['%s'] == 'NÃO')) || (input['%s'] == 'REGIÃO DE SAÚDE' && input['%s'] != 'SÃO PAULO')",
                            ns("nivel_selection"), ns("secondary_filter"),
                            ns("nivel_selection"), ns("analisar_sp"),
                            ns("nivel_selection"), ns("secondary_filter")),
        fluidRow(
          column(width = 4, uiOutput(ns("card_plot_nascidos_susdependentes_outros"))),
          column(width = 4, uiOutput(ns("card_plot_cobertura_ans"))),
          column(width = 4, uiOutput(ns("card_plot_cobertura_esf")))
        ),
        br(),
        fluidRow(
          column(width = 6, offset = 3, uiOutput(ns("card_plot_cobertura_ab")))
        )
      ),
      br(), br(),
      # Tabelas
      fluidRow(
        width = 12,
        column(
          width = 6,
          bs4Dash::bs4Card(
            title  = "Estabelecimentos de Referência para AAE (AGAR)",
            height = "100%",
            width = NULL,
            collapsible = FALSE,   # Desativa o recurso de minimizar
            DT::DTOutput(ns("table_aae"))
          )
        ),
        br(),
        column(
          width = 6,
          bs4Dash::bs4Card(
            title  = "Estabelecimentos de Referência para Parto (Baixo Risco)",
            height = "100%",
            width = NULL,
            collapsible = FALSE,   # Desativa o recurso de minimizar
            DT::DTOutput(ns("table_bxr"))
          )
        )
      )
    )
  )
}
