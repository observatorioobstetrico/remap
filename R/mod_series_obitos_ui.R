# R/mod_series_obitos_ui.R

#' UI: Séries de Mortalidade e Morbidade Materna
#'
#' Interface do módulo que exibe filtros e quatro gráficos de série histórica:
#' número de óbitos, razão por 100k, % causas diretas e % causas específicas.
#'
#' @param id módulo id
#' @import shiny
#' @import bs4Dash
#' @importFrom shinyWidgets actionBttn
#' @importFrom shinycssloaders withSpinner
#' @importFrom highcharter highchartOutput
#' @importFrom reactable reactableOutput
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
#' @export
mod_series_obitos_ui <- function(id) {
  ns <- shiny::NS(id)
  opcoes_selectize_causas_indiretas <- list(
    placeholder = "Selecione",
    render = htmlwidgets::JS(
      "{
        item: function(item, escape) {
          var label = item.label || item.text || item.value || '';
          return '<div class=\"series-obitos-select-item\" title=\"' + escape(label) + '\">' + escape(label) + '</div>';
        },
        option: function(item, escape) {
          var label = item.label || item.text || item.value || '';
          return '<div class=\"series-obitos-select-option\" title=\"' + escape(label) + '\">' + escape(label) + '</div>';
        }
      }"
    )
  )

  tagList(
    #-----------------------------------------------------------------------
    # 0. Injetando CSS para posicionar ícones no header
    #-----------------------------------------------------------------------
    tags$head(
      tags$style(HTML("
        /* Faz o <h3 class='card-title'> ocupar 100% do header */
        .card-header .card-title {
          position: relative !important;
          width: 100% !important;
          padding-right: 50px !important; /* espaço para o ícone */
        }
        /* Posiciona o ícone no canto direito, verticalmente centrado */
        .card-header .card-title .my-header-icon {
          position: absolute !important;
          top: 50% !important;
          right: 15px !important;
          transform: translateY(-50%) !important;
          z-index: 10 !important;
        }
        /* 1) Garante que o wrapper seja tratado como box */
        .my-header-icon {
          display: inline-block !important;
          transition: box-shadow 0.2s ease !important;
          /* (se quiser reforçar o círculo) */
          border-radius: 50% !important;
        }
        /* 2) Quando o mouse passar sobre o container, aplica a sombra ao redor dele */
        .my-header-icon:hover {
          box-shadow: 0 0 17px rgba(0, 0, 0, 0.6) !important;
        }
        /* Esconde os shiny alert por enquanto -----------------------*/
            /* ESCONDE TODOS os círculos vermelhos de alerta */
        .my-header-icon {
          display: none !important;
        }

        .series-obitos-tabela .rt-thead {
          position: sticky;
          top: 0;
          z-index: 2;
          background: #ffffff;
        }

        .series-obitos-tabela .rt-th {
          font-weight: 700 !important;
          font-size: 14px !important;
        }

        .series-obitos-tabela .rt-tfoot .rt-td,
        .series-obitos-tabela .rt-tr.-footer .rt-td {
          font-weight: 700 !important;
          background: #f8fafc;
        }

        .series-obitos-tabela-nota {
          background: #f7fbff;
          border-left: 4px solid #32a0ff;
          border-radius: 6px;
          color: #24364f;
          font-size: 14px;
          margin: 4px 0 14px 0;
          padding: 10px 12px;
        }

        .series-obitos-tabela-header-title {
          display: block;
          min-height: 34px;
          line-height: 34px;
          padding-right: 130px;
        }

        .series-obitos-tabela-header-actions {
          align-items: center;
          display: flex;
          position: absolute;
          right: 0;
          top: 50%;
          transform: translateY(-50%);
        }

        .series-obitos-tabela-download.btn {
          background-color: #ffffff !important;
          border-color: #ffffff !important;
          color: #0062cc !important;
          font-weight: 700;
          margin: 0;
          padding: 4px 12px;
        }

        .series-obitos-tabela-download.btn i,
        .series-obitos-tabela-download.btn svg {
          color: #0062cc !important;
          fill: #0062cc !important;
        }

        .series-obitos-tabela-download.btn:hover,
        .series-obitos-tabela-download.btn:focus {
          background-color: #eaf0f7 !important;
          border-color: #eaf0f7 !important;
          color: #084594 !important;
        }

        .series-obitos-tabela-download.btn:hover i,
        .series-obitos-tabela-download.btn:focus i,
        .series-obitos-tabela-download.btn:hover svg,
        .series-obitos-tabela-download.btn:focus svg {
          color: #084594 !important;
          fill: #084594 !important;
        }

        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-input {
          align-items: center;
          background: #ffffff !important;
          color: #24364f !important;
          display: flex !important;
          min-height: 38px;
          overflow: hidden;
          padding-right: 32px;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-input.focus,
        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-input.dropdown-active {
          background: #ffffff !important;
          color: #24364f !important;
        }

        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-input > .item {
          color: #24364f !important;
          display: block;
          flex: 1 1 auto;
          max-width: min(52ch, calc(100% - 8px));
          min-width: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-dropdown {
          box-sizing: border-box;
          left: auto !important;
          max-width: min(760px, calc(100vw - 32px));
          min-width: min(360px, 100%) !important;
          right: 0 !important;
          width: min(760px, calc(100vw - 32px)) !important;
          z-index: 3000;
        }

        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-dropdown .option {
          color: #24364f !important;
          line-height: 1.25;
          overflow-wrap: anywhere;
          padding: 8px 10px;
          white-space: normal;
          word-break: normal;
        }

        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-dropdown .option.active,
        select[id$='tabela_causa_indireta'] + .selectize-control.single .selectize-dropdown .option.selected {
          background: #eef6ff !important;
          color: #084594 !important;
        }

        .series-obitos-chart-centered {
          margin: 0 auto 14px auto;
          max-width: 980px;
        }

        .series-obitos-chart-centered .card-title {
          line-height: 1.25;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-input {
          align-items: center;
          background: #ffffff !important;
          color: #24364f !important;
          display: flex !important;
          min-height: 38px;
          overflow: hidden;
          padding-right: 32px;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-input.focus,
        .series-obitos-causa-select .selectize-control.single .selectize-input.dropdown-active {
          background: #ffffff !important;
          color: #24364f !important;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-input > .item {
          color: #24364f !important;
          display: block;
          flex: 1 1 auto;
          max-width: min(52ch, calc(100% - 8px));
          min-width: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-input.has-items input {
          flex: 0 0 4px !important;
          min-width: 4px !important;
          width: 4px !important;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-dropdown {
          box-sizing: border-box;
          left: auto !important;
          max-width: min(760px, calc(100vw - 32px));
          min-width: min(360px, 100%) !important;
          right: 0 !important;
          width: min(760px, calc(100vw - 32px)) !important;
          z-index: 3000;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-dropdown-content {
          max-height: 280px;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-dropdown .option,
        .series-obitos-causa-select .selectize-control.single .selectize-dropdown .series-obitos-select-option {
          color: #24364f !important;
          line-height: 1.25;
          overflow-wrap: break-word;
          padding: 8px 10px;
          white-space: normal;
          word-break: normal;
        }

        .series-obitos-causa-select .series-obitos-select-item {
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .series-obitos-causa-select .selectize-control.single .selectize-dropdown .option.active,
        .series-obitos-causa-select .selectize-control.single .selectize-dropdown .option.selected {
          background: #eef6ff !important;
          color: #084594 !important;
        }

      "))
    ),

    # Inclui shinyjs para usar show()/hide()
    shinyjs::useShinyjs(),

    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom panel-title-with-help",
          tags$span("Séries de Mortalidade"),
          shiny::actionButton(
            inputId = ns("help_btn"),
            label = NULL,
            icon = shiny::icon("circle-question"),
            class = "btn-help-toggle",
            `aria-label` = "Sobre Séries de Mortalidade"
          )
        )
      )
    ),

    #=======================================================================
    # 1. CARD DE FILTROS
    #=======================================================================
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width       = 12,
          title       = HTML("<b class='fonte-indicador-nivel3'>Filtros de interesse</b>"),
          status      = "primary",
          solidHeader = TRUE,
          icon        = icon("filter"),

          fluidRow(
            column(
              width = 3,
              sliderInput(
                ns("anos"), "Intervalo de anos:",
                min = 2012, max = 2025,
                value = c(2012, 2024), step = 1, sep = ""
              )
            ),
            column(
              width = 3,
              selectizeInput(
                ns("nivel"), "Nível de análise:",
                choices = c(
                  "Estado de SP"         = "estadual",
                  "DRS"                  = "drs",
                  "RRAS"                 = "rras",
                  "Região de Saúde"      = "regiao_saude",
                  "Municipal"            = "municipal"
                ),
                selected = "estadual",
                options = list(placeholder = "Selecione"), width = "100%"
              )
            ),
            column(width = 6, uiOutput(ns("ui_subfiltros")))
          ),

          conditionalPanel(
            condition = "input.anos[0] == 2025 || input.anos[1] == 2025",
            ns = ns,
            fluidRow(
              column(
                width = 12,
                HTML("
                  <div style='text-align: left;'>
                    <b class='fonte-grande'>
                      <i class='fa-solid fa-circle-info'></i>
                      &nbsp; Os dados de 2025 são preliminares<br/>
                      (atualizados em 26 de fevereiro de 2026)
                    </b>
                  </div>
                  <span style='display:block;margin-bottom:15px;'></span>
                ")
              )
            )
          ),

          fluidRow(
            column(
              width = 3,
              selectizeInput(
                ns("comparar"), "Comparar com outra localidade do estado de SP?",
                choices = c("Não", "Sim"), selected = "Não", width = "100%"
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                condition = "input.comparar == 'Sim'", ns = ns,
                selectizeInput(
                  ns("nivel2"), "Nível de análise (comparar):",
                  choices = c(
                    "Estado de SP"         = "estadual",
                    "DRS"                  = "drs",
                    "RRAS"                 = "rras",
                    "Região de saúde"      = "regiao_saude",
                    "Municipal"            = "municipal"
                  ),
                  options = list(placeholder = "Selecione"), width = "100%"
                )
              )
            ),
            column(width = 6, uiOutput(ns("ui_subfiltros_comp")))
          ),

          conditionalPanel(
            condition = "input.comparar == 'Sim'", ns = ns,
            tagList(
              fluidRow(
                column(
                  width = 3,
                  selectizeInput(
                    ns("comparar2"), "Comparar com outra localidade do estado de SP?",
                    choices = c("Não", "Sim"), selected = "Não", width = "100%"
                  )
                ),
                column(
                  width = 3,
                  conditionalPanel(
                    condition = "input.comparar2 == 'Sim'", ns = ns,
                    selectizeInput(
                      ns("nivel3"), "Nível de análise (comparar):",
                      choices = c(
                        "Estado de SP"         = "estadual",
                        "DRS"                  = "drs",
                        "RRAS"                 = "rras",
                        "Região de saúde"      = "regiao_saude",
                        "Municipal"            = "municipal"
                      ),
                      options = list(placeholder = "Selecione"), width = "100%"
                    )
                  )
                ),
                column(width = 6, uiOutput(ns("ui_subfiltros_comp2")))
              )
            )
          ),

          conditionalPanel(
            condition = "input.comparar == 'Sim'", ns = ns,
            fluidRow(
              column(
                width = 12, align = "center",
                radioButtons(
                  inputId = ns("mostrar_referencia"), label = NULL,
                  choiceNames = list(
                    HTML("Mostrar a linha de referência"),
                    HTML("Não mostrar a linha de referência")
                  ),
                  choiceValues = list("mostrar_referencia", "nao_mostrar_referencia"),
                  selected = "mostrar_referencia", inline = TRUE
                )
              )
            )
          ),

          fluidRow(
            column(
              width = 12, align = "center",
              shinyWidgets::actionBttn(
                ns("atualizar"), icon = icon("magnifying-glass"),
                label = "Atualizar resultados", style = "unite",
                color = "primary", size = "sm"
              )
            )
          )
        )
      )
    ),

    #=======================================================================
    # 2. GRÁFICOS EM CARDS
    #=======================================================================

    # Linha 1: número de óbitos e razão por 100k
    fluidRow(
      # Número de óbitos maternos
      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = tagList(
            HTML("<b>Número de óbitos maternos</b>"),
            tags$span(
              id = ns("mostrar_botao1"), class = "my-header-icon",
              style = "position:absolute; top:50%; right:15px; transform:translateY(-50%);
                       display:inline-block;
                       box-shadow: 0 2px 5px rgba(0,0,0,0.4);
                       border: 0px solid #ccc;
                       border-radius: 50%;
                       padding: 2px;
                       background: linear-gradient(135deg, #ffac30, red);",
              shinyWidgets::actionBttn(
                ns("botao1"), icon = icon("triangle-exclamation", style = "font-size: 14px; color: red;"),
                color = "default", style = "gradient", size = "xs"
              )
            )
          ),
          shinycssloaders::withSpinner(
            highcharter::highchartOutput(ns("plot_n_obitos"), height = "400px")
          )
        )
      ),

      # Razão de mortalidade por 100.000 nascidos vivos
      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = tagList(
            HTML("<b>Razão de mortalidade por 100.000 nascidos vivos</b>"),
            tags$span(
              id = ns("mostrar_botao2"), class = "my-header-icon",
              style = "position:absolute; top:50%; right:15px; transform:translateY(-50%);
                       display:inline-block;
                       box-shadow: 0 2px 5px rgba(0,0,0,0.4);
                       border: 0px solid #ccc;
                       border-radius: 50%;
                       padding: 2px;
                       background: linear-gradient(135deg, #ffac30, red);",
              shinyWidgets::actionBttn(
                ns("botao2"), icon = icon("triangle-exclamation", style = "font-size: 14px; color: red;"),
                color = "default", style = "gradient", size = "xs"
              )
            )
          ),
          shinycssloaders::withSpinner(
            highcharter::highchartOutput(ns("plot_rmm"), height = "400px")
          )
        )
      )
    ),

    # Linha 2: % causas diretas e % causas específicas
    fluidRow(
      # % causas obstétricas diretas
      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = tagList(
            HTML("<b>% óbitos por causas obstétricas diretas</b>"),
            tags$span(
              id = ns("mostrar_botao3"), class = "my-header-icon",
              style = "position:absolute; top:50%; right:15px; transform:translateY(-50%);
                       display:inline-block;
                       box-shadow: 0 2px 5px rgba(0,0,0,0.4);
                       border: 0px solid #ccc;
                       border-radius: 50%;
                       padding: 2px;
                       background: linear-gradient(135deg, #ffac30, red);",
              shinyWidgets::actionBttn(
                ns("botao3"), icon = icon("triangle-exclamation", style = "font-size: 14px; color: red;"),
                color = "default", style = "gradient", size = "xs"
              )
            )
          ),
          shinycssloaders::withSpinner(
            highcharter::highchartOutput(ns("plot_pct_diretas"), height = "400px")
          )
        )
      ),

      # % causas específicas entre causas diretas
      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = tagList(
            HTML("<b>% óbitos por causas específicas dentre os óbitos por causas obstétricas diretas</b>"),
            tags$span(
              id = ns("mostrar_botao4"), class = "my-header-icon",
              style = "position:absolute; top:50%; right:15px; transform:translateY(-50%);
                       display:inline-block;
                       box-shadow: 0 2px 5px rgba(0,0,0,0.4);
                       border: 0px solid #ccc;
                       border-radius: 50%;
                       padding: 2px;
                       background: linear-gradient(135deg, #ffac30, red);",
              shinyWidgets::actionBttn(
                ns("botao4"), icon = icon("triangle-exclamation", style = "font-size: 14px; color: red;"),
                color = "default", style = "gradient", size = "xs"
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              selectizeInput(
                ns("causa_especifica"), "Causa de óbito materno:",
                choices = c(
                  "Aborto"              = "prop_obitos_aborto",
                  "Hipertensivas"       = "prop_obitos_hipertens",
                  "Hemorrágicas"        = "prop_obitos_hemo",
                  "Infecção puerperal"  = "prop_obitos_infec"
                ),
                options = list(placeholder = "Selecione"),
                selected = "prop_obitos_aborto", width = "100%"
              )
            )
          ),
          shinycssloaders::withSpinner(
            highcharter::highchartOutput(ns("plot_pct_especificas"), height = "311px")
          )
        )
      )
    ),

    # Linha 3: % causas indiretas e % causas específicas entre causas indiretas
    fluidRow(
      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = HTML("<b>% óbitos por causas obstétricas indiretas</b>"),
          shinycssloaders::withSpinner(
            highcharter::highchartOutput(ns("plot_pct_indiretas"), height = "400px")
          )
        )
      ),

      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = HTML("<b>% óbitos por causas específicas dentre os óbitos por causas obstétricas indiretas</b>"),
          fluidRow(
            column(
              width = 12,
              tags$div(
                class = "series-obitos-causa-select",
                selectizeInput(
                  ns("causa_indireta_especifica"),
                  "Causa de óbito materno indireto:",
                  choices = character(0),
                  options = opcoes_selectize_causas_indiretas,
                  width = "100%"
                )
              )
            )
          ),
          shinycssloaders::withSpinner(
            highcharter::highchartOutput(ns("plot_pct_indiretas_especificas"), height = "311px")
          )
        )
      )
    ),

    #=======================================================================
    # 3. TABELA DETALHADA
    #=======================================================================
    fluidRow(
      column(
        width = 12,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title        = div(
            class = "series-obitos-tabela-header-title",
            tags$span(HTML("<b>Visão detalhada dos indicadores</b>")),
            tags$span(
              class = "series-obitos-tabela-header-actions",
              downloadButton(
                ns("download_tabela_mortalidade_xlsx"),
                "Baixar",
                class = "series-obitos-tabela-download",
                icon = icon("download")
              )
            )
          ),

          fluidRow(
            column(
              width = 3,
              uiOutput(ns("ui_tabela_ano"))
            ),
            column(
              width = 3,
              selectizeInput(
                ns("tabela_indicador"),
                "Indicador:",
                choices = c(
                  "Número de óbitos maternos" = "n_obitos",
                  "Razão de mortalidade por 100.000 nascidos vivos" = "rmm",
                  "% óbitos por causas obstétricas" = "prop_obstetricas",
                  "% óbitos por causa específica" = "causas_especificas"
                ),
                selected = "n_obitos",
                width = "100%"
              )
            ),
            column(
              width = 3,
              conditionalPanel(
                condition = "input.tabela_indicador == 'prop_obstetricas' || input.tabela_indicador == 'causas_especificas'", ns = ns,
                selectizeInput(
                  ns("tabela_tipo_causa"),
                  "Tipo de causa:",
                  choices = c(
                    "Óbitos maternos diretos" = "diretas",
                    "Óbitos maternos indiretos" = "indiretas"
                  ),
                  selected = "diretas",
                  width = "100%"
                )
              )
            ),
            column(width = 3, uiOutput(ns("ui_tabela_causa")))
          ),

          uiOutput(ns("tabela_contexto_causa")),

          div(
            class = "series-obitos-tabela",
            shinycssloaders::withSpinner(
              reactable::reactableOutput(ns("tabela_mortalidade_detalhada"), height = "650px")
            )
          )
        )
      )
    )
  )
}
