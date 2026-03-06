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
#' @importFrom shinyjs useShinyjs hidden
#' @noRd
#' @export
mod_series_obitos_ui <- function(id) {
  ns <- shiny::NS(id)

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

      "))
    ),

    # Inclui shinyjs para usar show()/hide()
    shinyjs::useShinyjs(),

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
                  "Estadual"             = "estadual",
                  "RRAS"                 = "rras",
                  "DRS"                  = "drs",
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
                ns("comparar"), "Comparar com outra localidade?",
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
                    "Nacional"             = "nacional",
                    "Região do país"       = "regional",
                    "Estadual"             = "estadual",
                    "Macrorregião"         = "macro",
                    "DRS"                  = "drs",
                    "Região de saúde"      = "micro",
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
    # 2. QUATRO GRÁFICOS EM CARDS
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

      # % causas específicas
      column(
        width = 6,
        bs4Dash::bs4Card(
          width        = 12,
          status       = "primary",
          solidHeader  = TRUE,
          headerBorder = FALSE,
          title = tagList(
            HTML("<b>% óbitos maternos diretos por causas específicas</b>"),
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
    )
  )
}
