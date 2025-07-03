# R/mod_robson_ui.R
#' UI: Classificação de Robson
#' @noRd
mod_robson_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Classificação de Robson")
      )
    ),
    # Layout principal
    fluidRow(
      # Box lateral de filtros
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          height      = "109vh",  # mesma altura do módulo de Anomalias
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",

            # Observação
            tags$p(
              style = "font-size:16px; font-style:italic;",
              "Obs: os dados de 2024 são preliminares."
            ),
            hr(),

            # Nível de análise
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices = c("Nacional", "Estadual", "Municipal")
            ),
            uiOutput(ns("filtros_locais")),
            hr(),

            # Seleção de ano
            tags$h5(class = "section-header", "Selecione o ano:"),
            numericInput(
              ns("ano"), NULL,
              value = NA, min = 2014, max = 2024, step = 1
            ),
            hr(),

            # Descrição completa
            tags$p(HTML(
              "Toda a análise deste menu é baseada na variável <code>TPROBSON
              </code> da base do <strong>SINASC</strong>."
            )),
            tags$p(HTML(
              "A <strong>Classificação de Robson</strong> tem como objetivo
              identificar quem são as mulheres que são submetidas à cesárea,
              sendo baseada em 6 fatores: idade gestacional (IG) termo ou
              pré-termo, paridade (nulíparas e multíparas); se multípara,
              com ou sem cesárea prévia (cesárea anterior), gestação única ou
              múltipla (número de fetos), apresentação fetal (cefálica, pélvica
              ou córmica) e início do trabalho de parto (espontâneo, induzido
              ou cesárea eletiva, sendo essa última categoria o caso em que
              não há trabalho de parto)."
            )),
            tags$p(HTML(
              "Criada em 2001, a <strong>Classificação de Robson</strong> só
              foi implementada no banco de dados do SINASC do DATASUS em
              2010. Ainda assim, em 2013 não há quaisquer informações com
              relação aos grupos de Robson, e por isso os resultados são zero
              para esse ano em específico. Somado a isso, existem muitos dados
              faltantes nos anos de 2010 e 2011, fazendo com que os anos
              possíveis a serem selecionados sejam a partir de 2014. Por
              último, a soma do número de casos nas categorias 1 a 10 de
              Robson em um dado ano de uma determinada região pode não
              coincidir com o número de nascimentos, pois há dados faltantes
              para a classificação de Robson, isto é, quando <code>TPROBSON = 11
              </code>."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <strong>porcentagem de Robson</strong> é dada por:<br/>
                $$\\%\\mathrm{Robson} =
                \\frac{\\mathrm{N^\\circ\\ nascimentos\\ no\\ grupo\\ de\\ Robson}}
                      {(\\mathrm{N^\\circ\\ nascimentos} - \\mathrm{N^\\circ\\ sem\\ informação})}
                \\times100$$"
              ))
            ),
            tags$p(HTML(
              "Na imagem abaixo é possível ver como cada <strong>Grupo de
              Robson</strong> é classificado (é possível navegar verticalmente
              pela imagem utilizando a barra lateral)."
            )),

            # Botão centralizado abaixo da fórmula
            div(
              style = "text-align:center; margin-top:15px;",
              actionButton(
                ns("btn_robson_modal"),
                "Ver diagrama Robson",
                icon = icon("image")
              )
            )
          )
        )
      ),

      # Box de conteúdo: tabela + gráfico
      column(
        width = 8,

        # Tabela
        bs4Dash::box(
          title       = "Tabela Classificação de Robson",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "45vh",
          div(
            style = "height:100%; overflow-y:auto;",
            reactable::reactableOutput(ns("tabela_robson"), height = "100%")
          )
        ),

        # Gráfico
        bs4Dash::box(
          title       = "Gráfico Classificação de Robson",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "45vh",
          div(
            style = "height:100%; overflow-y:auto;",
            plotly::plotlyOutput(ns("grafico_robson"), height = "100%")
          )
        )
      )
    )
  )
}
