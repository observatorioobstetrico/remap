# R/mod_robson_ui.R
#' UI: Classificação de Robson (SP)
#' @noRd
mod_robson_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(
      column(12, tags$div(class = "panel-title-custom", "Classificação de Robson"))
    ),
    fluidRow(
      column(
        width = 4,
        bs4Dash::box(
          title = "Filtros", status = "primary", solidHeader = TRUE,
          width = NULL, height = "109vh",
          div(
            style = "overflow-y:auto; padding:10px; height:100%;",
            tags$p(style = "font-size:16px; font-style:italic;",
                   "Obs: os dados de 2025 são preliminares."),
            hr(),
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices  = c("ESTADUAL","RRAS","DRS","REGIÃO DE SAÚDE","MUNICIPAL"),
              selected = "ESTADUAL"
            ),
            uiOutput(ns("filtros_locais")),
            hr(),
            tags$h5(class = "section-header", "Selecione o ano:"),
            numericInput(ns("ano"), NULL, value = NA, min = 2014, max = 2025, step = 1),

            hr(),

            #---- Informações e opções adicionais ----
            tags$h5(class = "section-header", "Informações e opções adicionais:"),

            tags$p(HTML(
              "Toda a análise deste menu usa <code>TPROBSON</code> do <code>SINASC</code>."
            )),
            tags$p(HTML(
              "A <code>Classificação de Robson</code> tem como objetivo identificar quem são as mulheres
              que são submetidas à cesárea, sendo baseada em 6 fatores: idade gestacional (IG)
              termo ou pré-termo, paridade (nulíparas e multíparas); se multípara, com ou sem
              cesárea prévia (cesárea anterior), gestação única ou múltipla (número de fetos),
              apresentação fetal (cefálica, pélvica ou córmica) e início do trabalho de
              parto (espontâneo, induzido ou cesárea eletiva, sendo essa última categoria o
              caso em que não há trabalho de parto)."
            )),
            tags$p(HTML(
              "Criada em 2001, a <code>Classificação</code> de Robson só foi implementada no banco de dados
              do SINASC do DATASUS em 2010. Ainda assim, em 2013 não há quaisquer informações
              com relação aos grupos de Robson, e por isso os resultados são zero para esse ano
              em específico. Somado a isso, existem muitos dados faltantes nos anos de 2010 e
              2011, fazendo com que os anos possíveis a serem selecionados sejam a partir de
              2014. Por último, a soma do número de casos nas categorias 1 a 10 de Robson em
              um dado ano de uma determinada região pode não coincidir com o número de
              nascimentos, pois há dados faltantes para a classificação de Robson, isto é,
              quando <code>TPROBSON = 11</code>."
            )),
            withMathJax(
              tags$p(HTML(
                "Algebricamente, a <code>porcentagem de Robson</code> é dada por:<br/>
                $$\\%\\,\\text{de Robson}=
                \\frac{N^{\\text{o}}\\,\\text{de nascimentos no grupo de Robson}}
                {(N^{\\text{o}}\\,\\text{nascimentos}-N^{\\text{o}}\\,\\text{sem informação})}
                \\times 100.$$"
              ))
            ),
            tags$p(HTML(
              "Clique no botão abaixo para ver o diagrama da Classificação de Robson. Na
              imagem, é possível ver como cada <code>Grupo de Robson</code> é classificado. É sugerido
              fazer a rolagem lateral para uma melhor visualização."
            )),
            div(style = "text-align:center; margin-top:15px;",
                actionButton(ns("btn_robson_modal"), "Ver diagrama de Robson", icon = icon("image")))
          )
        )
      ),
      column(
        width = 8,
        bs4Dash::box(
          title = "Tabela Classificação de Robson", status = "info",
          solidHeader = TRUE, width = NULL, height = "45vh",
          div(style = "height:100%; overflow-y:auto;",
              reactable::reactableOutput(ns("tabela_robson"), height = "100%"))
        ),
        bs4Dash::box(
          title = "Gráfico Classificação de Robson", status = "info",
          solidHeader = TRUE, width = NULL, height = "45vh",
          div(style = "height:100%; overflow-y:auto;",
              plotly::plotlyOutput(ns("grafico_robson"), height = "100%"))
        )
      )
    )
  )
}
