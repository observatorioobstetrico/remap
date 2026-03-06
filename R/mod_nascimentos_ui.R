# R/mod_nascimentos_ui.R
#' UI: Indicadores Obstétricos → Nascimentos (SP)
#'
#' Nível de análise: ESTADUAL (SP fixo) → RRAS → DRS → REGIÃO DE SAÚDE → MUNICIPAL
#' Layout: filtros à esquerda; tabela e gráfico (meia tela cada) à direita.
#'
#' @noRd
mod_nascimentos_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(class = "panel-title-custom", "Nascimentos")
      )
    ),

    # Layout: filtros (col-4) | conteudo (col-8)
    fluidRow(
      # -------------------- COLUNA DE FILTROS --------------------
      column(
        width = 4,
        bs4Dash::box(
          title       = "Filtros",
          status      = "primary",
          solidHeader = TRUE,
          width       = NULL,
          height      = "109vh",
          div(
            style = "overflow-y:auto; padding:10px; height: 100%;",

            # Observação
            tags$p(
              style = "font-size:16px;font-style:italic;",
              "Obs: os dados de 2025 são preliminares."
            ),

            # # Última atualização (se existir no global)
            # uiOutput(ns("ultima_atualizacao")),

            hr(),

            # Nível
            tags$h5(class = "section-header", "Selecione o nível de análise:"),
            selectInput(
              ns("nivel"), NULL,
              choices  = c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"),
              selected = "ESTADUAL"
            ),

            # Localidade dependente do nível
            uiOutput(ns("filtros_locais")),

            hr(),

            # Intervalo de anos
            tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
            sliderInput(
              ns("anos"), NULL,
              min   = 1996,   # será sobrescrito no server
              max   = 2025,   # será sobrescrito no server
              value = c(1996, 2025),
              step  = 1, sep = ""
            )
          )
        )
      ),

      # -------------------- COLUNA DE CONTEÚDO --------------------
      column(
        width = 8,
        # Tabela (metade superior)
        bs4Dash::box(
          title       = "Tabela de Nascimentos",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          # estilos de cabeçalho/rodapé fixos (opcional)
          tags$style(HTML("
             .reactable .rt-table {position: relative;}
             .reactable .rt-thead {position: sticky; top: 0; z-index: 2; background: white;}
             .reactable .rt-tr.-footer {position: sticky; bottom: 0; z-index: 2; background: white;}
          ")),
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            reactable::reactableOutput(ns("tabela_nascimentos"), height = "100%")
          )
        ),
        # Gráfico (metade inferior)
        bs4Dash::box(
          title       = "Gráfico de Nascimentos",
          status      = "info",
          solidHeader = TRUE,
          width       = NULL,
          height      = "50vh",
          div(
            style = "height:100%; overflow-y:auto; position:relative;",
            plotly::plotlyOutput(ns("grafico_nascimentos"), height = "100%")
          )
        )
      )
    )
  )
}
