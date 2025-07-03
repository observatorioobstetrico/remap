# R/mod_prenatal_ui.R
#' UI: Consultas de Pré-natal
#' @noRd
mod_prenatal_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # --- Custom CSS para estilização dos botões de série ---
    tags$style(HTML(
      "
      /* Estiliza radioGroupButtons dentro de wrapper custom-radio */
      .custom-radio .btn {
        background-color: #264061;
        border-color: #264061;
        color: #FFFFFF;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }
      .custom-radio .btn.active,
      .custom-radio .btn:active,
      .custom-radio .btn:focus {
        background-color: #1f2b4d;
        border-color: #1f2b4d;
      }
      "
    )),

    # --- Título principal ---
    fluidRow(
      column(12,
             tags$div(class = "panel-title-custom", "Consultas de Pré-natal")
      )
    ),

    # --- Layout principal ---
    fluidRow(
      # Filtros
      column(4,
             bs4Dash::box(
               title = "Filtros", status = "primary", solidHeader = TRUE,
               width = NULL, height = "122vh",
               div(style = "overflow-y:auto; padding:10px; height:100%;",
                   tags$p(style = "font-size:19px;font-style:italic;", "Obs: os dados de 2024 preliminares."),
                   hr(),
                   tags$h5(class = "section-header", "Selecione o nível de análise:"),
                   selectInput(ns("nivel"), NULL, choices = c("Nacional", "Estadual", "Municipal")),
                   uiOutput(ns("filtros_locais")), hr(),
                   tags$h5(class = "section-header", "Selecione o intervalo de anos:"),
                   sliderInput(ns("anos"), NULL, min = 1996, max = 2024, value = c(1996, 2024), sep = "", step = 1),
                   hr(),
                   # Texto completo a ser inserido sob os filtros:

                   # descrição em HTML normal
                   tags$p(HTML(
                     "<strong>Sobre as consultas de pré-natal:</strong><br/>
                     A variável <code>CONSULTAS</code> da base do <code>SINASC</code> é dividida em 5 classes, sendo:<br/>
                     &nbsp;&nbsp;<code>1 – nenhuma consulta</code>,<br/>
                     &nbsp;&nbsp;<code>2 – 1 a 3 consultas</code>,<br/>
                     &nbsp;&nbsp;<code>3 – 4 a 6 consultas</code>,<br/>
                     &nbsp;&nbsp;<code>4 – 7 ou mais consultas</code> e<br/>
                     &nbsp;&nbsp;<code>9 – ignorado (sem informação sobre número de consultas)</code>.<br/>
                     Como as classes 2 e 3 apresentam comportamento semelhante, elas foram agrupadas em uma única categoria de <em>1 a 6 consultas</em>.<br/>"
                   )),

                   # fórmula em MathJax via helpText (escapando apenas \, % e { })
                   withMathJax(
                     helpText(
                       "$$\\%\\{\\}\\text{consultas} = \\frac{\\text{N° \\{\\}consultas}}{(\\text{N° nascimentos} - \\text{N° sem informação})} \\times 100$$"
                     )
                   ),

                   withMathJax(
                     tags$p(HTML(
                       "em que \\(\\{\\}\\) deve ser substituído pela categoria <code>nenhuma consulta</code>, <code>1 a 6 consultas</code> ou <code>7+ consultas</code>, a depender da análise de interesse."
                     ))
                   )

               )
             )
      ),

      # Conteúdo: tabela + gráfico
      column(8,
             # Tabela
             bs4Dash::box(
               title = "Tabela Consultas Pré-natal", status = "info", solidHeader = TRUE,
               width = NULL, height = "50vh",
               div(style = "height:100%; overflow-y:auto;",
                   reactable::reactableOutput(ns("tabela_cpn"), height = "100%"))
             ),

             # Gráfico com botões abaixo do título
             bs4Dash::box(
               title = tagList(
                 # Título
                 tags$div(style = "font-weight: 600;", "Gráficos Consultas Pré-natal"),
                 hr(),
                 # Botões
                 tags$div(
                   class = "custom-radio",
                   style = "display: flex; justify-content: flex-end; gap: 0.5rem; margin-top: 0.25rem;",
                   shinyWidgets::radioGroupButtons(
                     inputId = ns("serie"), label = NULL,
                     choices = c("Nenhuma consulta" = "nenhuma",
                                 "1 a 6 consultas" = "1_6",
                                 "7+ consultas"    = "7plus"),
                     justified = FALSE, status = "primary", size = "xs"
                   )
                 )
               ),
               status = "info", solidHeader = TRUE, width = NULL, height = "50vh",
               div(style = "height:100%; overflow:hidden;",
                   plotly::plotlyOutput(ns("grafico_cpn"), height = "100%"))
             )
      )
    )
  )
}
