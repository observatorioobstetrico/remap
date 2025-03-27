#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @export
app_server <- function(input, output, session) {

  # Carrega dados uma única vez
  data_list <- load_data()

  # Inicia Módulo "Home"
  mod_home_server("home")

  # Inicia Módulo "RRAS APS" (com os novos filtros e layout)
  mod_rras_aps_server("rras_aps", data_list = data_list)

  # Outros módulos podem ser iniciados aqui...
}
