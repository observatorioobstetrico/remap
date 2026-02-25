# R/app_server.R

#' The application server-side
#'
#' Este arquivo instancia todos os módulos do painel. Ele carrega os
#' dados necessários para cada tela por meio das funções `load_*` e
#' passa as listas resultantes aos módulos correspondentes. Nesta
#' versão, adicionamos a leitura dos dados de estabelecimentos de
#' referência (`load_referencias_data`) e inicializamos o módulo
#' `mod_estabelecimentos_server` responsável por exibir e filtrar as
#' tabelas desta nova tela.
#'
#' @param input,output,session Internal parameters for {shiny}. DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @export
app_server <- function(input, output, session) {

  # 1. Carrega dados todas as vezes (global)
  data_list_aps      <- load_data()              # Dados RRAS APS
  data_list_obitos   <- load_obitos_data()       # Dados de óbitos gestantes/puérperas
  data_list_series   <- load_series_data()       # Dados de séries de óbitos (.rda)
  data_list_ind      <- load_indicadores_data()  # Dados indicadores obstétricos
  data_list_ref      <- load_referencias_data()  # Dados de estabelecimentos de referência

  # 2. Inicia módulos principais

  # Home
  mod_home_server("home")

  # Estatísticas RRAS – APS
  mod_rras_aps_server("rras_aps", data_list = data_list_aps)

  # Óbitos de Gestantes e Puérperas – Séries de mortalidade e morbidade
  mod_series_obitos_server("series_obitos", data_list = data_list_series)

  # Óbitos de Gestantes e Puérperas – Outros submenus
  mod_obitos_oficiais_server("oficiais", data_list = data_list_obitos)
  mod_obitos_nao_considerados_server("nao_cons", data_list = data_list_obitos)
  mod_analise_cruzada_server("cruzada", data_list = data_list_obitos)

  # Indicadores Obstétricos – todos os submenus
  mod_nascimentos_server("nasc", data_list = data_list_ind)
  mod_prematuros_server("pp", data_list = data_list_ind)
  mod_cesarias_server("pc", data_list = data_list_ind)
  mod_anomalias_server("an", data_list = data_list_ind)
  mod_prenatal_server("cpn", data_list = data_list_ind)
  mod_robson_server("robson", data_list = data_list_ind)
  mod_robson_cesareas_server("rc", data_list = data_list_ind)

  # Estabelecimentos de Referência
  mod_estabelecimentos_server("estabelecimentos", data_list = data_list_ref)
}
