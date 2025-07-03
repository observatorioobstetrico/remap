# R/global_data_indicadores.R
#' Carrega dados de Indicadores Obstétricos para todas as telas
#' @noRd
load_indicadores_data <- function(path_data = app_sys("app", "data")) {
  library(readr)
  library(dplyr)
  library(janitor)

  # SINASC principais indicadores
  sinasc <- read_csv(gzfile(file.path(path_data, "dados_oobr_indicadores_obstetricos_sinasc_1996_2024.csv.gz"))) %>%
    clean_names()

  # Partos prematuros + consultas
  premat_cons <- read_csv(gzfile(file.path(path_data, "dados_oobr_indicadores_obstetricos_prematuridade_consultas_1996_2024.csv.gz"))) %>%
    clean_names()

  # Robson (classificação)
  robson <- read_csv(gzfile(file.path(path_data, "dados_oobr_indicadores_obstetricos_robson_1996_2024.csv.gz"))) %>%
    clean_names()

  # Robson + cesáreas
  robson_cesarea <- read_csv(gzfile(file.path(path_data, "dados_oobr_indicadores_obstetricos_robson_cesareas_1996_2024.csv.gz"))) %>%
    clean_names()

  # Auxiliar municípios
  tabela_aux_municipios <- read_csv(file.path(path_data, "tabela_auxiliar_municipios.csv")) %>%
    clean_names()

  # Data por extenso (última atualização)
  data_por_extenso <- readRDS(file.path(path_data, "data_por_extenso.RDS"))

  # Retorna lista
  list(
    sinasc               = sinasc,
    premat_cons          = premat_cons,
    robson               = robson,
    robson_cesarea       = robson_cesarea,
    tabela_aux_municipios = tabela_aux_municipios,
    data_por_extenso     = data_por_extenso
  )
}
