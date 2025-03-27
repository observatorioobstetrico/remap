# R/global_data_call.R
#' Carrega os dados para o app
#'
#' Lê os arquivos Excel da pasta inst/app/data, realiza a transformação
#' (como converter colunas de texto para uppercase e ajustar o tipo das colunas)
#' e retorna uma lista com todas as tabelas.
#'
#' @return Uma lista com os datasets: tabela_APS, tabela_1_APS_AAE, tabela_1_APS_BXRISCO, etc.
#'
#' @importFrom magrittr %>%
#' @noRd
load_data <- function() {

  # Função para converter colunas de texto para uppercase
  to_upper_df <- function(df) {
    df[] <- lapply(df, function(x) if (is.character(x)) toupper(x) else x)
    df
  }

  # Primeiro, lemos os dados do remap1 para definir os nomes originais (baseline)
  remap1 <- file.path(app_sys("app", "data"), "remap1.xlsx")
  tabela_1_APS_temp <- to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Dados"))

  # Define os nomes esperados: os nomes originais de remap1 + "RRAS" + as colunas extras desejadas
  expected_cols <- unique(c(
    names(tabela_1_APS_temp),
    "RRAS",
    "COORDENADORIA DE SAÚDE",
    "SUPERVISÃO DE SAÚDE"
  ))

  # Função para padronizar a estrutura do data.frame de APS:
  # Verifica se cada coluna de expected_cols está presente (comparando em uppercase),
  # adiciona a coluna com NA se estiver ausente e reordena conforme expected_cols.
  standardize_aps_table <- function(df, expected_cols) {
    current_cols <- toupper(names(df))
    for (col in expected_cols) {
      if (!(col %in% current_cols)) {
        df[[col]] <- NA
      }
    }
    names(df) <- toupper(names(df))
    df <- df[, expected_cols, drop = FALSE]
    df
  }

  # Função auxiliar para conversão segura da coluna "CNES"
  safe_numeric <- function(df, colname = "CNES") {
    if (colname %in% names(df)) {
      df[[colname]] <- as.numeric(df[[colname]])
    } else {
      df[[colname]] <- rep(NA, nrow(df))
    }
    df
  }

  # Definir os caminhos dos arquivos (remaps)
  remap2  <- file.path(app_sys("app", "data"), "remap2.xlsx")
  remap3  <- file.path(app_sys("app", "data"), "remap3.xlsx")
  remap4  <- file.path(app_sys("app", "data"), "remap4.xlsx")
  remap5  <- file.path(app_sys("app", "data"), "remap5.xlsx")
  remap6  <- file.path(app_sys("app", "data"), "remap6.xlsx")   # RRAS 6 (São Paulo)
  remap7  <- file.path(app_sys("app", "data"), "remap7.xlsx")
  remap8  <- file.path(app_sys("app", "data"), "remap8.xlsx")
  remap9  <- file.path(app_sys("app", "data"), "remap9.xlsx")
  remap10 <- file.path(app_sys("app", "data"), "remap10.xlsx")
  remap11 <- file.path(app_sys("app", "data"), "remap11.xlsx")
  remap12 <- file.path(app_sys("app", "data"), "remap12.xlsx")
  remap13 <- file.path(app_sys("app", "data"), "remap13.xlsx")
  remap14 <- file.path(app_sys("app", "data"), "remap14.xlsx")
  remap15 <- file.path(app_sys("app", "data"), "remap15.xlsx")
  remap16 <- file.path(app_sys("app", "data"), "remap16.xlsx")
  remap17 <- file.path(app_sys("app", "data"), "remap17.xlsx")
  remap18 <- file.path(app_sys("app", "data"), "remap18.xlsx")
  rras_municipio_path <- file.path(app_sys("app", "data"), "RRAS-MUNICIPIO.xlsx")

  ## Leitura e padronização de cada remap (apenas para a planilha de APS)
  tabela_1_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_2_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap2, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_3_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap3, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_4_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap4, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_5_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap5, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_6_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap6, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_7_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap7, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_8_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap8, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_9_APS         <- standardize_aps_table(to_upper_df(readxl::read_excel(remap9, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_10_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap10, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_11_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap11, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_12_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap12, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_13_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap13, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_14_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap14, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_15_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap15, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_16_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap16, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_17_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap17, sheet = "Tabela 1 APS - Dados")), expected_cols)
  tabela_18_APS        <- standardize_aps_table(to_upper_df(readxl::read_excel(remap18, sheet = "Tabela 1 APS - Dados")), expected_cols)

  # Para as tabelas de AAE e BXRISCO, tratamos separadamente
  tabela_1_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_1_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap1, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_2_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap2, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_2_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap2, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_3_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap3, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_3_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap3, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_4_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap4, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_4_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap4, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_5_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap5, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_5_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap5, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_6_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap6, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_6_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap6, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_7_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap7, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_7_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap7, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_8_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap8, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_8_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap8, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_9_APS_AAE     <- safe_numeric(to_upper_df(readxl::read_excel(remap9, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_9_APS_BXRISCO <- safe_numeric(to_upper_df(readxl::read_excel(remap9, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_10_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap10, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_10_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap10, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_11_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap11, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_11_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap11, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_12_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap12, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_12_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap12, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_13_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap13, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_13_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap13, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_14_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap14, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_14_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap14, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_15_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap15, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_15_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap15, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_16_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap16, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_16_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap16, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_17_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap17, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_17_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap17, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  tabela_18_APS_AAE    <- safe_numeric(to_upper_df(readxl::read_excel(remap18, sheet = "Tabela 1 APS - Referência AAE")), "CNES")
  tabela_18_APS_BXRISCO<- safe_numeric(to_upper_df(readxl::read_excel(remap18, sheet = "Tabela 1 APS - Referência Bx. R")), "CNES")

  # Criando coluna RRAS nas tabelas de APS
  tabela_1_APS$RRAS <- "RRAS 1"
  tabela_2_APS$RRAS <- "RRAS 2"
  tabela_3_APS$RRAS <- "RRAS 3"
  tabela_4_APS$RRAS <- "RRAS 4"
  tabela_5_APS$RRAS <- "RRAS 5"
  tabela_6_APS$RRAS <- "RRAS 6"
  tabela_7_APS$RRAS <- "RRAS 7"
  tabela_8_APS$RRAS <- "RRAS 8"
  tabela_9_APS$RRAS <- "RRAS 9"
  tabela_10_APS$RRAS <- "RRAS 10"
  tabela_11_APS$RRAS <- "RRAS 11"
  tabela_12_APS$RRAS <- "RRAS 12"
  tabela_13_APS$RRAS <- "RRAS 13"
  tabela_14_APS$RRAS <- "RRAS 14"
  tabela_15_APS$RRAS <- "RRAS 15"
  tabela_16_APS$RRAS <- "RRAS 16"
  tabela_17_APS$RRAS <- "RRAS 17"
  tabela_18_APS$RRAS <- "RRAS 18"

  # Unindo os dados APS (com a estrutura padronizada, incluindo a RRAS 6)
  tabela_APS <- rbind(
    tabela_1_APS,
    tabela_2_APS,
    tabela_3_APS,
    tabela_4_APS,
    tabela_5_APS,
    tabela_6_APS,
    tabela_7_APS,
    tabela_8_APS,
    tabela_9_APS,
    tabela_10_APS,
    tabela_11_APS,
    tabela_12_APS,
    tabela_13_APS,
    tabela_14_APS,
    tabela_15_APS,
    tabela_16_APS,
    tabela_17_APS,
    tabela_18_APS
  )

  # Renomeando colunas de APS para garantir que serão exibidas com os nomes desejados,
  # incluindo as duas novas variáveis.
  colnames(tabela_APS) <- toupper(c(
    "DRS",
    "REGIÃO DE SAÚDE",
    "MUNICIPAL",
    "Nº DE NASCIDOS VIVOS",
    "COBERTURA ANS %",
    "Nº DE UBS",
    "COBERTURA ESF %",
    "COBERTURA AB %",
    "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO",
    "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
    "RRAS",
    "COORDENADORIA DE SAÚDE",
    "SUPERVISÃO DE SAÚDE"
  ))

  # Converter para numérico todas as colunas que devem ser somadas
  # Mantendo como caracteres as colunas: DRS, REGIÃO DE SAÚDE, MUNICIPAL, RRAS, COORDENADORIA DE SAÚDE e SUPERVISÃO DE SAUDE
  tabela_APS <- tabela_APS %>%
    dplyr::mutate(across(
      .cols = -c("DRS", "REGIÃO DE SAÚDE", "MUNICIPAL", "RRAS", "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE"),
      .fns = ~ as.numeric(.)
    ))

  # Leitura da planilha RRAS-MUNICIPIO e padronização
  rras_municipio <- to_upper_df(readxl::read_excel(rras_municipio_path))

  tabela_APS$RRAS <- factor(tabela_APS$RRAS, levels = paste0("RRAS ", 1:18))

  # Retorna lista com todos os dados
  list(
    tabela_APS = tabela_APS,
    tabela_1_APS_AAE = tabela_1_APS_AAE,
    tabela_1_APS_BXRISCO = tabela_1_APS_BXRISCO,
    tabela_2_APS_AAE = tabela_2_APS_AAE,
    tabela_2_APS_BXRISCO = tabela_2_APS_BXRISCO,
    tabela_3_APS_AAE = tabela_3_APS_AAE,
    tabela_3_APS_BXRISCO = tabela_3_APS_BXRISCO,
    tabela_4_APS_AAE = tabela_4_APS_AAE,
    tabela_4_APS_BXRISCO = tabela_4_APS_BXRISCO,
    tabela_5_APS_AAE = tabela_5_APS_AAE,
    tabela_5_APS_BXRISCO = tabela_5_APS_BXRISCO,
    tabela_6_APS_AAE = tabela_6_APS_AAE,
    tabela_6_APS_BXRISCO = tabela_6_APS_BXRISCO,
    tabela_7_APS_AAE = tabela_7_APS_AAE,
    tabela_7_APS_BXRISCO = tabela_7_APS_BXRISCO,
    tabela_8_APS_AAE = tabela_8_APS_AAE,
    tabela_8_APS_BXRISCO = tabela_8_APS_BXRISCO,
    tabela_9_APS_AAE = tabela_9_APS_AAE,
    tabela_9_APS_BXRISCO = tabela_9_APS_BXRISCO,
    tabela_10_APS_AAE = tabela_10_APS_AAE,
    tabela_10_APS_BXRISCO = tabela_10_APS_BXRISCO,
    tabela_11_APS_AAE = tabela_11_APS_AAE,
    tabela_11_APS_BXRISCO = tabela_11_APS_BXRISCO,
    tabela_12_APS_AAE = tabela_12_APS_AAE,
    tabela_12_APS_BXRISCO = tabela_12_APS_BXRISCO,
    tabela_13_APS_AAE = tabela_13_APS_AAE,
    tabela_13_APS_BXRISCO = tabela_13_APS_BXRISCO,
    tabela_14_APS_AAE = tabela_14_APS_AAE,
    tabela_14_APS_BXRISCO = tabela_14_APS_BXRISCO,
    tabela_15_APS_AAE = tabela_15_APS_AAE,
    tabela_15_APS_BXRISCO = tabela_15_APS_BXRISCO,
    tabela_16_APS_AAE = tabela_16_APS_AAE,
    tabela_16_APS_BXRISCO = tabela_16_APS_BXRISCO,
    tabela_17_APS_AAE = tabela_17_APS_AAE,
    tabela_17_APS_BXRISCO = tabela_17_APS_BXRISCO,
    tabela_18_APS_AAE = tabela_18_APS_AAE,
    tabela_18_APS_BXRISCO = tabela_18_APS_BXRISCO,
    rras_municipio = rras_municipio
  )
}
