# R/global_data_call.R
# Adaptado para cache .rda e mantendo toda a sua lógica original

#' @import dplyr readxl magrittr
NULL

# Se estiver fora do golem e app_sys não existir, você provavelmente já tem;
# se não tiver, descomente a fallback abaixo:
# if (!exists("app_sys")) app_sys <- function(...) file.path("inst", ...)

load_data <- function(rebuild = FALSE) {

  path_data <- app_sys("app", "data")

  # --- dependências (arquivos reais no disco) ---
  remaps <- file.path(path_data, paste0("remap", 1:18, ".xlsx"))
  deps   <- c(
    remaps,
    file.path(path_data, "RRAS-MUNICIPIO.xlsx"),
    file.path(path_data, "total_sp.xlsx")
  )

  with_cache("call_data", deps, builder = function() {

    # Helpers
    to_upper_df <- function(df) {
      df[] <- lapply(df, function(x) if (is.character(x)) toupper(x) else x)
      df
    }
    standardize_aps_table <- function(df, expected_cols) {
      current_cols <- toupper(names(df))
      for (col in expected_cols) if (!(col %in% current_cols)) df[[col]] <- NA
      names(df) <- toupper(names(df))
      df[, expected_cols, drop = FALSE]
    }
    safe_numeric <- function(df, colname = "CNES") {
      if (colname %in% names(df)) df[[colname]] <- as.numeric(df[[colname]]) else df[[colname]] <- NA_real_
      df
    }

    # Baseline de nomes (remap1)
    remap1 <- file.path(path_data, "remap1.xlsx")
    tabela_1_APS_temp <- to_upper_df(
      cached_excel(remap1, sheet = "Tabela 1 APS - Dados", rebuild = rebuild)
    )

    expected_cols <- unique(c(
      names(tabela_1_APS_temp),
      "RRAS",
      "COORDENADORIA DE SAÚDE",
      "SUPERVISÃO DE SAÚDE"
    ))

    # Lê e padroniza APS (remap1..18) sempre via cache .rda
    ler_APS <- function(i) {
      xlsx <- file.path(path_data, paste0("remap", i, ".xlsx"))
      standardize_aps_table(
        to_upper_df(cached_excel(xlsx, sheet = "Tabela 1 APS - Dados", rebuild = rebuild)),
        expected_cols
      )
    }
    APS_list <- lapply(1:18, ler_APS)
    for (i in 1:18) APS_list[[i]]$RRAS <- paste("RRAS", i)
    tabela_APS <- do.call(rbind, APS_list)

    # Renomeia colunas (garante vitrine)
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

    # Numéricas
    tabela_APS <- tabela_APS %>%
      dplyr::mutate(across(
        .cols = -c("DRS", "REGIÃO DE SAÚDE", "MUNICIPAL", "RRAS", "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE"),
        .fns  = ~ suppressWarnings(as.numeric(.))
      ))
    tabela_APS$RRAS <- factor(tabela_APS$RRAS, levels = paste0("RRAS ", 1:18))

    # AAE / Baixo Risco para cada remap (sempre via cache .rda)
    ler_sheet <- function(i, sh) {
      xlsx <- file.path(path_data, paste0("remap", i, ".xlsx"))
      safe_numeric(to_upper_df(cached_excel(xlsx, sheet = sh, rebuild = rebuild)), "CNES")
    }
    tabela_AAE     <- lapply(1:18, \(i) ler_sheet(i, "Tabela 1 APS - Referência AAE"))
    tabela_BXRISCO <- lapply(1:18, \(i) ler_sheet(i, "Tabela 1 APS - Referência Bx. R"))

    names(tabela_AAE)     <- paste0("tabela_", 1:18, "_APS_AAE")
    names(tabela_BXRISCO) <- paste0("tabela_", 1:18, "_APS_BXRISCO")

    # RRAS-MUNICIPIO e total_sp (via cache .rda)
    rras_municipio <- to_upper_df(
      cached_excel(file.path(path_data, "RRAS-MUNICIPIO.xlsx"), rebuild = rebuild)
    )
    total_sp <- to_upper_df(
      cached_excel(file.path(path_data, "total_sp.xlsx"), rebuild = rebuild)
    )
    colnames(total_sp) <- toupper(c(
      "MUNICIPAL",
      "Nº DE NASCIDOS VIVOS",
      "COBERTURA ANS %",
      "Nº DE UBS",
      "COBERTURA ESF %",
      "COBERTURA AB %",
      "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO",
      "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO"
    ))

    # Monta retorno (mesmo contrato do original)
    c(
      list(tabela_APS = tabela_APS),
      tabela_AAE,
      tabela_BXRISCO,
      list(
        rras_municipio = rras_municipio,
        total_sp       = total_sp
      )
    )
  }, rebuild = rebuild)
}
