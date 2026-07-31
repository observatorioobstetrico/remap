# Corrige nomes municipais nas planilhas legadas de 2020.
#
# Este script altera apenas a coluna "MUNICIPIO DA RRAS" da aba
# "Tabela 1 APS - Dados" nos arquivos remap*.xlsx listados abaixo.

required_packages <- c("openxlsx", "dplyr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages)) {
  stop(
    "Pacotes ausentes: ",
    paste(missing_packages, collapse = ", "),
    ". Instale-os antes de executar esta correcao.",
    call. = FALSE
  )
}

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "DESCRIPTION")) &&
        dir.exists(file.path(current, "inst", "app", "data"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Nao foi possivel localizar a raiz do projeto.", call. = FALSE)
    }
    current <- parent
  }
}

standardize_display <- function(x) {
  x <- toupper(as.character(x))
  x <- trimws(gsub("\\s+", " ", x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

find_municipio_col <- function(cols) {
  cols_std <- standardize_display(cols)
  idx <- which(grepl("MUNIC", cols_std))

  if (!length(idx)) {
    stop("Coluna de municipio nao encontrada.", call. = FALSE)
  }

  idx[[1]]
}

corrections <- tibble::tribble(
  ~arquivo,       ~municipio_2020,              ~municipio_corrigido,
  "remap12.xlsx", "BALSAMO",                    "BÁLSAMO",
  "remap12.xlsx", "ORINDIUVA",                  "ORINDIÚVA",
  "remap13.xlsx", "COLOMBIA",                   "COLÔMBIA",
  "remap13.xlsx", "LUÍS ANTONIO",               "LUÍS ANTÔNIO",
  "remap13.xlsx", "SANTA ROSA DO VITERBO",      "SANTA ROSA DE VITERBO",
  "remap13.xlsx", "SANTO ANTONIO DA ALEGRIA",   "SANTO ANTÔNIO DA ALEGRIA",
  "remap17.xlsx", "IIHABELA",                   "ILHABELA",
  "remap18.xlsx", "SANTA LUCIA",                "SANTA LÚCIA",
  "remap7.xlsx",  "CANANEIA",                   "CANANÉIA",
  "remap7.xlsx",  "JUQUIA",                     "JUQUIÁ",
  "remap7.xlsx",  "PARIQUERA AÇÚ",              "PARIQUERA-AÇU",
  "remap8.xlsx",  "ALUMINIO",                   "ALUMÍNIO",
  "remap8.xlsx",  "BOM SUCESSO DO ITARARÉ",     "BOM SUCESSO DE ITARARÉ",
  "remap8.xlsx",  "IBIUNA",                     "IBIÚNA",
  "remap9.xlsx",  "PONGAI",                     "PONGAÍ",
  "remap9.xlsx",  "SARUTAIA",                   "SARUTAIÁ"
)

project_root <- find_project_root()
data_dir <- file.path(project_root, "inst", "app", "data")
sheet_name <- "Tabela 1 APS - Dados"

results <- lapply(split(corrections, corrections$arquivo), function(file_corrections) {
  arquivo <- file_corrections$arquivo[[1]]
  path <- file.path(data_dir, arquivo)

  if (!file.exists(path)) {
    stop("Arquivo nao encontrado: ", path, call. = FALSE)
  }

  wb <- openxlsx::loadWorkbook(path)
  sheet_index <- match(sheet_name, names(wb))

  if (is.na(sheet_index)) {
    stop("Aba nao encontrada em ", arquivo, ": ", sheet_name, call. = FALSE)
  }

  dados <- openxlsx::readWorkbook(wb, sheet = sheet_name)
  municipio_col <- find_municipio_col(names(dados))
  municipio_values <- standardize_display(dados[[municipio_col]])

  file_results <- lapply(seq_len(nrow(file_corrections)), function(i) {
    old_name <- file_corrections$municipio_2020[[i]]
    new_name <- file_corrections$municipio_corrigido[[i]]
    rows_to_update <- which(municipio_values == standardize_display(old_name))
    already_corrected <- which(municipio_values == standardize_display(new_name))

    if (!length(rows_to_update)) {
      status <- if (length(already_corrected)) "ja_corrigido" else "nao_encontrado"
      return(tibble::tibble(
        arquivo = arquivo,
        municipio_2020 = old_name,
        municipio_corrigido = new_name,
        linhas_corrigidas = 0L,
        status = status
      ))
    }

    for (row_idx in rows_to_update) {
      openxlsx::writeData(
        wb,
        sheet = sheet_name,
        x = new_name,
        startCol = municipio_col,
        startRow = row_idx + 1L,
        colNames = FALSE,
        rowNames = FALSE
      )
    }

    tibble::tibble(
      arquivo = arquivo,
      municipio_2020 = old_name,
      municipio_corrigido = new_name,
      linhas_corrigidas = length(rows_to_update),
      status = "corrigido"
    )
  })

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  dplyr::bind_rows(file_results)
})

resultados <- dplyr::bind_rows(results)

print(resultados)

if (any(resultados$status == "nao_encontrado")) {
  stop("Ao menos uma correcao nao foi encontrada nas planilhas. Veja a tabela acima.", call. = FALSE)
}

message("Correcoes aplicadas. Linhas corrigidas: ", sum(resultados$linhas_corrigidas))
