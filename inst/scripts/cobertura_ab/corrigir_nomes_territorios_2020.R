# Corrige nomes de DRS e Regiao de Saude nas planilhas legadas de 2020.
#
# Este script altera apenas as colunas "DRS" e "REGIAO DE SAUDE" da aba
# "Tabela 1 APS - Dados" nos arquivos e contextos explicitamente listados.

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

find_col <- function(cols, pattern, label) {
  cols_std <- standardize_display(cols)
  idx <- which(grepl(pattern, cols_std))

  if (!length(idx)) {
    stop("Coluna nao encontrada: ", label, call. = FALSE)
  }

  idx[[1]]
}

corrections <- tibble::tribble(
  ~arquivo,       ~coluna,            ~contexto_drs,  ~valor_2020,                  ~valor_corrigido,
  "remap12.xlsx", "DRS",              NA_character_,  "Sao José do Rio Preto",      "São José do Rio Preto",
  "remap13.xlsx", "REGIÃO DE SAÚDE",  "Barretos",     "Norte",                     "Norte - Barretos",
  "remap13.xlsx", "REGIÃO DE SAÚDE",  "Barretos",     "Sul",                       "Sul - Barretos",
  "remap15.xlsx", "REGIÃO DE SAÚDE",  "Campinas",     "Circuito das Aguas",        "Circuito das Águas",
  "remap18.xlsx", "REGIÃO DE SAÚDE",  "Araraquara",   "Central",                   "Central do DRS III",
  "remap18.xlsx", "REGIÃO DE SAÚDE",  "Araraquara",   "Coração",                   "Coração do DRS III"
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
  if (!(sheet_name %in% names(wb))) {
    stop("Aba nao encontrada em ", arquivo, ": ", sheet_name, call. = FALSE)
  }

  dados <- openxlsx::readWorkbook(wb, sheet = sheet_name)
  drs_col <- find_col(names(dados), "^DRS$", "DRS")
  regiao_col <- find_col(names(dados), "REGI", "REGIAO DE SAUDE")

  file_results <- lapply(seq_len(nrow(file_corrections)), function(i) {
    col_to_update <- if (identical(file_corrections$coluna[[i]], "DRS")) drs_col else regiao_col
    current_values <- standardize_display(dados[[col_to_update]])
    context_drs <- file_corrections$contexto_drs[[i]]
    old_value <- file_corrections$valor_2020[[i]]
    new_value <- file_corrections$valor_corrigido[[i]]

    rows_to_update <- which(current_values == standardize_display(old_value))
    already_corrected <- which(current_values == standardize_display(new_value))

    if (!is.na(context_drs)) {
      drs_values <- standardize_display(dados[[drs_col]])
      rows_to_update <- rows_to_update[drs_values[rows_to_update] == standardize_display(context_drs)]
      already_corrected <- already_corrected[drs_values[already_corrected] == standardize_display(context_drs)]
    }

    if (!length(rows_to_update)) {
      status <- if (length(already_corrected)) "ja_corrigido" else "nao_encontrado"
      return(tibble::tibble(
        arquivo = arquivo,
        coluna = file_corrections$coluna[[i]],
        contexto_drs = context_drs,
        valor_2020 = old_value,
        valor_corrigido = new_value,
        linhas_corrigidas = 0L,
        status = status
      ))
    }

    for (row_idx in rows_to_update) {
      openxlsx::writeData(
        wb,
        sheet = sheet_name,
        x = new_value,
        startCol = col_to_update,
        startRow = row_idx + 1L,
        colNames = FALSE,
        rowNames = FALSE
      )
    }

    tibble::tibble(
      arquivo = arquivo,
      coluna = file_corrections$coluna[[i]],
      contexto_drs = context_drs,
      valor_2020 = old_value,
      valor_corrigido = new_value,
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
