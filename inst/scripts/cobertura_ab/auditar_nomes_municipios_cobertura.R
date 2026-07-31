# Audita divergencias de nomes municipais entre o historico do painel e a base APS.
#
# Uso, a partir da raiz do projeto:
# & 'C:\Program Files\R\R-4.5.1\bin\Rscript.exe' --vanilla 'inst/scripts/cobertura_ab/auditar_nomes_municipios_cobertura.R'

required_packages <- c("readxl", "openxlsx", "dplyr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages)) {
  stop(
    "Pacotes ausentes: ",
    paste(missing_packages, collapse = ", "),
    ". Instale-os antes de executar esta auditoria.",
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

normalize_key <- function(x) {
  x_original <- standardize_display(x)
  x_ascii <- iconv(x_original, from = "", to = "ASCII//TRANSLIT")
  x_ascii[is.na(x_ascii)] <- x_original[is.na(x_ascii)]
  x_ascii <- gsub("[^A-Z0-9]+", " ", x_ascii)
  trimws(gsub("\\s+", " ", x_ascii))
}

first_matching_col <- function(cols, patterns) {
  for (pattern in patterns) {
    idx <- grep(pattern, cols, ignore.case = TRUE)
    if (length(idx)) {
      return(cols[idx[[1]]])
    }
  }

  NA_character_
}

read_legacy_aps <- function(data_dir) {
  legacy_files <- file.path(data_dir, paste0("remap", 1:18, ".xlsx"))
  missing_files <- legacy_files[!file.exists(legacy_files)]

  if (length(missing_files)) {
    stop(
      "Arquivos legados ausentes:\n- ",
      paste(basename(missing_files), collapse = "\n- "),
      call. = FALSE
    )
  }

  dplyr::bind_rows(lapply(seq_along(legacy_files), function(i) {
    raw <- readxl::read_excel(legacy_files[[i]], sheet = "Tabela 1 APS - Dados")
    names(raw) <- trimws(names(raw))

    drs_col <- first_matching_col(names(raw), c("^DRS$"))
    regiao_col <- first_matching_col(names(raw), c("REGI"))
    municipio_col <- first_matching_col(names(raw), c("MUNIC"))

    if (any(is.na(c(drs_col, regiao_col, municipio_col)))) {
      stop("Colunas esperadas nao encontradas em ", basename(legacy_files[[i]]), call. = FALSE)
    }

    tibble::tibble(
      fonte = basename(legacy_files[[i]]),
      ano = 2020L,
      rras = paste("RRAS", i),
      drs = standardize_display(raw[[drs_col]]),
      regiao_de_saude = standardize_display(raw[[regiao_col]]),
      municipio = standardize_display(raw[[municipio_col]])
    )
  })) |>
    dplyr::filter(!is.na(.data$municipio)) |>
    dplyr::distinct()
}

read_updated_coverage <- function(data_dir) {
  rda_path <- file.path(data_dir, "cobertura_ab_aps.rda")
  if (!file.exists(rda_path)) {
    stop("Arquivo nao encontrado: ", rda_path, call. = FALSE)
  }

  env <- new.env(parent = emptyenv())
  load(rda_path, envir = env)

  if (!exists("aps_cobertura_ab", envir = env, inherits = FALSE)) {
    stop("Objeto 'aps_cobertura_ab' nao encontrado em cobertura_ab_aps.rda.", call. = FALSE)
  }

  coverage <- get("aps_cobertura_ab", envir = env)
  municipal <- coverage$municipal

  if (is.null(municipal) || !is.data.frame(municipal)) {
    stop("Objeto 'aps_cobertura_ab$municipal' invalido.", call. = FALSE)
  }

  municipal |>
    dplyr::filter(.data$ano %in% c(2024L, 2025L)) |>
    dplyr::transmute(
      ano = as.integer(.data$ano),
      rras = standardize_display(.data$rras),
      drs = standardize_display(.data$drs),
      regiao_de_saude = standardize_display(.data$regiao_de_saude),
      municipio = standardize_display(.data$municipal)
    ) |>
    dplyr::filter(!is.na(.data$municipio)) |>
    dplyr::distinct()
}

best_suggestion <- function(source_name, source_drs, source_rras, candidates) {
  empty_result <- list(
    sugestao = NA_character_,
    tipo_sugestao = "sem_sugestao",
    distancia = NA_integer_,
    similaridade = NA_real_
  )

  if (is.na(source_name) || !nrow(candidates)) {
    return(empty_result)
  }

  source_key <- normalize_key(source_name)
  same_drs <- candidates[normalize_key(candidates$drs) == normalize_key(source_drs), , drop = FALSE]
  same_rras <- candidates[normalize_key(candidates$rras) == normalize_key(source_rras), , drop = FALSE]

  scoped <- if (nrow(same_drs)) {
    same_drs
  } else if (nrow(same_rras)) {
    same_rras
  } else {
    candidates
  }

  same_key <- scoped[normalize_key(scoped$municipio) == source_key, , drop = FALSE]
  if (nrow(same_key)) {
    return(list(
      sugestao = same_key$municipio[[1]],
      tipo_sugestao = "mesma_chave_sem_acentos_ou_pontuacao",
      distancia = 0L,
      similaridade = 1
    ))
  }

  candidate_keys <- normalize_key(scoped$municipio)
  distance <- as.integer(utils::adist(source_key, candidate_keys)[1, ])
  best_idx <- which.min(distance)
  best_distance <- distance[[best_idx]]
  max_chars <- max(nchar(source_key), nchar(candidate_keys[[best_idx]]), 1)
  similarity <- round(1 - (best_distance / max_chars), 3)

  list(
    sugestao = scoped$municipio[[best_idx]],
    tipo_sugestao = if (similarity >= 0.85) "possivel_erro_de_grafia" else "baixa_similaridade",
    distancia = best_distance,
    similaridade = similarity
  )
}

build_forward_audit <- function(legacy, updated) {
  names_2024 <- unique(updated$municipio[updated$ano == 2024L])
  names_2025 <- unique(updated$municipio[updated$ano == 2025L])
  candidates_2024 <- updated[updated$ano == 2024L, , drop = FALSE]
  candidates_2025 <- updated[updated$ano == 2025L, , drop = FALSE]

  audited <- legacy |>
    dplyr::mutate(
      existe_exato_2024 = .data$municipio %in% names_2024,
      existe_exato_2025 = .data$municipio %in% names_2025
    ) |>
    dplyr::filter(!.data$existe_exato_2024 | !.data$existe_exato_2025) |>
    dplyr::arrange(.data$rras, .data$drs, .data$municipio)

  if (!nrow(audited)) {
    return(audited)
  }

  suggestions_2024 <- lapply(seq_len(nrow(audited)), function(i) {
    best_suggestion(audited$municipio[[i]], audited$drs[[i]], audited$rras[[i]], candidates_2024)
  })

  suggestions_2025 <- lapply(seq_len(nrow(audited)), function(i) {
    best_suggestion(audited$municipio[[i]], audited$drs[[i]], audited$rras[[i]], candidates_2025)
  })

  audited |>
    dplyr::mutate(
      sugestao_2024 = vapply(suggestions_2024, `[[`, character(1), "sugestao"),
      tipo_sugestao_2024 = vapply(suggestions_2024, `[[`, character(1), "tipo_sugestao"),
      distancia_2024 = vapply(suggestions_2024, `[[`, integer(1), "distancia"),
      similaridade_2024 = vapply(suggestions_2024, `[[`, numeric(1), "similaridade"),
      sugestao_2025 = vapply(suggestions_2025, `[[`, character(1), "sugestao"),
      tipo_sugestao_2025 = vapply(suggestions_2025, `[[`, character(1), "tipo_sugestao"),
      distancia_2025 = vapply(suggestions_2025, `[[`, integer(1), "distancia"),
      similaridade_2025 = vapply(suggestions_2025, `[[`, numeric(1), "similaridade")
    )
}

build_reverse_audit <- function(legacy, updated) {
  legacy_names <- unique(legacy$municipio)

  updated |>
    dplyr::mutate(existe_exato_2020 = .data$municipio %in% legacy_names) |>
    dplyr::filter(!.data$existe_exato_2020) |>
    dplyr::arrange(.data$ano, .data$rras, .data$drs, .data$municipio)
}

project_root <- find_project_root()
data_dir <- file.path(project_root, "inst", "app", "data")
output_dir <- file.path(project_root, "inst", "scripts", "cobertura_ab", "outputs")
output_path <- file.path(output_dir, "comparacao_nomes_municipios_cobertura_2020_vs_2024_2025.xlsx")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

legacy <- read_legacy_aps(data_dir)
updated <- read_updated_coverage(data_dir)

forward_audit <- build_forward_audit(legacy, updated)
reverse_audit <- build_reverse_audit(legacy, updated)

summary_df <- tibble::tibble(
  item = c(
    "municipios_distintos_2020",
    "municipios_distintos_2024",
    "municipios_distintos_2025",
    "nomes_2020_sem_exato_em_2024_ou_2025",
    "nomes_2024_2025_sem_exato_em_2020",
    "arquivo_gerado"
  ),
  valor = c(
    length(unique(legacy$municipio)),
    length(unique(updated$municipio[updated$ano == 2024L])),
    length(unique(updated$municipio[updated$ano == 2025L])),
    nrow(forward_audit),
    nrow(reverse_audit),
    output_path
  )
)

workbook <- openxlsx::createWorkbook()

add_sheet <- function(wb, sheet_name, data) {
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeDataTable(wb, sheet = sheet_name, x = data, tableStyle = "TableStyleMedium2")
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = seq_along(data), widths = "auto")
}

add_sheet(workbook, "resumo", summary_df)
add_sheet(workbook, "2020_sem_exato", forward_audit)
add_sheet(workbook, "2024_2025_sem_exato_2020", reverse_audit)
add_sheet(workbook, "base_2020", legacy)
add_sheet(workbook, "base_2024_2025", updated)

openxlsx::saveWorkbook(workbook, output_path, overwrite = TRUE)

message("Arquivo gerado: ", output_path)
message("Casos 2020 sem correspondencia exata em 2024 ou 2025: ", nrow(forward_audit))
message("Casos 2024/2025 sem correspondencia exata em 2020: ", nrow(reverse_audit))
