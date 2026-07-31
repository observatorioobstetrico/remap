resolve_ubs_cnes_project_dir <- function(project_dir = NULL) {
  if (!is.null(project_dir)) {
    return(normalizePath(project_dir, winslash = "/", mustWork = TRUE))
  }

  normalizePath(".", winslash = "/", mustWork = TRUE)
}

ensure_ubs_cnes_packages <- function() {
  required <- c("dplyr", "readxl", "read.dbc", "openxlsx", "jsonlite")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Pacotes ausentes: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

normalize_ubs_key <- function(x) {
  x <- toupper(as.character(x))
  x <- trimws(gsub("\\s+", " ", x))
  x_ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
  x_ascii <- gsub("[^A-Z0-9]+", " ", x_ascii)
  trimws(gsub("\\s+", " ", x_ascii))
}

get_ubs_cnes_paths <- function(project_dir = NULL) {
  project_dir <- resolve_ubs_cnes_project_dir(project_dir)
  data_dir <- file.path(project_dir, "inst", "app", "data")
  script_dir <- file.path(project_dir, "inst", "scripts", "ubs_cnes")

  list(
    project_dir = project_dir,
    data_dir = data_dir,
    raw_dir = file.path(data_dir, "ubs_cnes", "raw"),
    output_xlsx = file.path(data_dir, "ubs_cnes_aps.xlsx"),
    output_rda = file.path(data_dir, "ubs_cnes_aps.rda"),
    rras_path = file.path(data_dir, "RRAS-MUNICIPIO.xlsx"),
    metadata_path = file.path(script_dir, "metadata_ultima_atualizacao.json"),
    ftp_dir = "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/ST"
  )
}

read_ubs_cnes_mapping <- function(paths) {
  if (!file.exists(paths$rras_path)) {
    stop("Arquivo RRAS-MUNICIPIO.xlsx nao encontrado: ", paths$rras_path, call. = FALSE)
  }

  ref <- readxl::read_excel(paths$rras_path)
  names(ref) <- normalize_ubs_key(names(ref))

  required <- c("COD IBGE", "MUNICIPIO", "RRAS", "REGIAO DE SAUDE", "DRS")
  missing <- setdiff(required, names(ref))
  if (length(missing)) {
    stop("Colunas ausentes em RRAS-MUNICIPIO.xlsx: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  ref |>
    dplyr::transmute(
      cod_ibge = sprintf("%06s", as.character(.data[["COD IBGE"]])),
      municipal = toupper(as.character(.data[["MUNICIPIO"]])),
      municipal_key = normalize_ubs_key(.data[["MUNICIPIO"]]),
      rras = toupper(as.character(.data[["RRAS"]])),
      regiao_de_saude = toupper(as.character(.data[["REGIAO DE SAUDE"]])),
      drs = toupper(as.character(.data[["DRS"]]))
    ) |>
    dplyr::distinct()
}

ubs_cnes_file_name <- function(year, uf = "SP", month = 12L) {
  paste0("ST", toupper(uf), substr(as.character(year), 3L, 4L), sprintf("%02d", as.integer(month)), ".dbc")
}

download_ubs_cnes_raw <- function(project_dir = NULL,
                                  years = 2022:2025,
                                  uf = "SP",
                                  month = 12L) {
  ensure_ubs_cnes_packages()
  paths <- get_ubs_cnes_paths(project_dir)
  dir.create(paths$raw_dir, recursive = TRUE, showWarnings = FALSE)

  for (year in years) {
    file_name <- ubs_cnes_file_name(year, uf = uf, month = month)
    raw_path <- file.path(paths$raw_dir, file_name)
    source_url <- paste(paths$ftp_dir, file_name, sep = "/")

    message("Baixando CNES/ST ", year, "-", sprintf("%02d", as.integer(month)), ": ", raw_path)
    status <- system2(
      "curl.exe",
      args = c(
        "--ftp-pasv",
        "--retry", "2",
        "--connect-timeout", "30",
        "-o", raw_path,
        source_url
      )
    )

    if (!identical(status, 0L)) {
      stop("Falha ao baixar CNES/ST: ", source_url, call. = FALSE)
    }

    if (!file.exists(raw_path) || file.info(raw_path)$size == 0L) {
      stop("Download vazio para CNES/ST: ", source_url, call. = FALSE)
    }
  }

  invisible(paths)
}

read_ubs_cnes_st_file <- function(path, year) {
  if (!file.exists(path)) {
    stop("Arquivo CNES/ST ausente: ", path, call. = FALSE)
  }

  raw <- read.dbc::read.dbc(path)
  required <- c("CNES", "CODUFMUN", "TP_UNID")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("Colunas ausentes em ", basename(path), ": ", paste(missing, collapse = ", "), call. = FALSE)
  }

  raw |>
    dplyr::transmute(
      ano = as.integer(year),
      cod_ibge = sprintf("%06s", as.character(.data$CODUFMUN)),
      cnes = as.character(.data$CNES),
      tipo_unidade = as.character(.data$TP_UNID),
      competencia = as.character(.data$COMPETEN)
    ) |>
    dplyr::filter(.data$tipo_unidade == "02", !is.na(.data$cnes), nzchar(.data$cnes)) |>
    dplyr::distinct(.data$ano, .data$cod_ibge, .data$cnes, .keep_all = TRUE)
}

write_ubs_cnes_xlsx <- function(paths, municipal, validacao, metodo) {
  openxlsx::write.xlsx(
    x = list(
      municipal = as.data.frame(municipal),
      validacao = as.data.frame(validacao),
      metodo = as.data.frame(metodo)
    ),
    file = paths$output_xlsx,
    overwrite = TRUE
  )
}

build_ubs_cnes_aps_data <- function(project_dir = NULL,
                                    years = 2022:2025,
                                    uf = "SP",
                                    month = 12L,
                                    rebuild_only = FALSE) {
  ensure_ubs_cnes_packages()
  paths <- get_ubs_cnes_paths(project_dir)

  if (!isTRUE(rebuild_only)) {
    download_ubs_cnes_raw(project_dir = project_dir, years = years, uf = uf, month = month)
  }

  raw_files <- stats::setNames(
    file.path(paths$raw_dir, vapply(years, ubs_cnes_file_name, character(1), uf = uf, month = month)),
    years
  )
  missing_files <- raw_files[!file.exists(raw_files)]
  if (length(missing_files)) {
    stop(
      "Arquivos brutos ausentes. Rode sem --rebuild-only:\n- ",
      paste(missing_files, collapse = "\n- "),
      call. = FALSE
    )
  }

  mapping <- read_ubs_cnes_mapping(paths)

  raw_ubs <- dplyr::bind_rows(lapply(names(raw_files), function(year) {
    read_ubs_cnes_st_file(raw_files[[year]], as.integer(year))
  }))

  unmatched <- raw_ubs |>
    dplyr::anti_join(mapping, by = "cod_ibge") |>
    dplyr::distinct(.data$ano, .data$cod_ibge) |>
    dplyr::arrange(.data$ano, .data$cod_ibge)
  if (nrow(unmatched)) {
    stop(
      "Municipios CNES/ST sem correspondencia em RRAS-MUNICIPIO.xlsx:\n- ",
      paste(paste(unmatched$ano, unmatched$cod_ibge), collapse = "\n- "),
      call. = FALSE
    )
  }

  counts <- raw_ubs |>
    dplyr::group_by(.data$ano, .data$cod_ibge) |>
    dplyr::summarise(n_ubs = dplyr::n_distinct(.data$cnes), .groups = "drop")

  skeleton <- merge(
    data.frame(ano = as.integer(years), stringsAsFactors = FALSE),
    mapping,
    by = NULL
  )

  municipal <- skeleton |>
    dplyr::left_join(counts, by = c("ano", "cod_ibge")) |>
    dplyr::mutate(
      n_ubs = dplyr::coalesce(as.integer(.data$n_ubs), 0L),
      fonte = "CNES/ST DATASUS - Estabelecimentos",
      competencia = sprintf("%04d%02d", .data$ano, as.integer(month)),
      tipo_unidade_codigo = "02",
      tipo_unidade_descricao = "UNIDADE BASICA DE SAUDE"
    ) |>
    dplyr::select(
      "ano",
      "competencia",
      "cod_ibge",
      "rras",
      "drs",
      "regiao_de_saude",
      "municipal",
      "municipal_key",
      "n_ubs",
      "tipo_unidade_codigo",
      "tipo_unidade_descricao",
      "fonte"
    ) |>
    dplyr::arrange(.data$ano, .data$rras, .data$municipal)

  validacao <- municipal |>
    dplyr::group_by(.data$ano, .data$competencia) |>
    dplyr::summarise(
      municipios_total = dplyr::n(),
      municipios_com_ubs = sum(.data$n_ubs > 0L, na.rm = TRUE),
      total_ubs = sum(.data$n_ubs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      raw_ubs |>
        dplyr::group_by(.data$ano) |>
        dplyr::summarise(
          registros_ubs_raw = dplyr::n(),
          cnes_distintos_raw = dplyr::n_distinct(.data$cnes),
          .groups = "drop"
        ),
      by = "ano"
    )

  metodo <- data.frame(
    campo = c(
      "fonte",
      "diretorio_ftp",
      "arquivos",
      "uf",
      "mes_referencia",
      "anos",
      "regra_de_contagem",
      "nivel_disponivel_no_painel",
      "tipo_unidade_codigo",
      "tipo_unidade_descricao",
      "generated_at"
    ),
    valor = c(
      "DATASUS Transferencia de Arquivos; CNES; ST - Estabelecimentos",
      paths$ftp_dir,
      paste(basename(raw_files), collapse = ";"),
      toupper(uf),
      sprintf("%02d", as.integer(month)),
      paste(years, collapse = ";"),
      "Contagem distinta de CNES com TP_UNID == '02' por municipio e ano, usando apenas a competencia de dezembro.",
      "Municipios; agregacoes por RRAS/DRS/Regiao de Saude derivadas de RRAS-MUNICIPIO.xlsx; supervisao de saude nao atualizada nesta etapa.",
      "02",
      "UNIDADE BASICA DE SAUDE",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    stringsAsFactors = FALSE
  )

  aps_ubs_cnes <- list(
    municipal = as.data.frame(municipal),
    available_years = sort(unique(as.integer(municipal$ano))),
    consolidated_years = sort(unique(as.integer(municipal$ano))),
    preliminary_year = NA_integer_,
    generated_at = Sys.time(),
    validation = as.data.frame(validacao),
    sources = list(
      ftp_dir = paths$ftp_dir,
      raw_dir = normalizePath(paths$raw_dir, winslash = "/", mustWork = FALSE),
      uf = toupper(uf),
      month = as.integer(month),
      file_pattern = "STUFYYMM.dbc",
      count_rule = "distinct CNES where TP_UNID == '02'"
    )
  )

  dir.create(dirname(paths$output_rda), recursive = TRUE, showWarnings = FALSE)
  save(aps_ubs_cnes, file = paths$output_rda, compress = "gzip")
  write_ubs_cnes_xlsx(paths, municipal, validacao, metodo)

  jsonlite::write_json(
    list(
      generated_at = format(aps_ubs_cnes$generated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      available_years = aps_ubs_cnes$available_years,
      consolidated_years = aps_ubs_cnes$consolidated_years,
      preliminary_year = NA,
      output_rda = normalizePath(paths$output_rda, winslash = "/", mustWork = FALSE),
      output_xlsx = normalizePath(paths$output_xlsx, winslash = "/", mustWork = FALSE),
      validation = validacao
    ),
    path = paths$metadata_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  cat("Municipios/anos:", nrow(municipal), "\n")
  cat("Anos consolidados:", paste(aps_ubs_cnes$consolidated_years, collapse = ", "), "\n")
  cat("Total UBS por ano:\n")
  print(validacao[, c("ano", "total_ubs", "municipios_com_ubs")])
  cat("Arquivo RDA:", normalizePath(paths$output_rda, winslash = "/", mustWork = FALSE), "\n")

  invisible(aps_ubs_cnes)
}

parse_ubs_cnes_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  list(
    rebuild_only = any(args %in% c("--rebuild-only", "--build-only"))
  )
}
