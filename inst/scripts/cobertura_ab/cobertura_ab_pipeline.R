get_cobertura_ab_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (!length(file_arg)) {
    return(NULL)
  }

  normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
}

resolve_cobertura_ab_project_dir <- function(project_dir = NULL) {
  if (!is.null(project_dir)) {
    return(normalizePath(project_dir, winslash = "/", mustWork = TRUE))
  }

  script_path <- get_cobertura_ab_script_path()
  if (!is.null(script_path)) {
    return(dirname(dirname(dirname(dirname(script_path)))))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

get_cobertura_ab_paths <- function(project_dir = NULL) {
  project_dir <- resolve_cobertura_ab_project_dir(project_dir)

  list(
    project_dir = project_dir,
    rras_path = file.path(project_dir, "inst", "app", "data", "RRAS-MUNICIPIO.xlsx"),
    raw_dir = file.path(project_dir, "inst", "app", "data", "cobertura_ab", "raw"),
    raw_path = file.path(project_dir, "inst", "app", "data", "cobertura_ab", "raw", "cobertura_ab_aps_municipio_sp_raw.rds"),
    excel_path = file.path(project_dir, "inst", "app", "data", "cobertura_ab_aps.xlsx"),
    output_path = file.path(project_dir, "inst", "app", "data", "cobertura_ab_aps.rda"),
    api_base_url = "https://relatorioaps-prd.saude.gov.br",
    cobertura_path = "/cobertura/aps",
    competencias_path = "/data/competencias-cnes/2",
    state_code = "35",
    unit_code = "MUNICIPIO"
  )
}

ensure_cobertura_ab_packages <- function() {
  required_packages <- c("dplyr", "janitor", "jsonlite", "openxlsx", "readxl")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages)) {
    stop(
      paste0("Pacotes ausentes: ", paste(missing_packages, collapse = ", ")),
      call. = FALSE
    )
  }
}

build_query_string <- function(params) {
  paste(
    vapply(
      names(params),
      function(name) {
        value <- params[[name]]
        paste0(utils::URLencode(name, reserved = TRUE), "=", utils::URLencode(as.character(value), reserved = TRUE))
      },
      character(1)
    ),
    collapse = "&"
  )
}

fetch_api_text <- function(url) {
  options(timeout = max(300, getOption("timeout", default = 60)))

  con <- base::url(url, open = "rb")
  on.exit(close(con), add = TRUE)

  raw <- readBin(con, what = "raw", n = 1024 * 1024 * 200)
  rawToChar(raw)
}

fetch_api_json <- function(url) {
  json_txt <- fetch_api_text(url)
  jsonlite::fromJSON(json_txt, simplifyDataFrame = TRUE, flatten = TRUE)
}

get_available_cobertura_ab_competencies <- function(project_dir = NULL) {
  ensure_cobertura_ab_packages()
  paths <- get_cobertura_ab_paths(project_dir)

  url <- paste0(paths$api_base_url, paths$competencias_path)
  competencias <- fetch_api_json(url)

  sort(unique(as.character(unlist(competencias))), decreasing = FALSE)
}

download_cobertura_ab_raw <- function(project_dir = NULL) {
  ensure_cobertura_ab_packages()
  paths <- get_cobertura_ab_paths(project_dir)

  dir.create(paths$raw_dir, recursive = TRUE, showWarnings = FALSE)

  competencias <- get_available_cobertura_ab_competencies(project_dir)
  if (!length(competencias)) {
    stop("Nenhuma competencia disponivel foi encontrada na API da APS.", call. = FALSE)
  }

  params <- list(
    unidadeGeografica = paths$unit_code,
    coUf = paths$state_code,
    nuCompInicio = min(competencias),
    nuCompFim = max(competencias)
  )

  url <- paste0(paths$api_base_url, paths$cobertura_path, "?", build_query_string(params))
  message("Baixando Cobertura AB da APS: ", url)

  payload <- fetch_api_json(url)
  payload_df <- if (is.data.frame(payload)) {
    payload
  } else if (is.list(payload) && length(payload)) {
    as.data.frame(payload, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }

  raw_payload <- list(
    downloaded_at = Sys.time(),
    source_url = url,
    competencias = competencias,
    query = params,
    data = payload_df
  )

  saveRDS(raw_payload, paths$raw_path, compress = "gzip")
  message("Download bruto salvo em: ", paths$raw_path)

  invisible(raw_payload)
}

read_cobertura_ab_mapping <- function(project_dir = NULL) {
  ensure_cobertura_ab_packages()
  paths <- get_cobertura_ab_paths(project_dir)

  if (!file.exists(paths$rras_path)) {
    stop(paste0("Arquivo RRAS-MUNICIPIO.xlsx nao encontrado: ", paths$rras_path), call. = FALSE)
  }

  standardize_regiao_saude <- function(x) {
    x <- toupper(as.character(x))
    x[x == "EXTREM OESTE PAULISTA"] <- "EXTREMO OESTE PAULISTA"
    x
  }

  readxl::read_excel(paths$rras_path) |>
    janitor::clean_names() |>
    dplyr::transmute(
      cod_ibge = suppressWarnings(as.integer(.data$cod_ibge)),
      municipal = toupper(as.character(.data$municipio)),
      rras = toupper(as.character(.data$rras)),
      regiao_de_saude = standardize_regiao_saude(.data$regiao_de_saude),
      drs = toupper(as.character(.data$drs))
    ) |>
    dplyr::distinct()
}

write_cobertura_ab_excel <- function(cobertura_anual, raw_payload, paths) {
  metadata <- data.frame(
    campo = c(
      "available_years",
      "latest_year",
      "year_basis",
      "available_competencies",
      "latest_competency",
      "downloaded_at",
      "source_url",
      "raw_path"
    ),
    valor = c(
      paste(sort(unique(cobertura_anual$ano), decreasing = TRUE), collapse = ";"),
      as.character(max(cobertura_anual$ano, na.rm = TRUE)),
      "competencia_cnes",
      paste(sort(unique(as.character(raw_payload$competencias)), decreasing = TRUE), collapse = ";"),
      max(as.character(raw_payload$competencias)),
      format(
        as.POSIXct(raw_payload$downloaded_at, tz = "UTC"),
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      raw_payload$source_url,
      normalizePath(paths$raw_path, winslash = "/", mustWork = FALSE)
    ),
    stringsAsFactors = FALSE
  )

  dir.create(dirname(paths$excel_path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(
    x = list(
      municipal = as.data.frame(cobertura_anual, stringsAsFactors = FALSE),
      metadata = metadata
    ),
    file = paths$excel_path,
    overwrite = TRUE
  )
}

read_cobertura_ab_excel <- function(paths) {
  municipal <- readxl::read_excel(paths$excel_path, sheet = "municipal") |>
    dplyr::mutate(
      ano = suppressWarnings(as.integer(.data$ano)),
      cod_ibge = suppressWarnings(as.integer(.data$cod_ibge)),
      qt_cobertura_ab = suppressWarnings(as.numeric(.data$qt_cobertura_ab)),
      qt_cobertura_esf = suppressWarnings(as.numeric(.data$qt_cobertura_esf)),
      qt_populacao = suppressWarnings(as.numeric(.data$qt_populacao)),
      cobertura_ab = suppressWarnings(as.numeric(.data$cobertura_ab)),
      cobertura_esf = suppressWarnings(as.numeric(.data$cobertura_esf))
    )

  metadata_df <- readxl::read_excel(paths$excel_path, sheet = "metadata")
  metadata <- stats::setNames(as.list(metadata_df$valor), metadata_df$campo)

  available_years <- sort(unique(stats::na.omit(municipal$ano)), decreasing = TRUE)
  latest_year <- if (length(available_years)) max(available_years, na.rm = TRUE) else NA_integer_

  competencias <- metadata$available_competencies
  if (length(competencias) && !is.null(competencias) && !is.na(competencias)) {
    competencias <- strsplit(as.character(competencias), ";", fixed = TRUE)[[1]]
    competencias <- sort(unique(trimws(competencias)), decreasing = TRUE)
  } else {
    competencias <- character()
  }

  downloaded_at <- metadata$downloaded_at
  if (length(downloaded_at) && !is.null(downloaded_at) && !is.na(downloaded_at)) {
    downloaded_at <- as.POSIXct(as.character(downloaded_at), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else {
    downloaded_at <- NA
  }

  list(
    municipal = municipal,
    available_years = available_years,
    latest_year = latest_year,
    year_basis = if (!is.null(metadata$year_basis) && !is.na(metadata$year_basis)) {
      as.character(metadata$year_basis)
    } else {
      "competencia_cnes"
    },
    available_competencies = competencias,
    latest_competency = if (!is.null(metadata$latest_competency) && !is.na(metadata$latest_competency)) {
      as.character(metadata$latest_competency)
    } else if (length(competencias)) {
      max(competencias)
    } else {
      NA_character_
    },
    downloaded_at = downloaded_at,
    source_url = if (!is.null(metadata$source_url) && !is.na(metadata$source_url)) {
      as.character(metadata$source_url)
    } else {
      NA_character_
    },
    raw_path = if (!is.null(metadata$raw_path) && !is.na(metadata$raw_path)) {
      as.character(metadata$raw_path)
    } else {
      NA_character_
    }
  )
}

build_cobertura_ab_data <- function(project_dir = NULL) {
  ensure_cobertura_ab_packages()
  paths <- get_cobertura_ab_paths(project_dir)

  if (!file.exists(paths$raw_path)) {
    stop(
      paste0(
        "Arquivo bruto nao encontrado em ",
        paths$raw_path,
        ". Rode primeiro o script de atualizacao sem --rebuild-only."
      ),
      call. = FALSE
    )
  }

  raw_payload <- readRDS(paths$raw_path)
  raw_data <- raw_payload$data

  if (!is.data.frame(raw_data) || !nrow(raw_data)) {
    stop("A base bruta da APS nao contem linhas para consolidacao.", call. = FALSE)
  }

  mapping <- read_cobertura_ab_mapping(project_dir)

  extract_competencia_year <- function(x) {
    suppressWarnings(as.integer(sub("^\\d{2}/", "", as.character(x))))
  }

  anos_completos <- raw_data |>
    dplyr::transmute(
      ano = extract_competencia_year(.data$nuComp),
      competencia = as.character(.data$nuComp)
    ) |>
    dplyr::filter(!is.na(.data$ano), !is.na(.data$competencia)) |>
    dplyr::distinct(.data$ano, .data$competencia) |>
    dplyr::count(.data$ano, name = "n_competencias") |>
    dplyr::filter(.data$n_competencias == 12L) |>
    dplyr::pull(.data$ano)

  if (!length(anos_completos)) {
    stop("A base bruta da APS nao contem um ano de referencia completo para consolidacao.", call. = FALSE)
  }

  cobertura_bruta <- raw_data |>
    dplyr::transmute(
      competencia = as.character(.data$nuComp),
      ano = extract_competencia_year(.data$nuComp),
      cod_ibge = suppressWarnings(as.integer(.data$coMunicipioIbge)),
      qt_populacao = suppressWarnings(as.numeric(.data$qtPopulacao)),
      qt_cobertura_ab = suppressWarnings(as.numeric(.data$qtCapacidadeEquipe)),
      qt_cobertura_esf = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$qtEsf)),
        0
      ) * 3500
    ) |>
    dplyr::filter(
      !is.na(.data$ano),
      .data$ano %in% anos_completos,
      !is.na(.data$cod_ibge),
      !is.na(.data$qt_populacao)
    )

  cobertura_anual <- cobertura_bruta |>
    dplyr::group_by(.data$ano, .data$cod_ibge) |>
    dplyr::summarise(
      qt_cobertura_ab = mean(.data$qt_cobertura_ab, na.rm = TRUE),
      qt_cobertura_esf = mean(.data$qt_cobertura_esf, na.rm = TRUE),
      qt_populacao = mean(.data$qt_populacao, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      qt_cobertura_ab = pmin(.data$qt_cobertura_ab, .data$qt_populacao),
      qt_cobertura_esf = pmin(.data$qt_cobertura_esf, .data$qt_populacao),
      cobertura_ab = dplyr::if_else(
        .data$qt_populacao > 0,
        round(.data$qt_cobertura_ab / .data$qt_populacao * 100, 1),
        NA_real_
      ),
      cobertura_esf = dplyr::if_else(
        .data$qt_populacao > 0,
        round(.data$qt_cobertura_esf / .data$qt_populacao * 100, 1),
        NA_real_
      )
    ) |>
    dplyr::left_join(mapping, by = "cod_ibge") |>
    dplyr::filter(!is.na(.data$municipal)) |>
    dplyr::mutate(uf = "SAO PAULO") |>
    dplyr::select(
      "ano",
      "cod_ibge",
      "uf",
      "drs",
      "regiao_de_saude",
      "rras",
      "municipal",
      "qt_cobertura_ab",
      "qt_cobertura_esf",
      "qt_populacao",
      "cobertura_ab",
      "cobertura_esf"
    ) |>
    dplyr::arrange(.data$ano, .data$rras, .data$municipal)

  write_cobertura_ab_excel(
    cobertura_anual = cobertura_anual,
    raw_payload = raw_payload,
    paths = paths
  )

  aps_cobertura_ab <- read_cobertura_ab_excel(paths)

  dir.create(dirname(paths$output_path), recursive = TRUE, showWarnings = FALSE)
  save(aps_cobertura_ab, file = paths$output_path, compress = "gzip")

  message("Base consolidada em Excel salva em: ", paths$excel_path)
  message("Base consolidada salva em: ", paths$output_path)
  message("Ano mais recente disponivel: ", aps_cobertura_ab$latest_year)

  invisible(aps_cobertura_ab)
}

update_cobertura_ab_data <- function(project_dir = NULL, rebuild_only = FALSE) {
  project_dir <- resolve_cobertura_ab_project_dir(project_dir)

  if (!isTRUE(rebuild_only)) {
    download_cobertura_ab_raw(project_dir = project_dir)
  }

  build_cobertura_ab_data(project_dir = project_dir)
}

parse_cobertura_ab_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  list(
    rebuild_only = any(args %in% c("--rebuild-only", "--build-only"))
  )
}
