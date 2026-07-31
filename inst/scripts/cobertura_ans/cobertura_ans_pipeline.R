get_cobertura_ans_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (!length(file_arg)) {
    return(NULL)
  }

  normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
}

resolve_cobertura_ans_project_dir <- function(project_dir = NULL) {
  if (!is.null(project_dir)) {
    return(normalizePath(project_dir, winslash = "/", mustWork = TRUE))
  }

  script_path <- get_cobertura_ans_script_path()
  if (!is.null(script_path)) {
    return(dirname(dirname(dirname(dirname(script_path)))))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

ensure_cobertura_ans_packages <- function() {
  required_packages <- c("dplyr", "jsonlite", "openxlsx", "readxl")
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

normalize_ans_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x_ascii <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
  x_ascii <- gsub("[^A-Z0-9]+", " ", x_ascii)
  trimws(gsub("\\s+", " ", x_ascii))
}

parse_ans_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "...")] <- NA_character_
  x <- gsub(".", "", x, fixed = TRUE)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

extract_ans_prn_block <- function(path) {
  lines <- readLines(path, encoding = "latin1", warn = FALSE)
  start <- grep("<PRE>", lines, fixed = TRUE)
  end <- grep("</PRE>", lines, fixed = TRUE)

  if (!length(start) || !length(end) || end[[1]] <= start[[1]]) {
    stop("Bloco PRN nao encontrado em: ", path, call. = FALSE)
  }

  first <- sub(".*<PRE>", "", lines[[start[[1]]]])
  last <- sub("</PRE>.*", "", lines[[end[[1]]]])
  middle <- if ((start[[1]] + 1L) <= (end[[1]] - 1L)) {
    lines[(start[[1]] + 1L):(end[[1]] - 1L)]
  } else {
    character()
  }

  block <- c(first, middle, last)
  block <- trimws(block)
  block <- block[nzchar(block)]
  block <- block[block != "&"]
  block
}

parse_cobertura_ans_prn <- function(path, year) {
  block <- extract_ans_prn_block(path)
  df <- utils::read.csv2(
    text = paste(block, collapse = "\n"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (ncol(df) < 4L) {
    stop("Formato inesperado no PRN de Cobertura ANS: ", path, call. = FALSE)
  }

  municipio_label <- trimws(as.character(df[[1]]))
  cod_ibge <- ifelse(grepl("^[0-9]{6}", municipio_label), substr(municipio_label, 1L, 6L), NA_character_)
  municipio_tabnet <- trimws(sub("^[0-9]{6}\\s+", "", municipio_label))
  municipio_key <- normalize_ans_key(municipio_tabnet)

  data.frame(
    ano = as.integer(year),
    cod_ibge = cod_ibge,
    municipio_tabnet = municipio_tabnet,
    municipio_key = municipio_key,
    beneficiarios = parse_ans_number(df[[2]]),
    populacao = parse_ans_number(df[[3]]),
    cobertura_tabnet = parse_ans_number(df[[4]]),
    linha_tipo = dplyr::case_when(
      municipio_key == "TOTAL" ~ "total",
      grepl("IGNORADO", municipio_key) ~ "ignorado",
      !is.na(cod_ibge) ~ "municipio",
      TRUE ~ "outro"
    ),
    fonte = "TabNet SES-SP Matriz 47a",
    arquivo_tabnet = basename(path),
    stringsAsFactors = FALSE
  )
}

form_urlencode_cp1252 <- function(values) {
  encode_one <- function(x) {
    bytes <- iconv(as.character(x), from = "UTF-8", to = "CP1252", toRaw = TRUE)[[1]]
    safe <- as.integer(charToRaw("-_.~"))
    paste(
      vapply(as.integer(bytes), function(byte) {
        if ((byte >= 48 && byte <= 57) ||
            (byte >= 65 && byte <= 90) ||
            (byte >= 97 && byte <= 122) ||
            byte %in% safe) {
          rawToChar(as.raw(byte))
        } else {
          paste0("%", toupper(sprintf("%02x", byte)))
        }
      }, character(1)),
      collapse = ""
    )
  }

  paste(
    unlist(
      lapply(names(values), function(name) {
        value <- values[[name]]
        vapply(value, function(item) {
          paste0(encode_one(name), "=", encode_one(item))
        }, character(1))
      }),
      use.names = FALSE
    ),
    collapse = "&"
  )
}

get_cobertura_ans_paths <- function(project_dir = NULL) {
  project_dir <- resolve_cobertura_ans_project_dir(project_dir)
  data_dir <- file.path(project_dir, "inst", "app", "data")

  list(
    project_dir = project_dir,
    data_dir = data_dir,
    raw_dir = file.path(data_dir, "cobertura_ans", "raw"),
    output_xlsx = file.path(data_dir, "cobertura_ans_aps.xlsx"),
    output_rda = file.path(data_dir, "cobertura_ans_aps.rda"),
    rras_path = file.path(data_dir, "RRAS-MUNICIPIO.xlsx"),
    metadata_path = file.path(project_dir, "inst", "scripts", "cobertura_ans", "metadata_ultima_atualizacao.json"),
    form_url = "https://tabnet.saude.sp.gov.br/deftohtm.exe?tabnet/ind47a_matriz.def",
    post_url = "https://tabnet.saude.sp.gov.br/tabcgi.exe?tabnet/ind47a_matriz.def"
  )
}

read_cobertura_ans_mapping <- function(paths) {
  if (!file.exists(paths$rras_path)) {
    stop("Arquivo RRAS-MUNICIPIO.xlsx nao encontrado: ", paths$rras_path, call. = FALSE)
  }

  ref <- readxl::read_excel(paths$rras_path)
  names(ref) <- normalize_ans_key(names(ref))

  required <- c("COD IBGE", "MUNICIPIO", "RRAS", "REGIAO DE SAUDE", "DRS")
  missing <- setdiff(required, names(ref))
  if (length(missing)) {
    stop("Colunas ausentes em RRAS-MUNICIPIO.xlsx: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  ref |>
    dplyr::transmute(
      cod_ibge = as.character(.data[["COD IBGE"]]),
      municipal = toupper(as.character(.data[["MUNICIPIO"]])),
      municipal_key = normalize_ans_key(.data[["MUNICIPIO"]]),
      rras = toupper(as.character(.data[["RRAS"]])),
      regiao_de_saude = toupper(as.character(.data[["REGIAO DE SAUDE"]])),
      drs = toupper(as.character(.data[["DRS"]]))
    ) |>
    dplyr::distinct()
}

download_cobertura_ans_raw <- function(project_dir = NULL, years = 2020:2025) {
  ensure_cobertura_ans_packages()
  paths <- get_cobertura_ans_paths(project_dir)
  dir.create(paths$raw_dir, recursive = TRUE, showWarnings = FALSE)

  for (year in years) {
    raw_path <- file.path(
      paths$raw_dir,
      paste0("ses_sp_cobertura_ans_municipio_", year, ".prn.html")
    )

    payload <- form_urlencode_cp1252(list(
      Linha = "Munic\u00edpio",
      Coluna = "--N\u00e3o-Ativa--",
      Incremento = c("Benef_sa\u00fade_suplem", "Popula\u00e7\u00e3o_total", "Cobertura_SSuple"),
      Arquivos = paste0("ans", substr(as.character(year), 3L, 4L), ".dbf"),
      formato = "prn",
      zeradas = "exibirlz"
    ))

    message("Baixando Cobertura ANS SES-SP ", year, ": ", raw_path)
    status <- system2(
      "curl.exe",
      args = c(
        "-L",
        "--connect-timeout", "30",
        "-X", "POST",
        paths$post_url,
        "--data-raw", payload,
        "-o", raw_path
      )
    )

    if (!identical(status, 0L)) {
      stop("Falha ao baixar Cobertura ANS SES-SP para o ano ", year, call. = FALSE)
    }

    if (!file.exists(raw_path) || file.info(raw_path)$size == 0L) {
      stop("Download vazio para Cobertura ANS SES-SP no ano ", year, call. = FALSE)
    }
  }

  invisible(paths)
}

write_cobertura_ans_xlsx <- function(paths, municipal, validacao) {
  metadata <- data.frame(
    campo = c(
      "source_form_url",
      "source_post_url",
      "available_years",
      "consolidated_years",
      "preliminary_year",
      "coverage_formula",
      "generated_at"
    ),
    valor = c(
      paths$form_url,
      paths$post_url,
      paste(sort(unique(municipal$ano)), collapse = ";"),
      paste(sort(unique(municipal$ano[municipal$ano != 2025L])), collapse = ";"),
      "2025",
      "(beneficiarios / populacao) * 100",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    stringsAsFactors = FALSE
  )

  openxlsx::write.xlsx(
    x = list(
      municipal = as.data.frame(municipal),
      validacao = as.data.frame(validacao),
      metadata = metadata
    ),
    file = paths$output_xlsx,
    overwrite = TRUE
  )
}

build_cobertura_ans_data <- function(project_dir = NULL,
                                     years = 2020:2025,
                                     rebuild_only = FALSE) {
  ensure_cobertura_ans_packages()
  paths <- get_cobertura_ans_paths(project_dir)

  if (!isTRUE(rebuild_only)) {
    download_cobertura_ans_raw(project_dir = project_dir, years = years)
  }

  raw_files <- stats::setNames(
    file.path(paths$raw_dir, paste0("ses_sp_cobertura_ans_municipio_", years, ".prn.html")),
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

  raw <- dplyr::bind_rows(lapply(names(raw_files), function(year) {
    parse_cobertura_ans_prn(raw_files[[year]], as.integer(year))
  }))

  mapping <- read_cobertura_ans_mapping(paths)

  municipal <- raw |>
    dplyr::filter(.data$linha_tipo == "municipio", .data$cod_ibge != "350000") |>
    dplyr::left_join(mapping, by = "cod_ibge") |>
    dplyr::filter(!is.na(.data$municipal)) |>
    dplyr::mutate(
      beneficiarios = as.numeric(.data$beneficiarios),
      populacao = as.numeric(.data$populacao),
      cobertura_ans_calculada = dplyr::if_else(
        .data$populacao > 0,
        round((.data$beneficiarios / .data$populacao) * 100, 2),
        NA_real_
      ),
      cobertura_ans_tabnet = as.numeric(.data$cobertura_tabnet),
      diferenca_abs = abs(.data$cobertura_ans_calculada - .data$cobertura_ans_tabnet),
      cobertura_ans = .data$cobertura_ans_calculada,
      fonte = "TabNet SES-SP Matriz 47a - calculado de beneficiarios/populacao"
    ) |>
    dplyr::select(
      "ano",
      "cod_ibge",
      "rras",
      "drs",
      "regiao_de_saude",
      "municipal",
      "municipal_key",
      "beneficiarios",
      "populacao",
      "cobertura_ans",
      "cobertura_ans_tabnet",
      "diferenca_abs",
      "fonte",
      "arquivo_tabnet"
    ) |>
    dplyr::arrange(.data$ano, .data$rras, .data$municipal)

  validacao <- municipal |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(
      municipios = dplyr::n(),
      max_diferenca_abs = max(.data$diferenca_abs, na.rm = TRUE),
      media_diferenca_abs = mean(.data$diferenca_abs, na.rm = TRUE),
      divergencias_maiores_0_05 = sum(.data$diferenca_abs > 0.05, na.rm = TRUE),
      .groups = "drop"
    )

  if (any(validacao$divergencias_maiores_0_05 > 0L, na.rm = TRUE)) {
    warning(
      "Foram encontradas diferencas maiores que 0,05 ponto percentual entre a cobertura calculada e a cobertura do TabNet. ",
      "A base final manteve a cobertura calculada, conforme regra do painel.",
      call. = FALSE
    )
  }

  consolidated_years <- sort(setdiff(years, 2025L))
  preliminary_year <- 2025L

  aps_cobertura_ans <- list(
    municipal = as.data.frame(municipal),
    available_years = sort(unique(as.integer(municipal$ano))),
    consolidated_years = consolidated_years,
    preliminary_year = preliminary_year,
    generated_at = Sys.time(),
    validation = as.data.frame(validacao),
    sources = list(
      form_url = paths$form_url,
      post_url = paths$post_url,
      raw_dir = normalizePath(paths$raw_dir, winslash = "/", mustWork = FALSE),
      formula = "(beneficiarios / populacao) * 100"
    )
  )

  dir.create(dirname(paths$output_rda), recursive = TRUE, showWarnings = FALSE)
  save(aps_cobertura_ans, file = paths$output_rda, compress = "gzip")
  write_cobertura_ans_xlsx(paths, municipal, validacao)

  jsonlite::write_json(
    list(
      generated_at = format(aps_cobertura_ans$generated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      available_years = aps_cobertura_ans$available_years,
      consolidated_years = aps_cobertura_ans$consolidated_years,
      preliminary_year = aps_cobertura_ans$preliminary_year,
      output_rda = normalizePath(paths$output_rda, winslash = "/", mustWork = FALSE),
      output_xlsx = normalizePath(paths$output_xlsx, winslash = "/", mustWork = FALSE),
      validation = validacao
    ),
    path = paths$metadata_path,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  cat("Municipios/anos:", nrow(municipal), "\n")
  cat("Anos consolidados:", paste(consolidated_years, collapse = ", "), "\n")
  cat("Ano preliminar:", preliminary_year, "\n")
  cat("Arquivo RDA:", normalizePath(paths$output_rda, winslash = "/", mustWork = FALSE), "\n")

  invisible(aps_cobertura_ans)
}

parse_cobertura_ans_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  list(
    rebuild_only = any(args %in% c("--rebuild-only", "--build-only"))
  )
}
