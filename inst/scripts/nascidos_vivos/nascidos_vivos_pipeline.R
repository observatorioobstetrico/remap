normalize_nv_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

clean_nv_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  trimws(gsub("\\s+", " ", x))
}

decode_tabnet_html <- function(x) {
  x <- enc2utf8(as.character(x))
  vapply(x, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(value)
    }
    suppressWarnings(xml2::xml_text(xml2::read_html(paste0("<x>", value, "</x>"))))
  }, character(1), USE.NAMES = FALSE)
}

parse_tabnet_number <- function(x) {
  x <- clean_nv_text(x)
  x[x %in% c("", "-", "NA")] <- "0"
  x <- gsub("\\.", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

find_nv_col <- function(nms, pattern) {
  key <- normalize_nv_key(nms)
  pat <- normalize_nv_key(pattern)
  hit <- which(grepl(pat, key, fixed = TRUE))
  if (!length(hit)) {
    stop("Coluna nao encontrada: ", pattern, "\nDisponiveis: ", paste(nms, collapse = " | "), call. = FALSE)
  }
  hit[[1]]
}

alias_supervisao_key <- function(x) {
  dplyr::case_when(
    x == "MOOCA ARICANDUVA" ~ "MOOCA ARICANDUVA FORMOSA CARRAO",
    TRUE ~ x
  )
}

extract_prn_block <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  text <- rawToChar(bytes)
  text <- iconv(text, from = "CP1252", to = "UTF-8", sub = "")
  raw <- strsplit(text, "\n", fixed = TRUE)[[1]]
  raw <- gsub("\r", "", raw, fixed = TRUE)
  start <- grep("^\"", raw)[1]
  if (is.na(start)) {
    stop("Nao foi possivel localizar o bloco PRN em: ", path, call. = FALSE)
  }

  after_start <- seq.int(start + 1L, length(raw))
  end <- after_start[
    grepl("^&$", trimws(raw[after_start])) |
      grepl("</PRE>", raw[after_start], fixed = TRUE)
  ][1]
  if (is.na(end)) {
    end <- length(raw)
  } else {
    end <- end - 1L
  }

  out <- raw[start:end]
  out <- sub("</PRE>.*$", "", out)
  out[nzchar(trimws(out))]
}

parse_datasus_municipio_prn <- function(path, year) {
  block <- extract_prn_block(path)
  df <- utils::read.csv2(
    text = paste(block, collapse = "\n"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (ncol(df) < 2L) {
    stop("Formato inesperado no PRN DATASUS: ", path, call. = FALSE)
  }

  label <- decode_tabnet_html(df[[1]])
  value <- parse_tabnet_number(df[[2]])
  label_clean <- clean_nv_text(label)
  cod_ibge <- ifelse(grepl("^[0-9]{6}", label_clean), substr(label_clean, 1L, 6L), NA_character_)
  municipio <- sub("^[0-9]{6}\\s+", "", label_clean)

  data.frame(
    ano = as.integer(year),
    cod_ibge = cod_ibge,
    municipio_tabnet = municipio,
    municipio_key = normalize_nv_key(municipio),
    nascidos_vivos = value,
    linha_tipo = dplyr::case_when(
      normalize_nv_key(municipio) == "TOTAL" ~ "total",
      grepl("IGNORADO", normalize_nv_key(municipio)) ~ "ignorado",
      !is.na(cod_ibge) ~ "municipio",
      TRUE ~ "outro"
    ),
    fonte = "TabNet DATASUS SINASC SP",
    arquivo_tabnet = basename(path),
    stringsAsFactors = FALSE
  )
}

parse_prefeitura_supervisao_prn <- function(path, year) {
  block <- extract_prn_block(path)
  df <- utils::read.csv2(
    text = paste(block, collapse = "\n"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (ncol(df) < 2L) {
    stop("Formato inesperado no PRN Prefeitura SP: ", path, call. = FALSE)
  }

  supervisao <- decode_tabnet_html(df[[1]])
  supervisao <- clean_nv_text(supervisao)
  value <- parse_tabnet_number(df[[2]])
  supervisao_key <- normalize_nv_key(supervisao)

  data.frame(
    ano = as.integer(year),
    supervisao_tabnet_sp = supervisao,
    supervisao_key = alias_supervisao_key(supervisao_key),
    nascidos_vivos = value,
    linha_tipo = dplyr::case_when(
      supervisao_key == "TOTAL" ~ "total",
      supervisao_key == "IGNORADO" ~ "ignorado",
      TRUE ~ "supervisao"
    ),
    fonte = "TabNet Prefeitura SP SINASC",
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
        if ((byte >= 48L && byte <= 57L) ||
            (byte >= 65L && byte <= 90L) ||
            (byte >= 97L && byte <= 122L) ||
            byte %in% safe) {
          rawToChar(as.raw(byte))
        } else if (byte == 32L) {
          "+"
        } else {
          sprintf("%%%02X", byte)
        }
      }, character(1)),
      collapse = ""
    )
  }

  paste(
    paste0(
      vapply(names(values), encode_one, character(1)),
      "=",
      vapply(values, encode_one, character(1))
    ),
    collapse = "&"
  )
}

download_tabnet_prn <- function(url, body, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  curl_bin <- Sys.which("curl.exe")
  if (!nzchar(curl_bin)) {
    curl_bin <- Sys.which("curl")
  }
  if (!nzchar(curl_bin)) {
    stop("curl.exe nao encontrado no PATH. Use Windows 10+ ou instale curl para baixar os PRNs.", call. = FALSE)
  }

  status <- system2(
    curl_bin,
    args = c(
      "-s",
      "-L",
      "-X", "POST",
      url,
      "--data", form_urlencode_cp1252(body),
      "-o", path
    )
  )
  if (!identical(status, 0L)) {
    stop("Falha no download do TabNet via curl.exe: ", url, call. = FALSE)
  }

  invisible(path)
}

datasus_body <- function(year) {
  list(
    Linha = "Munic\u00edpio",
    Coluna = "--N\u00e3o-Ativa--",
    Incremento = "Nascim_p/resid.m\u00e3e",
    Arquivos = paste0("nvsp", substr(as.character(year), 3L, 4L), ".dbf"),
    zeradas = "exibirlz",
    formato = "prn",
    mostre = "Mostra"
  )
}

prefeitura_body <- function(year) {
  list(
    Linha = "Supervis\u00e3o_T._Sa\u00fade_resid\u00eancia",
    Coluna = "--N\u00e3o-Ativa--",
    Incremento = "NV_parturientes_residentes_MSP",
    Arquivos = paste0("dnsp", substr(as.character(year), 3L, 4L), ".dbf"),
    zeradas = "exibirlz",
    formato = "prn",
    mostre = "Mostra"
  )
}

load_rras_reference <- function(data_dir) {
  ref <- readxl::read_excel(file.path(data_dir, "RRAS-MUNICIPIO.xlsx"))
  data.frame(
    cod_ibge = sprintf("%06d", suppressWarnings(as.integer(ref[[find_nv_col(names(ref), "COD IBGE")]]))),
    municipal = toupper(clean_nv_text(ref[[find_nv_col(names(ref), "MUNICIPIO")]])),
    municipal_key = normalize_nv_key(ref[[find_nv_col(names(ref), "MUNICIPIO")]]),
    rras = clean_nv_text(ref[[find_nv_col(names(ref), "RRAS")]]),
    regiao_de_saude = toupper(clean_nv_text(ref[[find_nv_col(names(ref), "REGIAO DE SAUDE")]])),
    drs = toupper(clean_nv_text(ref[[find_nv_col(names(ref), "DRS")]])),
    stringsAsFactors = FALSE
  )
}

load_sp_supervisao_reference <- function(data_dir) {
  remap6 <- readxl::read_excel(file.path(data_dir, "remap6.xlsx"), sheet = "Tabela 1 APS - Dados")
  nms <- names(remap6)
  municipio_col <- find_nv_col(nms, "MUNICIPIO DA RRAS")
  supervisao_col <- find_nv_col(nms, "SUPERVISAO DE SAUDE")
  coordenadoria_col <- find_nv_col(nms, "COORDENADORIA DE SAUDE")
  regiao_col <- find_nv_col(nms, "REGIAO DE SAUDE")
  drs_col <- find_nv_col(nms, "DRS")

  out <- data.frame(
    municipal = toupper(clean_nv_text(remap6[[municipio_col]])),
    coordenadoria_de_saude = toupper(clean_nv_text(remap6[[coordenadoria_col]])),
    supervisao_de_saude = clean_nv_text(remap6[[supervisao_col]]),
    regiao_de_saude = toupper(clean_nv_text(remap6[[regiao_col]])),
    drs = toupper(clean_nv_text(remap6[[drs_col]])),
    stringsAsFactors = FALSE
  )
  out <- out[normalize_nv_key(out$municipal) == "SAO PAULO", , drop = FALSE]
  out$cod_ibge <- "355030"
  out$rras <- "RRAS 6"
  out$municipal_key <- "SAO PAULO"
  out$supervisao_key <- alias_supervisao_key(normalize_nv_key(out$supervisao_de_saude))
  out[!duplicated(out$supervisao_key), , drop = FALSE]
}

write_nv_xlsx <- function(path, municipal, supervisao, coordenadoria, resumo) {
  wb <- openxlsx::createWorkbook()

  add_table <- function(sheet, data) {
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeDataTable(wb, sheet, data)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = seq_along(data), widths = "auto")
  }

  add_table("Municipios", municipal)
  add_table("Supervisoes SP", supervisao)
  add_table("Coordenadorias SP", coordenadoria)
  add_table("Resumo", resumo)

  openxlsx::addWorksheet(wb, "Metodo")
  metodo <- data.frame(
    campo = c(
      "finalidade",
      "fonte_municipios",
      "fonte_sao_paulo_submunicipal",
      "anos_municipios",
      "anos_sao_paulo",
      "regra_sao_paulo",
      "arquivo_app"
    ),
    valor = c(
      "Base operacional multiano de Nascidos vivos para a tela de Atencao Primaria a Saude.",
      "TabNet DATASUS SINASC SP: Linha=Municipio; Coluna=Nao ativa; Incremento=Nascim p/resid.mae.",
      "TabNet Prefeitura SP SINASC: Linha=Supervisao T. Saude residencia; Coluna=Nao ativa; Incremento=NV parturientes residentes MSP.",
      "2020 a 2024 consolidados.",
      "2020 a 2024 consolidados e 2025 preliminar.",
      "O municipio de Sao Paulo nao usa o valor municipal do DATASUS; usa a soma das supervisoes da Prefeitura de SP.",
      "inst/app/data/nascidos_vivos_aps.rda"
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::writeDataTable(wb, "Metodo", metodo)
  openxlsx::setColWidths(wb, "Metodo", cols = 1:2, widths = c(28, 110))

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

build_nascidos_vivos_aps_data <- function(project_dir = normalizePath(".", winslash = "/", mustWork = TRUE),
                                          rebuild_only = FALSE,
                                          municipal_years = 2020:2024,
                                          sp_years = 2020:2025) {
  required <- c("dplyr", "readxl", "readr", "openxlsx", "jsonlite", "xml2")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Pacotes ausentes: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  data_dir <- file.path(project_dir, "inst", "app", "data")
  script_dir <- file.path(project_dir, "inst", "scripts", "nascidos_vivos")
  raw_dir <- file.path(data_dir, "nascidos_vivos", "raw")
  output_xlsx <- file.path(data_dir, "nascidos_vivos_aps.xlsx")
  output_rda <- file.path(data_dir, "nascidos_vivos_aps.rda")
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  datasus_form_url <- "https://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/cnv/nvsp.def"
  datasus_url <- "http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinasc/cnv/nvsp.def"
  prefeitura_form_url <- "https://tabnet.saude.prefeitura.sp.gov.br/cgi/deftohtm3.exe?secretarias/saude/TABNET/sinasc/nascido.def"
  prefeitura_url <- "https://tabnet.saude.prefeitura.sp.gov.br/cgi/tabcgi.exe?secretarias/saude/TABNET/sinasc/nascido.def"

  datasus_files <- stats::setNames(
    file.path(raw_dir, paste0("datasus_sinasc_sp_municipio_", municipal_years, ".prn.html")),
    municipal_years
  )
  prefeitura_files <- stats::setNames(
    file.path(raw_dir, paste0("prefeitura_sp_sinasc_supervisao_residencia_", sp_years, ".prn.html")),
    sp_years
  )

  if (!isTRUE(rebuild_only)) {
    for (year in municipal_years) {
      download_tabnet_prn(datasus_url, datasus_body(year), datasus_files[[as.character(year)]])
    }
    for (year in sp_years) {
      download_tabnet_prn(prefeitura_url, prefeitura_body(year), prefeitura_files[[as.character(year)]])
    }
  }

  missing_raw <- c(datasus_files, prefeitura_files)[!file.exists(c(datasus_files, prefeitura_files))]
  if (length(missing_raw)) {
    stop(
      "Arquivos brutos ausentes. Rode sem --rebuild-only para baixar:\n- ",
      paste(missing_raw, collapse = "\n- "),
      call. = FALSE
    )
  }

  rras_ref <- load_rras_reference(data_dir)
  sp_ref <- load_sp_supervisao_reference(data_dir)

  datasus_raw <- dplyr::bind_rows(lapply(names(datasus_files), function(year) {
    parse_datasus_municipio_prn(datasus_files[[year]], as.integer(year))
  }))
  prefeitura_raw <- dplyr::bind_rows(lapply(names(prefeitura_files), function(year) {
    parse_prefeitura_supervisao_prn(prefeitura_files[[year]], as.integer(year))
  }))

  datasus_municipios <- datasus_raw |>
    dplyr::filter(.data$linha_tipo == "municipio", .data$cod_ibge != "355030") |>
    dplyr::left_join(rras_ref, by = "cod_ibge") |>
    dplyr::transmute(
      ano = .data$ano,
      cod_ibge = .data$cod_ibge,
      municipal = dplyr::coalesce(.data$municipal, toupper(.data$municipio_tabnet)),
      municipal_key = dplyr::coalesce(.data$municipal_key, .data$municipio_key),
      rras = .data$rras,
      regiao_de_saude = .data$regiao_de_saude,
      drs = .data$drs,
      nascidos_vivos = as.integer(round(.data$nascidos_vivos)),
      fonte = .data$fonte,
      arquivo_tabnet = .data$arquivo_tabnet
    )

  supervisao_regular <- prefeitura_raw |>
    dplyr::filter(.data$linha_tipo == "supervisao") |>
    dplyr::left_join(sp_ref, by = "supervisao_key") |>
    dplyr::transmute(
      ano = .data$ano,
      cod_ibge = "355030",
      municipal = "SAO PAULO",
      municipal_key = "SAO PAULO",
      rras = "RRAS 6",
      regiao_de_saude = .data$regiao_de_saude,
      drs = .data$drs,
      coordenadoria_de_saude = .data$coordenadoria_de_saude,
      supervisao_de_saude = .data$supervisao_de_saude,
      supervisao_key = .data$supervisao_key,
      supervisao_tabnet_sp = .data$supervisao_tabnet_sp,
      nascidos_vivos = as.integer(round(.data$nascidos_vivos)),
      fonte = .data$fonte,
      arquivo_tabnet = .data$arquivo_tabnet
    )

  unmatched_supervisoes <- supervisao_regular |>
    dplyr::filter(is.na(.data$supervisao_de_saude)) |>
    dplyr::distinct(ano, supervisao_tabnet_sp, supervisao_key)
  if (nrow(unmatched_supervisoes)) {
    stop(
      "Supervisoes da Prefeitura SP sem correspondencia em remap6.xlsx:\n- ",
      paste(paste(unmatched_supervisoes$ano, unmatched_supervisoes$supervisao_tabnet_sp), collapse = "\n- "),
      call. = FALSE
    )
  }

  sp_regiao_de_saude <- stats::na.omit(unique(supervisao_regular$regiao_de_saude))
  sp_regiao_de_saude <- if (length(sp_regiao_de_saude)) sp_regiao_de_saude[[1L]] else "SÃO PAULO"
  sp_drs <- stats::na.omit(unique(supervisao_regular$drs))
  sp_drs <- if (length(sp_drs)) sp_drs[[1L]] else "DRS 1 - GRANDE SÃO PAULO"

  sp_ignorado <- prefeitura_raw |>
    dplyr::filter(.data$linha_tipo == "ignorado") |>
    dplyr::transmute(
      ano = as.integer(.data$ano),
      cod_ibge = "355030",
      municipal = "SAO PAULO",
      municipal_key = "SAO PAULO",
      rras = "RRAS 6",
      regiao_de_saude = sp_regiao_de_saude,
      drs = sp_drs,
      coordenadoria_de_saude = NA_character_,
      supervisao_de_saude = "Ignorado",
      supervisao_key = "IGNORADO",
      supervisao_tabnet_sp = .data$supervisao_tabnet_sp,
      nascidos_vivos = as.integer(round(.data$nascidos_vivos)),
      fonte = .data$fonte,
      arquivo_tabnet = .data$arquivo_tabnet
    )

  supervisao <- dplyr::bind_rows(supervisao_regular, sp_ignorado) |>
    dplyr::arrange(.data$ano, dplyr::if_else(.data$supervisao_key == "IGNORADO", 1L, 0L), .data$supervisao_de_saude)

  sp_municipal <- prefeitura_raw |>
    dplyr::filter(.data$linha_tipo == "total") |>
    dplyr::transmute(
      ano = as.integer(.data$ano),
      cod_ibge = "355030",
      municipal = "SAO PAULO",
      municipal_key = "SAO PAULO",
      rras = "RRAS 6",
      regiao_de_saude = sp_regiao_de_saude,
      drs = sp_drs,
      nascidos_vivos = as.integer(round(.data$nascidos_vivos)),
      fonte = "TabNet Prefeitura SP SINASC - total oficial com Ignorado",
      arquivo_tabnet = .data$arquivo_tabnet
    )

  sp_total_check <- supervisao |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(total_supervisoes_com_ignorado = sum(.data$nascidos_vivos, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(
      dplyr::select(sp_municipal, ano, total_oficial = nascidos_vivos),
      by = "ano"
    )
  if (any(sp_total_check$total_supervisoes_com_ignorado != sp_total_check$total_oficial, na.rm = TRUE)) {
    stop(
      "Divergencia entre o total oficial da Prefeitura SP e a soma de supervisoes + Ignorado:\n",
      paste(
        paste(
          sp_total_check$ano,
          sp_total_check$total_supervisoes_com_ignorado,
          sp_total_check$total_oficial,
          sep = " | "
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  municipal <- dplyr::bind_rows(datasus_municipios, sp_municipal) |>
    dplyr::arrange(.data$ano, .data$rras, .data$municipal)

  coordenadoria <- supervisao_regular |>
    dplyr::group_by(.data$ano, .data$coordenadoria_de_saude) |>
    dplyr::summarise(
      cod_ibge = "355030",
      municipal = "SAO PAULO",
      municipal_key = "SAO PAULO",
      rras = "RRAS 6",
      regiao_de_saude = dplyr::first(.data$regiao_de_saude),
      drs = dplyr::first(.data$drs),
      nascidos_vivos = sum(.data$nascidos_vivos, na.rm = TRUE),
      fonte = "TabNet Prefeitura SP SINASC - soma das supervisoes",
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$ano, .data$coordenadoria_de_saude)

  total_datasus_municipios <- datasus_raw |>
    dplyr::filter(.data$linha_tipo == "municipio") |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(total_datasus_sem_ignorado = sum(.data$nascidos_vivos, na.rm = TRUE), .groups = "drop")
  total_sp_prefeitura <- sp_municipal |>
    dplyr::select(ano, total_sao_paulo_prefeitura = nascidos_vivos)
  total_app <- municipal |>
    dplyr::filter(.data$ano %in% municipal_years) |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(total_app = sum(.data$nascidos_vivos, na.rm = TRUE), .groups = "drop")

  resumo <- dplyr::full_join(total_datasus_municipios, total_sp_prefeitura, by = "ano") |>
    dplyr::full_join(total_app, by = "ano") |>
    dplyr::arrange(.data$ano)

  aps_nascidos_vivos <- list(
    municipal = as.data.frame(municipal),
    supervisao = as.data.frame(supervisao),
    coordenadoria = as.data.frame(coordenadoria),
    available_years = sort(unique(c(municipal$ano, supervisao$ano))),
    municipal_years = as.integer(municipal_years),
    sp_years = as.integer(sp_years),
    consolidated_years = as.integer(municipal_years),
    preliminary_year = max(setdiff(sp_years, municipal_years)),
    generated_at = as.character(Sys.time()),
    sources = list(
      datasus_form_url = datasus_form_url,
      datasus_post_url = datasus_url,
      prefeitura_sp_form_url = prefeitura_form_url,
      prefeitura_sp_post_url = prefeitura_url
    )
  )

  write_nv_xlsx(output_xlsx, municipal, supervisao, coordenadoria, resumo)
  save(aps_nascidos_vivos, file = output_rda, compress = "gzip")

  metadata <- list(
    generated_at = aps_nascidos_vivos$generated_at,
    output_xlsx = output_xlsx,
    output_rda = output_rda,
    municipal_years = municipal_years,
    sp_years = sp_years,
    datasus_form_url = datasus_form_url,
    datasus_post_url = datasus_url,
    prefeitura_sp_form_url = prefeitura_form_url,
    prefeitura_sp_post_url = prefeitura_url,
    raw_dir = raw_dir
  )
  jsonlite::write_json(
    metadata,
    file.path(script_dir, "metadata_ultima_atualizacao.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  cat("Municipios/anos:", nrow(municipal), "\n")
  cat("Supervisoes SP/anos:", nrow(supervisao), "\n")
  cat("Anos municipais consolidados:", paste(municipal_years, collapse = ", "), "\n")
  cat("Ano preliminar SP:", aps_nascidos_vivos$preliminary_year, "\n")
  cat("Arquivo RDA:", output_rda, "\n")

  invisible(aps_nascidos_vivos)
}

parse_nascidos_vivos_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  list(rebuild_only = "--rebuild-only" %in% args)
}
