# Fluxo de extracao para as telas de obitos "Oficiais" e
# "Nao considerados".
#
# Objetivo:
# - reconstruir as duas bases consumidas pelo app a partir do SIM;
# - preservar a regra do painel original para classificar os registros;
# - acrescentar o municipio de ocorrencia a partir de CODMUNOCOR;
# - validar que a nova dimensao nao altera os totais quando agregada de volta.
#
# Este script nao depende da pasta painel-obitos-grav-puerp-main em tempo de
# execucao. As tabelas auxiliares necessarias ficam versionadas em:
# inst/scripts/obitos_grav_puerp/auxiliary/

get_obitos_grav_puerp_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }

  if (!is.null(sys.frame(1)$ofile)) {
    return(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))
  }

  normalizePath("inst/scripts/obitos_grav_puerp/update_obitos_grav_puerp.R", winslash = "/", mustWork = TRUE)
}

resolve_obitos_grav_puerp_project_dir <- function(project_dir = NULL) {
  if (!is.null(project_dir)) {
    return(normalizePath(project_dir, winslash = "/", mustWork = TRUE))
  }

  script_path <- get_obitos_grav_puerp_script_path()
  normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
}

get_obitos_grav_puerp_paths <- function(project_dir = NULL) {
  project_dir <- resolve_obitos_grav_puerp_project_dir(project_dir)
  script_dir <- file.path(project_dir, "inst", "scripts", "obitos_grav_puerp")
  data_dir <- file.path(project_dir, "inst", "app", "data")
  output_dir <- file.path(script_dir, "outputs")

  list(
    project_dir = project_dir,
    script_dir = script_dir,
    data_dir = data_dir,
    raw_dir = file.path(project_dir, "data-raw", "obitos_grav_puerp", "raw"),
    auxiliary_dir = file.path(script_dir, "auxiliary"),
    output_dir = output_dir,
    report_dir = file.path(output_dir, "validacao"),
    rras_path = file.path(data_dir, "RRAS-MUNICIPIO.xlsx"),
    cid10_path = file.path(script_dir, "auxiliary", "df_cid10.csv"),
    municipios_path = file.path(script_dir, "auxiliary", "df_aux_municipios.csv"),
    app_oficiais_path = file.path(data_dir, "dados_oobr_obitos_grav_puerp_maternos_oficiais_1996_2025.csv"),
    app_nao_considerados_path = file.path(data_dir, "dados_oobr_obitos_grav_puerp_desconsiderados_1996_2025.csv"),
    candidate_oficiais_path = file.path(output_dir, "dados_oobr_obitos_grav_puerp_maternos_oficiais_1996_2025.csv"),
    candidate_nao_considerados_path = file.path(output_dir, "dados_oobr_obitos_grav_puerp_desconsiderados_1996_2025.csv"),
    reference_oficiais_path = file.path(project_dir, "painel-obitos-grav-puerp-main", "dados_oobr_obitos_grav_puerp_maternos_oficiais_1996_2025.csv"),
    reference_nao_considerados_path = file.path(project_dir, "painel-obitos-grav-puerp-main", "dados_oobr_obitos_grav_puerp_desconsiderados_1996_2025.csv")
  )
}

ensure_obitos_grav_puerp_packages <- function() {
  required_packages <- c(
    "data.table",
    "dplyr",
    "janitor",
    "jsonlite",
    "microdatasus",
    "readr",
    "readxl",
    "tibble"
  )

  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages)) {
    stop(
      "Pacotes ausentes para atualizar obitos grav/puerp: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }
}

standardize_sim_code <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x <- sub("\\.0$", "", x)
  x
}

standardize_sim_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(gsub("\\s+", " ", x))
}

standardize_compare_text <- function(x) {
  # O painel de referencia foi gravado com write.table(), que serializa aspas
  # internas como \"...\". Ao ler com readr, essas aspas chegam como barras
  # invertidas literais. Para a comparacao, canonizamos esse detalhe de CSV
  # para aspas reais; os dados e agrupamentos permanecem inalterados.
  gsub("\\\\", "\"", standardize_sim_text(x))
}

parse_year_range <- function(value, default) {
  if (is.null(value) || !nzchar(value)) {
    return(as.integer(default))
  }

  value <- trimws(value)
  if (grepl("^[0-9]{4}:[0-9]{4}$", value)) {
    parts <- as.integer(strsplit(value, ":", fixed = TRUE)[[1]])
    return(seq.int(parts[[1]], parts[[2]]))
  }

  as.integer(strsplit(value, ",", fixed = TRUE)[[1]])
}

parse_obitos_grav_puerp_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  get_arg <- function(prefix, default = NULL) {
    hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
    if (length(hit)) sub(paste0("^", prefix, "="), "", hit[[1]]) else default
  }

  list(
    rebuild_only = "--rebuild-only" %in% args,
    force_download = "--force-download" %in% args,
    apply_to_app = !("--no-apply" %in% args),
    stop_on_target_mismatch = !("--allow-reference-mismatch" %in% args),
    resident_uf = toupper(get_arg("--resident-uf", "SP")),
    historical_years = parse_year_range(get_arg("--historical-years", NULL), 1996:2022),
    preliminary_years = parse_year_range(get_arg("--preliminary-years", NULL), 2023:2025),
    timeout = as.integer(get_arg("--timeout", "600"))
  )
}

sim_required_vars <- function() {
  c(
    "CODMUNRES", "CODMUNOCOR", "DTOBITO", "IDADE", "SEXO", "CAUSABAS",
    "OBITOGRAV", "OBITOPUERP", "RACACOR", "ESTCIV", "LOCOCOR",
    "ASSISTMED", "NECROPSIA", "ESC2010", "FONTEINV"
  )
}

preliminary_source_url <- function(year) {
  if (identical(as.integer(year), 2023L)) {
    return("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO23OPEN.csv")
  }
  if (identical(as.integer(year), 2024L)) {
    return("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/DO24OPEN.csv")
  }
  if (identical(as.integer(year), 2025L)) {
    return("https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SIM/csv/DO25OPEN_csv.zip")
  }

  stop("Ano preliminar sem URL cadastrada: ", year, call. = FALSE)
}

download_file_if_needed <- function(url, path, rebuild_only = FALSE, force_download = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path) && !isTRUE(force_download)) {
    return(path)
  }

  if (isTRUE(rebuild_only)) {
    stop(
      "Arquivo bruto ausente para rebuild-only: ", path,
      "\nRode sem --rebuild-only para baixar a fonte.",
      call. = FALSE
    )
  }

  message("Baixando: ", url)
  utils::download.file(url, path, mode = "wb", quiet = FALSE)
  path
}

fetch_historical_sim_year <- function(year,
                                      resident_uf,
                                      raw_dir,
                                      rebuild_only = FALSE,
                                      force_download = FALSE,
                                      timeout = 600) {
  raw_path <- file.path(raw_dir, paste0("sim_do_res_", tolower(resident_uf), "_", year, ".rds"))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  if (file.exists(raw_path) && !isTRUE(force_download)) {
    cached <- readRDS(raw_path)
    if (nrow(cached)) {
      return(cached)
    }

    if (isTRUE(rebuild_only)) {
      stop(
        "Arquivo bruto existe, mas esta vazio: ", raw_path,
        "\nRode sem --rebuild-only para baixar novamente.",
        call. = FALSE
      )
    }

    message("Arquivo bruto vazio encontrado; baixando novamente: ", raw_path)
  }

  if (isTRUE(rebuild_only)) {
    stop(
      "Arquivo bruto ausente para rebuild-only: ", raw_path,
      "\nRode sem --rebuild-only para baixar a fonte.",
      call. = FALSE
    )
  }

  message("Baixando SIM-DO ", year, " por UF de residencia: ", resident_uf)
  fetched <- tryCatch(
    microdatasus::fetch_datasus(
      year_start = year,
      year_end = year,
      uf = resident_uf,
      information_system = "SIM-DO",
      vars = sim_required_vars(),
      timeout = timeout
    ),
    error = function(err) {
      message(
        "Download com lista minima de variaveis falhou em ", year,
        ". Tentando arquivo completo. Erro original: ", conditionMessage(err)
      )
      microdatasus::fetch_datasus(
        year_start = year,
        year_end = year,
        uf = resident_uf,
        information_system = "SIM-DO",
        vars = NULL,
        timeout = timeout
      )
    }
  )

  fetched <- janitor::clean_names(as.data.frame(fetched))
  if (!nrow(fetched)) {
    stop(
      "Download do SIM-DO retornou zero linhas para ",
      resident_uf, " em ", year,
      ". Verifique conectividade com o DataSUS/FTP antes de continuar.",
      call. = FALSE
    )
  }

  saveRDS(fetched, raw_path, compress = "gzip")
  fetched
}

read_preliminary_sim_year <- function(year,
                                      resident_uf,
                                      raw_dir,
                                      rebuild_only = FALSE,
                                      force_download = FALSE) {
  url <- preliminary_source_url(year)
  ext <- if (grepl("\\.zip$", url, ignore.case = TRUE)) "zip" else "csv"
  raw_path <- file.path(raw_dir, paste0("DO", substr(as.character(year), 3L, 4L), "OPEN.", ext))

  download_file_if_needed(
    url = url,
    path = raw_path,
    rebuild_only = rebuild_only,
    force_download = force_download
  )

  message("Lendo SIM preliminar ", year, " e filtrando residentes em ", resident_uf)
  df <- data.table::fread(raw_path, sep = ";", showProgress = FALSE, data.table = FALSE)
  df <- janitor::clean_names(df)

  if (!"codmunres" %in% names(df)) {
    stop("A base preliminar nao contem CODMUNRES: ", raw_path, call. = FALSE)
  }

  prefix <- switch(
    resident_uf,
    SP = "35",
    stop("resident_uf ainda nao suportado para preliminares: ", resident_uf, call. = FALSE)
  )

  df |>
    dplyr::mutate(codmunres = standardize_sim_code(.data$codmunres)) |>
    dplyr::filter(substr(.data$codmunres, 1L, nchar(prefix)) == prefix)
}

load_cid10_lookup <- function(paths) {
  if (!file.exists(paths$cid10_path)) {
    stop(
      "Lookup CID10 ausente: ", paths$cid10_path,
      "\nRestaure esse arquivo ou gere uma nova versao documentada antes de atualizar os obitos.",
      call. = FALSE
    )
  }

  cid <- readr::read_csv(paths$cid10_path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names() |>
    dplyr::mutate(causabas = toupper(standardize_sim_text(.data$causabas))) |>
    dplyr::distinct(.data$causabas, .keep_all = TRUE)

  # O painel de referencia adiciona esta subcategoria para os anos preliminares.
  # Mantemos a mesma correcao de forma local e explicita.
  if (!"O142" %in% cid$causabas) {
    cid <- dplyr::bind_rows(
      cid,
      tibble::tibble(
        causabas = "O142",
        capitulo_cid10 = "XV - Gravidez, parto e puerpério",
        grupo_cid10 = "(O10-O16) Edema, proteinúria e transtornos hipertensivos na gravidez, no parto e no puerpério",
        causabas_categoria = "O14 Hipertensão gestacional (induzida pela gravidez) com proteinúria significativa",
        causabas_subcategoria = "Síndrome HELLP"
      )
    )
  }

  cid
}

load_municipio_lookup <- function(paths) {
  if (!file.exists(paths$municipios_path)) {
    stop(
      "Lookup de municipios SIM ausente: ", paths$municipios_path,
      "\nEsse arquivo precisa conter codmunres e res_codigo_adotado.",
      call. = FALSE
    )
  }

  municipios <- readr::read_csv(paths$municipios_path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names() |>
    dplyr::transmute(
      codmunres = standardize_sim_code(.data$codmunres),
      res_codigo_adotado = standardize_sim_code(.data$res_codigo_adotado),
      municipio = standardize_sim_text(.data$municipio),
      uf = standardize_sim_text(.data$uf),
      regiao = standardize_sim_text(.data$regiao)
    ) |>
    dplyr::filter(!is.na(.data$codmunres), nzchar(.data$codmunres)) |>
    dplyr::distinct(.data$codmunres, .keep_all = TRUE)

  if (anyDuplicated(municipios$codmunres)) {
    stop("Lookup de municipios contem codmunres duplicado.", call. = FALSE)
  }

  # Alguns arquivos podem trazer municipio ignorado/exterior como 0.
  if (!"0" %in% municipios$codmunres) {
    municipios <- dplyr::bind_rows(
      municipios,
      tibble::tibble(
        codmunres = "0",
        res_codigo_adotado = "0",
        municipio = "Ignorado ou exterior",
        uf = NA_character_,
        regiao = NA_character_
      )
    )
  }

  municipios
}

load_app_resident_codes <- function(paths) {
  if (!file.exists(paths$rras_path)) {
    stop("Arquivo RRAS-MUNICIPIO.xlsx ausente: ", paths$rras_path, call. = FALSE)
  }

  readxl::read_excel(paths$rras_path) |>
    janitor::clean_names() |>
    dplyr::transmute(codigo = sprintf("%06d", suppressWarnings(as.integer(.data$cod_ibge)))) |>
    dplyr::filter(!is.na(.data$codigo)) |>
    dplyr::distinct() |>
    dplyr::pull(.data$codigo)
}

normalize_sim_records <- function(df, source_type, year) {
  source_type <- match.arg(source_type, c("historical", "preliminary"))

  required <- c(
    "codmunres", "codmunocor", "dtobito", "idade", "sexo", "causabas",
    "obitograv", "obitopuerp", "racacor", "estciv", "lococor",
    "assistmed", "necropsia", "esc2010", "fonteinv"
  )

  missing_cols <- setdiff(required, names(df))
  for (col in missing_cols) {
    df[[col]] <- rep(NA_character_, nrow(df))
  }

  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::mutate(
      codmunres = standardize_sim_code(.data$codmunres),
      codmunocor = standardize_sim_code(.data$codmunocor),
      causabas = toupper(standardize_sim_text(.data$causabas)),
      dtobito = standardize_sim_text(.data$dtobito),
      idade = standardize_sim_text(.data$idade),
      sexo = standardize_sim_text(.data$sexo),
      obitograv = standardize_sim_text(.data$obitograv),
      obitopuerp = standardize_sim_text(.data$obitopuerp),
      racacor = standardize_sim_text(.data$racacor),
      estciv = standardize_sim_text(.data$estciv),
      lococor = standardize_sim_text(.data$lococor),
      assistmed = standardize_sim_text(.data$assistmed),
      necropsia = standardize_sim_text(.data$necropsia),
      esc2010 = standardize_sim_text(.data$esc2010),
      fonteinv = standardize_sim_text(.data$fonteinv)
    )

  if (identical(source_type, "historical")) {
    df <- df |>
      dplyr::mutate(
        causabas = dplyr::if_else(.data$causabas %in% c("O935", "O937"), "O95", .data$causabas),
        obitograv = dplyr::if_else(is.na(.data$obitograv) | .data$obitograv == "", "9", .data$obitograv),
        obitopuerp = dplyr::if_else(
          is.na(.data$obitopuerp) | .data$obitopuerp == "" | .data$obitopuerp %in% c("0", "4", "8"),
          "9",
          .data$obitopuerp
        )
      )
  } else {
    df <- df |>
      dplyr::mutate(
        causabas = dplyr::case_when(
          .data$causabas %in% c("O935", "O937", "O930") ~ "O95",
          .data$causabas == "O251" ~ "O25",
          .data$causabas == "O432" ~ "O439",
          .data$causabas %in% c("O969", "O960", "O961") ~ "O96",
          .data$causabas %in% c("A090", "A099") ~ "A09",
          .data$causabas %in% c("A972", "A979") ~ "A980",
          .data$causabas == "K358" ~ "K359",
          .data$causabas %in% c("I489", "I483") ~ "I48",
          .data$causabas %in% c("O971", "O979", "O970") ~ "O97",
          .data$causabas == "D686" ~ "D689",
          TRUE ~ .data$causabas
        ),
        obitograv = dplyr::if_else(is.na(.data$obitograv) | .data$obitograv == "", "9", .data$obitograv),
        obitopuerp = dplyr::if_else(is.na(.data$obitopuerp) | .data$obitopuerp == "", "9", .data$obitopuerp)
      )
  }

  df |>
    dplyr::mutate(
      ano = suppressWarnings(as.integer(substr(.data$dtobito, nchar(.data$dtobito) - 3L, nchar(.data$dtobito)))),
      ano = dplyr::if_else(is.na(.data$ano), as.integer(year), .data$ano),
      idade = suppressWarnings(as.numeric(
        dplyr::if_else(
          .data$idade == "999" | is.na(.data$idade) | .data$idade == "",
          "99",
          dplyr::if_else(
            suppressWarnings(as.numeric(.data$idade)) >= 400 & suppressWarnings(as.numeric(.data$idade)) <= 499,
            substr(.data$idade, 2L, 3L),
            "0"
          )
        )
      )),
      racacor = dplyr::case_when(
        .data$racacor == "1" ~ "Branca",
        .data$racacor == "2" ~ "Preta",
        .data$racacor == "3" ~ "Amarela",
        .data$racacor == "4" ~ "Parda",
        .data$racacor == "5" ~ "Indígena",
        is.na(.data$racacor) | .data$racacor %in% c("", "9") ~ "Ignorado",
        TRUE ~ NA_character_
      ),
      est_civil = dplyr::case_when(
        .data$estciv == "1" ~ if (identical(source_type, "preliminary")) "Solteiro" else "Solteira",
        .data$estciv == "2" ~ if (identical(source_type, "preliminary")) "Casado" else "Casada",
        .data$estciv == "3" ~ if (identical(source_type, "preliminary")) "Viúvo" else "Viúva",
        .data$estciv == "4" ~ "Separada Judic./Divorciada",
        .data$estciv == "5" ~ "União Estável",
        is.na(.data$estciv) | .data$estciv %in% c("", "9") ~ "Ignorado",
        TRUE ~ NA_character_
      ),
      local_ocorrencia_obito = dplyr::case_when(
        .data$lococor == "1" ~ "Hospital",
        .data$lococor == "2" ~ "Outro Estab. Saúde",
        .data$lococor == "3" ~ "Domicílio",
        .data$lococor == "4" ~ "Via Pública",
        .data$lococor == "5" ~ "Outros",
        is.na(.data$lococor) | .data$lococor %in% c("", "6", "9") ~ "Ignorado",
        TRUE ~ NA_character_
      ),
      assistencia_med = dplyr::case_when(
        .data$assistmed == "1" ~ "Com assistência",
        .data$assistmed == "2" ~ "Sem assistência",
        is.na(.data$assistmed) | .data$assistmed %in% c("", "9") ~ "Ignorado",
        TRUE ~ NA_character_
      ),
      necropsia = dplyr::case_when(
        .data$necropsia == "1" ~ "Sim",
        .data$necropsia == "2" ~ "Não",
        is.na(.data$necropsia) | .data$necropsia %in% c("", "9") ~ "Ignorado",
        TRUE ~ NA_character_
      ),
      obito_em_idade_fertil = dplyr::if_else(.data$idade >= 10 & .data$idade <= 49, "Sim", "Não"),
      tipo_de_morte_materna = dplyr::if_else(
        (.data$causabas >= "B200" & .data$causabas <= "B249") |
          (.data$causabas >= "O100" & .data$causabas <= "O109") |
          ((.data$causabas >= "O240" & .data$causabas != "O244") & .data$causabas <= "O259") |
          (.data$causabas == "O94") |
          (.data$causabas >= "O980" & .data$causabas <= "O999"),
        "Indireta",
        dplyr::if_else(.data$causabas == "O95", "Não especificada", "Direta")
      ),
      periodo_do_obito = dplyr::case_when(
        .data$obitograv == "1" & .data$obitopuerp != "1" & .data$obitopuerp != "2" ~ "Durante a gravidez, parto ou aborto",
        .data$obitograv != "1" & .data$obitopuerp == "1" ~ "Durante o puerpério, até 42 dias",
        .data$obitograv != "1" & .data$obitopuerp == "2" ~ "Durante o puerpério, de 43 dias a menos de 1 ano",
        (.data$obitograv == "2" & .data$obitopuerp == "3") |
          (.data$obitograv == "2" & .data$obitopuerp == "9") |
          (.data$obitograv == "9" & .data$obitopuerp == "3") ~ "Não na gravidez ou no puerpério",
        .data$obitograv == "9" & .data$obitopuerp == "9" ~ "Não informado ou ignorado",
        (.data$obitograv == "1" & .data$obitopuerp == "1") |
          (.data$obitograv == "1" & .data$obitopuerp == "2") ~ "Período inconsistente",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      escolaridade = dplyr::case_when(
        source_type == "historical" & year < 2011 ~ "Ignorado",
        .data$esc2010 == "0" ~ "Sem escolaridade",
        .data$esc2010 == "1" ~ "Fundamental I",
        .data$esc2010 == "2" ~ "Fundamental II",
        .data$esc2010 == "3" ~ "Médio",
        .data$esc2010 == "4" ~ "Superior incompleto",
        .data$esc2010 == "5" ~ "Superior completo",
        is.na(.data$esc2010) | .data$esc2010 %in% c("", "9") ~ "Ignorado",
        TRUE ~ NA_character_
      ),
      investigacao_cmm = dplyr::case_when(
        source_type == "historical" & year < 2006 ~ "Sem informação",
        .data$fonteinv == "1" ~ "Sim",
        .data$fonteinv == "9" ~ "Sem informação",
        is.na(.data$fonteinv) | .data$fonteinv == "" ~ "Sem informação",
        TRUE ~ "Não"
      )
    )
}

is_maternal_official_expr <- function(causabas, obitograv, obitopuerp) {
  ((causabas >= "O000" & causabas <= "O959") |
     (causabas >= "O980" & causabas <= "O999") |
     (causabas == "A34" & obitopuerp != "2") |
     ((causabas >= "B200" & causabas <= "B249") & (obitograv == "1" | obitopuerp == "1")) |
     (causabas == "D392" & (obitograv == "1" | obitopuerp == "1")) |
     (causabas == "E230" & (obitograv == "1" | obitopuerp == "1")) |
     ((causabas >= "F530" & causabas <= "F539") & (obitopuerp != "2")) |
     (causabas == "M830" & obitopuerp != "2"))
}

format_municipio_ocorrencia <- function(municipio, uf) {
  municipio <- as.character(municipio)
  uf <- as.character(uf)

  missing <- is.na(municipio) | trimws(municipio) == ""
  outside_sp <- !missing & !is.na(uf) & nzchar(uf) & uf != "SP" &
    !grepl("^Munic", municipio) &
    municipio != "Ignorado ou exterior"

  municipio[outside_sp] <- paste0(municipio[outside_sp], " - ", uf[outside_sp])
  municipio[missing] <- "Município de ocorrência não informado"
  municipio
}

attach_reference_dimensions <- function(df, cid10_lookup, municipio_lookup) {
  municipio_res <- municipio_lookup |>
    dplyr::select(
      codmunres,
      res_codigo_adotado,
      municipio,
      uf,
      regiao
    )

  municipio_ocor <- municipio_lookup |>
    dplyr::transmute(
      codmunocor = .data$codmunres,
      municipio_ocorrencia_nome = .data$municipio,
      municipio_ocorrencia_uf = .data$uf
    )

  df |>
    dplyr::left_join(cid10_lookup, by = "causabas") |>
    dplyr::left_join(municipio_res, by = "codmunres") |>
    dplyr::left_join(municipio_ocor, by = "codmunocor") |>
    dplyr::mutate(
      municipio_ocorrencia = format_municipio_ocorrencia(
        .data$municipio_ocorrencia_nome,
        .data$municipio_ocorrencia_uf
      )
    )
}

build_obitos_outputs_from_records <- function(df) {
  official_filter <- is_maternal_official_expr(df$causabas, df$obitograv, df$obitopuerp)

  oficiais <- df |>
    dplyr::filter(.data$sexo == "2", official_filter) |>
    dplyr::mutate(obitos = 1) |>
    dplyr::select(
      codigo = res_codigo_adotado,
      municipio,
      uf,
      regiao,
      ano,
      causabas,
      causabas_categoria,
      capitulo_cid10,
      tipo_de_morte_materna,
      municipio_ocorrencia,
      periodo_do_obito,
      investigacao_cmm,
      racacor,
      idade,
      obitos
    ) |>
    dplyr::group_by(dplyr::across(-obitos)) |>
    dplyr::summarise(obitos = sum(.data$obitos, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$codigo, .data$ano, .data$municipio_ocorrencia)

  nao_considerados <- df |>
    dplyr::filter(
      .data$sexo == "2",
      .data$obitograv == "1" | .data$obitopuerp == "1" | .data$obitopuerp == "2",
      !official_filter
    ) |>
    dplyr::mutate(obitos = 1) |>
    dplyr::select(
      codigo = res_codigo_adotado,
      ano,
      municipio,
      uf,
      regiao,
      capitulo_cid10,
      causabas_categoria,
      municipio_ocorrencia,
      periodo_do_obito,
      racacor,
      idade,
      investigacao_cmm,
      obitos
    ) |>
    dplyr::group_by(dplyr::across(-obitos)) |>
    dplyr::summarise(obitos = sum(.data$obitos, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$codigo, .data$ano, .data$municipio_ocorrencia)

  list(
    oficiais = oficiais,
    nao_considerados = nao_considerados
  )
}

old_columns_oficiais <- function() {
  c(
    "codigo", "municipio", "uf", "regiao", "ano", "causabas",
    "causabas_categoria", "capitulo_cid10", "tipo_de_morte_materna",
    "periodo_do_obito", "investigacao_cmm", "racacor", "idade", "obitos"
  )
}

old_columns_nao_considerados <- function() {
  c(
    "codigo", "ano", "municipio", "uf", "regiao", "capitulo_cid10",
    "causabas_categoria", "periodo_do_obito", "racacor", "idade",
    "investigacao_cmm", "obitos"
  )
}

add_csv_index <- function(df) {
  tibble::add_column(as.data.frame(df), X = seq_len(nrow(df)), .before = 1L)
}

write_output_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(add_csv_index(df), path, na = "")
  invisible(path)
}

read_app_csv_for_compare <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())) |>
    janitor::clean_names()
}

standardize_aggregate_for_compare <- function(df, columns, resident_codes = NULL) {
  df <- janitor::clean_names(df)

  if ("x" %in% names(df)) {
    df <- dplyr::select(df, -x)
  }

  missing_cols <- setdiff(columns, names(df))
  if (length(missing_cols)) {
    stop("Colunas ausentes para comparacao: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  df <- df |>
    dplyr::select(dplyr::all_of(columns)) |>
    dplyr::mutate(
      codigo = standardize_sim_code(.data$codigo),
      ano = suppressWarnings(as.integer(.data$ano)),
      idade = suppressWarnings(as.numeric(.data$idade)),
      obitos = suppressWarnings(as.numeric(.data$obitos))
    )

  char_cols <- setdiff(names(df), c("ano", "idade", "obitos"))
  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(char_cols), standardize_compare_text))

  if (!is.null(resident_codes)) {
    df <- dplyr::filter(df, .data$codigo %in% resident_codes)
  }

  df |>
    dplyr::group_by(dplyr::across(-obitos)) |>
    dplyr::summarise(obitos = sum(.data$obitos, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::across(dplyr::everything()))
}

collapse_new_occurrence <- function(df, old_columns) {
  standardize_aggregate_for_compare(
    dplyr::select(df, dplyr::all_of(old_columns)),
    old_columns
  )
}

compare_aggregates <- function(new_df, target_df, columns, dataset, comparison_name, report_dir) {
  keys <- setdiff(columns, "obitos")

  joined <- dplyr::full_join(
    dplyr::rename(new_df, obitos_novo = obitos),
    dplyr::rename(target_df, obitos_alvo = obitos),
    by = keys
  ) |>
    dplyr::mutate(
      obitos_novo = dplyr::coalesce(.data$obitos_novo, 0),
      obitos_alvo = dplyr::coalesce(.data$obitos_alvo, 0),
      diferenca = .data$obitos_novo - .data$obitos_alvo
    )

  diffs <- dplyr::filter(joined, .data$diferenca != 0)

  if (nrow(diffs)) {
    dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(
      diffs,
      file.path(report_dir, paste0("diferencas_", dataset, "_", comparison_name, ".csv")),
      na = ""
    )
  }

  by_year <- joined |>
    dplyr::group_by(.data$ano) |>
    dplyr::summarise(
      obitos_novo = sum(.data$obitos_novo, na.rm = TRUE),
      obitos_alvo = sum(.data$obitos_alvo, na.rm = TRUE),
      diferenca = .data$obitos_novo - .data$obitos_alvo,
      .groups = "drop"
    ) |>
    dplyr::mutate(dataset = dataset, comparacao = comparison_name, .before = 1L)

  list(
    summary = tibble::tibble(
      dataset = dataset,
      comparacao = comparison_name,
      linhas_novo = nrow(new_df),
      linhas_alvo = nrow(target_df),
      obitos_novo = sum(new_df$obitos, na.rm = TRUE),
      obitos_alvo = sum(target_df$obitos, na.rm = TRUE),
      diferenca = sum(new_df$obitos, na.rm = TRUE) - sum(target_df$obitos, na.rm = TRUE),
      combinacoes_divergentes = nrow(diffs),
      bate_exatamente = nrow(diffs) == 0
    ),
    by_year = by_year,
    differences = diffs
  )
}

validate_obitos_outputs <- function(outputs, paths, resident_codes, stop_on_target_mismatch = FALSE) {
  dir.create(paths$report_dir, recursive = TRUE, showWarnings = FALSE)
  stale_reports <- list.files(
    paths$report_dir,
    pattern = "^(diferencas_|municipio_ocorrencia_nao_informado).*\\.csv$",
    full.names = TRUE
  )
  if (length(stale_reports)) {
    unlink(stale_reports)
  }

  specs <- list(
    oficiais = list(
      data = outputs$oficiais,
      old_columns = old_columns_oficiais(),
      app_path = paths$app_oficiais_path,
      reference_path = paths$reference_oficiais_path
    ),
    nao_considerados = list(
      data = outputs$nao_considerados,
      old_columns = old_columns_nao_considerados(),
      app_path = paths$app_nao_considerados_path,
      reference_path = paths$reference_nao_considerados_path
    )
  )

  summary_rows <- list()
  by_year_rows <- list()
  hard_failures <- character()

  for (dataset in names(specs)) {
    spec <- specs[[dataset]]
    new_collapsed <- collapse_new_occurrence(spec$data, spec$old_columns)

    self_compare <- compare_aggregates(
      new_df = new_collapsed,
      target_df = new_collapsed,
      columns = spec$old_columns,
      dataset = dataset,
      comparison_name = "autochecagem_sem_municipio_ocorrencia",
      report_dir = paths$report_dir
    )
    summary_rows[[length(summary_rows) + 1L]] <- self_compare$summary
    by_year_rows[[length(by_year_rows) + 1L]] <- self_compare$by_year

    if (!isTRUE(self_compare$summary$bate_exatamente)) {
      hard_failures <- c(hard_failures, paste(dataset, "falhou na autochecagem"))
    }

    targets <- list(painel_referencia = spec$reference_path)
    for (target_name in names(targets)) {
      target_path <- targets[[target_name]]
      if (!file.exists(target_path)) {
        next
      }

      target <- read_app_csv_for_compare(target_path)
      target_std <- standardize_aggregate_for_compare(
        target,
        spec$old_columns,
        resident_codes = resident_codes
      )

      comp <- compare_aggregates(
        new_df = new_collapsed,
        target_df = target_std,
        columns = spec$old_columns,
        dataset = dataset,
        comparison_name = target_name,
        report_dir = paths$report_dir
      )
      summary_rows[[length(summary_rows) + 1L]] <- comp$summary
      by_year_rows[[length(by_year_rows) + 1L]] <- comp$by_year

      if (isTRUE(stop_on_target_mismatch) && !isTRUE(comp$summary$bate_exatamente)) {
        hard_failures <- c(hard_failures, paste(dataset, "diverge de", target_name))
      }
    }
  }

  summary <- dplyr::bind_rows(summary_rows)
  by_year <- dplyr::bind_rows(by_year_rows)

  readr::write_csv(summary, file.path(paths$report_dir, "resumo_validacao.csv"), na = "")
  readr::write_csv(by_year, file.path(paths$report_dir, "validacao_por_ano.csv"), na = "")

  if (length(hard_failures)) {
    stop(
      "Validacao interrompida:\n- ",
      paste(hard_failures, collapse = "\n- "),
      "\nVeja os arquivos em: ", paths$report_dir,
      call. = FALSE
    )
  }

  summary
}

apply_obitos_outputs_to_app <- function(paths) {
  file.copy(paths$candidate_oficiais_path, paths$app_oficiais_path, overwrite = TRUE)
  file.copy(paths$candidate_nao_considerados_path, paths$app_nao_considerados_path, overwrite = TRUE)

  # O cache principal observa o mtime dos CSVs. Tocar nos arquivos evita que
  # um cache antigo seja reutilizado quando o sistema preserva datas no copy.
  Sys.setFileTime(paths$app_oficiais_path, Sys.time())
  Sys.setFileTime(paths$app_nao_considerados_path, Sys.time())

  invisible(TRUE)
}

build_obitos_grav_puerp_data <- function(project_dir = NULL,
                                         rebuild_only = FALSE,
                                         force_download = FALSE,
                                         apply_to_app = TRUE,
                                         stop_on_target_mismatch = TRUE,
                                         resident_uf = "SP",
                                         historical_years = 1996:2022,
                                         preliminary_years = 2023:2025,
                                         timeout = 600) {
  ensure_obitos_grav_puerp_packages()

  paths <- get_obitos_grav_puerp_paths(project_dir)
  dir.create(paths$raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$report_dir, recursive = TRUE, showWarnings = FALSE)

  cid10_lookup <- load_cid10_lookup(paths)
  municipio_lookup <- load_municipio_lookup(paths)
  resident_codes <- load_app_resident_codes(paths)

  processed_chunks <- list()

  for (year in historical_years) {
    raw_year <- fetch_historical_sim_year(
      year = year,
      resident_uf = resident_uf,
      raw_dir = paths$raw_dir,
      rebuild_only = rebuild_only,
      force_download = force_download,
      timeout = timeout
    )

    processed_chunks[[paste0("historical_", year)]] <- raw_year |>
      normalize_sim_records(source_type = "historical", year = year) |>
      attach_reference_dimensions(cid10_lookup, municipio_lookup)

    rm(raw_year)
    gc(verbose = FALSE)
  }

  for (year in preliminary_years) {
    raw_year <- read_preliminary_sim_year(
      year = year,
      resident_uf = resident_uf,
      raw_dir = paths$raw_dir,
      rebuild_only = rebuild_only,
      force_download = force_download
    )

    processed_chunks[[paste0("preliminary_", year)]] <- raw_year |>
      normalize_sim_records(source_type = "preliminary", year = year) |>
      attach_reference_dimensions(cid10_lookup, municipio_lookup)

    rm(raw_year)
    gc(verbose = FALSE)
  }

  processed <- dplyr::bind_rows(processed_chunks)

  missing_residence <- processed |>
    dplyr::filter(is.na(.data$res_codigo_adotado) | is.na(.data$municipio)) |>
    dplyr::distinct(.data$codmunres)
  if (nrow(missing_residence)) {
    stop(
      "Ha codigos de municipio de residencia sem correspondencia no lookup:\n- ",
      paste(missing_residence$codmunres, collapse = "\n- "),
      call. = FALSE
    )
  }

  output_records <- processed |>
    dplyr::filter(.data$res_codigo_adotado %in% resident_codes)

  outputs <- build_obitos_outputs_from_records(output_records)

  occurrence_missing_summary <- dplyr::bind_rows(
    dplyr::mutate(outputs$oficiais, dataset = "oficiais"),
    dplyr::mutate(outputs$nao_considerados, dataset = "nao_considerados")
  ) |>
    dplyr::group_by(.data$dataset, .data$municipio_ocorrencia) |>
    dplyr::summarise(obitos = sum(.data$obitos, na.rm = TRUE), .groups = "drop") |>
    dplyr::filter(.data$municipio_ocorrencia == "Município de ocorrência não informado")

  if (nrow(occurrence_missing_summary)) {
    readr::write_csv(
      occurrence_missing_summary,
      file.path(paths$report_dir, "municipio_ocorrencia_nao_informado.csv"),
      na = ""
    )
  }

  validation_summary <- validate_obitos_outputs(
    outputs = outputs,
    paths = paths,
    resident_codes = resident_codes,
    stop_on_target_mismatch = stop_on_target_mismatch
  )

  write_output_csv(outputs$oficiais, paths$candidate_oficiais_path)
  write_output_csv(outputs$nao_considerados, paths$candidate_nao_considerados_path)

  metadata <- list(
    generated_at = as.character(Sys.time()),
    resident_uf = resident_uf,
    historical_years = as.integer(historical_years),
    preliminary_years = as.integer(preliminary_years),
    raw_dir = normalizePath(paths$raw_dir, winslash = "/", mustWork = FALSE),
    auxiliary_dir = normalizePath(paths$auxiliary_dir, winslash = "/", mustWork = FALSE),
    candidate_oficiais_path = normalizePath(paths$candidate_oficiais_path, winslash = "/", mustWork = FALSE),
    candidate_nao_considerados_path = normalizePath(paths$candidate_nao_considerados_path, winslash = "/", mustWork = FALSE),
    applied_to_app = isTRUE(apply_to_app),
    validation_summary_path = normalizePath(file.path(paths$report_dir, "resumo_validacao.csv"), winslash = "/", mustWork = FALSE)
  )

  jsonlite::write_json(
    metadata,
    file.path(paths$script_dir, "metadata_ultima_atualizacao.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  if (isTRUE(apply_to_app)) {
    apply_obitos_outputs_to_app(paths)
  }

  message("Oficiais: ", sum(outputs$oficiais$obitos, na.rm = TRUE), " obitos; ", nrow(outputs$oficiais), " linhas.")
  message("Nao considerados: ", sum(outputs$nao_considerados$obitos, na.rm = TRUE), " obitos; ", nrow(outputs$nao_considerados), " linhas.")
  message("Resumo da validacao: ", file.path(paths$report_dir, "resumo_validacao.csv"))
  if (isTRUE(apply_to_app)) {
    message("CSVs do app atualizados em: ", paths$data_dir)
  } else {
    message("CSVs candidatos gravados em: ", paths$output_dir)
  }

  invisible(list(
    outputs = outputs,
    validation_summary = validation_summary,
    paths = paths,
    metadata = metadata
  ))
}
