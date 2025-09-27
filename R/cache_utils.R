# R/cache_utils.R
# Utilidades simples para cache em .rda

cache_dir <- function() {
  file.path(app_sys("app", "data"), "_rda")
}

.ensure_cache_dir <- function() {
  dir.create(cache_dir(), recursive = TRUE, showWarnings = FALSE)
}

cache_file <- function(name) {
  file.path(cache_dir(), paste0(name, ".rda"))
}

.newest_mtime <- function(paths) {
  # ignora NAs (arquivos que não existem)
  mt <- suppressWarnings(file.mtime(paths))
  max(mt, na.rm = TRUE)
}

deps_fresh <- function(name, deps) {
  f <- cache_file(name)
  if (!file.exists(f)) return(FALSE)
  if (!length(deps))   return(TRUE)
  file.mtime(f) >= .newest_mtime(deps)
}

with_cache <- function(name, deps, builder, rebuild = FALSE, compress = "gzip") {
  .ensure_cache_dir()
  # Checagem de inexistência de arquivos de dependência (para mensagens amigáveis)
  missing <- deps[!file.exists(deps)]
  if (length(missing)) {
    stop(
      paste0(
        "Arquivos de dados ausentes:\n- ",
        paste(basename(missing), collapse = "\n- ")
      ),
      call. = FALSE
    )
  }

  f <- cache_file(name)

  if (!rebuild && deps_fresh(name, deps)) {
    e <- new.env(parent = emptyenv())
    load(f, envir = e)
    return(get(name, envir = e))
  }

  obj <- builder()
  assign(name, obj)
  save(list = name, file = f, compress = compress)
  obj
}

# ------------------------------------------------------------------------------
# Camada de cache "bruto": garante que toda leitura venha de .rda
# ------------------------------------------------------------------------------

.cache_safe_name <- function(path, sheet = NULL, prefix = "raw__") {
  stem <- tools::file_path_sans_ext(basename(path))
  # remove encadeamento .csv.gz -> fica só o nome-base
  stem <- sub("\\.csv$", "", stem, ignore.case = TRUE)
  if (!is.null(sheet) && nzchar(sheet)) {
    sheet_safe <- gsub("[^A-Za-z0-9_]+", "_", sheet)
    paste0(prefix, stem, "__", sheet_safe)
  } else {
    paste0(prefix, stem)
  }
}

# Lê um Excel e materializa/consome via .rda
cached_excel <- function(path, sheet = NULL, rebuild = FALSE, ...) {
  name <- .cache_safe_name(path, sheet = sheet)
  with_cache(
    name,
    deps    = c(path),
    builder = function() readxl::read_excel(path, sheet = sheet, ...),
    rebuild = rebuild
  )
}

# Lê um CSV (inclui .csv.gz automaticamente) e materializa/consome via .rda
cached_csv <- function(path, delim = ",", rebuild = FALSE, ...) {
  name <- .cache_safe_name(path)
  with_cache(
    name,
    deps    = c(path),
    builder = function() readr::read_delim(path, delim = delim, show_col_types = FALSE, ...),
    rebuild = rebuild
  )
}

# Lê um RDS e materializa/consome via .rda
cached_rds <- function(path, rebuild = FALSE) {
  name <- .cache_safe_name(path)
  with_cache(
    name,
    deps    = c(path),
    builder = function() readRDS(path),
    rebuild = rebuild
  )
}

