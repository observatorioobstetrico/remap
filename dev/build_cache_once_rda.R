# dev/build_cache_once_rda.R
# Pré-aquece os caches .rda (camada bruta + agregada)
# RODAR SEMPRE QUE HOUVER ALTERAÇÕES: source("dev/build_cache_once_rda.R")

message(">> Pré-aquecendo cache em inst/app/data/_rda ...")

# Fallback de app_sys quando fora do golem (permite rodar este script isolado)
if (!exists("app_sys")) app_sys <- function(...) file.path("inst", ...)

# Carrega utilidades e loaders (cache_utils primeiro)
source(file.path("R", "cache_utils.R"))
source(file.path("R", "global_data_call.R"))
source(file.path("R", "global_data_indicadores.R"))
source(file.path("R", "global_data_obitos.R"))
source(file.path("R", "global_data_referencias.R"))  # NOVO

# Garante a pasta de cache existente
try(.ensure_cache_dir(), silent = TRUE)

ok <- function(nome, obj) {
  nlinhas <- tryCatch({
    if (is.list(obj)) {
      sum(sapply(obj, function(x) if (is.data.frame(x)) nrow(x) else 0))
    } else if (is.data.frame(obj)) {
      nrow(obj)
    } else {
      NA_integer_
    }
  }, error = function(e) NA_integer_)
  message(sprintf(
    "OK %s (%s linhas somadas)",
    nome,
    ifelse(is.na(nlinhas), "?", format(nlinhas, big.mark = ".", decimal.mark = ","))
  ))
}

# rebuild=TRUE força reprocessar tanto os caches brutos (raw__) quanto os agregados
obj1 <- try(load_data(rebuild = TRUE), silent = TRUE)
if (inherits(obj1, "try-error")) stop(obj1) else ok("call_data", obj1)

obj2 <- try(load_indicadores_data(rebuild = TRUE), silent = TRUE)
if (inherits(obj2, "try-error")) stop(obj2) else ok("indicadores_data", obj2)

obj3 <- try(load_obitos_data(rebuild = TRUE), silent = TRUE)
if (inherits(obj3, "try-error")) stop(obj3) else ok("obitos_data", obj3)

obj4 <- try(load_series_data(rebuild = TRUE), silent = TRUE)
if (inherits(obj4, "try-error")) stop(obj4) else ok("series_data", obj4)

obj5 <- try(load_referencias_data(rebuild = TRUE), silent = TRUE)  # NOVO
if (inherits(obj5, "try-error")) stop(obj5) else ok("referencias_data", obj5)

# Checagem simples: confirma existência dos .rda agregados
agg <- c("call_data", "indicadores_data", "obitos_data", "series_data", "referencias_data")
agg_files <- vapply(agg, cache_file, character(1))
missing <- agg_files[!file.exists(agg_files)]
if (length(missing)) {
  stop(paste0(
    "Falhou em materializar alguns caches agregados (.rda):\n- ",
    paste(basename(missing), collapse = "\n- ")
  ))
}

message(">> Cache pronto em: ", file.path("inst", "app", "data", "_rda"))
