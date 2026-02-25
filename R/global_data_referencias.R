#' Carrega dados de estabelecimentos de referência
#'
#' Lê o Excel consolidado (todas as RRAS) e retorna 3 data frames (um por planilha),
#' usando cache em .rda:
#'   - cache "bruto" por planilha via cached_excel()
#'   - cache "agregado" via referencias_data.rda (este loader)
#'
#' @param path_data Diretório dos dados (default: inst/app/data).
#' @param rebuild Se TRUE, força rebuild do cache.
#' @return Lista: tabela_baixo, tabela_agpar, tabela_posnatal.
#' @export
load_referencias_data <- function(path_data = app_sys("app", "data"), rebuild = FALSE) {

  # ------------------------------------------------------------------
  # 0) Cache agregado (referencias_data.rda)
  # ------------------------------------------------------------------
  # A infraestrutura de cache do seu projeto (cache_utils.R) normalmente
  # expõe cache_file() e utilitários relacionados.
  #
  # O dev/build_cache_once_rda.R verifica explicitamente a existência de:
  #   cache_file("referencias_data")  -> .../referencias_data.rda
  #
  # Então garantimos que este loader materialize esse arquivo.
  # ------------------------------------------------------------------

  if (exists("cache_file")) {
    agg_path <- cache_file("referencias_data")

    if (!isTRUE(rebuild) && file.exists(agg_path)) {
      # Carrega do .rda e retorna o objeto 'referencias_data'
      e <- new.env(parent = emptyenv())
      load(agg_path, envir = e)

      if (exists("referencias_data", envir = e, inherits = FALSE)) {
        return(get("referencias_data", envir = e, inherits = FALSE))
      }
      # Se por algum motivo o .rda existe mas não contém o objeto esperado,
      # cai para rebuild (abaixo).
    }
  } else {
    # Se cache_file não existir por qualquer razão, seguimos sem cache agregado.
    # (Mas no seu projeto ele deve existir, pois build_cache_once usa cache_file.)
    agg_path <- NULL
  }

  # ------------------------------------------------------------------
  # 1) Leitura do Excel consolidado
  # ------------------------------------------------------------------
  file_path <- file.path(path_data, "Estabelecimentos de referência - ReMaP.xlsx")

  tabela_baixo <- cached_excel(
    file_path,
    sheet = "Tabela 2 APS - Ref. Partos BR",
    rebuild = rebuild
  )

  tabela_agpar <- cached_excel(
    file_path,
    sheet = "Tabela 1 APS - Ref. AGPAR e Par",
    rebuild = rebuild
  )

  tabela_posnatal <- cached_excel(
    file_path,
    sheet = "Tabela 3 APS - Ref. A-SEG",
    rebuild = rebuild
  )

  referencias_data <- list(
    tabela_baixo    = tabela_baixo,
    tabela_agpar    = tabela_agpar,
    tabela_posnatal = tabela_posnatal
  )

  # ------------------------------------------------------------------
  # 2) Materializa o cache agregado (referencias_data.rda)
  # ------------------------------------------------------------------
  if (!is.null(agg_path)) {
    # garante diretório existente (geralmente cache_utils já garante, mas reforçamos)
    dir.create(dirname(agg_path), recursive = TRUE, showWarnings = FALSE)

    # salva objeto com nome "referencias_data" dentro do .rda
    save(referencias_data, file = agg_path)
  }

  referencias_data
}
