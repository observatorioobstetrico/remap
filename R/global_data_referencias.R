#' Carrega dados de estabelecimentos de referência
#'
#' Lê o Excel consolidado (todas as RRAS) e retorna 3 data frames (um por planilha),
#' usando cache em .rda:
#'   - cache "bruto" por planilha via cached_excel()
#'   - cache "agregado" via referencias_data.rda (este loader)
#'
#' Nesta versão, além de ler as novas abas do arquivo Excel, também
#' padroniza os nomes das colunas para evitar problemas decorrentes de:
#'   - quebras de linha nos cabeçalhos;
#'   - espaços extras no início/fim;
#'   - pequenas variações de nomenclatura entre versões da planilha.
#'
#' @param path_data Diretório dos dados (default: inst/app/data).
#' @param rebuild Se TRUE, força rebuild do cache.
#' @return Lista: tabela_baixo, tabela_agpar, tabela_posnatal.
#' @export
load_referencias_data <- function(path_data = app_sys("app", "data"), rebuild = FALSE) {

  # ------------------------------------------------------------------
  # 0) Cache agregado (referencias_data.rda)
  # ------------------------------------------------------------------
  if (exists("cache_file")) {
    agg_path <- cache_file("referencias_data")

    if (!isTRUE(rebuild) && file.exists(agg_path)) {
      e <- new.env(parent = emptyenv())
      load(agg_path, envir = e)

      if (exists("referencias_data", envir = e, inherits = FALSE)) {
        return(get("referencias_data", envir = e, inherits = FALSE))
      }
    }
  } else {
    agg_path <- NULL
  }

  # ------------------------------------------------------------------
  # 1) Helpers de padronização de nomes de colunas
  # ------------------------------------------------------------------
  squish_label <- function(x) {
    x <- as.character(x)
    x <- gsub("\u00A0", " ", x, fixed = TRUE)
    x <- gsub("[\r\n\t]+", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  normalize_key <- function(x) {
    x <- squish_label(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    x <- toupper(x)
    x <- trimws(x)
    x
  }

  standardize_ref_cols <- function(df) {
    if (!is.data.frame(df)) return(df)

    old_names <- names(df)
    clean_names <- vapply(old_names, squish_label, character(1))

    name_map <- c(
      # Colunas estruturais
      "COORDENADORIA DE SAUDE" = "COORDENADORIA DE SAÚDE",
      "SUPERVISAO DE SAUDE" = "SUPERVISÃO DE SAÚDE",
      "REGIAO DE SAUDE" = "REGIÃO DE SAÚDE",
      "MUNICIPIO DA RRAS" = "MUNICÍPIO DA RRAS",

      # Tabela AGPAR e Mat
      "CNES (AGPAR)" = "CNES (AGPAR)",
      "AMBULATORIO DE GESTACAO E PUERPERIO DE ALTO RISCO (AGPAR)" =
        "AMBULATÓRIO DE GESTAÇÃO E PUERPÉRIO DE ALTO RISCO (AGPAR)",
      "MUNICIPIO DO ESTABELECIMENTO (AGPAR)" =
        "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
      "MATERNIDADE DE ALTO RISCO DE REFERENCIA" =
        "MATERNIDADE DE ALTO RISCO DE REFERÊNCIA",
      "MUNICIPIO DA MATERNIDADE DE ALTO RISCO" =
        "MUNICÍPIO DA MATERNIDADE DE ALTO RISCO",

      # Tabela Baixo Risco
      "CNES" = "CNES",
      "MATERNIDADE DE BAIXO RISCO DE REFERENCIA" =
        "MATERNIDADE DE BAIXO RISCO DE REFERÊNCIA",
      "MUNICIPIO DO ESTABELECIMENTO" =
        "MUNICÍPIO DO ESTABELECIMENTO",

      # Tabela A-SEG
      "AMBULATORIO DE ACOMPANHAMENTO DE CRIANCAS DE ALTO RISCO PRIORITARIAMENTE EGRESSAS DE UNIDADE NEONATAL (A-SEG)" =
        "AMBULATÓRIO DE ACOMPANHAMENTO DE CRIANÇAS DE ALTO RISCO PRIORITARIAMENTE EGRESSAS DE UNIDADE NEONATAL (A-SEG)",
      "MUNICIPIO DO ESTABELECIMENTO (A-SEG)" =
        "MUNICÍPIO DO ESTABELECIMENTO (A-SEG)"
    )

    final_names <- vapply(clean_names, function(nm) {
      key <- normalize_key(nm)
      if (key %in% names(name_map)) {
        name_map[[key]]
      } else {
        nm
      }
    }, character(1))

    names(df) <- final_names
    df
  }

  # ------------------------------------------------------------------
  # 2) Leitura do Excel consolidado
  # ------------------------------------------------------------------
  file_path <- file.path(path_data, "Estabelecimentos de referência - ReMaP.xlsx")

  tabela_baixo <- cached_excel(
    file_path,
    sheet = "Tabela 2 APS - Ref. Baixo Risco",
    rebuild = rebuild
  )

  tabela_agpar <- cached_excel(
    file_path,
    sheet = "Tabela 1 APS - Ref. AGPAR e Mat",
    rebuild = rebuild
  )

  tabela_posnatal <- cached_excel(
    file_path,
    sheet = "Tabela 3 APS - Ref. A-SEG",
    rebuild = rebuild
  )

  # ------------------------------------------------------------------
  # 3) Padronização dos cabeçalhos
  # ------------------------------------------------------------------
  tabela_baixo    <- standardize_ref_cols(tabela_baixo)
  tabela_agpar    <- standardize_ref_cols(tabela_agpar)
  tabela_posnatal <- standardize_ref_cols(tabela_posnatal)

  referencias_data <- list(
    tabela_baixo    = tabela_baixo,
    tabela_agpar    = tabela_agpar,
    tabela_posnatal = tabela_posnatal
  )

  # ------------------------------------------------------------------
  # 4) Materializa o cache agregado (referencias_data.rda)
  # ------------------------------------------------------------------
  if (!is.null(agg_path)) {
    dir.create(dirname(agg_path), recursive = TRUE, showWarnings = FALSE)
    save(referencias_data, file = agg_path)
  }

  referencias_data
}
