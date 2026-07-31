#' Carrega os indicadores assistenciais de acesso ao pré-natal.
#'
#' Os dados são extraídos do relatório público de pré-natal do SISAB e
#' consolidados anualmente pelo script em `data-raw/indicadores_prenatal`.
#' @noRd
load_indicadores_prenatal_data <- function(path_data = app_sys("app", "data")) {
  arquivo <- file.path(path_data, "indicadores_prenatal_aps.csv")
  referencia <- readxl::read_excel(file.path(path_data, "RRAS-MUNICIPIO.xlsx")) %>%
    janitor::clean_names() %>%
    dplyr::transmute(
      municipio_sp = trimws(municipio),
      rras = trimws(rras),
      regiao_de_saude = trimws(regiao_de_saude),
      drs = trimws(drs)
    ) %>%
    dplyr::distinct()

  normalizar_municipio <- function(x) {
    x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
    x <- toupper(x)
    gsub("[^A-Z0-9]", "", x)
  }

  vazio <- tibble::tibble(
    municipio = character(), ano = integer(), primeira_consulta = numeric(),
    primeira_ate_12_semana = numeric(), exames_ate_20_semana = numeric(),
    consultas_1_3 = numeric(), consultas_4_5 = numeric(), consultas_6_mais = numeric()
  )
  dados <- if (file.exists(arquivo)) readr::read_csv(arquivo, show_col_types = FALSE) else vazio

  dados <- dados %>%
    dplyr::mutate(
      municipio_chave = normalizar_municipio(municipio),
      # O relatório SISAB usa "Florínea"; a referência territorial usa "Florínia".
      municipio_chave = dplyr::if_else(municipio_chave == "FLORINEA", "FLORINIA", municipio_chave),
      ano = as.integer(ano)
    ) %>%
    dplyr::left_join(
      referencia %>% dplyr::mutate(municipio_chave = normalizar_municipio(municipio_sp)),
      by = "municipio_chave"
    ) %>%
    dplyr::filter(!is.na(municipio_sp))

  list(
    dados = dados,
    rras_choices = sort(unique(referencia$rras)),
    drs_choices = sort(unique(referencia$drs)),
    regiao_saude_choices = sort(unique(referencia$regiao_de_saude)),
    municipios_sp_choices = sort(unique(referencia$municipio_sp)),
    atualizado = if (file.exists(arquivo)) file.info(arquivo)$mtime else NA
  )
}
