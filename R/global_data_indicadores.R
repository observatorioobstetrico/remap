# R/global_data_indicadores.R
# Cache em .rda + normalizações leves para performance e robustez

#' @import dplyr readr readxl janitor stringr
NULL

load_indicadores_data <- function(path_data = app_sys("app", "data"), rebuild = FALSE) {

  # Dependências reais (arquivos-fonte)
  deps <- c(
    file.path(path_data, "RRAS-MUNICIPIO.xlsx"),
    file.path(path_data, "tabela_auxiliar_municipios.csv"),
    file.path(path_data, "data_por_extenso.RDS"),
    file.path(path_data, "dados_oobr_indicadores_obstetricos_sinasc_1996_2024.csv.gz"),
    file.path(path_data, "dados_oobr_indicadores_obstetricos_prematuridade_consultas_1996_2024.csv.gz"),
    file.path(path_data, "dados_oobr_indicadores_obstetricos_robson_1996_2024.csv.gz"),
    file.path(path_data, "dados_oobr_indicadores_obstetricos_robson_cesareas_1996_2024.csv.gz")
  )

  with_cache("indicadores_data", deps, builder = function() {

    # --- Referência/Mapa SP (apenas colunas necessárias) ---
    ref_sp <- cached_excel(file.path(path_data, "RRAS-MUNICIPIO.xlsx"), rebuild = rebuild) %>%
      janitor::clean_names() %>%
      dplyr::transmute(
        cod_ibge        = suppressWarnings(as.numeric(cod_ibge)),
        municipio_sp    = trimws(municipio),
        rras            = trimws(rras),
        regiao_de_saude = trimws(regiao_de_saude),
        drs             = trimws(drs)
      )

    # Função de join robusta (por código ou nome) mantendo o padrão de "expansão SP"
    join_rras_sp <- function(df) {
      df <- janitor::clean_names(df)
      if ("codigo" %in% names(df)) {
        df <- dplyr::mutate(df, codigo = suppressWarnings(as.numeric(codigo)))
        # expansão: mantém todos os municípios SP e injeta as colunas do df
        dplyr::left_join(ref_sp, df, by = c("cod_ibge" = "codigo"))
      } else if ("municipio" %in% names(df)) {
        df <- dplyr::mutate(df, municipio = trimws(municipio))
        dplyr::left_join(ref_sp, df, by = c("municipio_sp" = "municipio"))
      } else {
        df
      }
    }

    # --- Leitura via cache .rda (readr já trata .gz) ---
    sinasc <- cached_csv(
      file.path(path_data, "dados_oobr_indicadores_obstetricos_sinasc_1996_2024.csv.gz"),
      rebuild = rebuild
    )
    premat_cons <- cached_csv(
      file.path(path_data, "dados_oobr_indicadores_obstetricos_prematuridade_consultas_1996_2024.csv.gz"),
      rebuild = rebuild
    )
    robson <- cached_csv(
      file.path(path_data, "dados_oobr_indicadores_obstetricos_robson_1996_2024.csv.gz"),
      rebuild = rebuild
    )
    robson_cesarea <- cached_csv(
      file.path(path_data, "dados_oobr_indicadores_obstetricos_robson_cesareas_1996_2024.csv.gz"),
      rebuild = rebuild
    )
    tabela_aux_municipios <- cached_csv(
      file.path(path_data, "tabela_auxiliar_municipios.csv"),
      rebuild = rebuild
    ) %>% janitor::clean_names()
    data_por_extenso <- cached_rds(
      file.path(path_data, "data_por_extenso.RDS"),
      rebuild = rebuild
    )

    # --- Expansão para SP + normalizações leves de tipo ---
    norm_ano <- function(x) suppressWarnings(as.integer(x))

    sinasc_sp <- join_rras_sp(sinasc) %>%
      dplyr::mutate(
        ano = norm_ano(ano),
        nascidos = suppressWarnings(as.numeric(nascidos)),
        total_nascidos = dplyr::coalesce(suppressWarnings(as.numeric(total_nascidos)), nascidos)
      )

    premat_cons_sp <- join_rras_sp(premat_cons) %>%
      dplyr::mutate(
        ano               = norm_ano(ano),
        nascidos          = suppressWarnings(as.numeric(nascidos)),
        faltante_premat   = suppressWarnings(as.numeric(faltante_premat)),
        premat            = suppressWarnings(as.numeric(premat)),
        faltante_consulta = suppressWarnings(as.numeric(faltante_consulta)),
        nenhuma_consulta  = suppressWarnings(as.numeric(nenhuma_consulta)),
        consulta1         = suppressWarnings(as.numeric(consulta1)),
        consulta4         = suppressWarnings(as.numeric(consulta4))
      )

    add_grupo_aux <- function(df) {
      if ("grupo_robson_aux" %in% names(df)) return(df)
      if ("grupo_robson" %in% names(df)) {
        dplyr::mutate(df,
                      grupo_robson_aux = dplyr::if_else(
                        is.na(grupo_robson) | grupo_robson %in% c("NA", "", "Ignorado"),
                        "faltante", as.character(grupo_robson)
                      )
        )
      } else df
    }

    robson_sp <- join_rras_sp(robson) %>%
      dplyr::mutate(
        ano      = norm_ano(ano),
        nascidos = suppressWarnings(as.numeric(nascidos))
      ) %>%
      add_grupo_aux()

    robson_cesarea_sp <- join_rras_sp(robson_cesarea) %>%
      dplyr::mutate(
        ano        = norm_ano(ano),
        nascidos   = suppressWarnings(as.numeric(nascidos)),
        tipo_parto = ifelse(is.na(tipo_parto) | tipo_parto == "", "faltante", tolower(tipo_parto))
      ) %>%
      add_grupo_aux()

    # --- Choices ordenados (salvaguardas) ---
    ord_rras <- function(x) {
      x <- unique(x)
      nums <- suppressWarnings(as.integer(stringr::str_extract(x, "\\d+")))
      x[order(nums, na.last = TRUE)]
    }
    rras_choices          <- ref_sp %>% dplyr::distinct(rras)             %>% dplyr::pull(rras)             %>% ord_rras()
    drs_choices           <- ref_sp %>% dplyr::distinct(drs)              %>% dplyr::pull(drs)              %>% unique() %>% sort()
    regiao_saude_choices  <- ref_sp %>% dplyr::distinct(regiao_de_saude)  %>% dplyr::pull(regiao_de_saude)  %>% unique() %>% sort()
    municipios_sp_choices <- ref_sp %>% dplyr::distinct(municipio_sp)     %>% dplyr::pull(municipio_sp)     %>% unique() %>% sort()

    list(
      sinasc                 = sinasc_sp,
      premat_cons            = premat_cons_sp,
      robson                 = robson_sp,
      robson_cesarea         = robson_cesarea_sp,
      tabela_aux_municipios  = tabela_aux_municipios,
      data_por_extenso       = data_por_extenso,
      ref_sp                 = ref_sp,
      rras_choices           = rras_choices,
      drs_choices            = drs_choices,
      regiao_saude_choices   = regiao_saude_choices,
      municipios_sp_choices  = municipios_sp_choices
    )
  }, rebuild = rebuild)
}
