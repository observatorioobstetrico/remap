# R/global_data_obitos.R

#----------------------------------------------------------
# Seção 1: Dados para Óbitos Oficiais / Não considerados / Análise Cruzada
#----------------------------------------------------------

#' Carrega dados de óbitos de gestantes e puérperas
#'
#' @param path_data Caminho para a pasta de dados (por padrão inst/app/data)
#' @return Lista com três data.frames: oficiais, nao_considerados e estendido
#' @importFrom readxl read_excel
#' @importFrom readr read_delim
#' @importFrom janitor clean_names
#' @importFrom dplyr left_join mutate select
#' @importFrom rlang parse_expr
#' @noRd
load_obitos_data <- function(path_data = app_sys("app", "data")) {
  # 1. Leitura da referência de SP (todos municípios)
  rras_full <- readxl::read_excel(
    path = file.path(path_data, "RRAS-MUNICIPIO.xlsx")
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(cod_ibge = as.numeric(cod_ibge)) %>%
    dplyr::rename(
      municipio_sp    = municipio,
      rras            = rras,
      regiao_de_saude = regiao_de_saude,
      drs             = drs
    )

  # 2. Função auxiliar para leitura e expansão
  read_and_expand <- function(file_name) {
    df_raw <- readr::read_delim(
      file.path(path_data, file_name),
      delim = ",", trim_ws = TRUE
    ) %>%
      janitor::clean_names()
    if ("municipio" %in% names(df_raw)) {
      df_raw <- dplyr::select(df_raw, -municipio)
    }
    dplyr::left_join(rras_full, df_raw, by = c("cod_ibge" = "codigo"))
  }

  # 3. Carrega cada base
  df_oficiais <- read_and_expand("dados_oobr_obitos_grav_puerp_maternos_oficiais_1996_2024.csv")
  df_nao_cons  <- read_and_expand("dados_oobr_obitos_grav_puerp_desconsiderados_1996_2024.csv")
  df_ext       <- read_and_expand("dados_oobr_obitos_grav_puerp_analise_cruzada_1996_2024.csv")

  # 4. Retorna lista
  list(
    oficiais         = df_oficiais,
    nao_considerados = df_nao_cons,
    estendido        = df_ext
  )
}

#----------------------------------------------------------
# Seção 2: Dados para Séries de Mortalidade e Morbidade
#----------------------------------------------------------

#' Carrega dados de séries de mortalidade e morbidade materna
#'
#' @param path_data Caminho para a pasta de dados (por padrão inst/app/data)
#' @return Lista com todos os objetos necessários para o módulo de séries
#' @noRd
load_series_data <- function(path_data = app_sys("app", "data", "data_obitos")) {
  # Ambiente temporário para load()
  e <- new.env()

  # Carrega cada .rda
  load(file.path(path_data, "bloco6.rda"),                   envir = e)  # contém objeto bloco6
  load(file.path(path_data, "base_incompletude.rda"),        envir = e)
  load(file.path(path_data, "sub_registro_sim_muni_2015_2021.rda"),  envir = e)
  load(file.path(path_data, "sub_registro_sim_uf_regioes_2015_2021.rda"), envir = e)
  load(file.path(path_data, "rmm_corrigida.rda"),            envir = e)
  load(file.path(path_data, "municipios_choices.rda"),       envir = e)
  load(file.path(path_data, "micro_r_saude_choices.rda"),    envir = e)
  load(file.path(path_data, "macro_r_saude_choices.rda"),    envir = e)
  load(file.path(path_data, "tabela_aux_municipios.rda"),    envir = e)
  load(file.path(path_data, "tabela_radar.rda"),             envir = e)
  load(file.path(path_data, "tabela_indicadores.rda"),       envir = e)
  load(file.path(path_data, "estados_choices.rda"),          envir = e)

  # NOVO: Carrega arquivo muni_rras_rs_drs.rda
  load(file.path(path_data, "muni_rras_rs_drs.rda"),         envir = e)

  # Padroniza nomes das colunas do muni_rras_rs_drs
  muni_rras_rs_drs <- e$muni_rras_rs_drs %>%
    janitor::clean_names() %>%
    dplyr::rename(
      cod_ibge = cod_ibge,
      municipio = municipio,
      rras = rras,
      regiao_de_saude = regiao_de_saude,
      drs = drs
    )

  # Correção do espaçamento das strings RRAS em bloco 6 com muni_rras_rs_drs
  # em bloco 6 está tudo bagunçado
  e$bloco6 <- e$bloco6 %>%
    mutate(
      macro_r_saude = trimws(macro_r_saude),
      macro_r_saude = ifelse(
        grepl("^RRAS\\s*\\d+$", macro_r_saude),
        gsub("^RRAS\\s*(\\d+)$", "RRAS \\1", macro_r_saude),
        macro_r_saude
      )
    )
  e$macro_r_saude_choices <- e$macro_r_saude_choices %>%
    mutate(
      macro_r_saude = trimws(macro_r_saude),
      macro_r_saude = ifelse(
        grepl("^RRAS\\s*\\d+$", macro_r_saude),
        gsub("^RRAS\\s*(\\d+)$", "RRAS \\1", macro_r_saude),
        macro_r_saude
      )
    )

  # Adiciona DRS ao bloco6 via join com municipio
  # Primeiro, padroniza os nomes dos municípios para facilitar o join
  e$bloco6 <- e$bloco6 %>%
    dplyr::mutate(municipio_lower = tolower(trimws(municipio))) %>%  # Padroniza para lowercase
    dplyr::left_join(
      muni_rras_rs_drs %>%
        dplyr::select(municipio, drs) %>%
        dplyr::mutate(municipio_lower = tolower(trimws(municipio))) %>%  # Padroniza para lowercase
        dplyr::distinct(),
      by = "municipio_lower"
    ) %>%
    dplyr::select(-municipio_lower, municipio = municipio.x, drs)  # Remove coluna auxiliar e mantém municipio original

  # Adiciona DRS à base_incompletude também
  e$base_incompletude <- e$base_incompletude %>%
    dplyr::mutate(municipio_lower = tolower(trimws(municipio))) %>%  # Padroniza para lowercase
    dplyr::left_join(
      muni_rras_rs_drs %>%
        dplyr::select(municipio, drs) %>%
        dplyr::mutate(municipio_lower = tolower(trimws(municipio))) %>%  # Padroniza para lowercase
        dplyr::distinct(),
      by = "municipio_lower"
    ) %>%
    dplyr::select(-municipio_lower, municipio = municipio.x, drs)  # Remove coluna auxiliar e mantém municipio original


  # Cria choices para DRS (apenas para SP)
  drs_choices <- muni_rras_rs_drs %>%
    dplyr::filter(!is.na(drs)) %>%
    dplyr::distinct(drs) %>%                  # distinct direto na coluna drs
    dplyr::mutate(uf = "São Paulo")           # Adiciona a identificação do estado

  # Cria choices para RRAS de SP
  rras_choices <- muni_rras_rs_drs %>%
    dplyr::filter(!is.na(rras)) %>%
    dplyr::distinct(rras) %>%
    dplyr::mutate(uf = "São Paulo")

  # Cria choices para Região de Saúde de SP
  regiao_saude_choices <- muni_rras_rs_drs %>%
    dplyr::filter(!is.na(regiao_de_saude)) %>%
    dplyr::distinct(regiao_de_saude) %>%
    dplyr::mutate(uf = "São Paulo")

  # Cria choices para Municípios de SP
  municipios_sp_choices <- muni_rras_rs_drs %>%
    dplyr::filter(!is.na(municipio)) %>%
    dplyr::distinct(municipio) %>%
    dplyr::mutate(uf = "São Paulo")

  # Monta e retorna a lista de dados
  list(
    bloco6                        = e$bloco6,
    base_incompletude             = e$base_incompletude,
    sub_registro_sim_muni         = e$sub_registro_sim_muni_2015_2021,
    sub_registro_sim_uf           = e$sub_registro_sim_uf_regioes_2015_2021,
    rmm_corrigida                 = e$rmm_corrigida,
    municipios_choices            = e$municipios_choices,
    micro_r_saude_choices         = e$micro_r_saude_choices,
    macro_r_saude_choices         = e$macro_r_saude_choices,
    tabela_aux_municipios         = e$tabela_aux_municipios,
    tabela_radar                  = e$tabela_radar,
    tabela_indicadores            = e$tabela_indicadores,
    estados_choices               = e$estados_choices,
    # NOVOS objetos específicos para SP
    muni_rras_rs_drs              = muni_rras_rs_drs,
    drs_choices                   = drs_choices,
    rras_choices                  = rras_choices,
    regiao_saude_choices          = regiao_saude_choices,
    municipios_sp_choices         = municipios_sp_choices
  )
}

#-------------------------------------------------------------------------------
# Seção 3: Funções auxiliares (antes em funcoes_globais.R)
#-------------------------------------------------------------------------------

#' Cria indicadores para o bloco 6 (Mortalidade e morbidade materna)
#'
#' @param df_localidade data.frame de valores brutos para a localidade
#' @param df_calcs      data.frame com expressões de cálculo (coluna `tipo` + colunas de expressões)
#' @param filtros       lista de filtros vindos do servidor (input$...)
#' @param referencia    lógico, se TRUE gera os valores de referência
#' @param comp          lógico, se TRUE gera comparação secundária
#' @param adicionar_localidade lógico, se TRUE adiciona coluna `class` para rotular a localidade
#' @param input         não utilizado aqui (mantido por compatibilidade)
#' @param bloco         string, deve ser "bloco6"
#' @param localidade_resumo string, controla sufixo de comparação
#' @return tibble com colunas: ano, indicadores e class
#' @import dplyr rlang
#' @noRd
cria_indicadores <- function(
    df_localidade,
    df_calcs,
    filtros,
    referencia           = FALSE,
    comp                 = FALSE,
    adicionar_localidade = TRUE,
    input                = NULL,
    bloco                = "bloco6",
    localidade_resumo    = "escolha1"
) {
  # filtra local vs referência
  if (!referencia) {
    df_calcs <- df_calcs %>%
      filter(tipo == "local") %>%
      select(-tipo)
  } else {
    df_calcs <- df_calcs %>%
      filter(tipo == "referencia") %>%
      select(-tipo)
  }

  colunas_summarise <- names(df_calcs)

  # base vazia para acumular indicadores
  df_localidade_aux <- df_localidade %>%
    summarise() %>%
    ungroup()

  # preenche colunas de indicadores
  if (ncol(df_localidade_aux) == 0) {
    for (coluna in colunas_summarise) {
      df_localidade_aux <- cbind(
        df_localidade_aux,
        summarise(
          df_localidade,
          !!coluna := !!rlang::parse_expr(df_calcs[[coluna]])
        )
      )
    }
  } else {
    for (coluna in colunas_summarise) {
      df_localidade_aux <- full_join(
        df_localidade_aux,
        summarise(
          df_localidade,
          !!coluna := !!rlang::parse_expr(df_calcs[[coluna]])
        ),
        by = join_by(ano)
      )
    }
  }

  # adiciona coluna `class` para rotular a série nos gráficos
  if (adicionar_localidade) {
    sufixo <- ifelse(comp | localidade_resumo == "escolha2", "2", "")

    # extrai cada filtro como escalar, usando NA_character_ se não existir
    nivel_val     <- filtros[[paste0("nivel", sufixo)]]
    regiao_val    <- if (!is.null(filtros[[paste0("regiao", sufixo)]]))
      filtros[[paste0("regiao", sufixo)]]
    else
      NA_character_
    estado_val    <- if (!is.null(filtros[[paste0("estado", sufixo)]]))
      filtros[[paste0("estado", sufixo)]]
    else
      NA_character_
    rras_val      <- if (!is.null(filtros[[paste0("rras", sufixo)]]))
      filtros[[paste0("rras", sufixo)]]
    else
      NA_character_
    drs_val       <- if (!is.null(filtros[[paste0("drs", sufixo)]]))
      filtros[[paste0("drs", sufixo)]]
    else
      NA_character_
    regiao_saude_val <- if (!is.null(filtros[[paste0("regiao_saude", sufixo)]]))
      filtros[[paste0("regiao_saude", sufixo)]]
    else
      NA_character_
    municipio_val <- if (!is.null(filtros[[paste0("municipio", sufixo)]]))
      filtros[[paste0("municipio", sufixo)]]
    else
      NA_character_

    # constrói rótulo fixo de Brasil
    label_brasil <- if (isTRUE(filtros$comparar == "Não") || is.null(filtros$mostrar_referencia)) {
      "Brasil (valor de referência)"
    } else if (identical(filtros$mostrar_referencia, "nao_mostrar_referencia")) {
      "Brasil"
    } else {
      "Brasil (valor de referência)"
    }

    # sufixo já definido ali em cima
    macro_val <- if (!is.null(filtros[[paste0("macro", sufixo)]]))
      filtros[[paste0("macro", sufixo)]]
    else
      NA_character_

    micro_val <- if (!is.null(filtros[[paste0("micro", sufixo)]]))
      filtros[[paste0("micro", sufixo)]]
    else
      NA_character_

    df_localidade_aux <- df_localidade_aux %>%
      mutate(
        class = case_when(
          # nacional ou referência
          nivel_val == "nacional" | referencia ~ label_brasil,

          # demais níveis
          nivel_val == "regional"            ~ regiao_val,
          nivel_val == "estadual"            ~ estado_val,
          nivel_val == "rras"                ~ rras_val,
          nivel_val == "macro"               ~ macro_val,
          nivel_val == "drs"                 ~ drs_val,
          nivel_val == "micro"               ~ micro_val,
          nivel_val == "regiao_saude"        ~ regiao_saude_val,
          nivel_val == "municipal"           ~ municipio_val,
          nivel_val == "municipios_semelhantes" ~ "Média dos municípios semelhantes",

          TRUE ~ NA_character_
        )
      ) %>%
      ungroup()
  }

  df_localidade_aux
}

#-------------------------------------------------------------------------------
#' Cria caixa de indicador único (card)
#'
#' Esta função gera um bs4Dash::box com o valor do indicador, a comparação
#' e a coloração adequada.
#'
#' @param dados              tibble com um registro contendo a coluna `indicador`
#' @param indicador          nome da coluna dentro de `dados`
#' @param titulo             título a exibir no card
#' @param tem_meta           lógico, se verdadeiro usa `tipo_referencia` em vez de "média nacional"
#' @param nivel_de_analise   string, nível de análise ("nacional", "regional", ...)
#' @param tipo_referencia    string, descrição da referência (ex.: "Portaria XYZ")
#' @param valor_de_referencia numérico ou vetor numérico de referência
#' @param valor_indicador    valor alternativo para exibir (default NULL usa `dados[[indicador]]`)
#' @param tipo               "porcentagem", "km" ou outro (apenas para formatação)
#' @param invertido          lógico, se TRUE inverte lógica de cor
#' @param texto_caixa        texto fixo opcional para o valor
#' @param cor                cor fixa para o fundo de comparação (override)
#' @param texto_footer       texto fixo para o rodapé (override)
#' @param tamanho_caixa      altura do card (ex.: "300px")
#' @param fonte_titulo       classe CSS para o título
#' @param fonte_comparacao   classe CSS para o texto de comparação
#' @param pagina             não utilizado diretamente aqui
#' @param width_caixa        largura do card (1–12)
#' @return um elemento shiny (bs4Dash::box)
#' @import dplyr glue
#' @importFrom bs4Dash box
#' @noRd
cria_caixa_server <- function(
    dados,
    indicador,
    titulo,
    tem_meta            = FALSE,
    nivel_de_analise,
    tipo_referencia,
    valor_de_referencia,
    valor_indicador     = NULL,
    tipo                = "porcentagem",
    invertido           = FALSE,
    texto_caixa         = NULL,
    cor                 = NULL,
    texto_footer        = NULL,
    tamanho_caixa       = "300px",
    fonte_titulo        = "fonte-grande",
    fonte_comparacao    = "fonte-media",
    pagina,
    width_caixa         = 12
) {
  # --- calcula valor_indicador_aux ---
  if (is.null(valor_indicador)) {
    if (isTruthy(dados[[indicador]])) {
      if (as.integer(dados[[indicador]]) == dados[[indicador]]) {
        valor_indicador_aux <- as.integer(dados[[indicador]])
      } else {
        valor_indicador_aux <- dados[[indicador]]
      }
    } else {
      valor_indicador_aux <- NaN
    }
  } else {
    if (isTruthy(valor_indicador)) {
      if (as.integer(valor_indicador) == valor_indicador) {
        valor_indicador_aux <- as.integer(valor_indicador)
      } else {
        valor_indicador_aux <- valor_indicador
      }
    } else {
      valor_indicador_aux <- NaN
    }
  }

  # --- calcula razão e variação percentual (valor_comp) ---
  if (isTruthy(valor_de_referencia) && isTruthy(valor_indicador_aux)) {
    if (length(valor_de_referencia) == 1) {
      if (valor_indicador_aux == 0 && valor_de_referencia == 0) {
        razao <- 0
      } else {
        razao <- round(valor_indicador_aux / valor_de_referencia, 1)
      }
    } else {
      if (valor_indicador_aux < min(valor_de_referencia)) {
        razao <- round(valor_indicador_aux / min(valor_de_referencia), 1)
      } else if (valor_indicador_aux > max(valor_de_referencia)) {
        razao <- round(valor_indicador_aux / max(valor_de_referencia), 1)
      } else {
        razao <- 0
      }
    }
  } else {
    razao <- 0
  }

  if (length(valor_de_referencia) == 1) {
    if (razao >= 2) {
      valor_comp <- razao
    } else {
      valor_comp <- round(100 - 100 * valor_indicador_aux / valor_de_referencia, 1)
    }
  } else {
    if (is.nan(valor_indicador_aux)) {
      valor_comp <- NaN
    } else if (valor_indicador_aux < min(valor_de_referencia)) {
      if (razao >= 2) {
        valor_comp <- razao
      } else {
        valor_comp <- round(100 - 100 * valor_indicador_aux / min(valor_de_referencia), 1)
      }
    } else if (valor_indicador_aux > max(valor_de_referencia)) {
      if (razao >= 2) {
        valor_comp <- razao
      } else {
        valor_comp <- round(100 - 100 * valor_indicador_aux / max(valor_de_referencia), 1)
      }
    } else {
      valor_comp <- 0
    }
  }

  valor_comp_formatado          <- formatC(abs(valor_comp), big.mark='.', decimal.mark=',')
  valor_de_referencia_formatado <- formatC(valor_de_referencia, big.mark='.', decimal.mark=',')

  if (!tem_meta) tipo_referencia <- "média nacional"

  if (tipo_referencia == "média nacional" &&
      nivel_de_analise == "nacional" &&
      is.null(texto_footer)) {
    texto_footer <- if (is.nan(valor_comp)) {
      "Comparação não aplicável"
    } else {
      "Comparação não aplicável (este é o valor de referência)"
    }
    cor <- "lightgrey"
  }

  # --- define cor de comparação ---
  if (is.null(cor)) {
    if (length(valor_de_referencia) == 1) {
      if (invertido) {
        if (razao >= 2) {
          cor_comp <- "#a2e4b8"
        } else {
          cor_comp <- dplyr::case_when(
            valor_comp > 0     ~ "#d998a0",
            valor_comp <= 0    ~ "#a2e4b8",
            is.nan(valor_comp) ~ "lightgrey"
          )
        }
      } else {
        if (razao >= 2) {
          cor_comp <- "#d998a0"
        } else {
          cor_comp <- dplyr::case_when(
            valor_comp < 0     ~ "#d998a0",
            valor_comp >= 0    ~ "#a2e4b8",
            is.nan(valor_comp) ~ "lightgrey"
          )
        }
      }
    } else {
      cor_comp <- dplyr::case_when(
        valor_comp == 0     ~ "#a2e4b8",
        valor_comp != 0     ~ "#d998a0",
        is.nan(valor_comp)  ~ "lightgrey"
      )
    }
  } else {
    cor_comp <- cor
  }

  # --- formata valor principal ---
  if (is.null(texto_caixa)) {
    if (is.nan(valor_indicador_aux)) {
      texto <- "---"
    } else if (tipo == "porcentagem") {
      texto <- glue::glue("{formatC(valor_indicador_aux, big.mark='.', decimal.mark=',')}%")
    } else if (tipo == "km") {
      texto <- glue::glue("{formatC(valor_indicador_aux, big.mark='.', decimal.mark=',')} km")
    } else {
      texto <- glue::glue("{formatC(valor_indicador_aux, big.mark='.', decimal.mark=',')}")
    }
  } else {
    texto <- texto_caixa
  }

  # --- formata texto de comparação ---
  final_texto_comp <- if (tipo_referencia == "média nacional") {
    glue::glue(" ({valor_de_referencia_formatado}{ifelse(tipo=='porcentagem','%','')}, média nacional)")
  } else if (!invertido) {
    glue::glue(", de no máximo {valor_de_referencia_formatado}{ifelse(tipo=='porcentagem','%','')} ({tipo_referencia})")
  } else {
    glue::glue(", de no mínimo {valor_de_referencia_formatado}{ifelse(tipo=='porcentagem','%','')} ({tipo_referencia})")
  }

  if (is.null(texto_footer)) {
    if (length(valor_de_referencia) != 1) {
      if (razao >= 2) {
        if (valor_indicador_aux < min(valor_de_referencia)) {
          texto_comp <- glue::glue(
            "<i class='fa-solid fa-caret-down'></i> {valor_comp_formatado} vezes menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]}% ({tipo_referencia})"
          )
        } else {
          texto_comp <- glue::glue(
            "<i class='fa-solid fa-caret-up'></i> {valor_comp_formatado} vezes maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]}% ({tipo_referencia})"
          )
        }
      } else {
        texto_comp <- dplyr::case_when(
          valor_comp < 0 & tipo=="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-up'></i> {valor_comp_formatado}% maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]}% ({tipo_referencia})"
          ),
          valor_comp < 0 & tipo!="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-up'></i> {valor_comp_formatado}% maior que o valor de referência máximo, de {valor_de_referencia_formatado[2]} ({tipo_referencia})"
          ),
          valor_comp > 0 & tipo=="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-down'></i> {valor_comp_formatado}% menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]}% ({tipo_referencia})"
          ),
          valor_comp > 0 & tipo!="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-down'></i> {valor_comp_formatado}% menor que o valor de referência mínimo, de {valor_de_referencia_formatado[1]} ({tipo_referencia})"
          ),
          valor_comp == 0                        ~ glue::glue(
            "Dentro da faixa de referência, de {valor_de_referencia_formatado[1]} a {valor_de_referencia_formatado[2]}% ({tipo_referencia})"
          ),
          TRUE                                   ~ "Comparação não aplicável"
        )
      }
    } else {
      texto_comp <- if (razao >= 2) {
        glue::glue(
          "<i class='fa-solid fa-caret-up'></i> {valor_comp_formatado} vezes maior que o valor de referência{final_texto_comp}"
        )
      } else {
        dplyr::case_when(
          valor_comp < 0 & tipo=="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-up'></i> {valor_comp_formatado}% maior que o valor de referência{final_texto_comp}"
          ),
          valor_comp < 0 & tipo!="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-up'></i> {valor_comp_formatado}% maior que o valor de referência{final_texto_comp}"
          ),
          valor_comp > 0 & tipo=="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-down'></i> {valor_comp_formatado}% menor que o valor de referência{final_texto_comp}"
          ),
          valor_comp > 0 & tipo!="porcentagem" ~ glue::glue(
            "<i class='fa-solid fa-caret-down'></i> {valor_comp_formatado}% menor que o valor de referência{final_texto_comp}"
          ),
          valor_comp == 0                        ~ glue::glue(
            "Igual ao valor de referência ({tipo_referencia})"
          ),
          TRUE                                   ~ "Comparação não aplicável"
        )
      }
    }
  } else {
    texto_comp <- texto_footer
  }

  fonte_texto <- if (floor(log10(valor_indicador_aux)) + 1 < 7 || is.nan(valor_indicador_aux)) {
    "fonte-destaque-caixas1"
  } else {
    "fonte-destaque-caixas2"
  }

  style_texto <- glue::glue(
    "height: 28%; overflow: auto; padding: 0 10px; display: flex; justify-content: center; text-align: center;"
  )

  # monta o box
  bs4Dash::box(
    style        = glue::glue("height: {tamanho_caixa}; overflow: auto; padding: 0;"),
    width        = width_caixa,
    collapsible  = FALSE,
    headerBorder = FALSE,
    div(
      class = fonte_titulo,
      style = glue::glue("height: 31%; overflow: auto; padding: 0 10px;"),
      HTML(glue::glue("<b> {titulo} </b>"))
    ),
    div(style = "height: 3%"),
    div(
      class = fonte_texto,
      style = style_texto,
      HTML(glue::glue("<b> {texto} </b>"))
    ),
    div(
      class = fonte_comparacao,
      style = glue::glue(
        "overflow: auto; height: 38%; padding: 10px 5px;
         display: flex; align-items:center; justify-content:center;
         text-align: center; background-color: {cor_comp};"
      ),
      HTML(glue::glue("<b> {texto_comp} </b>"))
    )
  )
}


#-------------------------------------------------------------------------------
#' Cria modal de incompletude e cobertura (apenas para bloco6)
#'
#' Exibe um modal com informações detalhadas de incompletude e cobertura
#' para óbitos maternos, conforme lógica original.
#' @param df Tibble com ano, indicadores de incompletude e cobertura
#' @param incompletude1 Vetor de indicadores de incompletude 1
#' @param variavel_incompletude1 Nome da variável (opcional)
#' @param descricao_incompletude1 Descrição da incompletude (opcional)
#' @param incompletude2 Vetor de indicadores de incompletude 2 (opcional)
#' @param variavel_incompletude2 Nome da segunda variável (opcional)
#' @param descricao_incompletude2 Descrição da segunda incompletude (opcional)
#' @param incompletude3 Vetor de indicadores de incompletude 3 (opcional)
#' @param variavel_incompletude3 Nome da terceira variável (opcional)
#' @param descricao_incompletude3 Descrição da terceira incompletude (opcional)
#' @param incompletude4 Vetor de indicadores de incompletude 4 (opcional)
#' @param variavel_incompletude4 Nome da quarta variável (opcional)
#' @param descricao_incompletude4 Descrição da quarta incompletude (opcional)
#' @param cobertura    Vetor de cobertura
#' @param base         Base de dados (ex.: "SIM")
#' @param bloco        Nome do bloco (ex.: "bloco6")
#' @param nivel        Nível de detalhamento (1–3)
#' @return Dispara um shinyalert com informações de qualidade
#' @import shinyalert glue dplyr
#' @noRd
cria_modal_incompletude <- function(
    df,
    incompletude1,
    variavel_incompletude1 = NULL,
    descricao_incompletude1 = NULL,
    incompletude2 = NULL,
    variavel_incompletude2 = NULL,
    descricao_incompletude2 = NULL,
    incompletude3 = NULL,
    variavel_incompletude3 = NULL,
    descricao_incompletude3 = NULL,
    incompletude4 = NULL,
    variavel_incompletude4 = NULL,
    descricao_incompletude4 = NULL,
    cobertura,
    base  = "SIM",
    bloco = "bloco6",
    nivel = 2
) {

  # Ajusta lógica de base para bloco6
  if (bloco == "bloco6" | substr(bloco, 1, 6) == "bloco7") {
    base <- "SIM"
  }

  # Condição para exibir modal
  if (bloco != "bloco6") {
    req(
      any(incompletude1 > 5,  na.rm = TRUE) |
        any(incompletude2 > 5,  na.rm = TRUE) |
        any(incompletude3 > 5,  na.rm = TRUE) |
        any(incompletude4 > 5,  na.rm = TRUE) |
        any(cobertura    < 90, na.rm = TRUE)
    )
  } else {
    req(
      any(incompletude1 < 90, na.rm = TRUE) |
        any(incompletude2 <100, na.rm = TRUE) |
        any(cobertura    < 90, na.rm = TRUE)
    )
  }

  # --- Incompletude 1 ---
  if (bloco != "bloco6") {
    anos1_aux       <- df$ano[which(incompletude1 > 5)]
    val_incomp1_aux <- formatC(incompletude1[which(incompletude1 > 5)], big.mark='.', decimal.mark=',')
  } else {
    anos1_aux       <- df$ano[which(incompletude1 < 90)]
    val_incomp1_aux <- formatC(incompletude1[which(incompletude1 < 90)], big.mark='.', decimal.mark=',')
  }
  anos1 <- if (length(anos1_aux)>1) {
    paste(paste0(anos1_aux[-length(anos1_aux)], collapse=", "), "e", anos1_aux[length(anos1_aux)])
  } else {
    as.character(anos1_aux)
  }
  valores_incompletude1 <- if (length(val_incomp1_aux)>1) {
    paste0(
      paste0(val_incomp1_aux[-length(val_incomp1_aux)], "%", collapse=", "),
      " e ",
      val_incomp1_aux[length(val_incomp1_aux)], "%"
    )
  } else {
    paste0(val_incomp1_aux, "%")
  }

  # Texto adicional de detalhes
  mais_detalhes <- if (nivel != 3) {
    "Para mais detalhes, vá para o <span style='font-weight:700'>Nível 3: Visão detalhada dos indicadores.</span>"
  } else {
    ""
  }

  # Montagem do texto principal de incompletude
  if (length(anos1_aux) > 1) {
    if (bloco == "bloco6") {
      texto_incompletude <- glue::glue(
        "Os <span style='font-weight:700'>óbitos de mulheres em idade fértil (MIF)</span>
         apresentam problemas de investigação nos anos de <span style='font-weight:700'>{anos1}.</span>
         Nesses anos, a porcentagem de óbitos investigados dessa população foi, respectivamente, de
         <span style='font-weight:700'>{valores_incompletude1}.</span>
         São considerados como valores ideais aqueles acima de 90%."
      )
    } else {
      texto_incompletude <- glue::glue(
        "A variável <span style='font-weight:700'>{variavel_incompletude1}</span>, do {base},
         apresenta problemas de incompletude nos anos de <span style='font-weight:700'>{anos1}.</span>
         Nesses anos, a porcentagem de valores {descricao_incompletude1} dessa variável foi,
         respectivamente, de <span style='font-weight:700'>{valores_incompletude1}.</span>
         Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são considerados bons."
      )
    }
  } else if (length(anos1_aux) == 1) {
    if (bloco == "bloco6") {
      texto_incompletude <- glue::glue(
        "Os <span style='font-weight:700'>óbitos de mulheres em idade fértil (MIF)</span>
         apresentam problemas de investigação no ano de <span style='font-weight:700'>{anos1}.</span>
         Nesse ano, a porcentagem de óbitos investigados foi de <span style='font-weight:700'>{val_incomp1_aux}%.</span>
         São considerados como valores ideais aqueles acima de 90%."
      )
    } else {
      texto_incompletude <- glue::glue(
        "A variável <span style='font-weight:700'>{variavel_incompletude1}</span>, do {base},
         apresenta problemas de incompletude no ano de <span style='font-weight:700'>{anos1}.</span>
         Nesse ano, a porcentagem de valores {descricao_incompletude1} dessa variável foi de
         <span style='font-weight:700'>{val_incomp1_aux}%.</span>
         Valores abaixo de 5% são considerados excelentes, enquanto valores abaixo de 10% são bons."
      )
    }
  } else {
    texto_incompletude <- ""
  }

  # --- Agora a cobertura ---
  if ((bloco != "bloco6" && !any(incompletude2>5, na.rm=TRUE)) ||
      (bloco=="bloco6" && !any(incompletude2<100, na.rm=TRUE))) {
    if (!any(cobertura<90, na.rm=TRUE)) {
      texto <- glue::glue(
        "<div style='text-align:justify; text-justify:inter-word;'>
           {texto_incompletude} {mais_detalhes}
         </div>"
      )
    } else {
      anos_cov_aux <- df$ano[which(cobertura<90)]
      val_cov_aux  <- formatC(cobertura[which(cobertura<90)], big.mark='.', decimal.mark=',')
      anos_cov     <- if (length(anos_cov_aux)>1) {
        paste(paste0(anos_cov_aux[-length(anos_cov_aux)], collapse=", "), "e", anos_cov_aux[length(anos_cov_aux)])
      } else {
        as.character(anos_cov_aux)
      }
      vals_cov     <- if (length(val_cov_aux)>1) {
        paste0(paste0(val_cov_aux[-length(val_cov_aux)], collapse=", "), " e ", val_cov_aux[length(val_cov_aux)], "%")
      } else {
        paste0(val_cov_aux, "%")
      }
      texto_cobertura <- glue::glue(
        "A localidade apresenta <span style='font-weight:700'>problemas na cobertura do {base}</span>
         nos anos de <span style='font-weight:700'>{anos_cov}.</span>
         Nesses anos, a cobertura foi de <span style='font-weight:700'>{vals_cov}.</span>
         Valores acima de 90% são considerados ideais."
      )
      texto <- glue::glue(
        "<div style='text-align:justify; text-justify:inter-word;'>
           {texto_incompletude} </br></br>Além disso, {texto_cobertura} {mais_detalhes}
         </div>"
      )
    }
  } else {
    # quando também há incompletude2
    if (length(df$ano[which(incompletude2 < ifelse(bloco=="bloco6",100,5))])>0) {
      anos2_aux <- df$ano[which(incompletude2 < ifelse(bloco=="bloco6",100,5))]
      val2_aux  <- formatC(incompletude2[which(incompletude2 < ifelse(bloco=="bloco6",100,5))], big.mark='.', decimal.mark=',')
      anos2     <- if (length(anos2_aux)>1) {
        paste(paste0(anos2_aux[-length(anos2_aux)], collapse=", "), "e", anos2_aux[length(anos2_aux)])
      } else {
        as.character(anos2_aux)
      }
      vals2     <- if (length(val2_aux)>1) {
        paste0(paste0(val2_aux[-length(val2_aux)], collapse=", "), " e ", val2_aux[length(val2_aux)], "%")
      } else {
        paste0(val2_aux, "%")
      }
      # texto incompletude2
      if (bloco=="bloco6") {
        texto_incomp2 <- glue::glue(
          "</br></br>Além disso, os <span style='font-weight:700'>óbitos maternos</span>
           apresentam problemas de investigação nos anos de <span style='font-weight:700'>{anos2}.</span>
           Nesses anos, a porcentagem investigada foi de <span style='font-weight:700'>{vals2}.</span>
           Idealmente 100%."
        )
      } else {
        texto_incomp2 <- glue::glue(
          "</br></br>Além disso, a variável <span style='font-weight:700'>{variavel_incompletude2}</span>,
           apresenta incompletude nos anos de <span style='font-weight:700'>{anos2}.</span>
           Valores de {descricao_incompletude2} foram de <span style='font-weight:700'>{vals2}.</span>"
        )
      }
      # junta tudo
      texto <- glue::glue(
        "<div style='text-align:justify; text-justify:inter-word;'>
           {texto_incompletude} {texto_incomp2} {mais_detalhes}
         </div>"
      )
    } else {
      texto <- glue::glue(
        "<div style='text-align:justify; text-justify:inter-word;'>
           {texto_incompletude} {mais_detalhes}
         </div>"
      )
    }
  }

  shinyalert::shinyalert(
    html                = TRUE,
    title               = "<div class='fonte-titulos-modal'>Qualidade da informação</div>",
    text                = texto,
    size                = "s",
    closeOnEsc          = TRUE,
    closeOnClickOutside = TRUE,
    type                = "warning",
    showConfirmButton   = TRUE,
    confirmButtonText   = "OK",
    confirmButtonCol    = "#007bff",
    animation           = TRUE,
    immediate           = TRUE
  )
}
