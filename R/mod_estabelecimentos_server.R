# R/mod_estabelecimentos_server.R

#' Server do módulo de Estabelecimentos de Referência
#'
#' @param id Identificador do módulo
#' @param data_list Lista com: tabela_baixo, tabela_agpar, tabela_posnatal
#' @import shiny
#' @import dplyr
#' @importFrom reactable renderReactable reactable colDef
#' @noRd
mod_estabelecimentos_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # =========================================================
    # 0) Helpers utilitários
    # =========================================================
    `%||%` <- function(x, y) {
      if (is.null(x) || length(x) == 0) y else x
    }

    # ------------------------------------------------------------------
    # Popup de ajuda (modal)
    # ------------------------------------------------------------------
    observeEvent(input$help_btn, {
      shiny::showModal(
        shiny::modalDialog(
          title = tagList(shiny::icon("circle-question"), " Ajuda — Estabelecimentos de Referência"),
          size  = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Fechar"),
          tags$div(
            style = "font-size: 15px; line-height: 1.55;",
            tags$p(
              tags$b("O que esta tela mostra?"),
              br(),
              "Esta tela lista, para cada município pertencente ao nível de análise selecionado, os estabelecimentos de referência definidos para: ",
              tags$b("Baixo Risco (Partos)"), ", ", tags$b("Alto Risco (Partos / Ambulatório de Gestação e Puerpério)"), " e ", tags$b("Pós-natal (A-SEG – Alto Risco)"), "."
            ),
            tags$hr(),
            tags$p(
              tags$b("Baixo Risco (Partos): "),
              "Cobertura de Acesso e Capacidade Instalada na Atenção Primária à Saúde (APS) para ",
              tags$b("Partos de Baixo Risco"), "."
            ),
            tags$p(
              tags$b("Alto Risco (Partos / Ambulatório de Gestação e Puerpério): "),
              "Cobertura de Acesso e Capacidade Instalada na Atenção Primária à Saúde (APS) para ",
              tags$b("Ambulatório de Gestação e Puerpério de Alto Risco"), " e ",
              tags$b("Partos de Alto Risco"), "."
            ),
            tags$p(
              tags$b("Pós-natal (A-SEG – Alto Risco): "),
              "Cobertura de Acesso e Capacidade Instalada na Atenção Primária à Saúde (APS) para ",
              tags$b("Acompanhamento de Crianças de Alto Risco (A-SEG)"), " — ",
              tags$b("Pós Natal"), "."
            ),
            tags$hr(),
            tags$p(
              tags$b("Dica:"), " se a tabela estiver grande, use a rolagem interna da tabela para ver todas as linhas ou colunas."
            )
          )
        )
      )
    })

    # ------------------------------------------------------------------
    # 1) Dados
    # ------------------------------------------------------------------
    tabela_baixo    <- data_list$tabela_baixo
    tabela_agpar    <- data_list$tabela_agpar
    tabela_posnatal <- data_list$tabela_posnatal

    # ------------------------------------------------------------------
    # 2) Helpers (robustos)
    # ------------------------------------------------------------------
    normalize_str <- function(x) {
      if (is.null(x)) return(NA_character_)
      y <- as.character(x)

      # remove NBSP e outros espaços problemáticos
      y <- gsub("\u00A0", " ", y, fixed = TRUE)

      # normalização básica
      y <- trimws(y)

      # colapsa múltiplos espaços internos
      y <- gsub("\\s+", " ", y)

      # translit + caixa alta
      y <- iconv(y, from = "UTF-8", to = "ASCII//TRANSLIT")
      y <- toupper(y)

      # trim novamente (após iconv pode surgir espaço)
      y <- trimws(y)
      y
    }

    safe_eq <- function(a, b) {
      aa <- normalize_str(a)
      bb <- normalize_str(b)
      if (is.na(aa) || is.na(bb)) return(FALSE)
      identical(aa, bb)
    }

    format_rras_choices <- function(x) {
      x <- stats::na.omit(x)
      x <- as.character(x)
      x <- trimws(x)

      num <- gsub("[^0-9]+", "", x)
      num[num == ""] <- x[num == ""]
      num <- sub("^0+", "", num)
      num[num == ""] <- "0"

      labels <- paste0("RRAS ", num)
      stats::setNames(num, labels)
    }

    # Seleciona colunas preservando a ordem do arquivo (names(df))
    select_cols_preserve_order <- function(df, desired_set) {
      keep <- names(df)[names(df) %in% desired_set]
      df[, keep, drop = FALSE]
    }

    # match robusto
    match_idx <- function(vec, target) {
      vx <- vapply(vec, normalize_str, character(1))
      tg <- normalize_str(target)
      if (is.na(tg) || !nzchar(tg)) return(integer(0))

      idx <- which(vx == tg)
      if (length(idx) > 0) return(idx)

      # fallback substring
      which(grepl(tg, vx, fixed = TRUE))
    }

    # ------------------------------------------------------------------
    # 3) Domínios possíveis (união das 3 planilhas)
    # ------------------------------------------------------------------
    all_rras <- sort(unique(stats::na.omit(c(
      tabela_baixo$RRAS, tabela_agpar$RRAS, tabela_posnatal$RRAS
    ))))

    all_drs <- sort(unique(stats::na.omit(c(
      tabela_baixo$DRS, tabela_agpar$DRS, tabela_posnatal$DRS
    ))))

    all_regiao <- sort(unique(stats::na.omit(c(
      tabela_baixo$`REGIÃO DE SAÚDE`,
      tabela_agpar$`REGIÃO DE SAÚDE`,
      tabela_posnatal$`REGIÃO DE SAÚDE`
    ))))

    mun_vals <- unlist(lapply(
      list(tabela_baixo, tabela_agpar, tabela_posnatal),
      function(d) {
        cols <- intersect(names(d), c("MUNICÍPIO DA RRAS", "MUNICÍPIO"))
        if (length(cols) == 0) return(NULL)
        as.character(d[[cols[1]]])
      }
    ))
    all_munici <- sort(unique(stats::na.omit(mun_vals)))

    coord_vals <- unlist(lapply(
      list(tabela_baixo, tabela_agpar, tabela_posnatal),
      function(d) if ("COORDENADORIA DE SAÚDE" %in% names(d)) as.character(d[["COORDENADORIA DE SAÚDE"]]) else NULL
    ))
    all_coord <- sort(unique(stats::na.omit(coord_vals)))

    sup_vals <- unlist(lapply(
      list(tabela_baixo, tabela_agpar, tabela_posnatal),
      function(d) if ("SUPERVISÃO DE SAÚDE" %in% names(d)) as.character(d[["SUPERVISÃO DE SAÚDE"]]) else NULL
    ))
    all_sup <- sort(unique(stats::na.omit(sup_vals)))

    # ------------------------------------------------------------------
    # 4) UI dinâmica: filtro secundário + terciário
    #     (COM DIGITAÇÃO: pickerInput + live-search)
    # ------------------------------------------------------------------
    output$secondary_filter_ui <- renderUI({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level %in% c("DRS", "MUNICIPAL")) {
        selectInput(
          ns("sp_detail"),
          label    = "Especificar a cidade de São Paulo?",
          choices  = c("NÃO", "SIM"),
          selected = "NÃO"
        )
      } else {
        label_text <- switch(
          level,
          "RRAS"            = "Selecione a RRAS:",
          "REGIÃO DE SAÚDE" = "Selecione a região de saúde:",
          "Selecione:"
        )

        choices <- switch(
          level,
          "RRAS"            = format_rras_choices(all_rras),
          "REGIÃO DE SAÚDE" = all_regiao,
          format_rras_choices(all_rras)
        )

        selected_val <- if (length(choices) > 0) {
          if (is.character(choices) && is.null(names(choices))) choices[[1]] else unname(choices[[1]])
        } else NULL

        shinyWidgets::pickerInput(
          inputId = ns("secondary_filter"),
          label   = label_text,
          choices = choices,
          selected = selected_val,
          options = list(
            "live-search" = TRUE,
            "actions-box" = TRUE
          )
        )
      }
    })

    output$tertiary_filter_ui <- renderUI({
      tryCatch({

        req(input$nivel_selection)
        level <- input$nivel_selection

        # Continua sem 3º filtro para RRAS e REGIÃO DE SAÚDE
        if (level %in% c("RRAS", "REGIÃO DE SAÚDE")) return(NULL)

        sp <- input$sp_detail %||% "NÃO"

        if (level == "DRS") {
          if (safe_eq(sp, "SIM")) {
            return(
              shinyWidgets::pickerInput(
                inputId = ns("tertiary_filter"),
                label   = "Selecione a coordenadoria de saúde:",
                choices = all_coord,
                selected = if (length(all_coord) > 0) all_coord[[1]] else NULL,
                options = list("live-search" = TRUE, "actions-box" = TRUE)
              )
            )
          } else {
            return(
              shinyWidgets::pickerInput(
                inputId = ns("tertiary_filter"),
                label   = "Selecione a DRS:",
                choices = all_drs,
                selected = if (length(all_drs) > 0) all_drs[[1]] else NULL,
                options = list("live-search" = TRUE, "actions-box" = TRUE)
              )
            )
          }
        }

        if (level == "MUNICIPAL") {
          if (safe_eq(sp, "SIM")) {
            return(
              shinyWidgets::pickerInput(
                inputId = ns("tertiary_filter"),
                label   = "Selecione a supervisão de saúde:",
                choices = all_sup,
                selected = if (length(all_sup) > 0) all_sup[[1]] else NULL,
                options = list("live-search" = TRUE, "actions-box" = TRUE)
              )
            )
          } else {
            return(
              shinyWidgets::pickerInput(
                inputId = ns("tertiary_filter"),
                label   = "Selecione o município:",
                choices = all_munici,
                selected = if (length(all_munici) > 0) all_munici[[1]] else NULL,
                options = list("live-search" = TRUE, "actions-box" = TRUE)
              )
            )
          }
        }

        NULL

      }, error = function(e) {
        tags$div(
          style = "padding: 8px; border: 1px solid #c00; color: #c00; background: #fff5f5; border-radius: 6px;",
          tags$b("Erro ao renderizar o 3º filtro: "),
          tags$code(conditionMessage(e))
        )
      })
    })

    # ------------------------------------------------------------------
    # 4.1) bs4Dash/abas: manter outputs ativos mesmo quando ocultos
    # ------------------------------------------------------------------
    session$onFlushed(function() {
      try(outputOptions(output, "secondary_filter_ui", suspendWhenHidden = FALSE), silent = TRUE)
      try(outputOptions(output, "tertiary_filter_ui",  suspendWhenHidden = FALSE), silent = TRUE)
      try(outputOptions(output, "tables_ui",           suspendWhenHidden = FALSE), silent = TRUE)
      try(outputOptions(output, "table_baixo",         suspendWhenHidden = FALSE), silent = TRUE)
      try(outputOptions(output, "table_agpar",         suspendWhenHidden = FALSE), silent = TRUE)
      try(outputOptions(output, "table_posnatal",      suspendWhenHidden = FALSE), silent = TRUE)
    }, once = TRUE)

    # ------------------------------------------------------------------
    # 4.2) Evitar "piscada" de "Nenhum registro..." durante transição
    # ------------------------------------------------------------------
    rv <- reactiveValues(last_change = Sys.time())
    touch_change <- function() rv$last_change <- Sys.time()

    observeEvent(input$nivel_selection, touch_change(), ignoreInit = TRUE)
    observeEvent(input$secondary_filter, touch_change(), ignoreInit = TRUE)
    observeEvent(input$sp_detail, touch_change(), ignoreInit = TRUE)
    observeEvent(input$tertiary_filter, touch_change(), ignoreInit = TRUE)

    is_recent_change <- reactive({
      as.numeric(difftime(Sys.time(), rv$last_change, units = "secs")) < 0.7
    })

    # ------------------------------------------------------------------
    # 5) Contexto atual (garantindo defaults válidos)
    # ------------------------------------------------------------------
    current_ctx <- reactive({
      level <- input$nivel_selection %||% "RRAS"

      default_main_rras <- if (length(all_rras) > 0) unname(format_rras_choices(all_rras)[[1]]) else NA_character_
      default_main_reg  <- if (length(all_regiao) > 0) all_regiao[[1]] else NA_character_

      main_value <- input$secondary_filter %||% NA_character_
      if (level == "RRAS" && (is.na(main_value) || !nzchar(main_value))) main_value <- default_main_rras
      if (level == "REGIÃO DE SAÚDE" && (is.na(main_value) || !nzchar(main_value))) main_value <- default_main_reg

      sp_detail <- input$sp_detail %||% "NÃO"
      sp_detail_sim <- safe_eq(sp_detail, "SIM")

      third_value <- input$tertiary_filter %||% NA_character_
      if (level == "DRS") {
        if (sp_detail_sim) {
          if (is.na(third_value) || !nzchar(third_value)) third_value <- if (length(all_coord) > 0) all_coord[[1]] else NA_character_
        } else {
          if (is.na(third_value) || !nzchar(third_value)) third_value <- if (length(all_drs) > 0) all_drs[[1]] else NA_character_
        }
      }
      if (level == "MUNICIPAL") {
        if (sp_detail_sim) {
          if (is.na(third_value) || !nzchar(third_value)) third_value <- if (length(all_sup) > 0) all_sup[[1]] else NA_character_
        } else {
          if (is.na(third_value) || !nzchar(third_value)) third_value <- if (length(all_munici) > 0) all_munici[[1]] else NA_character_
        }
      }

      list(
        level       = level,
        main_value  = main_value,
        sp_detail   = sp_detail,
        third_value = third_value
      )
    })

    # ------------------------------------------------------------------
    # 6) Colunas base
    # ------------------------------------------------------------------
    cols_baixo_all <- c(
      "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICÍPIO DA RRAS",
      "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE",
      "ESTABELECIMENTO DE REFERÊNCIA PARA PARTOS DE BAIXO RISCO",
      "MUNICÍPIO DO ESTABELECIMENTO (PBR)"
    )

    cols_posnatal_all <- c(
      "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICÍPIO DA RRAS",
      "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE",
      "ESTABELECIMENTO DE REFERÊNCIA PARA ACOMPANHAMENTO PÓS NATAL (A-SEG)",
      "MUNICÍPIO DO ESTABELECIMENTO (A-SEG)"
    )

    cols_agpar_all <- c(
      "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICÍPIO DA RRAS",
      "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE",
      "ESTABELECIMENTO DE REFERÊNCIA PARA ATENDIMENTO AMBULATORIAL",
      "MUNICÍPIO DO ESTABELECIMENTO (AA)",
      "ESTABELECIMENTO DE REFERÊNCIA MATERNIDADE DE ALTO RISCO (AGPAR)",
      "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
      "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
      "ESTABELECIMENTO DE REFERÊNCIA PARA PARTOS DE ALTO RISCO",
      "MUNICÍPIO DO ESTABELECIMENTO (PAR)",
      "SUPERVISÃO DE SAÚDE (PAR)",
      "É REFERÊNCIA PARA AGPAR?"
    )

    # ------------------------------------------------------------------
    # 7) Contexto SP (quando supervisões fazem sentido)
    #    >>> ALTERAÇÃO: REGIÃO "SÃO PAULO" deve se comportar como RRAS 6
    # ------------------------------------------------------------------
    is_sp_context <- function(level, sp_detail, is_rras6, is_regiao_sp, is_drs_gsp, is_muni_sp) {
      isTRUE(is_rras6 || is_regiao_sp || is_drs_gsp || is_muni_sp || sp_detail)
    }

    # ------------------------------------------------------------------
    # 8) Regras de colunas por tabela e contexto
    #    >>> ALTERAÇÃO CRÍTICA:
    #    - REGIÃO DE SAÚDE: por padrão NÃO mostra supervisões
    #    - EXCEÇÃO: se REGIÃO == "SÃO PAULO" (is_regiao_sp), então:
    #         * BAIXO/PÓS: remover MUNICÍPIO e mostrar SUPERVISÃO
    #         * AGPAR: remover MUNICÍPIO e mostrar SUPERVISÕES (AGPAR/PAR)
    # ------------------------------------------------------------------
    compute_desired_cols <- function(table_type, ctx) {

      level <- ctx$level
      sp_detail <- safe_eq(ctx$sp_detail, "SIM")

      is_rras6     <- (level == "RRAS") && safe_eq(ctx$main_value, "6")
      is_regiao_sp <- (level == "REGIÃO DE SAÚDE") && safe_eq(ctx$main_value, "SAO PAULO")
      is_drs_gsp   <- (level == "DRS") && (!sp_detail) && safe_eq(ctx$third_value, "GRANDE SAO PAULO")
      is_muni_sp   <- (level == "MUNICIPAL") && (!sp_detail) && safe_eq(ctx$third_value, "SAO PAULO")

      sp_ctx <- is_sp_context(level, sp_detail, is_rras6, is_regiao_sp, is_drs_gsp, is_muni_sp)

      desired <- switch(
        table_type,
        "baixo"    = cols_baixo_all,
        "agpar"    = cols_agpar_all,
        "posnatal" = cols_posnatal_all,
        cols_baixo_all
      )

      # remover RRAS/DRS/REGIÃO/COORDENADORIA em TODOS os níveis
      desired <- setdiff(desired, c("RRAS", "DRS", "REGIÃO DE SAÚDE", "COORDENADORIA DE SAÚDE"))

      # fora de contexto SP: remover supervisões
      if (!sp_ctx) {
        desired <- setdiff(desired, c(
          "SUPERVISÃO DE SAÚDE",
          "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
          "SUPERVISÃO DE SAÚDE (PAR)"
        ))
      }

      # RRAS6
      if (level == "RRAS" && is_rras6) {
        if (table_type %in% c("baixo", "posnatal")) {
          desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
          if (sp_ctx) desired <- unique(c(desired, "SUPERVISÃO DE SAÚDE"))
        }
        if (table_type == "agpar") {
          desired <- setdiff(desired, c(
            "MUNICÍPIO DA RRAS",
            "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
            "MUNICÍPIO DO ESTABELECIMENTO (PAR)"
          ))
          if (sp_ctx) desired <- unique(c(
            desired,
            "SUPERVISÃO DE SAÚDE",
            "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
            "SUPERVISÃO DE SAÚDE (PAR)"
          ))
        }
      }

      # REGIÃO DE SAÚDE
      if (level == "REGIÃO DE SAÚDE") {
        if (is_regiao_sp) {
          # São Paulo: comporta como RRAS 6 (por supervisão)
          if (table_type %in% c("baixo", "posnatal")) {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            desired <- unique(c(desired, "SUPERVISÃO DE SAÚDE"))
          }
          if (table_type == "agpar") {
            desired <- setdiff(desired, c(
              "MUNICÍPIO DA RRAS",
              "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
              "MUNICÍPIO DO ESTABELECIMENTO (PAR)"
            ))
            desired <- unique(c(
              desired,
              "SUPERVISÃO DE SAÚDE",
              "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
              "SUPERVISÃO DE SAÚDE (PAR)"
            ))
          }
        } else {
          # Outras regiões: nunca mostrar supervisões
          desired <- setdiff(desired, c(
            "SUPERVISÃO DE SAÚDE",
            "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
            "SUPERVISÃO DE SAÚDE (PAR)"
          ))
        }
      }

      # DRS
      if (level == "DRS") {
        if (!sp_detail) {
          if (is_drs_gsp) {
            if (table_type %in% c("baixo", "posnatal")) {
              desired <- setdiff(desired, c("SUPERVISÃO DE SAÚDE"))
            }
            if (table_type == "agpar") {
              desired <- setdiff(desired, c("SUPERVISÃO DE SAÚDE"))
              if (sp_ctx) desired <- unique(c(
                desired,
                "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
                "SUPERVISÃO DE SAÚDE (PAR)"
              ))
            }
          }
        } else {
          if (table_type %in% c("baixo", "posnatal")) {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            if (sp_ctx) desired <- unique(c(desired, "SUPERVISÃO DE SAÚDE"))
          }
          if (table_type == "agpar") {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            if (sp_ctx) desired <- unique(c(
              desired,
              "SUPERVISÃO DE SAÚDE",
              "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
              "SUPERVISÃO DE SAÚDE (PAR)"
            ))
          }
        }
      }

      # MUNICIPAL
      if (level == "MUNICIPAL") {
        if (!sp_detail) {
          if (is_muni_sp) {
            if (table_type %in% c("baixo", "posnatal")) {
              if (sp_ctx) desired <- unique(c(desired, "SUPERVISÃO DE SAÚDE"))
            }
            if (table_type == "agpar") {
              if (sp_ctx) desired <- unique(c(
                desired,
                "SUPERVISÃO DE SAÚDE",
                "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
                "SUPERVISÃO DE SAÚDE (PAR)"
              ))
            }
          }
        } else {
          if (table_type %in% c("baixo", "posnatal")) {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            if (sp_ctx) desired <- unique(c(desired, "SUPERVISÃO DE SAÚDE"))
          }
          if (table_type == "agpar") {
            desired <- setdiff(desired, c(
              "MUNICÍPIO DA RRAS",
              "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
              "MUNICÍPIO DO ESTABELECIMENTO (PAR)"
            ))
            if (sp_ctx) desired <- unique(c(
              desired,
              "SUPERVISÃO DE SAÚDE",
              "SUPERVISÃO DE SAÚDE DO ESTABELECIMENTO (AGPAR)",
              "SUPERVISÃO DE SAÚDE (PAR)"
            ))
          }
        }
      }

      desired
    }

    # ------------------------------------------------------------------
    # 9) Filtragem por contexto - nunca retorna tudo se falhar
    # ------------------------------------------------------------------
    filter_df_by_ctx <- function(df, ctx) {

      level <- ctx$level
      sp_detail <- safe_eq(ctx$sp_detail, "SIM")

      if (level == "RRAS") {
        col <- "RRAS"
        if (!col %in% names(df)) return(df[0, , drop = FALSE])
        idx <- match_idx(df[[col]], ctx$main_value)
        return(df[idx, , drop = FALSE])
      }

      if (level == "REGIÃO DE SAÚDE") {
        col <- "REGIÃO DE SAÚDE"
        if (!col %in% names(df)) return(df[0, , drop = FALSE])
        idx <- match_idx(df[[col]], ctx$main_value)
        return(df[idx, , drop = FALSE])
      }

      if (level == "DRS") {
        if (sp_detail) {
          col <- "COORDENADORIA DE SAÚDE"
          if (!col %in% names(df)) return(df[0, , drop = FALSE])
          idx <- match_idx(df[[col]], ctx$third_value)
          return(df[idx, , drop = FALSE])
        } else {
          col <- "DRS"
          if (!col %in% names(df)) return(df[0, , drop = FALSE])
          idx <- match_idx(df[[col]], ctx$third_value)
          return(df[idx, , drop = FALSE])
        }
      }

      if (level == "MUNICIPAL") {
        if (sp_detail) {
          col <- "SUPERVISÃO DE SAÚDE"
          if (!col %in% names(df)) return(df[0, , drop = FALSE])
          idx <- match_idx(df[[col]], ctx$third_value)
          return(df[idx, , drop = FALSE])
        } else {
          col <- if ("MUNICÍPIO DA RRAS" %in% names(df)) "MUNICÍPIO DA RRAS" else if ("MUNICÍPIO" %in% names(df)) "MUNICÍPIO" else NULL
          if (is.null(col)) return(df[0, , drop = FALSE])
          idx <- match_idx(df[[col]], ctx$third_value)
          return(df[idx, , drop = FALSE])
        }
      }

      df[0, , drop = FALSE]
    }

    # ------------------------------------------------------------------
    # 10) Renomeia apenas para display (MUNICÍPIO DA RRAS -> MUNICÍPIO)
    # ------------------------------------------------------------------
    rename_for_display <- function(df) {
      if ("MUNICÍPIO DA RRAS" %in% names(df)) {
        names(df)[names(df) == "MUNICÍPIO DA RRAS"] <- "MUNICÍPIO"
      }
      df
    }

    # ------------------------------------------------------------------
    # 10.1) Determina coluna-chave (Município vs Supervisão)
    #     >>> ALTERAÇÃO: REGIÃO "SÃO PAULO" deve agrupar por supervisão
    # ------------------------------------------------------------------
    determine_group_col <- function(df, ctx) {
      level <- ctx$level
      sp_detail_sim <- safe_eq(ctx$sp_detail, "SIM")

      is_rras6     <- (level == "RRAS") && safe_eq(ctx$main_value, "6")
      is_regiao_sp <- (level == "REGIÃO DE SAÚDE") && safe_eq(ctx$main_value, "SAO PAULO")
      is_drs_sp    <- (level == "DRS") && sp_detail_sim
      is_muni_spd  <- (level == "MUNICIPAL") && sp_detail_sim

      prefer_sup <- isTRUE(is_rras6 || is_regiao_sp || is_drs_sp || is_muni_spd)

      if (prefer_sup && "SUPERVISÃO DE SAÚDE" %in% names(df)) return("SUPERVISÃO DE SAÚDE")
      if ("MUNICÍPIO" %in% names(df)) return("MUNICÍPIO")
      if ("SUPERVISÃO DE SAÚDE" %in% names(df)) return("SUPERVISÃO DE SAÚDE")

      NULL
    }

    # ------------------------------------------------------------------
    # 11) Layout das tabelas
    # ------------------------------------------------------------------
    output$tables_ui <- renderUI({
      req(input$nivel_selection)

      if (input$nivel_selection == "MUNICIPAL") {
        tagList(
          fluidRow(
            column(
              12,
              bs4Dash::box(
                title = "Baixo Risco (Partos)",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                reactable::reactableOutput(ns("table_baixo"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              bs4Dash::box(
                title = "Ambulatório de Gestação e Puerpério de Alto Risco e Partos de Alto Risco",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                reactable::reactableOutput(ns("table_agpar"))
              )
            )
          ),
          fluidRow(
            column(
              12,
              bs4Dash::box(
                title = "Acompanhamento de Crianças de Alto Risco (A-SEG) — Pós Natal",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                collapsible = FALSE,
                reactable::reactableOutput(ns("table_posnatal"))
              )
            )
          )
        )
      } else {
        fluidRow(
          column(
            12,
            tags$div(
              class = "estab-tabbox-fill estab-tabs-prenatal",
              bs4Dash::tabBox(
                id          = ns("tabbox"),
                title       = NULL,
                side        = "left",
                status      = "primary",
                solidHeader = TRUE,
                width       = 12,
                type        = "tabs",
                selected    = "Baixo Risco (Partos)",
                tabPanel("Baixo Risco (Partos)", reactable::reactableOutput(ns("table_baixo"))),
                tabPanel("Alto Risco (Partos / Ambulatório de Gestação e Puerpério)", reactable::reactableOutput(ns("table_agpar"))),
                tabPanel("Pós-natal (A-SEG – Alto Risco)", reactable::reactableOutput(ns("table_posnatal")))
              )
            )
          )
        )
      }
    })

    # ------------------------------------------------------------------
    # 12) Reactable — flat + expandível (sem emptyMessage)
    # ------------------------------------------------------------------
    base_coldef <- reactable::colDef(
      align = "center",
      style = list(
        whiteSpace = "normal",
        verticalAlign = "top"
      )
    )

    build_reactable_flat <- function(df) {
      reactable::reactable(
        df,
        compact       = TRUE,
        bordered      = TRUE,
        highlight     = TRUE,
        striped       = TRUE,
        wrap          = TRUE,
        pagination    = FALSE,
        defaultColDef = base_coldef
      )
    }

    build_reactable_placeholder <- function(msg = "Carregando...") {
      dfp <- data.frame(Mensagem = msg, stringsAsFactors = FALSE)
      reactable::reactable(
        dfp,
        compact       = TRUE,
        bordered      = TRUE,
        highlight     = FALSE,
        striped       = FALSE,
        wrap          = TRUE,
        pagination    = FALSE,
        sortable      = FALSE,
        resizable     = FALSE,
        defaultColDef = reactable::colDef(
          align = "center",
          style = list(fontWeight = 600)
        )
      )
    }

    build_reactable_expandable <- function(df, group_col) {

      if (is.null(group_col) || !group_col %in% names(df)) {
        return(build_reactable_flat(df))
      }

      if (nrow(df) == 0) {
        return(build_reactable_flat(df))
      }

      df$.grp_key <- vapply(df[[group_col]], normalize_str, character(1))

      first_idx <- which(!duplicated(df$.grp_key))
      summary_df <- df[first_idx, , drop = FALSE]

      main_df <- summary_df[, c(group_col, ".grp_key"), drop = FALSE]

      col_defs <- list()
      col_defs[[".grp_key"]] <- reactable::colDef(show = FALSE)
      col_defs[[group_col]]  <- reactable::colDef(
        name  = group_col,
        style = list(fontWeight = 600)
      )

      reactable::reactable(
        main_df,
        columns     = col_defs,
        compact     = TRUE,
        bordered    = TRUE,
        highlight   = TRUE,
        striped     = TRUE,
        wrap        = TRUE,
        pagination  = FALSE,
        onClick     = "expand",
        details = function(index) {
          key <- main_df$.grp_key[index]
          sub <- df[df$.grp_key == key, , drop = FALSE]

          sub[[group_col]] <- NULL
          sub$.grp_key <- NULL

          if (ncol(sub) == 0) {
            return(tags$div(style = "padding: 8px;", "Sem detalhes adicionais."))
          }

          tags$div(
            style = "padding: 8px 12px;",
            reactable::reactable(
              sub,
              compact     = TRUE,
              bordered    = TRUE,
              highlight   = TRUE,
              striped     = TRUE,
              wrap        = TRUE,
              pagination  = FALSE,
              defaultColDef = base_coldef
            )
          )
        }
      )
    }

    # ------------------------------------------------------------------
    # 13) Render das tabelas (sem “piscada”)
    # ------------------------------------------------------------------
    render_table_core <- function(df_raw, table_type, ctx) {

      df <- filter_df_by_ctx(df_raw, ctx)

      desired <- compute_desired_cols(table_type, ctx)
      df <- select_cols_preserve_order(df, desired)
      df <- rename_for_display(df)

      group_col <- determine_group_col(df, ctx)

      if (nrow(df) == 0 && isTRUE(is_recent_change())) {
        return(build_reactable_placeholder("Carregando..."))
      }

      validate(need(nrow(df) > 0, "Nenhum registro encontrado para os filtros selecionados."))

      build_reactable_expandable(df, group_col)
    }

    output$table_baixo <- reactable::renderReactable({
      ctx <- current_ctx()
      render_table_core(tabela_baixo, "baixo", ctx)
    })

    output$table_agpar <- reactable::renderReactable({
      ctx <- current_ctx()
      render_table_core(tabela_agpar, "agpar", ctx)
    })

    output$table_posnatal <- reactable::renderReactable({
      ctx <- current_ctx()
      render_table_core(tabela_posnatal, "posnatal", ctx)
    })

  })
}
