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
          title = tagList(shiny::icon("circle-question"), " Estabelecimentos de referência"),
          size  = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Fechar"),
          tags$div(
            style = "font-size: 15px; line-height: 1.55;",
            tags$p(tags$b("Estabelecimentos de referência")),
            tags$p(
              "As informações deste painel foram organizadas a partir dos ",
              tags$b("planos de ação da Rede Alyne"),
              ", considerando a definição de estabelecimentos de referência para o atendimento de gestantes e recém-nascidos no estado de São Paulo."
            ),
            tags$p(
              "O painel permite identificar onde cada tipo de paciente deve ser atendida, de acordo com suas necessidades de cuidado."
            ),
            tags$p(
              "Essa organização apoia o encaminhamento adequado das gestantes e recém-nascidos na rede de atenção à saúde."
            ),
            tags$p(
              tags$b("Para saber a fonte dessa informação, acesse a seção Documentação dos Indicadores, disponível no menu lateral.")
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

      y <- gsub("\u00A0", " ", y, fixed = TRUE)
      y <- trimws(y)
      y <- gsub("\\s+", " ", y)
      y <- iconv(y, from = "UTF-8", to = "ASCII//TRANSLIT")
      y <- toupper(y)
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

    # Seleciona colunas na ordem definida para exibição.
    select_cols_preserve_order <- function(df, desired_set) {
      keep <- desired_set[desired_set %in% names(df)]
      df[, keep, drop = FALSE]
    }

    match_idx <- function(vec, target) {
      vx <- vapply(vec, normalize_str, character(1))
      tg <- normalize_str(target)
      if (is.na(tg) || !nzchar(tg)) return(integer(0))

      idx <- which(vx == tg)
      if (length(idx) > 0) return(idx)

      which(grepl(tg, vx, fixed = TRUE))
    }

    is_blank_value <- function(x) {
      if (is.null(x)) return(rep(TRUE, 0))
      if (is.list(x)) x <- unlist(x, use.names = FALSE)

      if (is.character(x)) {
        y <- trimws(x)
        return(is.na(y) | y == "")
      }

      if (is.factor(x)) {
        y <- trimws(as.character(x))
        return(is.na(y) | y == "")
      }

      if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
        return(is.na(x))
      }

      is.na(x)
    }

    cell_text <- function(x) {
      if (is.null(x) || length(x) == 0) return("")
      if (is.list(x)) x <- unlist(x, use.names = FALSE)
      if (length(x) == 0 || isTRUE(is_blank_value(x[1]))) return("")

      y <- as.character(x[1])
      y <- gsub("\u00A0", " ", y, fixed = TRUE)
      y <- gsub("\\s+", " ", y)
      trimws(y)
    }

    observeEvent(input$address_click, {
      payload <- input$address_click %||% list()
      title <- cell_text(payload$title)
      address <- cell_text(payload$address)

      if (!nzchar(address)) return(NULL)
      if (!nzchar(title)) title <- "Estabelecimento"

      shiny::showModal(
        shiny::modalDialog(
          title = tagList(shiny::icon("map-marker-alt"), " Endereço do estabelecimento"),
          size = "m",
          easyClose = TRUE,
          footer = shiny::modalButton("Fechar"),
          tags$div(
            class = "estab-address-modal",
            tags$div(class = "estab-address-modal-title", title),
            tags$div(class = "estab-address-modal-text", address)
          )
        )
      )
    })

    # Remove apenas linhas vazias excedentes dentro de cada grupo.
    # Regra:
    # - se um grupo possui ao menos 1 linha com algum detalhe preenchido,
    #   removemos as linhas totalmente vazias desse grupo;
    # - se um grupo possui somente linhas vazias nos detalhes,
    #   preservamos 1 linha para que o município/supervisão continue aparecendo.
    prune_blank_rows_within_group <- function(df, group_col = NULL) {
      if (!is.data.frame(df) || nrow(df) == 0) return(df)
      if (is.null(group_col) || !group_col %in% names(df)) return(df)

      # Não faz sentido manter grupos sem identificador visual
      keep_group <- !is_blank_value(df[[group_col]])
      df <- df[keep_group, , drop = FALSE]
      if (nrow(df) == 0) return(df)

      detail_cols <- setdiff(names(df), group_col)
      if (length(detail_cols) == 0) return(df)

      has_any_detail <- Reduce(
        `|`,
        lapply(detail_cols, function(col) !is_blank_value(df[[col]]))
      )

      grp_key <- vapply(df[[group_col]], normalize_str, character(1))
      keep <- rep(TRUE, nrow(df))

      for (g in unique(grp_key)) {
        idx <- which(grp_key == g)
        grp_has_detail <- any(has_any_detail[idx])

        if (grp_has_detail) {
          # Remove apenas as linhas totalmente vazias deste grupo
          keep[idx] <- has_any_detail[idx]
        } else {
          # Todas as linhas estão vazias: mantém apenas uma linha
          keep[idx] <- FALSE
          keep[idx[1]] <- TRUE
        }
      }

      df[keep, , drop = FALSE]
    }

    order_display_df <- function(df, group_col = NULL) {
      if (!is.data.frame(df) || nrow(df) == 0) return(df)

      ord_cols <- character(0)

      if (!is.null(group_col) && group_col %in% names(df)) {
        ord_cols <- c(ord_cols, group_col)
      }

      extra_cols <- names(df)[vapply(df, function(col) {
        is.character(col) || is.factor(col)
      }, logical(1))]

      extra_cols <- setdiff(extra_cols, ord_cols)
      ord_cols <- c(ord_cols, extra_cols)

      if (length(ord_cols) == 0) return(df)

      ord_list <- lapply(ord_cols, function(col) normalize_str(df[[col]]))
      ord <- do.call(order, c(ord_list, list(na.last = TRUE)))

      df[ord, , drop = FALSE]
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
      level <- input$nivel_selection %||% "DRS"

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
      "MATERNIDADE DE BAIXO RISCO DE REFERÊNCIA",
      "MUNICÍPIO DO ESTABELECIMENTO",
      "CNES",
      "ENDEREÇO DO ESTABELECIMENTO DE BAIXO RISCO"
    )

    cols_posnatal_all <- c(
      "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICÍPIO DA RRAS",
      "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE",
      "AMBULATÓRIO DE ACOMPANHAMENTO DE CRIANÇAS DE ALTO RISCO PRIORITARIAMENTE EGRESSAS DE UNIDADE NEONATAL (A-SEG)",
      "MUNICÍPIO DO ESTABELECIMENTO (A-SEG)",
      "CNES",
      "ENDEREÇO DO ESTABELECIMENTO (A-SEG)"
    )

    cols_agpar_all <- c(
      "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICÍPIO DA RRAS",
      "COORDENADORIA DE SAÚDE", "SUPERVISÃO DE SAÚDE",
      "AMBULATÓRIO DE GESTAÇÃO E PUERPÉRIO DE ALTO RISCO (AGPAR)",
      "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
      "CNES (AGPAR)",
      "ENDEREÇO DO ESTABELECIMENTO (AGPAR)",
      "MATERNIDADE DE ALTO RISCO DE REFERÊNCIA",
      "MUNICÍPIO DA MATERNIDADE DE ALTO RISCO",
      "CNES (MATERNIDADE DE ALTO RISCO)",
      "ENDEREÇO DO ESTABELECIMENTO DE ALTO RISCO"
    )

    address_specs <- list(
      baixo = list(
        list(
          est_col = "MATERNIDADE DE BAIXO RISCO DE REFERÊNCIA",
          address_col = "ENDEREÇO DO ESTABELECIMENTO DE BAIXO RISCO"
        )
      ),
      agpar = list(
        list(
          est_col = "AMBULATÓRIO DE GESTAÇÃO E PUERPÉRIO DE ALTO RISCO (AGPAR)",
          address_col = "ENDEREÇO DO ESTABELECIMENTO (AGPAR)"
        ),
        list(
          est_col = "MATERNIDADE DE ALTO RISCO DE REFERÊNCIA",
          address_col = "ENDEREÇO DO ESTABELECIMENTO DE ALTO RISCO"
        )
      ),
      posnatal = list(
        list(
          est_col = "AMBULATÓRIO DE ACOMPANHAMENTO DE CRIANÇAS DE ALTO RISCO PRIORITARIAMENTE EGRESSAS DE UNIDADE NEONATAL (A-SEG)",
          address_col = "ENDEREÇO DO ESTABELECIMENTO (A-SEG)"
        )
      )
    )

    address_hidden_cols <- unique(
      unlist(
        lapply(address_specs, function(specs) {
          vapply(specs, `[[`, character(1), "address_col")
        }),
        use.names = FALSE
      )
    )

    # ------------------------------------------------------------------
    # 7) Contexto SP
    # ------------------------------------------------------------------
    is_sp_context <- function(level, sp_detail, is_rras6, is_regiao_sp, is_drs_gsp, is_muni_sp) {
      isTRUE(is_rras6 || is_regiao_sp || is_drs_gsp || is_muni_sp || sp_detail)
    }

    # ------------------------------------------------------------------
    # 8) Regras de colunas por tabela e contexto
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

      desired <- setdiff(desired, c("RRAS", "DRS", "REGIÃO DE SAÚDE", "COORDENADORIA DE SAÚDE"))

      if (!sp_ctx) {
        desired <- setdiff(desired, c("SUPERVISÃO DE SAÚDE"))
      }

      # RRAS 6
      if (level == "RRAS" && is_rras6) {
        if (table_type %in% c("baixo", "posnatal", "agpar")) {
          desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
          if (sp_ctx) desired <- unique(c("SUPERVISÃO DE SAÚDE", desired))
        }
      }

      # RRAS diferente da 6 nunca deve exibir Supervisão de Saúde
      if (level == "RRAS" && !is_rras6) {
        desired <- setdiff(desired, c("SUPERVISÃO DE SAÚDE"))
      }

      # REGIÃO DE SAÚDE
      if (level == "REGIÃO DE SAÚDE") {
        if (is_regiao_sp) {
          if (table_type %in% c("baixo", "posnatal", "agpar")) {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            desired <- unique(c("SUPERVISÃO DE SAÚDE", desired))
          }
        } else {
          desired <- setdiff(desired, c("SUPERVISÃO DE SAÚDE"))
        }
      }

      # DRS
      if (level == "DRS") {
        if (!sp_detail) {
          if (is_drs_gsp) {
            desired <- setdiff(desired, c("SUPERVISÃO DE SAÚDE"))
          }
        } else {
          if (table_type %in% c("baixo", "posnatal", "agpar")) {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            if (sp_ctx) desired <- unique(c("SUPERVISÃO DE SAÚDE", desired))
          }
        }
      }

      # MUNICIPAL
      if (level == "MUNICIPAL") {
        if (!sp_detail) {
          if (is_muni_sp) {
            if (table_type %in% c("baixo", "posnatal", "agpar")) {
              if (sp_ctx) desired <- unique(c("SUPERVISÃO DE SAÚDE", desired))
            }
          }
        } else {
          if (table_type %in% c("baixo", "posnatal", "agpar")) {
            desired <- setdiff(desired, c("MUNICÍPIO DA RRAS"))
            if (sp_ctx) desired <- unique(c("SUPERVISÃO DE SAÚDE", desired))
          }
        }
      }

      desired
    }

    # ------------------------------------------------------------------
    # 9) Filtragem por contexto
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
    # 10) Renomeia apenas para display
    # ------------------------------------------------------------------
    rename_for_display <- function(df) {
      if ("MUNICÍPIO DA RRAS" %in% names(df)) {
        names(df)[names(df) == "MUNICÍPIO DA RRAS"] <- "MUNICÍPIO"
      }
      df
    }

    # ------------------------------------------------------------------
    # 10.1) Determina coluna-chave (Município vs Supervisão)
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
                title = "Gestação de Baixo Risco",
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
                title = "Gestação de Alto Risco",
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
                title = "Acompanhamento Neonatal de Alto Risco",
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
                selected    = "Gestação de Baixo Risco",
                tabPanel("Gestação de Baixo Risco", reactable::reactableOutput(ns("table_baixo"))),
                tabPanel("Gestação de Alto Risco", reactable::reactableOutput(ns("table_agpar"))),
                tabPanel("Acompanhamento Neonatal de Alto Risco", reactable::reactableOutput(ns("table_posnatal")))
              )
            )
          )
        )
      }
    })

    # ------------------------------------------------------------------
    # 12) Reactable
    # ------------------------------------------------------------------
    base_coldef <- reactable::colDef(
      align = "center",
      style = list(
        whiteSpace = "normal",
        verticalAlign = "top"
      )
    )

    render_establishment_cell <- function(value, address) {
      establishment <- cell_text(value)
      address <- cell_text(address)

      if (!nzchar(establishment)) return("")

      if (!nzchar(address)) {
        return(
          tags$div(
            class = "estab-ref-cell",
            tags$span(class = "estab-ref-name", establishment)
          )
        )
      }

      payload_json <- jsonlite::toJSON(
        list(title = establishment, address = address),
        auto_unbox = TRUE,
        null = "null"
      )
      input_id_json <- jsonlite::toJSON(ns("address_click"), auto_unbox = TRUE)
      click_js <- sprintf(
        "Shiny.setInputValue(%s, Object.assign(%s, {nonce: Math.random()}), {priority: 'event'}); return false;",
        input_id_json,
        payload_json
      )

      tags$div(
        class = "estab-ref-cell",
        tags$span(class = "estab-ref-name", establishment),
        tags$button(
          type = "button",
          class = "estab-address-btn",
          onclick = click_js,
          title = "Ver endereço",
          `aria-label` = paste("Ver endereço de", establishment),
          shiny::icon("map-marker-alt"),
          tags$span("Endereço")
        )
      )
    }

    make_establishment_coldef <- function(df, address_col) {
      force(df)
      force(address_col)

      reactable::colDef(
        align = "center",
        style = list(
          whiteSpace = "normal",
          verticalAlign = "top"
        ),
        cell = function(value, index, name) {
          render_establishment_cell(value, df[[address_col]][index])
        }
      )
    }

    column_defs_for_table <- function(df, table_type = NULL) {
      col_defs <- list()

      for (address_col in intersect(address_hidden_cols, names(df))) {
        col_defs[[address_col]] <- reactable::colDef(show = FALSE)
      }

      specs <- address_specs[[table_type]] %||% list()
      for (spec in specs) {
        est_col <- spec$est_col
        address_col <- spec$address_col
        if (est_col %in% names(df) && address_col %in% names(df)) {
          col_defs[[est_col]] <- make_establishment_coldef(df, address_col)
        }
      }

      col_defs
    }

    build_reactable_flat <- function(df, table_type = NULL) {
      reactable::reactable(
        df,
        compact       = TRUE,
        bordered      = TRUE,
        highlight     = TRUE,
        striped       = TRUE,
        wrap          = TRUE,
        pagination    = FALSE,
        columns       = column_defs_for_table(df, table_type),
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

    build_reactable_expandable <- function(df, group_col, table_type = NULL) {

      if (is.null(group_col) || !group_col %in% names(df)) {
        return(build_reactable_flat(df, table_type = table_type))
      }

      if (nrow(df) == 0) {
        return(build_reactable_flat(df, table_type = table_type))
      }

      df <- order_display_df(df, group_col = group_col)

      df$.grp_key <- vapply(df[[group_col]], normalize_str, character(1))

      first_idx <- which(!duplicated(df$.grp_key))
      summary_df <- df[first_idx, , drop = FALSE]

      main_df <- summary_df[, c(group_col, ".grp_key"), drop = FALSE]
      main_df <- order_display_df(main_df, group_col = group_col)

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
          sub <- order_display_df(sub)
          visible_cols <- setdiff(names(sub), address_hidden_cols)

          if (length(visible_cols) == 0) {
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
              columns     = column_defs_for_table(sub, table_type),
              defaultColDef = base_coldef
            )
          )
        }
      )
    }

    # ------------------------------------------------------------------
    # 13) Render das tabelas
    # ------------------------------------------------------------------
    render_table_core <- function(df_raw, table_type, ctx) {

      df <- filter_df_by_ctx(df_raw, ctx)

      desired <- compute_desired_cols(table_type, ctx)
      df <- select_cols_preserve_order(df, desired)
      df <- rename_for_display(df)

      group_col <- determine_group_col(df, ctx)

      df <- prune_blank_rows_within_group(df, group_col = group_col)
      df <- order_display_df(df, group_col = group_col)

      if (nrow(df) == 0 && isTRUE(is_recent_change())) {
        shiny::invalidateLater(800, session)
        return(build_reactable_placeholder("Carregando..."))
      }

      validate(need(nrow(df) > 0, "Nenhum registro encontrado para os filtros selecionados."))

      build_reactable_expandable(df, group_col, table_type = table_type)
    }

    export_col_config <- function(table_type, group_col) {
      group_col <- group_col %||% "MUNICÍPIO"

      switch(
        table_type,
        "baixo" = list(
          cols = c(
            group_col,
            "MATERNIDADE DE BAIXO RISCO DE REFERÊNCIA",
            "ENDEREÇO DO ESTABELECIMENTO DE BAIXO RISCO",
            "MUNICÍPIO DO ESTABELECIMENTO",
            "CNES"
          ),
          names = c(
            group_col,
            "MATERNIDADE DE BAIXO RISCO DE REFERÊNCIA",
            "ENDEREÇO",
            "MUNICÍPIO DO ESTABELECIMENTO",
            "CNES"
          )
        ),
        "agpar" = list(
          cols = c(
            group_col,
            "AMBULATÓRIO DE GESTAÇÃO E PUERPÉRIO DE ALTO RISCO (AGPAR)",
            "ENDEREÇO DO ESTABELECIMENTO (AGPAR)",
            "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
            "CNES (AGPAR)",
            "MATERNIDADE DE ALTO RISCO DE REFERÊNCIA",
            "ENDEREÇO DO ESTABELECIMENTO DE ALTO RISCO",
            "MUNICÍPIO DA MATERNIDADE DE ALTO RISCO",
            "CNES (MATERNIDADE DE ALTO RISCO)"
          ),
          names = c(
            group_col,
            "AMBULATÓRIO DE GESTAÇÃO E PUERPÉRIO DE ALTO RISCO (AGPAR)",
            "ENDEREÇO (AGPAR)",
            "MUNICÍPIO DO ESTABELECIMENTO (AGPAR)",
            "CNES (AGPAR)",
            "MATERNIDADE DE ALTO RISCO DE REFERÊNCIA",
            "ENDEREÇO (MATERNIDADE DE ALTO RISCO)",
            "MUNICÍPIO DA MATERNIDADE DE ALTO RISCO",
            "CNES (MATERNIDADE DE ALTO RISCO)"
          )
        ),
        "posnatal" = list(
          cols = c(
            group_col,
            "AMBULATÓRIO DE ACOMPANHAMENTO DE CRIANÇAS DE ALTO RISCO PRIORITARIAMENTE EGRESSAS DE UNIDADE NEONATAL (A-SEG)",
            "ENDEREÇO DO ESTABELECIMENTO (A-SEG)",
            "MUNICÍPIO DO ESTABELECIMENTO (A-SEG)",
            "CNES"
          ),
          names = c(
            group_col,
            "AMBULATÓRIO DE ACOMPANHAMENTO DE CRIANÇAS DE ALTO RISCO PRIORITARIAMENTE EGRESSAS DE UNIDADE NEONATAL (A-SEG)",
            "ENDEREÇO",
            "MUNICÍPIO DO ESTABELECIMENTO (A-SEG)",
            "CNES"
          )
        )
      )
    }

    build_export_table <- function(df_raw, table_type, ctx) {
      df <- filter_df_by_ctx(df_raw, ctx)

      desired <- compute_desired_cols(table_type, ctx)
      df <- select_cols_preserve_order(df, desired)
      df <- rename_for_display(df)

      group_col <- determine_group_col(df, ctx)
      df <- prune_blank_rows_within_group(df, group_col = group_col)
      df <- order_display_df(df, group_col = group_col)

      cfg <- export_col_config(table_type, group_col)
      for (col in cfg$cols) {
        if (!col %in% names(df)) {
          df[[col]] <- ""
        }
      }

      out <- df[, cfg$cols, drop = FALSE]
      names(out) <- cfg$names
      out[] <- lapply(out, function(col) {
        vapply(seq_along(col), function(i) cell_text(col[[i]]), character(1))
      })

      out
    }

    export_tables <- reactive({
      ctx <- current_ctx()
      list(
        "Gestação de Baixo Risco" = build_export_table(tabela_baixo, "baixo", ctx),
        "Gestação de Alto Risco" = build_export_table(tabela_agpar, "agpar", ctx),
        "Acomp Neonatal Alto Risco" = build_export_table(tabela_posnatal, "posnatal", ctx)
      )
    })

    download_slug <- reactive({
      ctx <- current_ctx()
      value <- switch(
        ctx$level,
        "RRAS" = paste0("rras_", ctx$main_value),
        "REGIÃO DE SAÚDE" = paste0("regiao_", ctx$main_value),
        "DRS" = paste0("drs_", ctx$third_value),
        "MUNICIPAL" = paste0("municipal_", ctx$third_value),
        "estabelecimentos"
      )
      slug <- normalize_str(value)
      slug <- tolower(gsub("[^A-Z0-9]+", "_", slug))
      slug <- gsub("^_+|_+$", "", slug)
      if (!nzchar(slug)) "estabelecimentos" else slug
    })

    output$download_estab_xlsx <- downloadHandler(
      filename = function() {
        paste0("estabelecimentos_referencia_", download_slug(), ".xlsx")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        header_style <- openxlsx::createStyle(
          textDecoration = "bold",
          fgFill = "#EAF0F7",
          border = "bottom"
        )

        tables <- export_tables()
        for (sheet_name in names(tables)) {
          df <- tables[[sheet_name]]
          openxlsx::addWorksheet(wb, sheet_name)
          openxlsx::writeData(wb, sheet_name, df, headerStyle = header_style)
          openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
          if (ncol(df) > 0) {
            openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
          }
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    outputOptions(output, "download_estab_xlsx", suspendWhenHidden = FALSE)

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
