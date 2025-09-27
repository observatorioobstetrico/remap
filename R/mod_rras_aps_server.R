# R/mod_rras_aps_server.R
#' RRAS APS Server
#'
#' @param id Module id
#' @param data_list Lista com os dados carregados em load_data()
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @export
mod_rras_aps_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Helpers de validação e update seguro
    valid_choice <- function(x) {
      isTruthy(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    }

    safe_update_picker <- function(id, choices) {
      choices <- sort(unique(na.omit(choices)))
      shinyWidgets::updatePickerInput(
        session, id,
        choices  = if (length(choices)) choices else NULL,
        selected = if (length(choices)) choices[1] else NULL
      )
    }

    # Helper de categoria do eixo e orientação
    get_cat_config <- function(level, secondary_filter, analisar_sp) {
      # valores padrão
      cat_var <- if (identical(level, "ESTADUAL")) "RRAS" else "MUNICIPAL"
      force_v <- FALSE

      # Casos especiais que usam "SUPERVISÃO DE SAÚDE"
      if (level %in% c("RRAS", "REGIÃO DE SAÚDE") && valid_choice(secondary_filter)) {
        if ((identical(level, "RRAS") && identical(secondary_filter, "RRAS 6")) ||
            (identical(level, "REGIÃO DE SAÚDE") && identical(secondary_filter, "SÃO PAULO"))) {
          cat_var <- "SUPERVISÃO DE SAÚDE"
        }
      }

      # Coordenadoria de Saúde (quando analisar_sp == "SIM")
      if (identical(level, "DRS") && isTRUE(analisar_sp == "SIM")) {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      }

      list(cat_var = cat_var, force_v = force_v)
    }
    # ------------------------------------------------------------

    # Função auxiliar para calcular a altura dinâmica com base no número de barras
    calc_dynamic_height <- function(n_bars) {
      if(n_bars <= 10) {
        400
      } else if(n_bars <= 25) {
        500
      } else if(n_bars <= 40) {
        700
      } else if(n_bars <= 55) {
        900
      } else if(n_bars <= 75) {
        1200
      } else {
        2500
      }
    }

    # Função auxiliar para construir o card que conterá o gráfico (à prova de dados vazios)
    build_plot_card <- function(card_title, plot_output_id, data_to_plot, caption = NULL, height_override = NULL) {
      n_bars <- if (is.null(data_to_plot) || !is.data.frame(data_to_plot)) 0L else nrow(data_to_plot)
      # Se height_override não for NULL, usa-o; caso contrário, calcula dinamicamente
      height_val <- if (is.null(height_override)) calc_dynamic_height(n_bars) else height_override
      bs4Dash::bs4Card(
        title  = card_title,
        height = "100%",
        width = NULL,
        tagList(
          plotly::plotlyOutput(ns(plot_output_id), height = paste0(height_val, "px")),
          if (!is.null(caption)) {
            tags$div(caption, style = "margin-top: 15px; font-size: 12px; color: #555;")
          }
        )
      )
    }

    # Funções auxiliares para formatação dos rótulos
    wrap_after_second <- function(text, threshold) {
      if(nchar(text) > threshold) {
        words <- unlist(strsplit(text, "\\s+"))
        if(length(words) > 2) {
          paste(paste(words[1:2], collapse = " "), paste(words[-(1:2)], collapse = " "), sep = "<br>")
        } else {
          text
        }
      } else {
        text
      }
    }

    wrap_vertical_title <- function(text) {
      if(nchar(text) <= 15) {
        return(text)
      } else if(nchar(text) > 15 && nchar(text) <= 20) {
        words <- unlist(strsplit(text, "\\s+"))
        if(length(words) > 2) {
          return(paste(paste(words[1:2], collapse = " "), paste(words[-(1:2)], collapse = " "), sep = "<br>"))
        } else {
          return(text)
        }
      } else if(nchar(text) > 20) {
        words <- unlist(strsplit(text, "\\s+"))
        return(paste(words, collapse = "<br>"))
      }
    }

    # Função auxiliar para formatar inteiros com separador de milhar (ponto)
    format_integer <- function(x) {
      formatC(as.integer(x), format = "d", big.mark = ".", decimal.mark = ",")
    }

    build_bar_plot <- function(data, var_numeric, var_category, is_percentage = FALSE, force_vertical = FALSE) {
      # Se a categoria for "MUNICIPAL", exibe como "MUNICÍPIO"
      display_category <- ifelse(var_category == "MUNICIPAL", "MUNICÍPIO", var_category)

      # Guardas contra dados/colunas inexistentes
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
          !(var_numeric %in% names(data)) || !(var_category %in% names(data))) {
        # Traço invisível para calar os avisos "No trace type..."
        return(
          plotly::plot_ly(
            x = 0, y = 0,
            type = "scatter", mode = "markers",
            opacity = 0, hoverinfo = "skip", showlegend = FALSE
          ) |>
            plotly::layout(
              annotations = list(
                text = "Loading...",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 14)
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              margin = list(l = 20, r = 20, t = 20, b = 20)
            )
        )
      }

      n_bars <- nrow(data)

      # Pré-formata os valores para o hover (customdata)
      if (is_percentage){
        data$formatted_value <- sapply(data[[var_numeric]], function(x) {
          x_num <- suppressWarnings(as.numeric(x))
          format(round(x_num, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
        })
      } else {
        data$formatted_value <- sapply(data[[var_numeric]], function(x) {
          x_num <- suppressWarnings(as.numeric(x))
          format_integer(x_num)
        })
      }

      # Define orientação com tolerância a dados vazios
      if (isTRUE(force_vertical)) {
        orientation <- "v"
      } else if (n_bars <= 10) {
        orientation <- "v"
      } else {
        tick_names <- as.character(data[[var_category]])
        all_short <- all(nchar(tick_names) <= 12 &
                           sapply(tick_names, function(x) length(unlist(strsplit(x, "\\s+")))) <= 2)
        orientation <- if (isTRUE(all_short)) "v" else "h"
      }

      # Hovertemplate
      hovertemplate <- if (identical(orientation, "h")) {
        "%{y}<br>%{customdata}<extra></extra>"
      } else {
        "%{x}<br>%{customdata}<extra></extra>"
      }

      # Hover label padrão
      hl <- list(bgcolor = "white",
                 bordercolor = "#0A1E3C",
                 font = list(color = "black", size = 14, family = "Arial Black"))

      # Ticks numéricos (quando não for %)
      if (!is_percentage) {
        numeric_vals <- suppressWarnings(as.numeric(data[[var_numeric]]))
        rng <- range(c(0, numeric_vals), na.rm = TRUE)
        if (!is.finite(rng[1]) || !is.finite(rng[2])) rng <- c(0, 0)
        tick_vals_numeric <- pretty(rng)
        tick_text_numeric <- sapply(tick_vals_numeric, format_integer)
      }

      if (identical(orientation, "h")) {
        xaxis_config <- list(
          title = list(text = wrap_vertical_title(var_numeric), standoff = 0L),
          tickfont = list(size = 12, color = "#000000")
        )
        if (is_percentage) {
          xaxis_config$tickformat <- ".1f"
          xaxis_config <- c(xaxis_config, list(range = c(0, 100), dtick = 20))
        } else {
          xaxis_config$tickvals <- tick_vals_numeric
          xaxis_config$ticktext <- tick_text_numeric
        }

        p <- plotly::plot_ly(
          data = data,
          x = as.formula(paste0("~`", var_numeric, "`")),
          y = as.formula(paste0("~`", var_category, "`")),
          type = "bar",
          orientation = "h",
          marker = list(color = "#0A1E3C"),
          customdata = data$formatted_value,
          hovertemplate = hovertemplate
        )
        p <- plotly::layout(
          p,
          xaxis = xaxis_config,
          yaxis = list(
            title = list(text = display_category, standoff = 0L),
            tickfont = list(size = 12, color = "#000000"),
            categoryorder = "category ascending",
            autorange = "reversed"
          ),
          hoverlabel = hl
        )
      } else {
        original_categories <- data[[var_category]]
        categories <- ifelse(
          grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
          sub("\\s+", "<br>", original_categories),
          gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
        )

        yaxis_config <- list(
          title = list(text = wrap_after_second(var_numeric, threshold = 19), standoff = 20L, size = 1),
          tickfont = list(size = 12, color = "#000000")
        )
        if (is_percentage) {
          yaxis_config$tickformat <- ".1f"
          yaxis_config <- c(yaxis_config, list(range = c(0, 100), dtick = 20))
        } else {
          yaxis_config$tickvals <- tick_vals_numeric
          yaxis_config$ticktext <- tick_text_numeric
        }

        p <- plotly::plot_ly(
          data = data,
          x = as.formula(paste0("~`", var_category, "`")),
          y = as.formula(paste0("~`", var_numeric, "`")),
          type = "bar",
          marker = list(color = "#0A1E3C"),
          customdata = data$formatted_value,
          hovertemplate = hovertemplate
        )
        p <- plotly::layout(
          p,
          xaxis = list(
            title = list(text = display_category, standoff = 20L),
            tickmode = "array",
            tickvals = original_categories,
            ticktext = categories,
            tickangle = 90,
            automargin = TRUE,
            tickfont = list(size = 12, color = "#000000")
          ),
          yaxis = yaxis_config,
          margin = list(b = 90),
          hoverlabel = hl
        )
      }
      p
    }

    # Formata o símbolo de separador decimal e milhar nas caixinhas de totais
    format_number <- function(x) {
      format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }


    # Dados base
    tabela_APS <- data_list$tabela_APS

    # Atualiza o filtro secundário conforme o nível selecionado
    output$secondary_filter_ui <- renderUI({
      if (input$nivel_selection == "ESTADUAL") {
        return(NULL)
      } else {
        if (input$nivel_selection == "DRS") {
          if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
            label_text <- "Selecione a coordenadoria de saúde:"
          } else {
            label_text <- "Selecione a DRS:"
          }
        } else if (input$nivel_selection == "RRAS") {
          label_text <- "Selecione a RRAS:"
        } else if (input$nivel_selection == "REGIÃO DE SAÚDE") {
          label_text <- "Selecione a região de saúde:"
        } else if (input$nivel_selection == "MUNICIPAL") {
          if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
            label_text <- "Selecione a supervisão de saúde:"
          } else {
            label_text <- "Selecione o município:"
          }
        } else {
          label_text <- "Selecione:"
        }
        shinyWidgets::pickerInput(
          inputId = ns("secondary_filter"),
          label = label_text,
          choices = NULL,
          options = list("live-search" = TRUE)
        )
      }
    })

    # Atualiza as escolhas do filtro secundário
    observeEvent(list(input$nivel_selection, input$analisar_sp, input$analisar_muni_sp), {
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "RRAS") {
        safe_update_picker("secondary_filter", tabela_APS$RRAS)

      } else if (level == "DRS") {
        if (identical(input$analisar_sp, "SIM")) {
          safe_update_picker("secondary_filter", tabela_APS$`COORDENADORIA DE SAÚDE`)
        } else {
          safe_update_picker("secondary_filter", tabela_APS$DRS)
        }

      } else if (level == "REGIÃO DE SAÚDE") {
        safe_update_picker("secondary_filter", tabela_APS$`REGIÃO DE SAÚDE`)

      } else if (level == "MUNICIPAL") {
        if (identical(input$analisar_muni_sp, "SIM")) {
          safe_update_picker("secondary_filter", tabela_APS$`SUPERVISÃO DE SAÚDE`)
        } else {
          safe_update_picker("secondary_filter", tabela_APS$MUNICIPAL)
        }

      } else {
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = NULL, selected = NULL)
      }
    }, ignoreInit = TRUE)

    # Filtra dados conforme o nível e o filtro secundário
    filtered_data <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "ESTADUAL") {
        return(tabela_APS)
      }

      if (level == "RRAS") {
        if (!valid_choice(input$secondary_filter)) return(tabela_APS[0, , drop = FALSE])
        return(dplyr::filter(tabela_APS, .data$RRAS == input$secondary_filter))
      }

      if (level == "DRS") {
        if (!valid_choice(input$secondary_filter)) return(tabela_APS[0, , drop = FALSE])
        if (identical(input$analisar_sp, "SIM")) {
          return(dplyr::filter(tabela_APS, .data$`COORDENADORIA DE SAÚDE` == input$secondary_filter))
        } else {
          return(dplyr::filter(tabela_APS, .data$DRS == input$secondary_filter))
        }
      }

      if (level == "REGIÃO DE SAÚDE") {
        if (!valid_choice(input$secondary_filter)) return(tabela_APS[0, , drop = FALSE])
        return(dplyr::filter(tabela_APS, .data$`REGIÃO DE SAÚDE` == input$secondary_filter))
      }

      if (level == "MUNICIPAL") {
        if (!valid_choice(input$secondary_filter)) return(tabela_APS[0, , drop = FALSE])
        if (identical(input$analisar_muni_sp, "SIM")) {
          return(dplyr::filter(tabela_APS, .data$`SUPERVISÃO DE SAÚDE` == input$secondary_filter))
        } else {
          return(dplyr::filter(tabela_APS, .data$MUNICIPAL == input$secondary_filter))
        }
      }

      tabela_APS[0, , drop = FALSE]
    })

    # # Dados para gráficos: se ESTADUAL, agregação por RRAS; caso contrário, usa os dados filtrados
    # plot_data <- reactive({
    #   if (input$nivel_selection == "ESTADUAL") {
    #     aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
    #                     `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
    #                     `Nº DE UBS`,
    #                     `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`
    #                     # `COBERTURA ANS %`,
    #                     # `COBERTURA ESF %`,
    #                     # `COBERTURA AB %`
    #                     ) ~ RRAS,
    #               data = tabela_APS, FUN = sum, na.rm = TRUE)
    #   } else {
    #     filtered_data()
    #   }
    # })

    # Plot geral sem dor de cabeça com SP (com dplyr e tolerância a 0 linhas)
    plot_data <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection
      cols_sum <- c("Nº DE NASCIDOS VIVOS",
                    "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
                    "Nº DE UBS",
                    "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO",
                    "COBERTURA ANS %",
                    "COBERTURA ESF %",
                    "COBERTURA AB %")

      if (level == "ESTADUAL") {
        if (!all(c("RRAS", cols_sum) %in% names(tabela_APS))) return(tabela_APS[0, , drop = FALSE])
        return(
          dplyr::as_tibble(tabela_APS) |>
            dplyr::group_by(.data$RRAS) |>
            dplyr::summarise(dplyr::across(dplyr::all_of(cols_sum), ~ sum(as.numeric(.), na.rm = TRUE)),
                             .groups = "drop") |>
            as.data.frame()
        )
      }

      if (level == "DRS" && !identical(input$analisar_sp, "SIM")) {
        if (!valid_choice(input$secondary_filter)) return(tabela_APS[0, , drop = FALSE])
        df <- dplyr::filter(tabela_APS, .data$DRS == input$secondary_filter)
        if (!all(c("MUNICIPAL", cols_sum) %in% names(df))) return(df[0, , drop = FALSE])
        return(
          dplyr::as_tibble(df) |>
            dplyr::group_by(.data$MUNICIPAL) |>
            dplyr::summarise(dplyr::across(dplyr::all_of(cols_sum), ~ sum(as.numeric(.), na.rm = TRUE)),
                             .groups = "drop") |>
            as.data.frame()
        )
      }

      # Demais níveis usam o filtro pronto
      filtered_data()
    })

    # Versão robusta que injeta "SÃO PAULO" na DRS Grande SP quando necessário
    plot_data_main <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "DRS" && !identical(input$analisar_sp, "SIM")) {
        if (!valid_choice(input$secondary_filter)) return(tabela_APS[0, , drop = FALSE])
        df <- dplyr::filter(tabela_APS, .data$DRS == input$secondary_filter)

        cols_sum <- c("Nº DE NASCIDOS VIVOS",
                      "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
                      "Nº DE UBS",
                      "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO",
                      "COBERTURA ANS %",
                      "COBERTURA ESF %",
                      "COBERTURA AB %")

        if (nrow(df) == 0L || !all(c("MUNICIPAL", cols_sum) %in% names(df))) {
          return(df[0, , drop = FALSE])
        }

        agg <- dplyr::as_tibble(df) |>
          dplyr::group_by(.data$MUNICIPAL) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(cols_sum), ~ sum(as.numeric(.), na.rm = TRUE)),
                           .groups = "drop") |>
          as.data.frame()

        if (identical(input$secondary_filter, "GRANDE SÃO PAULO") && !("SÃO PAULO" %in% agg$MUNICIPAL)) {
          df2 <- dplyr::filter(df, .data$MUNICIPAL == "SÃO PAULO")
          total_sp <- data_list$total_sp
          total_sp$`COBERTURA ANS %` <- as.numeric(gsub(",", ".", total_sp$`COBERTURA ANS %`))
          total_sp$`COBERTURA AB %`  <- as.numeric(gsub(",", ".",  total_sp$`COBERTURA AB %`))

          sp <- data.frame(
            MUNICIPAL                                = "SÃO PAULO",
            `Nº DE NASCIDOS VIVOS`                   = sum(df2$`Nº DE NASCIDOS VIVOS`, na.rm = TRUE),
            `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO` = sum(df2$`NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`, na.rm = TRUE),
            `Nº DE UBS`                              = sum(df2$`Nº DE UBS`, na.rm = TRUE),
            `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO` = sum(df2$`GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`, na.rm = TRUE),
            `COBERTURA ANS %`                        = sum(total_sp$`COBERTURA ANS %`, na.rm = TRUE),
            `COBERTURA ESF %`                        = sum(df$`COBERTURA ESF %`, na.rm = TRUE),
            `COBERTURA AB %`                         = sum(total_sp$`COBERTURA AB %`,  na.rm = TRUE),
            check.names = FALSE
          )
          agg <- rbind(agg, sp)
        }
        return(agg)
      }

      if (level == "ESTADUAL") {
        return(plot_data())
      }

      filtered_data()
    })

    # Plot para não considerar SP (casos específicos como cobertura ESF)
    plot_data_cov <- reactive({
      df <- plot_data_main()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
      # Se a DRS selecionada for GRANDE SÃO PAULO, remove SP apenas quando houver coluna MUNICIPAL
      if (input$nivel_selection == "DRS" &&
          identical(input$secondary_filter, "GRANDE SÃO PAULO") &&
          "MUNICIPAL" %in% names(df)) {
        df <- dplyr::filter(df, MUNICIPAL != "SÃO PAULO")
      }
      df
    })

    # Dados para tabelas de AAE
    filtered_data_aae <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "RRAS") {
        req(isTruthy(input$secondary_filter))
        switch(
          input$secondary_filter,
          "RRAS 1"  = data_list$tabela_1_APS_AAE,
          "RRAS 2"  = data_list$tabela_2_APS_AAE,
          "RRAS 3"  = data_list$tabela_3_APS_AAE,
          "RRAS 4"  = data_list$tabela_4_APS_AAE,
          "RRAS 5"  = data_list$tabela_5_APS_AAE,
          "RRAS 6"  = data_list$tabela_6_APS_AAE,
          "RRAS 7"  = data_list$tabela_7_APS_AAE,
          "RRAS 8"  = data_list$tabela_8_APS_AAE,
          "RRAS 9"  = data_list$tabela_9_APS_AAE,
          "RRAS 10" = data_list$tabela_10_APS_AAE,
          "RRAS 11" = data_list$tabela_11_APS_AAE,
          "RRAS 12" = data_list$tabela_12_APS_AAE,
          "RRAS 13" = data_list$tabela_13_APS_AAE,
          "RRAS 14" = data_list$tabela_14_APS_AAE,
          "RRAS 15" = data_list$tabela_15_APS_AAE,
          "RRAS 16" = data_list$tabela_16_APS_AAE,
          "RRAS 17" = data_list$tabela_17_APS_AAE,
          "RRAS 18" = data_list$tabela_18_APS_AAE
        )
      } else {
        table_aae_all <- dplyr::bind_rows(
          data_list$tabela_1_APS_AAE,  data_list$tabela_2_APS_AAE,
          data_list$tabela_3_APS_AAE,  data_list$tabela_4_APS_AAE,
          data_list$tabela_5_APS_AAE,  data_list$tabela_6_APS_AAE,
          data_list$tabela_7_APS_AAE,  data_list$tabela_8_APS_AAE,
          data_list$tabela_9_APS_AAE,  data_list$tabela_10_APS_AAE,
          data_list$tabela_11_APS_AAE, data_list$tabela_12_APS_AAE,
          data_list$tabela_13_APS_AAE, data_list$tabela_14_APS_AAE,
          data_list$tabela_15_APS_AAE, data_list$tabela_16_APS_AAE,
          data_list$tabela_17_APS_AAE, data_list$tabela_18_APS_AAE
        )

        if (level == "ESTADUAL") {
          return(table_aae_all)
        }

        req(isTruthy(input$secondary_filter))

        if (level == "DRS") {
          if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
            # Preferência: filtrar pela própria coluna se ela existir;
            # fallback: mapear municípios da coordenadoria via tabela_APS
            if ("COORDENADORIA DE SAÚDE" %in% names(table_aae_all)) {
              dados <- table_aae_all[table_aae_all$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
            } else {
              munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`COORDENADORIA DE SAÚDE` == input$secondary_filter])
              dados <- table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` %in% munis, ]
            }
          } else {
            dados <- table_aae_all[table_aae_all$DRS == input$secondary_filter, ]
          }
          dados <- dados |>
            dplyr::rename(`MUNICÍPIO DA DRS` = `MUNICÍPIO DA RRAS`) |>
            dplyr::filter(!if_all(dplyr::everything(), is.na))
          return(dados)
        }

        if (level == "REGIÃO DE SAÚDE") {
          dados <- table_aae_all[table_aae_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ] |>
            dplyr::rename(`MUNICÍPIO DA REGIÃO DE SAÚDE` = `MUNICÍPIO DA RRAS`)
          return(dados)
        }

        if (level == "MUNICIPAL") {
          if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
            # secondary_filter é uma SUPERVISÃO; mapeia p/ municípios e filtra
            munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`SUPERVISÃO DE SAÚDE` == input$secondary_filter])
            dados <- table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` %in% munis, ]
          } else {
            dados <- table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
          }
          return(dados)
        }
      }
    })


    # Dados para tabelas de BAIXO RISCO
    filtered_data_bxr <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "RRAS") {
        req(isTruthy(input$secondary_filter))
        switch(
          input$secondary_filter,
          "RRAS 1"  = data_list$tabela_1_APS_BXRISCO,
          "RRAS 2"  = data_list$tabela_2_APS_BXRISCO,
          "RRAS 3"  = data_list$tabela_3_APS_BXRISCO,
          "RRAS 4"  = data_list$tabela_4_APS_BXRISCO,
          "RRAS 5"  = data_list$tabela_5_APS_BXRISCO,
          "RRAS 6"  = data_list$tabela_6_APS_BXRISCO,
          "RRAS 7"  = data_list$tabela_7_APS_BXRISCO,
          "RRAS 8"  = data_list$tabela_8_APS_BXRISCO,
          "RRAS 9"  = data_list$tabela_9_APS_BXRISCO,
          "RRAS 10" = data_list$tabela_10_APS_BXRISCO,
          "RRAS 11" = data_list$tabela_11_APS_BXRISCO,
          "RRAS 12" = data_list$tabela_12_APS_BXRISCO,
          "RRAS 13" = data_list$tabela_13_APS_BXRISCO,
          "RRAS 14" = data_list$tabela_14_APS_BXRISCO,
          "RRAS 15" = data_list$tabela_15_APS_BXRISCO,
          "RRAS 16" = data_list$tabela_16_APS_BXRISCO,
          "RRAS 17" = data_list$tabela_17_APS_BXRISCO,
          "RRAS 18" = data_list$tabela_18_APS_BXRISCO
        )
      } else {
        table_bxr_all <- dplyr::bind_rows(
          data_list$tabela_1_APS_BXRISCO,  data_list$tabela_2_APS_BXRISCO,
          data_list$tabela_3_APS_BXRISCO,  data_list$tabela_4_APS_BXRISCO,
          data_list$tabela_5_APS_BXRISCO,  data_list$tabela_6_APS_BXRISCO,
          data_list$tabela_7_APS_BXRISCO,  data_list$tabela_8_APS_BXRISCO,
          data_list$tabela_9_APS_BXRISCO,  data_list$tabela_10_APS_BXRISCO,
          data_list$tabela_11_APS_BXRISCO, data_list$tabela_12_APS_BXRISCO,
          data_list$tabela_13_APS_BXRISCO, data_list$tabela_14_APS_BXRISCO,
          data_list$tabela_15_APS_BXRISCO, data_list$tabela_16_APS_BXRISCO,
          data_list$tabela_17_APS_BXRISCO, data_list$tabela_18_APS_BXRISCO
        )

        if (level == "ESTADUAL") {
          return(table_bxr_all)
        }

        req(isTruthy(input$secondary_filter))

        if (level == "DRS") {
          if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
            if ("COORDENADORIA DE SAÚDE" %in% names(table_bxr_all)) {
              dados <- table_bxr_all[table_bxr_all$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
            } else {
              munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`COORDENADORIA DE SAÚDE` == input$secondary_filter])
              dados <- table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` %in% munis, ]
            }
          } else {
            dados <- table_bxr_all[table_bxr_all$DRS == input$secondary_filter, ]
          }
          dados <- dados |>
            dplyr::rename(`MUNICÍPIO DA DRS` = `MUNICÍPIO DA RRAS`) |>
            dplyr::filter(!if_all(dplyr::everything(), is.na))
          return(dados)
        }

        if (level == "REGIÃO DE SAÚDE") {
          dados <- table_bxr_all[table_bxr_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ] |>
            dplyr::rename(`MUNICÍPIO DA REGIÃO DE SAÚDE` = `MUNICÍPIO DA RRAS`)
          return(dados)
        }

        if (level == "MUNICIPAL") {
          if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
            munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`SUPERVISÃO DE SAÚDE` == input$secondary_filter])
            dados <- table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` %in% munis, ]
          } else {
            dados <- table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
          }
          return(dados)
        }
      }
    })

    # Renderiza as tabelas
    output$table_aae <- DT::renderDT({
      data <- filtered_data_aae()
      validate(
        need(!is.null(data) && ncol(data) > 0, "Dados não disponíveis para exibição")
      )
      DT::datatable(
        data,
        options = list(
          pageLength = -1,
          autoWidth  = TRUE,
          scrollX    = TRUE,
          scrollY    = "400px",
          scrollCollapse = TRUE,
          paging     = FALSE,
          dom        = 't',
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = '10%', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover nowrap"
      ) |> DT::formatStyle(
        columns = names(filtered_data_aae()),
        `padding-left` = '0px',
        `padding-right` = '0px'
      )
    })

    output$table_bxr <- DT::renderDT({
      data <- filtered_data_bxr()
      validate(
        need(!is.null(data) && ncol(data) > 0, "Dados não disponíveis para exibição")
      )
      DT::datatable(
        data,
        options = list(
          pageLength = -1,
          autoWidth  = TRUE,
          scrollX    = TRUE,
          scrollY    = "400px",
          scrollCollapse = TRUE,
          paging     = FALSE,
          dom        = 't',
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(width = '10%', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover nowrap"
      ) |> DT::formatStyle(
        columns = names(filtered_data_bxr()),
        `padding-left` = '0px',
        `padding-right` = '0px'
      )
    })

    # Caixas resumo principais
    output$summary_box_1 <- renderUI({
      total_nascidos <- round(sum(filtered_data()[["Nº DE NASCIDOS VIVOS"]], na.rm = TRUE))
      # Condicional: se o nível for MUNICIPAL, define o caption; senão, fica NULL
      caption <- if (!is.null(input$nivel_selection) && input$nivel_selection == "MUNICIPAL") {
        tags$div(
          "Ano de atualização dos dados: 2023",
          style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
        )
      } else {
        NULL
      }
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nascidos vivos"),
        h3(format_number(total_nascidos)),
        caption
      )
    })

    output$summary_box_2 <- renderUI({
      total_sus_nasc <- sum(filtered_data()[["NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO"]], na.rm = TRUE)
      # Condicional: se o nível for MUNICIPAL, define o caption; senão, fica NULL
      caption <- if (!is.null(input$nivel_selection) && input$nivel_selection == "MUNICIPAL") {
        tags$div(
          "Ano de atualização dos dados: 2023",
          style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
        )
      } else {
        NULL
      }
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nascidos vivos SUSdependentes estimados/ano"),
        h3(format_number(round(total_sus_nasc, 0))),
        caption
      )
    })

    output$summary_box_3 <- renderUI({
      total_ubs <- sum(filtered_data()[["Nº DE UBS"]], na.rm = TRUE)
      # Condicional: se o nível for MUNICIPAL, define o caption; senão, fica NULL
      caption <- if (!is.null(input$nivel_selection) && input$nivel_selection == "MUNICIPAL") {
        tags$div(
          "Ano de atualização dos dados: 2022/2023",
          style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
        )
      } else {
        NULL
      }
      div(
        class = "custom-box box-danger",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nº de UBS"),
        h3(format_number(total_ubs)),
        caption
      )
    })

    output$summary_box_4 <- renderUI({
      total_gestantes <- sum(filtered_data()[["GESTANTES SUSDEPENDENTES ESTIMADAS/ANO"]], na.rm = TRUE)
      # Condicional: se o nível for MUNICIPAL, define o caption; senão, fica NULL
      caption <- if (!is.null(input$nivel_selection) && input$nivel_selection == "MUNICIPAL") {
        tags$div(
          "Ano de atualização dos dados: 2023",
          style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
        )
      } else {
        NULL
      }
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Gestantes SUSdependentes estimadas/ano"),
        h3(format_number(round(total_gestantes, 0))),
        caption
      )
    })

    output$municipal_extras <- renderUI({
      if(input$nivel_selection != "MUNICIPAL") return(NULL)

      # Condição: se o usuário selecionou supervisão (analisar_muni_sp = "SIM")
      # OU se o filtro secundário for "SÃO PAULO", então não exibe o box de Cobertura ESF.
      if((!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") ||
         (!is.null(input$secondary_filter) && input$secondary_filter == "SÃO PAULO")) {
        fluidRow(
          column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
          column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
        )
      } else {
        fluidRow(
          column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
          column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_2")))),
          column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
        )
      }
    })

    # Caixas resumo extras para nível MUNICIPAL
    output$extra_summary_box_1 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!is.null(input$secondary_filter) && input$secondary_filter == "SÃO PAULO") {
        total_sp <- data_list$total_sp
        metric <- as.numeric(gsub(",", ".", total_sp$`COBERTURA ANS %`)) # virgula por ponto pra nao dar NA

        metric_fmt <- format(round(metric, 1), nsmall = 1, decimal.mark = ",") # sempre 1 casa decimal
        div(
          class = "custom-box box-primary",
          style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
          h4("Cobertura ANS (%)"),
          h3(format_number(metric_fmt)),
          tags$div(
            "Ano de atualização dos dados: 2023",
            style = "position: absolute; bottom: 1px; left: 10px; left: 0; right: 0;font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 6px; border-radius: 3px;"
          )
        )
      } else {
        data <- filtered_data()
        metric <- round(mean(data$`COBERTURA ANS %`, na.rm = TRUE), 1)
        metric_fmt <- format(round(metric, 1), nsmall = 1, decimal.mark = ",")
        div(
          class = "custom-box box-primary",
          style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
          h4("Cobertura ANS (%)"),
          h3(format_number(metric_fmt)),
          tags$div(
            "Ano de atualização dos dados: 2023",
            style = "position: absolute; bottom: 1px; left: 10px; left: 0; right: 0;font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 6px; border-radius: 3px;"
          )
        )
      }
    })

    # Box que originalmente mostra Cobertura ESF (%)
    output$extra_summary_box_2 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!is.null(input$secondary_filter) && input$secondary_filter == "SÃO PAULO") {
        return(NULL)
      }
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA ESF %`, na.rm = TRUE), 1)
      metric_fmt <- format(round(metric, 1), nsmall = 1, decimal.mark = ",")  # sempre 1 casa decimal
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura ESF (%)"),
        h3(format_number(metric_fmt)),
        tags$div(
          "Ano de atualização dos dados: 2020",
          style = "position: absolute; bottom: 1px; left: 10px; left: 0; right: 0;font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 6px; border-radius: 3px;"
        )
      )
    })

    # Box original de Cobertura AB (%)
    output$extra_summary_box_3 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!is.null(input$secondary_filter) && input$secondary_filter == "SÃO PAULO") {
        total_sp <- data_list$total_sp
        metric <- as.numeric(gsub(",", ".", total_sp$`COBERTURA AB %`))
        metric_fmt <- format(round(metric, 1), nsmall = 1, decimal.mark = ",")  # sempre 1 casa decimal
        div(
          class = "custom-box box-warning",
          style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
          h4("Cobertura AB (%)"),
          h3(format_number(metric_fmt)),
          tags$div(
            "Ano de atualização dos dados: 2020",
            style = "position: absolute; bottom: 1px; left: 10px; left: 0; right: 0;font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 6px; border-radius: 3px;"
          )
        )
      } else {
        data <- filtered_data()
        metric <- round(mean(data$`COBERTURA AB %`, na.rm = TRUE), 1)
        metric_fmt <- format(round(metric, 1), nsmall = 1, decimal.mark = ",")
        div(
          class = "custom-box box-warning",
          style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
          h4("Cobertura AB (%)"),
          h3(format_number(metric_fmt)),
          tags$div(
            "Ano de atualização dos dados: 2020",
            style = "position: absolute; bottom: 1px; left: 10px; left: 0; right: 0;font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 6px; border-radius: 3px;"
          )
        )
      }
    })

    # Renderização dos cards com gráficos, usando a função auxiliar build_plot_card
    output$card_plot_nascidos_vivos <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Nascidos vivos", "plot_nascidos_vivos", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        build_plot_card("Nascidos vivos", "plot_nascidos_vivos", plot_data(), caption = "Ano: 2023")
      }
    })
    output$card_plot_ubs <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("UBS", "plot_ubs", plot_data(), caption = "Ano: 2022/2023", height_override = 400)
      } else {
        build_plot_card("UBS", "plot_ubs", plot_data(), caption = "Ano: 2022/2023")
      }
    })
    output$card_plot_gestantes_susdependentes <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", plot_data(), caption = "Ano: 2023")
      }
    })
    # Para nível ESTADUAL
    output$card_plot_nascidos_susdependentes_estadual <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection != "ESTADUAL") return(NULL)
      build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_estado", plot_data(), caption = "Ano: 2023")
    })

    # Para níveis RRAS (- RRAS 6), DRS ou REGIÃO DE SAÚDE
    output$card_plot_nascidos_susdependentes_outros <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_outros", plot_data(), caption = "Ano: 2023")
    })

    # Para níveis RRAS 6
    output$card_plot_nascidos_susdependentes_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_rras6", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_rras6", plot_data(), caption = "Ano: 2023")
      }
    })

    # NOVOS CARDS DE COBERTURA (para RRAS, DRS e REGIÃO DE SAÚDE)
    output$card_plot_cobertura_ans <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura ANS (%)", "plot_cobertura_ans", plot_data(), caption = "Ano: 2023")
    })
    output$card_plot_cobertura_esf <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura ESF (%)", "plot_cobertura_esf", plot_data(), caption = "Ano: 2020")
    })
    output$card_plot_cobertura_ab <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura AB (%)", "plot_cobertura_ab", plot_data(), caption = "Ano: 2020")
    })

    # NOVOS CARDS DE COBERTURA (para RRAS 6)
    output$card_plot_cobertura_ans_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Cobertura ANS (%)", "plot_cobertura_ans_rras6", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        build_plot_card("Cobertura ANS (%)", "plot_cobertura_ans_rras6", plot_data(), caption = "Ano: 2020")
      }
    })
    output$card_plot_cobertura_ab_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Cobertura AB (%)", "plot_cobertura_ab_rras6", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("Cobertura AB (%)", "plot_cobertura_ab_rras6", plot_data(), caption = "Ano: 2020")
      }
    })

    # Renderização dos gráficos utilizando a função auxiliar build_bar_plot
    output$plot_nascidos_vivos <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (input$nivel_selection == "MUNICIPAL") return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data_main(),
        var_numeric = "Nº DE NASCIDOS VIVOS",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    })

    output$plot_ubs <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (input$nivel_selection == "MUNICIPAL") return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data_main(),
        var_numeric = "Nº DE UBS",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    })

    output$plot_gestantes_susdependentes <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (input$nivel_selection == "MUNICIPAL") return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data_main(),
        var_numeric = "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    })

    output$plot_nascidos_susdependentes_estado <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (input$nivel_selection == "MUNICIPAL") return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data(),  # mantém sua fonte original
        var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    })

    output$plot_nascidos_susdependentes_outros <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)

      build_bar_plot(
        data         = plot_data_main(),
        var_numeric  = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    })

    # RRAS 6
    output$plot_nascidos_susdependentes_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)

      build_bar_plot(
        data         = plot_data(),  # mantém a mesma fonte de dados usada originalmente neste card
        var_numeric  = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    })

    # GRÁFICOS DE COBERTURA (para RRAS, DRS e REGIÃO DE SAÚDE)
    output$plot_cobertura_ans <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data_main(), var_numeric = "COBERTURA ANS %", var_category = "MUNICIPAL", is_percentage = TRUE)
    })

    output$plot_cobertura_esf <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data_cov(), var_numeric = "COBERTURA ESF %", var_category = "MUNICIPAL", is_percentage = TRUE)
    })

    output$plot_cobertura_ab <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data_main(), var_numeric = "COBERTURA AB %", var_category = "MUNICIPAL", is_percentage = TRUE)
    })

    # GRÁFICOS DE COBERTURA (para RRAS 6)
    output$plot_cobertura_ans_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)

      build_bar_plot(
        data           = plot_data(),   # preservado como no original deste card
        var_numeric    = "COBERTURA ANS %",
        var_category   = cfg$cat_var,
        is_percentage  = TRUE,
        force_vertical = cfg$force_v
      )
    })

    output$plot_cobertura_ab_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)

      build_bar_plot(
        data           = plot_data(),   # preservado como no original deste card
        var_numeric    = "COBERTURA AB %",
        var_category   = cfg$cat_var,
        is_percentage  = TRUE,
        force_vertical = cfg$force_v
      )
    })
  })
}
