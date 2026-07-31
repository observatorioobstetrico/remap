# R/mod_rras_aps2_server.R
#' RRAS APS 2 Server
#'
#' @param id Module id
#' @param data_list Lista com os dados carregados em load_data()
#' @importFrom magrittr %>%
#' @import dplyr
#'
#' @export
mod_rras_aps2_server <- function(id, data_list) {
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
    calc_multiyear_grouped_height <- function(n_groups, visible_groups = 20L, bars_per_group = 1L) {
      visible_groups <- max(1L, as.integer(visible_groups))
      n_groups <- max(0L, as.integer(n_groups))
      bars_per_group <- max(1L, as.integer(bars_per_group))
      per_group_height <- if (bars_per_group > 1L) {
        max(124L, 28L + (bars_per_group * 16L))
      } else {
        28L
      }

      full_height <- 110 + (n_groups * per_group_height)
      visible_height <- 110 + (min(n_groups, visible_groups) * per_group_height)

      list(
        full_height = max(320L, full_height),
        visible_height = max(320L, visible_height)
      )
    }

    build_plot_card <- function(card_title,
                                plot_output_id,
                                data_to_plot,
                                caption = NULL,
                                height_override = NULL,
                                scroll_max_height = NULL,
                                fixed_axis = NULL) {
      # APS 2 usa linhas sobrepostas; a altura nao deve escalar pelo numero
      # de municipios/supervisoes/RRAS como nos graficos de barras horizontais.
      height_val <- 420L
      plot_tag <- plotly::plotlyOutput(ns(plot_output_id), height = paste0(height_val, "px"))

      bs4Dash::bs4Card(
        title  = card_title,
        height = "100%",
        width = NULL,
        tagList(
          plot_tag,
          if (!is.null(caption)) {
            tags$div(caption, style = "margin-top: 15px; font-size: 12px; color: #555;")
          }
        )
      )
    }

    compute_axis_spec <- function(data, var_numeric, is_percentage = FALSE) {
      if (isTRUE(is_percentage)) {
        return(list(
          range = c(0, 100),
          tickvals = c(0, 25, 50, 75, 100),
          ticktext = c("0%", "25%", "50%", "75%", "100%")
        ))
      }

      numeric_vals <- suppressWarnings(as.numeric(data[[var_numeric]]))
      rng <- range(c(0, numeric_vals), na.rm = TRUE)
      if (!is.finite(rng[1]) || !is.finite(rng[2])) {
        rng <- c(0, 0)
      }
      tick_vals_numeric <- pretty(rng)
      tick_vals_numeric <- unique(c(0, tick_vals_numeric[tick_vals_numeric > 0]))
      tick_vals_numeric <- tick_vals_numeric[tick_vals_numeric <= max(tick_vals_numeric, rng[2], na.rm = TRUE)]

      list(
        range = c(min(tick_vals_numeric, na.rm = TRUE), max(tick_vals_numeric, na.rm = TRUE)),
        tickvals = tick_vals_numeric,
        ticktext = vapply(tick_vals_numeric, format_integer, character(1))
      )
    }

    build_fixed_axis_legend <- function(axis_spec) {
      axis_min <- axis_spec$range[1]
      axis_max <- axis_spec$range[2]
      axis_span <- if ((axis_max - axis_min) == 0) 1 else (axis_max - axis_min)

      tick_nodes <- Map(
        function(value, label) {
          position_pct <- ((value - axis_min) / axis_span) * 100
          tags$div(
            style = paste0(
              "position:absolute; left:", sprintf("%.4f", position_pct), "%; bottom:0; transform:translateX(-50%);"
            ),
            tags$div(
              style = "width:1px; height:8px; background-color:#7F8A99; margin:0 auto 2px auto;"
            ),
            tags$div(
              label,
              style = "font-size:11px; color:#000000; transform:rotate(90deg); transform-origin:center; white-space:nowrap;"
            )
          )
        },
        axis_spec$tickvals,
        axis_spec$ticktext
      )

      tags$div(
        style = "position:relative; height:34px; margin-top:6px; margin-left:2px; margin-right:14px;",
        tags$div(
          style = "position:absolute; left:0; right:0; top:0; border-top:1px solid #C3CBD5;"
        ),
        tick_nodes
      )
    }

    format_year_sequence <- function(years) {
      years <- sort(unique(stats::na.omit(as.integer(years))))
      if (!length(years)) {
        return("")
      }
      if (length(years) > 1L && identical(years, seq.int(min(years), max(years)))) {
        return(paste0(min(years), "-", max(years)))
      }
      if (length(years) == 1L) {
        return(as.character(years))
      }
      paste0(
        paste(head(years, -1L), collapse = ", "),
        " e ",
        tail(years, 1L)
      )
    }

    cobertura_year_color <- function(year) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }
      if (year %in% cobertura_ab_legacy_year) {
        return("#6e7a8c")
      }
      if (year %in% cobertura_preliminary_year) {
        return("#0a1e3c")
      }

      non_legacy_years <- cobertura_display_years[cobertura_display_years != cobertura_ab_legacy_year]
      non_legacy_years <- sort(unique(stats::na.omit(as.integer(non_legacy_years))))
      if (!length(non_legacy_years)) {
        return("#0a1e3c")
      }

      palette <- grDevices::colorRampPalette(c("#bfe7ff", "#32a0ff", "#0a1e3c"))(length(non_legacy_years))
      year_index <- match(year, non_legacy_years)
      if (is.na(year_index)) {
        return("#0a1e3c")
      }

      palette[[year_index]]
    }

    build_multiyear_caption_legend <- function(legacy_year, consolidated_years, preliminary_year) {
      legend_item <- function(color, text) {
        tags$span(
          style = "display:inline-flex; align-items:center; gap:6px; white-space:nowrap;",
          tags$span(
            style = paste0(
              "display:inline-block; width:10px; height:10px; border-radius:2px; background-color:",
              color,
              "; border:1px solid ",
              color,
              ";"
            )
          ),
          tags$span(text)
        )
      }

      legend_row <- function(label, items) {
        if (inherits(items, "shiny.tag")) {
          items <- list(items)
        }

        do.call(
          tags$div,
          c(
            list(
              style = "display:flex; flex-wrap:wrap; align-items:center; gap:8px; line-height:1.4;",
              tags$span(label, style = "min-width:92px; color:#555; font-weight:600;")
            ),
            items
          )
        )
      }

      consolidated_years <- sort(unique(stats::na.omit(as.integer(consolidated_years))))

      tags$div(
        style = "display:flex; flex-direction:column; gap:4px;",
        legend_row("Histórico:", legend_item(cobertura_year_color(legacy_year), as.character(legacy_year))),
        legend_row("Consolidados:", lapply(consolidated_years, function(year) {
          legend_item(cobertura_year_color(year), as.character(year))
        })),
        legend_row("Preliminar:", legend_item(cobertura_year_color(preliminary_year), as.character(preliminary_year)))
      )
    }

    nascidos_year_color <- function(year) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }
      if (year %in% nascidos_preliminary_year) {
        return("#0a1e3c")
      }

      all_years <- sort(unique(stats::na.omit(as.integer(c(nascidos_consolidated_years, nascidos_preliminary_year)))))
      if (!length(all_years)) {
        return("#0a1e3c")
      }

      palette <- grDevices::colorRampPalette(c("#bfe7ff", "#32a0ff", "#0a1e3c"))(length(all_years))
      year_index <- match(year, all_years)
      if (is.na(year_index)) {
        return("#0a1e3c")
      }

      palette[[year_index]]
    }

    build_nascidos_caption_legend <- function(consolidated_years,
                                              preliminary_year = NA_integer_,
                                              year_color = nascidos_year_color) {
      legend_item <- function(color, text) {
        tags$span(
          style = "display:inline-flex; align-items:center; gap:6px; white-space:nowrap; flex:0 0 auto;",
          tags$span(
            style = paste0(
              "display:inline-block; width:10px; height:10px; border-radius:2px; background-color:",
              color,
              "; border:1px solid ",
              color,
              ";"
            )
          ),
          tags$span(text)
        )
      }

      legend_row <- function(label, items) {
        if (inherits(items, "shiny.tag")) {
          items <- list(items)
        }

        tags$div(
          style = "display:grid; grid-template-columns:92px minmax(0, 1fr); column-gap:8px; align-items:flex-start; line-height:1.4;",
          tags$span(label, style = "color:#555; font-weight:600;"),
          do.call(
            tags$div,
            c(
              list(style = "display:flex; flex-wrap:wrap; align-items:center; gap:8px; min-width:0;"),
              items
            )
          )
        )
      }

      consolidated_years <- sort(unique(stats::na.omit(as.integer(consolidated_years))))
      rows <- list(
        legend_row("Consolidados:", lapply(consolidated_years, function(year) {
          legend_item(year_color(year), as.character(year))
        }))
      )

      if (length(preliminary_year) == 1L && !is.na(preliminary_year)) {
        rows <- c(rows, list(
          legend_row("Preliminar:", legend_item(year_color(preliminary_year), as.character(preliminary_year)))
        ))
      }

      do.call(
        tags$div,
        c(list(style = "display:flex; flex-direction:column; gap:4px;"), rows)
      )
    }

    cobertura_ans_year_color <- function(year) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }
      if (year %in% cobertura_ans_preliminary_year) {
        return("#0a1e3c")
      }

      all_years <- sort(unique(stats::na.omit(as.integer(c(cobertura_ans_consolidated_years, cobertura_ans_preliminary_year)))))
      if (!length(all_years)) {
        return("#0a1e3c")
      }

      palette <- grDevices::colorRampPalette(c("#bfe7ff", "#32a0ff", "#0a1e3c"))(length(all_years))
      year_index <- match(year, all_years)
      if (is.na(year_index)) {
        return("#0a1e3c")
      }

      palette[[year_index]]
    }

    build_cobertura_ans_caption_legend <- function(consolidated_years, preliminary_year = NA_integer_) {
      legend_item <- function(color, text) {
        tags$span(
          style = "display:inline-flex; align-items:center; gap:6px; white-space:nowrap; flex:0 0 auto;",
          tags$span(
            style = paste0(
              "display:inline-block; width:10px; height:10px; border-radius:2px; background-color:",
              color,
              "; border:1px solid ",
              color,
              ";"
            )
          ),
          tags$span(text)
        )
      }

      legend_row <- function(label, items) {
        if (inherits(items, "shiny.tag")) {
          items <- list(items)
        }

        tags$div(
          style = "display:grid; grid-template-columns:92px minmax(0, 1fr); column-gap:8px; align-items:flex-start; line-height:1.4;",
          tags$span(label, style = "color:#555; font-weight:600;"),
          do.call(
            tags$div,
            c(
              list(style = "display:flex; flex-wrap:wrap; align-items:center; gap:8px; min-width:0;"),
              items
            )
          )
        )
      }

      consolidated_years <- sort(unique(stats::na.omit(as.integer(consolidated_years))))
      rows <- list(
        legend_row("Consolidados:", lapply(consolidated_years, function(year) {
          legend_item(cobertura_ans_year_color(year), as.character(year))
        }))
      )

      if (length(preliminary_year) == 1L && !is.na(preliminary_year)) {
        rows <- c(rows, list(
          legend_row("Preliminar:", legend_item(cobertura_ans_year_color(preliminary_year), as.character(preliminary_year)))
        ))
      }

      do.call(
        tags$div,
        c(list(style = "display:flex; flex-direction:column; gap:4px;"), rows)
      )
    }

    hide_aps_modebar <- function(p) {
      if (is.null(p)) {
        return(NULL)
      }

      plotly::config(
        p,
        displayModeBar = FALSE,
        displaylogo = FALSE
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

    empty_line_plot <- function(message = "Sem dados disponiveis") {
      hide_aps_modebar(
        plotly::plot_ly(
          x = 0, y = 0,
          type = "scatter", mode = "markers",
          opacity = 0, hoverinfo = "skip", showlegend = FALSE
        ) |>
          plotly::layout(
            annotations = list(
              text = message,
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(size = 14)
            ),
            xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
            margin = list(l = 20, r = 20, t = 20, b = 20)
          )
      )
    }

    format_line_value <- function(x, is_percentage = FALSE) {
      ifelse(
        is.na(x),
        "-",
        if (isTRUE(is_percentage)) {
          paste0(format(round(x, 1), nsmall = 1, big.mark = ".", decimal.mark = ","), "%")
        } else {
          format_integer(round(x))
        }
      )
    }

    collapse_line_value <- function(x, is_percentage = FALSE) {
      x <- suppressWarnings(as.numeric(x))
      if (!length(x) || !any(!is.na(x))) {
        return(NA_real_)
      }
      if (isTRUE(is_percentage)) {
        mean(x, na.rm = TRUE)
      } else {
        sum(x, na.rm = TRUE)
      }
    }

    build_multiyear_line_plot <- function(data,
                                          years,
                                          category_col,
                                          metric_title,
                                          is_percentage = FALSE) {
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
          !(category_col %in% names(data))) {
        return(empty_line_plot())
      }

      years <- sort(unique(stats::na.omit(as.integer(years))))
      if (!length(years)) {
        return(empty_line_plot())
      }

      value_cols <- paste0("valor_", years)
      for (col in value_cols) {
        if (!(col %in% names(data))) {
          data[[col]] <- NA_real_
        }
        data[[col]] <- suppressWarnings(as.numeric(data[[col]]))
      }

      plot_data <- data.frame(
        categoria = as.character(data[[category_col]]),
        data[, value_cols, drop = FALSE],
        check.names = FALSE
      )
      plot_data <- plot_data[!is.na(plot_data$categoria) & nzchar(plot_data$categoria), , drop = FALSE]
      if (!nrow(plot_data)) {
        return(empty_line_plot())
      }

      plot_data <- plot_data |>
        dplyr::group_by(.data$categoria) |>
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(value_cols),
            ~ collapse_line_value(.x, is_percentage = is_percentage)
          ),
          .groups = "drop"
        ) |>
        as.data.frame()

      sort_key <- iconv(toupper(as.character(plot_data$categoria)), from = "", to = "ASCII//TRANSLIT")
      sort_key[is.na(sort_key)] <- toupper(as.character(plot_data$categoria)[is.na(sort_key)])
      plot_data <- plot_data[order(sort_key, plot_data$categoria, na.last = TRUE), , drop = FALSE]

      n_series <- nrow(plot_data)
      if (!n_series) {
        return(empty_line_plot())
      }

      show_legend <- n_series <= 18L
      line_width <- if (n_series > 80L) 1 else if (n_series > 30L) 1.4 else 2
      marker_size <- if (n_series > 80L) 3 else if (n_series > 30L) 4 else 6
      trace_opacity <- if (n_series > 80L) 0.35 else if (n_series > 30L) 0.55 else 0.9
      base_palette <- c("#0A1E3C", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#E69F00", "#6A3D9A")
      palette <- if (n_series <= length(base_palette)) {
        base_palette[seq_len(n_series)]
      } else {
        grDevices::colorRampPalette(base_palette)(n_series)
      }

      hover_style <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      p <- plotly::plot_ly()
      added_trace <- FALSE
      for (i in seq_len(n_series)) {
        y_values <- suppressWarnings(as.numeric(plot_data[i, value_cols]))
        if (!any(!is.na(y_values))) {
          next
        }
        categoria <- as.character(plot_data$categoria[[i]])
        formatted <- format_line_value(y_values, is_percentage = is_percentage)
        trace_args <- list(
          p = p,
          x = years,
          y = y_values,
          type = "scatter",
          mode = "lines+markers",
          name = categoria,
          line = list(color = palette[[i]], width = line_width),
          marker = list(color = palette[[i]], size = marker_size),
          opacity = trace_opacity,
          customdata = I(Map(c, rep(categoria, length(years)), formatted)),
          hovertemplate = "%{customdata[0]}<br>Ano %{x}: %{customdata[1]}<extra></extra>",
          showlegend = show_legend
        )
        p <- do.call(plotly::add_trace, trace_args)
        added_trace <- TRUE
      }

      if (!isTRUE(added_trace)) {
        return(empty_line_plot())
      }

      yaxis_cfg <- list(
        title = list(text = metric_title, standoff = 12L),
        tickfont = list(size = 11, color = "#000000"),
        showgrid = TRUE,
        gridcolor = "#D9DEE5",
        zeroline = FALSE,
        fixedrange = FALSE
      )
      if (isTRUE(is_percentage)) {
        yaxis_cfg$range <- c(0, 100)
        yaxis_cfg$tickmode <- "array"
        yaxis_cfg$tickvals <- c(0, 25, 50, 75, 100)
        yaxis_cfg$ticktext <- c("0%", "25%", "50%", "75%", "100%")
      }

      x_range <- if (length(years) == 1L) {
        c(years[[1]] - 0.5, years[[1]] + 0.5)
      } else {
        c(min(years), max(years))
      }

      hide_aps_modebar(
        plotly::layout(
          p,
          xaxis = list(
            title = list(text = "Ano", standoff = 8L),
            tickmode = "array",
            tickvals = years,
            ticktext = as.character(years),
            range = x_range,
            showgrid = TRUE,
            gridcolor = "#EEF2F6",
            showline = TRUE,
            linecolor = "#C3CBD5",
            linewidth = 1,
            zeroline = FALSE,
            fixedrange = TRUE
          ),
          yaxis = yaxis_cfg,
          margin = list(l = 72, r = 28, t = 18, b = 58),
          hoverlabel = hover_style,
          hovermode = "closest",
          paper_bgcolor = "#FFFFFF",
          plot_bgcolor = "#FBFCFE",
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.25,
            font = list(size = 10),
            itemclick = "toggleothers",
            itemdoubleclick = "toggle"
          ),
          showlegend = show_legend
        )
      )
    }

    infer_single_year <- function(var_numeric) {
      metric_key <- iconv(toupper(as.character(var_numeric)), from = "", to = "ASCII//TRANSLIT")
      if (is.na(metric_key)) {
        metric_key <- toupper(as.character(var_numeric))
      }
      if (grepl("COBERTURA AB|COBERTURA ESF", metric_key)) {
        return(2020L)
      }
      if (grepl("ANS|GESTANTES|SUSDEPENDENTES|UBS", metric_key)) {
        return(2023L)
      }
      2023L
    }

    build_single_year_line_plot <- function(data, var_numeric, var_category, is_percentage = FALSE) {
      year <- infer_single_year(var_numeric)
      out <- data.frame(
        categoria = as.character(data[[var_category]]),
        valor = suppressWarnings(as.numeric(data[[var_numeric]])),
        stringsAsFactors = FALSE
      )
      names(out)[2] <- paste0("valor_", year)
      build_multiyear_line_plot(
        data = out,
        years = year,
        category_col = "categoria",
        metric_title = var_numeric,
        is_percentage = is_percentage
      )
    }

    build_bar_plot <- function(data, var_numeric, var_category, is_percentage = FALSE, force_vertical = FALSE) {
      percent_tick_vals <- c(0, 25, 50, 75, 100)
      percent_tick_text <- paste0(percent_tick_vals, "%")
      # Se a categoria for "MUNICIPAL", exibe como "MUNICÍPIO"
      display_category <- ifelse(var_category == "MUNICIPAL", "MUNICÍPIO", var_category)

      # Guardas contra dados/colunas inexistentes
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
          !(var_numeric %in% names(data)) || !(var_category %in% names(data))) {
        # Traço invisível para calar os avisos "No trace type..."
        return(
          hide_aps_modebar(
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
        )
      }

      return(
        build_single_year_line_plot(
          data = data,
          var_numeric = var_numeric,
          var_category = var_category,
          is_percentage = is_percentage
        )
      )

      normalized_var_category <- iconv(toupper(var_category), from = "", to = "ASCII//TRANSLIT")
      if (is.na(normalized_var_category)) {
        normalized_var_category <- toupper(var_category)
      }

      if (normalized_var_category %in% c("MUNICIPAL", "SUPERVISAO DE SAUDE")) {
        sort_key <- iconv(toupper(as.character(data[[var_category]])), from = "", to = "ASCII//TRANSLIT")
        sort_key[is.na(sort_key)] <- toupper(as.character(data[[var_category]])[is.na(sort_key)])
        data <- data[order(sort_key, as.character(data[[var_category]]), na.last = TRUE), , drop = FALSE]
      }

      n_bars <- nrow(data)

      # Pré-formata os valores para o hover (customdata)
      if (is_percentage){
        data$formatted_value <- sapply(data[[var_numeric]], function(x) {
          x_num <- suppressWarnings(as.numeric(x))
          paste0(format(round(x_num, 1), nsmall = 1, big.mark = ".", decimal.mark = ","), "%")
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

      data <- data |>
        dplyr::mutate(row_id = dplyr::row_number())

      trace_data <- data[
        !is.na(suppressWarnings(as.numeric(data[[var_numeric]]))),
        ,
        drop = FALSE
      ]

      # Ticks numéricos (quando não for %)
      if (!is_percentage) {
        numeric_vals <- suppressWarnings(as.numeric(data[[var_numeric]]))
        rng <- range(c(0, numeric_vals), na.rm = TRUE)
        if (!is.finite(rng[1]) || !is.finite(rng[2])) rng <- c(0, 0)
        tick_vals_numeric <- pretty(rng)
        tick_text_numeric <- sapply(tick_vals_numeric, format_integer)
      }

      if (identical(orientation, "h")) {
        show_top_axis <- n_bars > 20
        xaxis_config <- list(
          title = list(text = wrap_vertical_title(var_numeric), standoff = 0L),
          tickfont = list(size = 12, color = "#000000")
        )
        if (is_percentage) {
          xaxis_config <- c(xaxis_config, list(
            range = c(0, 100),
            tickmode = "array",
            tickvals = percent_tick_vals,
            ticktext = percent_tick_text,
            tickangle = if (isTRUE(show_top_axis)) 0 else 90
          ))
        } else {
          xaxis_config$tickvals <- tick_vals_numeric
          xaxis_config$ticktext <- tick_text_numeric
          xaxis_config$tickangle <- if (isTRUE(show_top_axis)) 0 else 90
        }

        p <- plotly::plot_ly(
          data = trace_data,
          x = as.formula(paste0("~`", var_numeric, "`")),
          y = ~row_id,
          type = "bar",
          orientation = "h",
          marker = list(color = "#0a1e3c"),
          hovertext = as.formula(paste0("~`", var_category, "`")),
          customdata = trace_data$formatted_value,
          hovertemplate = "%{hovertext}<br>%{customdata}<extra></extra>"
        )
        p <- plotly::layout(
          p,
          xaxis = xaxis_config,
          yaxis = list(
            title = list(text = display_category, standoff = 0L),
            tickfont = list(size = 12, color = "#000000"),
            tickmode = "array",
            tickvals = data$row_id,
            ticktext = as.character(data[[var_category]]),
            range = c(n_bars + 0.5, 0.5),
            fixedrange = TRUE
          ),
          bargap = if (n_bars > 20) 0.12 else 0.24,
          margin = list(l = 80, r = 20, t = 0, b = if (is_percentage) 52 else 40),
          hoverlabel = hl
        )
        p <- hide_aps_modebar(p)
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
          yaxis_config <- c(yaxis_config, list(
            range = c(0, 100),
            tickmode = "array",
            tickvals = percent_tick_vals,
            ticktext = percent_tick_text,
            tickangle = if (identical(var_numeric, "COBERTURA ANS %")) 0 else 90
          ))
        } else {
          yaxis_config$tickvals <- tick_vals_numeric
          yaxis_config$ticktext <- tick_text_numeric
        }

        p <- plotly::plot_ly(
          data = trace_data,
          x = as.formula(paste0("~`", var_category, "`")),
          y = as.formula(paste0("~`", var_numeric, "`")),
          type = "bar",
          marker = list(color = "#0a1e3c"),
          customdata = trace_data$formatted_value,
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
        p <- hide_aps_modebar(p)
      }
      p
    }

    # Formata o símbolo de separador decimal e milhar nas caixinhas de totais
    format_number <- function(x) {
      format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }


    build_ab_comparison_plot <- function(data, current_year, legacy_year = 2020, force_vertical = FALSE) {
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
        return(
          plotly::plot_ly(
            x = 0, y = 0,
            type = "scatter", mode = "markers",
            opacity = 0, hoverinfo = "skip", showlegend = FALSE
          ) |>
            plotly::layout(
              annotations = list(
                text = "Sem dados disponíveis",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 14)
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              margin = list(l = 20, r = 20, t = 20, b = 20)
            )
        )
      }

      data <- data |>
        dplyr::mutate(
          cobertura_ab_legado = suppressWarnings(as.numeric(.data$cobertura_ab_legado)),
          cobertura_ab_atual = suppressWarnings(as.numeric(.data$cobertura_ab_atual)),
          formatted_legacy = dplyr::if_else(
            is.na(.data$cobertura_ab_legado),
            "-",
            format(round(.data$cobertura_ab_legado, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
          ),
          formatted_current = dplyr::if_else(
            is.na(.data$cobertura_ab_atual),
            "-",
            format(round(.data$cobertura_ab_atual, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
          )
        )

      n_bars <- nrow(data)
      if (isTRUE(force_vertical)) {
        orientation <- "v"
      } else if (n_bars <= 10) {
        orientation <- "v"
      } else {
        tick_names <- as.character(data$MUNICIPAL)
        all_short <- all(
          nchar(tick_names) <= 12 &
            sapply(tick_names, function(x) length(unlist(strsplit(x, "\\s+")))) <= 2
        )
        orientation <- if (isTRUE(all_short)) "v" else "h"
      }

      legend_legacy <- paste0("Ano ", legacy_year, " (histórico)")
      legend_current <- paste0("Ano ", current_year)

      hl <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      if (identical(orientation, "h")) {
        p <- plotly::plot_ly(data = data)
        p <- plotly::add_bars(
          p,
          x = ~cobertura_ab_legado,
          y = ~MUNICIPAL,
          orientation = "h",
          name = legend_legacy,
          marker = list(color = "#6e7a8c"),
          customdata = ~formatted_legacy,
          hovertemplate = "%{y}<br>%{customdata}<extra></extra>"
        )
        p <- plotly::add_bars(
          p,
          x = ~cobertura_ab_atual,
          y = ~MUNICIPAL,
          orientation = "h",
          name = legend_current,
          marker = list(color = "#0a1e3c"),
          customdata = ~formatted_current,
          hovertemplate = "%{y}<br>%{customdata}<extra></extra>"
        )

        return(
          plotly::layout(
            p,
            barmode = "group",
            bargap = 0.18,
            xaxis = list(
              title = list(text = "Cobertura AB (%)", standoff = 0L),
              tickformat = ".1f",
              range = c(0, 100),
              dtick = 20,
              tickfont = list(size = 12, color = "#000000")
            ),
            yaxis = list(
              title = list(text = "MUNICÍPIO", standoff = 0L),
              tickfont = list(size = 12, color = "#000000"),
              categoryorder = "category ascending",
              autorange = "reversed"
            ),
            hoverlabel = hl,
            legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11))
          )
        )
      }

      original_categories <- data$MUNICIPAL
      categories <- ifelse(
        grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
        sub("\\s+", "<br>", original_categories),
        gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
      )

      p <- plotly::plot_ly(data = data)
      p <- plotly::add_bars(
        p,
        x = ~MUNICIPAL,
        y = ~cobertura_ab_legado,
        name = legend_legacy,
        marker = list(color = "#6e7a8c"),
        customdata = ~formatted_legacy,
        hovertemplate = "%{x}<br>%{customdata}<extra></extra>"
      )
      p <- plotly::add_bars(
        p,
        x = ~MUNICIPAL,
        y = ~cobertura_ab_atual,
        name = legend_current,
        marker = list(color = "#0a1e3c"),
        customdata = ~formatted_current,
        hovertemplate = "%{x}<br>%{customdata}<extra></extra>"
      )

      plotly::layout(
        p,
        barmode = "group",
        bargap = 0.18,
        xaxis = list(
          title = list(text = "MUNICÍPIO", standoff = 20L),
          tickmode = "array",
          tickvals = original_categories,
          ticktext = categories,
          tickangle = 90,
          automargin = TRUE,
          tickfont = list(size = 12, color = "#000000")
        ),
        yaxis = list(
          title = list(text = "Cobertura AB (%)", standoff = 20L, size = 1),
          tickformat = ".1f",
          range = c(0, 100),
          dtick = 20,
          tickfont = list(size = 12, color = "#000000")
        ),
        margin = list(b = 90),
        hoverlabel = hl,
        legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11))
      )
    }

    build_coverage_comparison_plot <- function(data, metric_title, current_year, legacy_year = 2020, force_vertical = FALSE) {
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
        return(
          hide_aps_modebar(
            plotly::plot_ly(
              x = 0, y = 0,
              type = "scatter", mode = "markers",
              opacity = 0, hoverinfo = "skip", showlegend = FALSE
            ) |>
              plotly::layout(
                annotations = list(
                  text = "Sem dados disponiveis",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE, font = list(size = 14)
                ),
                xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                margin = list(l = 20, r = 20, t = 20, b = 20)
              )
          )
        )
      }

      data <- data |>
        dplyr::mutate(
          valor_legado = suppressWarnings(as.numeric(.data$valor_legado)),
          valor_atual = suppressWarnings(as.numeric(.data$valor_atual)),
          formatted_legacy = dplyr::if_else(
            is.na(.data$valor_legado),
            "-",
            format(round(.data$valor_legado, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
          ),
          formatted_current = dplyr::if_else(
            is.na(.data$valor_atual),
            "-",
            format(round(.data$valor_atual, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
          )
        )

      n_bars <- nrow(data)
      if (isTRUE(force_vertical)) {
        orientation <- "v"
      } else if (n_bars <= 10) {
        orientation <- "v"
      } else {
        tick_names <- as.character(data$MUNICIPAL)
        all_short <- all(
          nchar(tick_names) <= 12 &
            sapply(tick_names, function(x) length(unlist(strsplit(x, "\\s+")))) <= 2
        )
        orientation <- if (isTRUE(all_short)) "v" else "h"
      }

      legend_legacy <- paste0("Ano ", legacy_year, " (historico)")
      legend_current <- paste0("Ano ", current_year)

      hl <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      if (identical(orientation, "h")) {
        p <- plotly::plot_ly(data = data)
        p <- plotly::add_bars(
          p,
          x = ~valor_legado,
          y = ~MUNICIPAL,
          orientation = "h",
          name = legend_legacy,
          marker = list(color = "#6e7a8c"),
          customdata = ~formatted_legacy,
          hovertemplate = "%{y}<br>%{customdata}<extra></extra>"
        )
        p <- plotly::add_bars(
          p,
          x = ~valor_atual,
          y = ~MUNICIPAL,
          orientation = "h",
          name = legend_current,
          marker = list(color = "#0a1e3c"),
          customdata = ~formatted_current,
          hovertemplate = "%{y}<br>%{customdata}<extra></extra>"
        )

        return(
          plotly::layout(
            p,
            barmode = "group",
            bargap = 0.18,
            xaxis = list(
              title = list(text = metric_title, standoff = 0L),
              tickformat = ".1f",
              range = c(0, 100),
              dtick = 20,
              tickfont = list(size = 12, color = "#000000")
            ),
            yaxis = list(
              title = list(text = "MUNICIPIO", standoff = 0L),
              tickfont = list(size = 12, color = "#000000"),
              categoryorder = "category ascending",
              autorange = "reversed"
            ),
            hoverlabel = hl,
            legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11))
          )
        )
      }

      original_categories <- data$MUNICIPAL
      categories <- ifelse(
        grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
        sub("\\s+", "<br>", original_categories),
        gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
      )

      p <- plotly::plot_ly(data = data)
      p <- plotly::add_bars(
        p,
        x = ~MUNICIPAL,
        y = ~valor_legado,
        name = legend_legacy,
        marker = list(color = "#6e7a8c"),
        customdata = ~formatted_legacy,
        hovertemplate = "%{x}<br>%{customdata}<extra></extra>"
      )
      p <- plotly::add_bars(
        p,
        x = ~MUNICIPAL,
        y = ~valor_atual,
        name = legend_current,
        marker = list(color = "#0a1e3c"),
        customdata = ~formatted_current,
        hovertemplate = "%{x}<br>%{customdata}<extra></extra>"
      )

      plotly::layout(
        p,
        barmode = "group",
        bargap = 0.18,
        xaxis = list(
          title = list(text = "MUNICIPIO", standoff = 20L),
          tickmode = "array",
          tickvals = original_categories,
          ticktext = categories,
          tickangle = 90,
          automargin = TRUE,
          tickfont = list(size = 12, color = "#000000")
        ),
        yaxis = list(
          title = list(text = metric_title, standoff = 20L, size = 1),
          tickformat = ".1f",
          range = c(0, 100),
          dtick = 20,
          tickfont = list(size = 12, color = "#000000")
        ),
        margin = list(b = 90),
        hoverlabel = hl,
        legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11))
      )
    }

    build_updated_coverage_box <- function(title, box_class, values, current_year, legacy_year = 2020) {
      div(
        class = paste("custom-box", box_class),
        style = "height:145px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4(title),
        tags$div(
          format_metric_percent(values$current),
          style = "font-size: 30px; font-weight: 700; color: #0A1E3C; line-height: 1.1;"
        ),
        tags$div(
          paste0("Ano ", current_year),
          style = "font-size: 12px; color: #0A1E3C; margin-top: 2px;"
        ),
        tags$div(
          format_metric_percent(values$legacy),
          style = "font-size: 18px; font-weight: 600; color: #6E7A8C; margin-top: 8px; line-height: 1.1;"
        ),
        tags$div(
          paste0("Ano ", legacy_year, " (historico)"),
          style = "font-size: 12px; color: #6E7A8C;"
        ),
        tags$div(
          paste0("Atual: ", current_year, " | Historico do painel: ", legacy_year),
          style = "position: absolute; bottom: 1px; left: 10px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 6px; border-radius: 3px;"
        )
      )
    }

    build_coverage_multiyear_plot_old <- function(data, metric_title, force_vertical = FALSE) {
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
        return(
          plotly::plot_ly(
            x = 0, y = 0,
            type = "scatter", mode = "markers",
            opacity = 0, hoverinfo = "skip", showlegend = FALSE
          ) |>
            plotly::layout(
              annotations = list(
                text = "Sem dados disponiveis",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 14)
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              margin = list(l = 20, r = 20, t = 20, b = 20)
            )
        )
      }

      format_value <- function(x) {
        ifelse(
          is.na(x),
          "-",
          format(round(x, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
        )
      }

      data <- data |>
        dplyr::mutate(
          valor_historico = suppressWarnings(as.numeric(.data$valor_historico)),
          valor_consolidado = suppressWarnings(as.numeric(.data$valor_consolidado)),
          valor_preliminar = suppressWarnings(as.numeric(.data$valor_preliminar)),
          formatted_historico = format_value(.data$valor_historico),
          formatted_consolidado = format_value(.data$valor_consolidado),
          formatted_preliminar = format_value(.data$valor_preliminar)
        ) |>
        dplyr::arrange(
          dplyr::desc(dplyr::coalesce(.data$valor_consolidado, .data$valor_preliminar, .data$valor_historico)),
          .data$MUNICIPAL
        )

      legend_historico <- paste0("Ano ", cobertura_ab_legacy_year, " (Histórico)")
      legend_consolidado <- paste0("Ano ", cobertura_consolidated_year, " (consolidado)")
      legend_preliminar <- paste0("Ano ", cobertura_preliminary_year, " (preliminar)")

      hover_style <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      use_grouped_bars <- isTRUE(force_vertical) || nrow(data) <= 10L

      if (isTRUE(use_grouped_bars)) {
        original_categories <- data$MUNICIPAL
        categories <- ifelse(
          grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
          sub("\\s+", "<br>", original_categories),
          gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
        )

        p <- plotly::plot_ly(data = data)
        p <- plotly::add_bars(
          p,
          x = ~MUNICIPAL,
          y = ~valor_historico,
          name = legend_historico,
          marker = list(color = "#6e7a8c"),
          customdata = ~formatted_historico,
          hovertemplate = paste0("%{x}<br>", legend_historico, ": %{customdata}<extra></extra>")
        )
        p <- plotly::add_bars(
          p,
          x = ~MUNICIPAL,
          y = ~valor_consolidado,
          name = legend_consolidado,
          marker = list(color = "#0a1e3c"),
          customdata = ~formatted_consolidado,
          hovertemplate = paste0("%{x}<br>", legend_consolidado, ": %{customdata}<extra></extra>")
        )
        p <- plotly::add_bars(
          p,
          x = ~MUNICIPAL,
          y = ~valor_preliminar,
          name = legend_preliminar,
          marker = list(color = "#32a0ff", opacity = 0.85),
          customdata = ~formatted_preliminar,
          hovertemplate = paste0("%{x}<br>", legend_preliminar, ": %{customdata}<extra></extra>")
        )

        return(
          plotly::layout(
            p,
            barmode = "group",
            bargap = 0.18,
            xaxis = list(
              title = list(text = "MUNICÍPIO", standoff = 20L),
              tickmode = "array",
              tickvals = original_categories,
              ticktext = categories,
              tickangle = 90,
              automargin = TRUE,
              tickfont = list(size = 12, color = "#000000")
            ),
            yaxis = list(
              title = list(text = metric_title, standoff = 20L, size = 1),
              tickformat = ".1f",
              range = c(0, 100),
              dtick = 20,
              tickfont = list(size = 12, color = "#000000")
            ),
            margin = list(b = 90),
            hoverlabel = hover_style,
            legend = list(orientation = "h", x = 0, y = 1.16, font = list(size = 11))
          )
        )
      }

      build_segment_trace <- function(df, start_col, end_col, color, dash = "solid") {
        segment_df <- df |>
          dplyr::filter(!is.na(.data[[start_col]]), !is.na(.data[[end_col]]))

        if (!nrow(segment_df)) {
          return(NULL)
        }

        x_values <- unlist(
          Map(
            function(x0, x1) c(x0, x1, NA_real_),
            segment_df[[start_col]],
            segment_df[[end_col]]
          )
        )
        y_values <- unlist(
          Map(
            function(lbl) c(lbl, lbl, NA_character_),
            segment_df$MUNICIPAL
          )
        )

        list(
          x = x_values,
          y = y_values,
          line = list(color = color, width = 2, dash = dash)
        )
      }

      hist_cons <- build_segment_trace(data, "valor_historico", "valor_consolidado", "#6e7a8c", "solid")
      cons_prelim <- build_segment_trace(data, "valor_consolidado", "valor_preliminar", "#32a0ff", "dash")

      p <- plotly::plot_ly()

      if (!is.null(hist_cons)) {
        p <- plotly::add_trace(
          p,
          type = "scatter",
          mode = "lines",
          x = hist_cons$x,
          y = hist_cons$y,
          line = hist_cons$line,
          hoverinfo = "skip",
          showlegend = FALSE
        )
      }

      if (!is.null(cons_prelim)) {
        p <- plotly::add_trace(
          p,
          type = "scatter",
          mode = "lines",
          x = cons_prelim$x,
          y = cons_prelim$y,
          line = cons_prelim$line,
          hoverinfo = "skip",
          showlegend = FALSE
        )
      }

      p <- plotly::add_trace(
        p,
        data = data,
        type = "scatter",
        mode = "markers",
        x = ~valor_historico,
        y = ~MUNICIPAL,
        name = legend_historico,
        marker = list(color = "#6e7a8c", size = 10, symbol = "circle"),
        customdata = ~formatted_historico,
        hovertemplate = paste0("%{y}<br>", legend_historico, ": %{customdata}<extra></extra>")
      )

      p <- plotly::add_trace(
        p,
        data = data,
        type = "scatter",
        mode = "markers",
        x = ~valor_consolidado,
        y = ~MUNICIPAL,
        name = legend_consolidado,
        marker = list(color = "#0a1e3c", size = 11, symbol = "circle"),
        customdata = ~formatted_consolidado,
        hovertemplate = paste0("%{y}<br>", legend_consolidado, ": %{customdata}<extra></extra>")
      )

      p <- plotly::add_trace(
        p,
        data = data,
        type = "scatter",
        mode = "markers",
        x = ~valor_preliminar,
        y = ~MUNICIPAL,
        name = legend_preliminar,
        marker = list(color = "#32a0ff", size = 11, symbol = "circle-open"),
        customdata = ~formatted_preliminar,
        hovertemplate = paste0("%{y}<br>", legend_preliminar, ": %{customdata}<extra></extra>")
      )

      plotly::layout(
        p,
        xaxis = list(
          title = list(text = metric_title, standoff = 0L),
          tickformat = ".1f",
          range = c(0, 100),
          dtick = 20,
          tickfont = list(size = 12, color = "#000000")
        ),
        yaxis = list(
          title = list(text = "MUNICÍPIO", standoff = 0L),
          tickfont = list(size = 12, color = "#000000"),
          categoryorder = "array",
          categoryarray = data$MUNICIPAL,
          autorange = "reversed"
        ),
        margin = list(l = 80, r = 20, t = 20, b = 50),
        hoverlabel = hover_style,
        legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11))
      )
    }

    build_coverage_multiyear_plot <- function(data, metric_title, force_vertical = FALSE) {
      return(
        build_multiyear_line_plot(
          data = data,
          years = cobertura_display_years,
          category_col = "MUNICIPAL",
          metric_title = metric_title,
          is_percentage = TRUE
        )
      )

      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L) {
        return(
          plotly::plot_ly(
            x = 0, y = 0,
            type = "scatter", mode = "markers",
            opacity = 0, hoverinfo = "skip", showlegend = FALSE
          ) |>
            plotly::layout(
              annotations = list(
                text = "Sem dados disponiveis",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 14)
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              margin = list(l = 20, r = 20, t = 20, b = 20)
            )
        )
      }

      years <- sort(unique(stats::na.omit(as.integer(cobertura_display_years))))
      if (!length(years)) {
        return(
          plotly::plot_ly(
            x = 0, y = 0,
            type = "scatter", mode = "markers",
            opacity = 0, hoverinfo = "skip", showlegend = FALSE
          ) |>
            plotly::layout(
              annotations = list(
                text = "Sem dados disponiveis",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                showarrow = FALSE, font = list(size = 14)
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              margin = list(l = 20, r = 20, t = 20, b = 20)
            )
        )
      }

      format_value <- function(x) {
        ifelse(
          is.na(x),
          "-",
          paste0(format(round(x, 1), nsmall = 1, big.mark = ".", decimal.mark = ","), "%")
        )
      }

      year_status <- function(year) {
        if (year %in% cobertura_ab_legacy_year) {
          "historico"
        } else if (year %in% cobertura_preliminary_year) {
          "preliminar"
        } else {
          "consolidado"
        }
      }

      year_color <- function(year) {
        cobertura_year_color(year)
      }

      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        if (!(value_col %in% names(data))) {
          data[[value_col]] <- NA_real_
        }
        data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
        data[[formatted_col]] <- format_value(data[[value_col]])
      }

      data <- data |>
        dplyr::mutate(
          sort_key = {
            x <- iconv(toupper(as.character(.data$MUNICIPAL)), from = "", to = "ASCII//TRANSLIT")
            x[is.na(x)] <- toupper(as.character(.data$MUNICIPAL)[is.na(x)])
            x
          }
        ) |>
        dplyr::arrange(.data$sort_key, .data$MUNICIPAL) |>
        dplyr::select(-.data$sort_key) |>
        dplyr::mutate(row_id = dplyr::row_number())

      tick_vals <- c(0, 25, 50, 75, 100)
      tick_text <- paste0(tick_vals, "%")
      show_top_axis <- nrow(data) > 20L
      max_name_chars <- if (nrow(data) == 0L) 0L else max(nchar(as.character(data$MUNICIPAL)), na.rm = TRUE)
      left_margin <- max(145L, min(280L, 95L + (max_name_chars * 5L)))

      hover_style <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      p <- plotly::plot_ly(data = data)
      bar_width <- min(0.18, 0.9 / max(length(years), 1L))
      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        marker_cfg <- list(color = year_color(year))
        if (year %in% cobertura_preliminary_year) {
          marker_cfg$line <- list(color = "#0a1e3c", width = 1)
        }

        p <- plotly::add_bars(
          p,
          x = data[[value_col]],
          y = data$row_id,
          orientation = "h",
          width = bar_width,
          name = paste0("Ano ", year),
          marker = marker_cfg,
          opacity = if (year %in% cobertura_preliminary_year) 0.9 else 1,
          textposition = "none",
          customdata = I(Map(c, as.character(data$MUNICIPAL), data[[formatted_col]])),
          hovertemplate = paste0("%{customdata[0]}<br>Ano ", year, " (", year_status(year), "): %{customdata[1]}<extra></extra>"),
          showlegend = FALSE
        )
      }

      p <- plotly::layout(
        p,
        barmode = "group",
        bargap = 0.16,
        xaxis = list(
          title = list(text = metric_title, standoff = 0L),
          range = c(0, 100),
          tickmode = "array",
          tickvals = tick_vals,
          ticktext = tick_text,
          tickangle = if (isTRUE(show_top_axis)) 0 else 90,
          tickfont = list(size = 10, color = "#000000"),
          showgrid = TRUE,
          gridcolor = "#D9DEE5",
          showline = TRUE,
          linecolor = "#C3CBD5",
          linewidth = 1,
          zeroline = FALSE,
          ticks = "outside",
          ticklen = 4,
          tickcolor = "#7F8A99",
          fixedrange = TRUE,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = NULL),
          tickfont = list(size = 12, color = "#000000"),
          tickmode = "array",
          tickvals = data$row_id,
          ticktext = as.character(data$MUNICIPAL),
          range = c(nrow(data) + 0.5, 0.5),
          fixedrange = TRUE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        margin = list(l = left_margin, r = 20, t = 8, b = 52),
        hoverlabel = hover_style,
        hovermode = "closest",
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FBFCFE",
        showlegend = FALSE
      )

      hide_aps_modebar(p)
    }

    build_cobertura_ans_multiyear_plot <- function(data, years, metric_title = "Cobertura da Saúde Suplementar (%)") {
      return(
        build_multiyear_line_plot(
          data = data,
          years = years,
          category_col = "MUNICIPAL",
          metric_title = metric_title,
          is_percentage = TRUE
        )
      )

      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L || !"MUNICIPAL" %in% names(data)) {
        return(
          hide_aps_modebar(
            plotly::plot_ly(
              x = 0, y = 0,
              type = "scatter", mode = "markers",
              opacity = 0, hoverinfo = "skip", showlegend = FALSE
            ) |>
              plotly::layout(
                annotations = list(
                  text = "Sem dados disponiveis",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE, font = list(size = 14)
                ),
                xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                margin = list(l = 20, r = 20, t = 20, b = 20)
              )
          )
        )
      }

      years <- sort(unique(stats::na.omit(as.integer(years))))
      if (!length(years)) {
        return(
          hide_aps_modebar(
            plotly::plot_ly(
              x = 0, y = 0,
              type = "scatter", mode = "markers",
              opacity = 0, hoverinfo = "skip", showlegend = FALSE
            ) |>
              plotly::layout(
                annotations = list(
                  text = "Sem dados disponiveis",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE, font = list(size = 14)
                ),
                xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                margin = list(l = 20, r = 20, t = 20, b = 20)
              )
          )
        )
      }

      format_value <- function(x) {
        ifelse(
          is.na(x),
          "-",
          paste0(format(round(x, 1), nsmall = 1, big.mark = ".", decimal.mark = ","), "%")
        )
      }

      year_status <- function(year) {
        if (year %in% cobertura_ans_preliminary_year) "preliminar" else "consolidado"
      }

      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        if (!(value_col %in% names(data))) {
          data[[value_col]] <- NA_real_
        }
        data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
        data[[formatted_col]] <- format_value(data[[value_col]])
      }

      data <- data |>
        dplyr::mutate(
          sort_key = {
            x <- iconv(toupper(as.character(.data$MUNICIPAL)), from = "", to = "ASCII//TRANSLIT")
            x[is.na(x)] <- toupper(as.character(.data$MUNICIPAL)[is.na(x)])
            x
          }
        ) |>
        dplyr::arrange(.data$sort_key, .data$MUNICIPAL) |>
        dplyr::select(-.data$sort_key) |>
        dplyr::mutate(row_id = dplyr::row_number())

      tick_vals <- c(0, 25, 50, 75, 100)
      tick_text <- paste0(tick_vals, "%")
      show_top_axis <- nrow(data) > 20L
      max_name_chars <- if (nrow(data) == 0L) 0L else max(nchar(as.character(data$MUNICIPAL)), na.rm = TRUE)
      left_margin <- max(145L, min(280L, 95L + (max_name_chars * 5L)))

      hover_style <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      p <- plotly::plot_ly(data = data)
      bar_width <- min(0.18, 0.9 / max(length(years), 1L))
      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        marker_cfg <- list(color = cobertura_ans_year_color(year))
        if (year %in% cobertura_ans_preliminary_year) {
          marker_cfg$line <- list(color = "#0a1e3c", width = 1)
        }

        p <- plotly::add_bars(
          p,
          x = data[[value_col]],
          y = data$row_id,
          orientation = "h",
          width = bar_width,
          name = paste0("Ano ", year),
          marker = marker_cfg,
          opacity = if (year %in% cobertura_ans_preliminary_year) 0.9 else 1,
          textposition = "none",
          customdata = I(Map(c, as.character(data$MUNICIPAL), data[[formatted_col]])),
          hovertemplate = paste0("%{customdata[0]}<br>Ano ", year, " (", year_status(year), "): %{customdata[1]}<extra></extra>"),
          showlegend = FALSE
        )
      }

      p <- plotly::layout(
        p,
        barmode = "group",
        bargap = 0.16,
        xaxis = list(
          title = list(text = metric_title, standoff = 0L),
          range = c(0, 100),
          tickmode = "array",
          tickvals = tick_vals,
          ticktext = tick_text,
          tickangle = if (isTRUE(show_top_axis)) 0 else 90,
          tickfont = list(size = 10, color = "#000000"),
          showgrid = TRUE,
          gridcolor = "#D9DEE5",
          showline = TRUE,
          linecolor = "#C3CBD5",
          linewidth = 1,
          zeroline = FALSE,
          ticks = "outside",
          ticklen = 4,
          tickcolor = "#7F8A99",
          fixedrange = TRUE,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = NULL),
          tickfont = list(size = 12, color = "#000000"),
          tickmode = "array",
          tickvals = data$row_id,
          ticktext = as.character(data$MUNICIPAL),
          range = c(nrow(data) + 0.5, 0.5),
          fixedrange = TRUE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        margin = list(l = left_margin, r = 20, t = 8, b = 52),
        hoverlabel = hover_style,
        hovermode = "closest",
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FBFCFE",
        showlegend = FALSE
      )

      hide_aps_modebar(p)
    }

    build_nascidos_multiyear_plot <- function(data,
                                              years,
                                              metric_title = "Nascidos vivos",
                                              axis_title = "Nº DE NASCIDOS VIVOS",
                                              year_color = nascidos_year_color,
                                              preliminary_year = nascidos_preliminary_year) {
      return(
        build_multiyear_line_plot(
          data = data,
          years = years,
          category_col = "LOCALIDADE",
          metric_title = metric_title,
          is_percentage = FALSE
        )
      )

      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L || !"LOCALIDADE" %in% names(data)) {
        return(
          hide_aps_modebar(
            plotly::plot_ly(
              x = 0, y = 0,
              type = "scatter", mode = "markers",
              opacity = 0, hoverinfo = "skip", showlegend = FALSE
            ) |>
              plotly::layout(
                annotations = list(
                  text = "Sem dados disponiveis",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE, font = list(size = 14)
                ),
                xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                margin = list(l = 20, r = 20, t = 20, b = 20)
              )
          )
        )
      }

      years <- sort(unique(stats::na.omit(as.integer(years))))
      if (!length(years)) {
        return(
          hide_aps_modebar(
            plotly::plot_ly(
              x = 0, y = 0,
              type = "scatter", mode = "markers",
              opacity = 0, hoverinfo = "skip", showlegend = FALSE
            ) |>
              plotly::layout(
                annotations = list(
                  text = "Sem dados disponiveis",
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                  showarrow = FALSE, font = list(size = 14)
                ),
                xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                margin = list(l = 20, r = 20, t = 20, b = 20)
              )
          )
        )
      }

      format_count <- function(x) {
        ifelse(is.na(x), "-", format_integer(round(suppressWarnings(as.numeric(x)))))
      }

      preliminary_year <- suppressWarnings(as.integer(preliminary_year))
      preliminary_year <- preliminary_year[is.finite(preliminary_year)]

      year_status <- function(year) {
        if (year %in% preliminary_year) "preliminar" else "consolidado"
      }

      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        if (!(value_col %in% names(data))) {
          data[[value_col]] <- NA_real_
        }
        data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
        data[[formatted_col]] <- format_count(data[[value_col]])
      }

      data <- data |>
        dplyr::mutate(
          sort_key = {
            x <- iconv(toupper(as.character(.data$LOCALIDADE)), from = "", to = "ASCII//TRANSLIT")
            x[is.na(x)] <- toupper(as.character(.data$LOCALIDADE)[is.na(x)])
            x
          },
          rras_order = suppressWarnings(as.integer(sub("^RRAS\\s+([0-9]+)$", "\\1", .data$sort_key))),
          ignored_order = .data$sort_key == "IGNORADO"
        ) |>
        dplyr::arrange(.data$ignored_order, is.na(.data$rras_order), .data$rras_order, .data$sort_key, .data$LOCALIDADE) |>
        dplyr::select(-.data$sort_key, -.data$rras_order, -.data$ignored_order) |>
        dplyr::mutate(row_id = dplyr::row_number())

      numeric_vals <- unlist(data[paste0("valor_", years)], use.names = FALSE)
      numeric_vals <- suppressWarnings(as.numeric(numeric_vals))
      max_val <- max(numeric_vals, na.rm = TRUE)
      if (!is.finite(max_val) || max_val <= 0) {
        max_val <- 1
      }
      tick_vals <- pretty(c(0, max_val), n = 5)
      tick_vals <- unique(c(0, tick_vals[tick_vals > 0]))
      tick_vals <- tick_vals[tick_vals <= max(tick_vals, max_val, na.rm = TRUE)]
      axis_max <- max(tick_vals, max_val, na.rm = TRUE)
      tick_text <- vapply(tick_vals, format_integer, character(1))
      show_top_axis <- nrow(data) > 20L
      max_name_chars <- if (nrow(data) == 0L) 0L else max(nchar(as.character(data$LOCALIDADE)), na.rm = TRUE)
      left_margin <- max(180L, min(340L, 130L + (max_name_chars * 5L)))

      hover_style <- list(
        bgcolor = "white",
        bordercolor = "#0A1E3C",
        font = list(color = "black", size = 14, family = "Arial Black")
      )

      p <- plotly::plot_ly(data = data)
      bar_width <- min(0.18, 0.9 / max(length(years), 1L))
      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        marker_cfg <- list(color = year_color(year))
        if (year %in% preliminary_year) {
          marker_cfg$line <- list(color = "#0a1e3c", width = 1)
        }

        p <- plotly::add_bars(
          p,
          x = data[[value_col]],
          y = data$row_id,
          orientation = "h",
          width = bar_width,
          name = paste0("Ano ", year),
          marker = marker_cfg,
          opacity = if (year %in% preliminary_year) 0.9 else 1,
          textposition = "none",
          customdata = I(Map(c, as.character(data$LOCALIDADE), data[[formatted_col]])),
          hovertemplate = paste0("%{customdata[0]}<br>Ano ", year, " (", year_status(year), "): %{customdata[1]}<extra></extra>"),
          showlegend = FALSE
        )
      }

      p <- plotly::layout(
        p,
        barmode = "group",
        bargap = 0.16,
        xaxis = list(
          title = list(text = NULL, standoff = 0L),
          range = c(0, axis_max * 1.05),
          tickmode = "array",
          tickvals = tick_vals,
          ticktext = tick_text,
          tickangle = if (isTRUE(show_top_axis)) 0 else 90,
          tickfont = list(size = 10, color = "#000000"),
          showgrid = TRUE,
          gridcolor = "#D9DEE5",
          showline = TRUE,
          linecolor = "#C3CBD5",
          linewidth = 1,
          zeroline = FALSE,
          ticks = "outside",
          ticklen = 4,
          tickcolor = "#7F8A99",
          fixedrange = TRUE,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = axis_title, standoff = 10L),
          tickfont = list(size = 12, color = "#000000"),
          tickmode = "array",
          tickvals = data$row_id,
          ticktext = as.character(data$LOCALIDADE),
          range = c(nrow(data) + 0.5, 0.5),
          fixedrange = TRUE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        margin = list(l = left_margin, r = 20, t = 8, b = 52),
        hoverlabel = hover_style,
        hovermode = "closest",
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FBFCFE",
        showlegend = FALSE
      )

      hide_aps_modebar(p)
    }

    build_multiyear_coverage_box <- function(title, box_class, values) {
      build_tile <- function(year_label, subtitle, value, color, background, dashed = FALSE) {
        border_style <- if (isTRUE(dashed)) {
          paste0("1.5px dashed ", color)
        } else {
          paste0("1px solid ", color)
        }

        tags$div(
          style = paste0(
            "flex:1; min-width:0; border-radius:10px; padding:10px 8px 0 8px; ",
            "background-color:", background, "; border:", border_style, ";"
          ),
          tags$div(
            style = "display:flex; flex-direction:column; justify-content:space-between; height:100%; min-height:96px;",
            tags$div(
              style = "padding:0 2px 10px 2px;",
              tags$div(
                format_metric_percent(value),
                style = paste0("font-size:24px; font-weight:700; color:", color, "; line-height:1.1;")
              )
            ),
            tags$div(
              style = "margin-left:-8px; margin-right:-8px; background-color:#0A1E3C; color:#FFFFFF; padding:7px 8px; border-radius:0 0 9px 9px;",
              tags$div(
                year_label,
                style = "font-size:12px; font-weight:700; line-height:1.1;"
              ),
              tags$div(
                subtitle,
                style = "font-size:11px; opacity:0.95; margin-top:2px;"
              )
            )
          )
        )
      }

      div(
        class = paste("custom-box", box_class),
        style = "height:175px; display:flex; flex-direction:column; justify-content:center; align-items:stretch;",
        h4(title, style = "text-align:center; margin-bottom:12px;"),
        tags$div(
          style = "display:flex; gap:10px; width:100%; padding:0 10px 8px 10px;",
          build_tile(
            year_label = paste0("Ano ", cobertura_ab_legacy_year),
            subtitle = "Histórico",
            value = values$historico,
            color = "#6e7a8c",
            background = "#f4f6f8"
          ),
          build_tile(
            year_label = paste0("Ano ", cobertura_consolidated_year),
            subtitle = "Consolidado",
            value = values$consolidado,
            color = "#0a1e3c",
            background = "#eef3f9"
          ),
          build_tile(
            year_label = paste0("Ano ", cobertura_preliminary_year),
            subtitle = "Preliminar",
            value = values$preliminar,
            color = "#32a0ff",
            background = "#f1f9ff",
            dashed = TRUE
          )
        )
      )
    }

    format_metric_percent <- function(x) {
      if (!length(x) || is.na(x[1])) {
        return("-")
      }
      format(round(as.numeric(x[1]), 1), nsmall = 1, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }

    canonicalize_municipio_display <- function(x) {
      x_display <- toupper(as.character(x))
      x_display <- trimws(gsub("\\s+", " ", x_display))

      x_display
    }

    normalize_municipio_key <- function(x) {
      x_original <- canonicalize_municipio_display(x)
      x_ascii <- iconv(x_original, from = "", to = "ASCII//TRANSLIT")
      x_ascii[is.na(x_ascii)] <- x_original[is.na(x_ascii)]
      x_ascii <- gsub("[^A-Z0-9]+", " ", x_ascii)
      trimws(gsub("\\s+", " ", x_ascii))
    }

    # Dados base
    tabela_APS <- data_list$tabela_APS
    empty_tabela_APS <- tabela_APS[0, , drop = FALSE]

    aps_col_drs <- names(tabela_APS)[1]
    aps_col_regiao <- names(tabela_APS)[2]
    aps_col_municipio <- names(tabela_APS)[3]
    aps_col_nascidos <- names(tabela_APS)[4]
    aps_col_cobertura_ans <- names(tabela_APS)[5]
    aps_col_ubs <- names(tabela_APS)[6]
    aps_col_cobertura_esf <- names(tabela_APS)[7]
    aps_col_cobertura_ab <- names(tabela_APS)[8]
    aps_col_gestantes <- names(tabela_APS)[9]
    aps_col_nascidos_sus <- names(tabela_APS)[10]
    aps_col_rras <- names(tabela_APS)[11]
    aps_col_coord <- names(tabela_APS)[12]
    aps_col_supervisao <- names(tabela_APS)[13]

    aps_cols_sum <- c(
      aps_col_nascidos,
      aps_col_nascidos_sus,
      aps_col_ubs,
      aps_col_gestantes,
      aps_col_cobertura_ans,
      aps_col_cobertura_esf,
      aps_col_cobertura_ab
    )

    choices_from <- function(x) {
      as.character(sort(unique(na.omit(x))))
    }

    split_aps_by <- function(column) {
      split(tabela_APS, tabela_APS[[column]], drop = TRUE)
    }

    get_split_data <- function(index, key, empty = empty_tabela_APS) {
      if (!valid_choice(key)) {
        return(empty)
      }
      out <- index[[as.character(key)]]
      if (is.null(out)) empty else out
    }

    summarize_aps_by <- function(data, group_cols, cols_sum = aps_cols_sum) {
      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
          !all(c(group_cols, cols_sum) %in% names(data))) {
        return(data[0, c(group_cols, cols_sum), drop = FALSE])
      }

      dplyr::as_tibble(data) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(cols_sum), ~ sum(as.numeric(.), na.rm = TRUE)),
          .groups = "drop"
        ) |>
        as.data.frame()
    }

    parse_decimal <- function(x) {
      suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
    }

    aps_choices <- list(
      rras = choices_from(tabela_APS[[aps_col_rras]]),
      drs = choices_from(tabela_APS[[aps_col_drs]]),
      coordenadoria = choices_from(tabela_APS[[aps_col_coord]]),
      regiao = choices_from(tabela_APS[[aps_col_regiao]]),
      supervisao = choices_from(tabela_APS[[aps_col_supervisao]]),
      municipal = choices_from(tabela_APS[[aps_col_municipio]])
    )

    aps_by_rras <- split_aps_by(aps_col_rras)
    aps_by_drs <- split_aps_by(aps_col_drs)
    aps_by_coord <- split_aps_by(aps_col_coord)
    aps_by_regiao <- split_aps_by(aps_col_regiao)
    aps_by_supervisao <- split_aps_by(aps_col_supervisao)
    aps_by_municipio <- split_aps_by(aps_col_municipio)

    aps_estado_plot <- summarize_aps_by(tabela_APS, aps_col_rras)
    aps_drs_municipal_plot <- summarize_aps_by(tabela_APS, c(aps_col_drs, aps_col_municipio))
    aps_drs_municipal_plot_by_drs <- lapply(
      split(aps_drs_municipal_plot, aps_drs_municipal_plot[[aps_col_drs]], drop = TRUE),
      function(df) {
        df[[aps_col_drs]] <- NULL
        df
      }
    )

    total_sp_values <- list(
      cobertura_ans = parse_decimal(data_list$total_sp[[aps_col_cobertura_ans]]),
      cobertura_ab = parse_decimal(data_list$total_sp[[aps_col_cobertura_ab]])
    )

    cobertura_ab_aps <- data_list$cobertura_ab_aps
    cobertura_ab_aps$municipal <- cobertura_ab_aps$municipal |>
      dplyr::mutate(municipal_key = normalize_municipio_key(.data$municipal))
    cobertura_ab_by_year <- if (is.data.frame(cobertura_ab_aps$municipal) && nrow(cobertura_ab_aps$municipal)) {
      split(cobertura_ab_aps$municipal, cobertura_ab_aps$municipal$ano, drop = TRUE)
    } else {
      list()
    }
    coverage_year_data <- function(year_value, current_column = NULL) {
      if (length(year_value) != 1L || is.na(year_value)) {
        return(data.frame())
      }
      df <- cobertura_ab_by_year[[as.character(year_value)]]
      if (is.null(df)) {
        return(data.frame())
      }
      if (!is.null(current_column) && !(current_column %in% names(df))) {
        return(df[0, , drop = FALSE])
      }
      df
    }
    cobertura_ab_legacy_year <- 2020
    max_year_or_na <- function(years) {
      years <- stats::na.omit(as.integer(years))
      years <- years[is.finite(years)]
      if (length(years)) max(years) else NA_integer_
    }

    available_cobertura_years <- sort(unique(stats::na.omit(as.integer(cobertura_ab_aps$available_years))))
    cobertura_egestor_years <- available_cobertura_years[available_cobertura_years > cobertura_ab_legacy_year]
    cobertura_ab_latest_year <- max_year_or_na(cobertura_egestor_years)
    cobertura_preliminary_year <- cobertura_ab_latest_year
    cobertura_consolidated_years <- if (is.na(cobertura_preliminary_year)) {
      integer()
    } else {
      cobertura_egestor_years[cobertura_egestor_years < cobertura_preliminary_year]
    }
    cobertura_consolidated_year <- max_year_or_na(cobertura_consolidated_years)
    cobertura_display_years <- sort(unique(stats::na.omit(c(
      cobertura_ab_legacy_year,
      cobertura_consolidated_years,
      cobertura_preliminary_year
    ))))
    cobertura_esf_latest_year <- cobertura_ab_latest_year
    cobertura_esf_legacy_year <- cobertura_ab_legacy_year

    cobertura_ans_aps <- data_list$cobertura_ans_aps
    if (is.null(cobertura_ans_aps) || !is.list(cobertura_ans_aps)) {
      cobertura_ans_aps <- list(
        municipal = data.frame(),
        consolidated_years = 2020:2024,
        preliminary_year = 2025L
      )
    }
    if (is.data.frame(cobertura_ans_aps$municipal) && nrow(cobertura_ans_aps$municipal)) {
      cobertura_ans_aps$municipal <- cobertura_ans_aps$municipal |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          municipal = dplyr::if_else(.data$municipal_key == "SAO PAULO", "SÃO PAULO", .data$municipal),
          rras = toupper(as.character(.data$rras)),
          regiao_de_saude = toupper(as.character(.data$regiao_de_saude)),
          drs = toupper(as.character(.data$drs))
        )
    }
    cobertura_ans_consolidated_years <- sort(unique(stats::na.omit(as.integer(cobertura_ans_aps$consolidated_years))))
    cobertura_ans_preliminary_year <- suppressWarnings(as.integer(cobertura_ans_aps$preliminary_year))
    if (length(cobertura_ans_preliminary_year) != 1L || !is.finite(cobertura_ans_preliminary_year)) {
      cobertura_ans_preliminary_year <- NA_integer_
    }
    cobertura_ans_display_years <- sort(unique(stats::na.omit(as.integer(c(
      cobertura_ans_consolidated_years,
      cobertura_ans_preliminary_year
    )))))

    nascidos_vivos_aps <- data_list$nascidos_vivos_aps
    if (is.null(nascidos_vivos_aps) || !is.list(nascidos_vivos_aps)) {
      nascidos_vivos_aps <- list(
        municipal = data.frame(),
        supervisao = data.frame(),
        consolidated_years = 2020:2024,
        preliminary_year = NA_integer_
      )
    }
    if (is.data.frame(nascidos_vivos_aps$municipal) && nrow(nascidos_vivos_aps$municipal)) {
      nascidos_vivos_aps$municipal <- nascidos_vivos_aps$municipal |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          municipal = dplyr::if_else(.data$municipal_key == "SAO PAULO", "SÃO PAULO", .data$municipal)
        )
    }
    if (is.data.frame(nascidos_vivos_aps$supervisao) && nrow(nascidos_vivos_aps$supervisao)) {
      nascidos_vivos_aps$supervisao <- nascidos_vivos_aps$supervisao |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          supervisao_key = normalize_municipio_key(.data$supervisao_de_saude),
          coordenadoria_de_saude = toupper(as.character(.data$coordenadoria_de_saude)),
          regiao_de_saude = toupper(as.character(.data$regiao_de_saude)),
          drs = toupper(as.character(.data$drs))
        )
    }
    nascidos_consolidated_years <- sort(unique(stats::na.omit(as.integer(nascidos_vivos_aps$consolidated_years))))
    nascidos_preliminary_year <- suppressWarnings(as.integer(nascidos_vivos_aps$preliminary_year))
    if (length(nascidos_preliminary_year) != 1L || !is.finite(nascidos_preliminary_year)) {
      nascidos_preliminary_year <- NA_integer_
    }
    nascidos_municipal_years <- sort(unique(stats::na.omit(as.integer(c(nascidos_vivos_aps$municipal_years, nascidos_consolidated_years)))))
    nascidos_sp_years <- sort(unique(stats::na.omit(as.integer(c(nascidos_vivos_aps$sp_years, nascidos_consolidated_years, nascidos_preliminary_year)))))
    nascidos_default_summary_year <- if (2024L %in% nascidos_consolidated_years) 2024L else max_year_or_na(nascidos_consolidated_years)
    susdependente_display_years <- sort(unique(stats::na.omit(as.integer(intersect(
      2020:2024,
      intersect(nascidos_municipal_years, cobertura_ans_consolidated_years)
    )))))
    susdependente_default_summary_year <- if (2024L %in% susdependente_display_years) {
      2024L
    } else {
      max_year_or_na(susdependente_display_years)
    }

    ubs_cnes_aps <- data_list$ubs_cnes_aps
    if (is.null(ubs_cnes_aps) || !is.list(ubs_cnes_aps)) {
      ubs_cnes_aps <- list(
        municipal = data.frame(),
        consolidated_years = 2022:2025,
        preliminary_year = NA_integer_
      )
    }
    if (is.data.frame(ubs_cnes_aps$municipal) && nrow(ubs_cnes_aps$municipal)) {
      ubs_cnes_aps$municipal <- ubs_cnes_aps$municipal |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          municipal = dplyr::if_else(.data$municipal_key == "SAO PAULO", "SÃO PAULO", .data$municipal),
          rras = toupper(as.character(.data$rras)),
          regiao_de_saude = toupper(as.character(.data$regiao_de_saude)),
          drs = toupper(as.character(.data$drs)),
          n_ubs = suppressWarnings(as.numeric(.data$n_ubs))
        )
    }
    ubs_cnes_consolidated_years <- sort(unique(stats::na.omit(as.integer(ubs_cnes_aps$consolidated_years))))
    ubs_cnes_display_years <- sort(unique(stats::na.omit(as.integer(intersect(2022:2025, ubs_cnes_consolidated_years)))))
    ubs_cnes_default_summary_year <- if (2025L %in% ubs_cnes_display_years) {
      2025L
    } else {
      max_year_or_na(ubs_cnes_display_years)
    }

    ubs_cnes_year_color <- function(year) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }

      all_years <- sort(unique(stats::na.omit(as.integer(ubs_cnes_display_years))))
      if (!length(all_years)) {
        return("#0a1e3c")
      }

      palette <- grDevices::colorRampPalette(c("#bfe7ff", "#32a0ff", "#0a1e3c"))(length(all_years))
      year_index <- match(year, all_years)
      if (is.na(year_index)) {
        return("#0a1e3c")
      }

      palette[[year_index]]
    }

    is_submunicipal_sp_context <- reactive({
      req(input$nivel_selection)

      (identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) ||
        (identical(input$nivel_selection, "RRAS") && identical(input$secondary_filter, "RRAS 6")) ||
        (identical(input$nivel_selection, "REGIÃO DE SAÚDE") && identical(input$secondary_filter, "SÃO PAULO")) ||
        (identical(input$nivel_selection, "MUNICIPAL") && identical(input$analisar_muni_sp, "SIM"))
    })

    is_updated_ab_context <- reactive({
      req(input$nivel_selection)

      input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL") &&
        !isTRUE(is_submunicipal_sp_context()) &&
        !is.null(cobertura_ab_latest_year) &&
        is.finite(cobertura_ab_latest_year)
    })

    is_updated_ans_context <- reactive({
      req(input$nivel_selection)

      input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL") &&
        !isTRUE(is_submunicipal_sp_context()) &&
        length(cobertura_ans_display_years) > 0L
    })

    is_updated_susdependente_context <- reactive({
      req(input$nivel_selection)

      input$nivel_selection %in% c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL") &&
        !isTRUE(is_submunicipal_sp_context()) &&
        length(susdependente_display_years) > 0L
    })

    is_updated_ubs_context <- reactive({
      req(input$nivel_selection)

      input$nivel_selection %in% c("ESTADUAL", "RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL") &&
        !isTRUE(is_submunicipal_sp_context()) &&
        length(ubs_cnes_display_years) > 0L
    })

    is_scrollable_municipal_chart_context <- reactive({
      req(input$nivel_selection)

      input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE") &&
        !isTRUE(is_submunicipal_sp_context())
    })

    is_nascidos_sp_context <- reactive({
      req(input$nivel_selection)

      (identical(input$nivel_selection, "RRAS") && identical(input$secondary_filter, "RRAS 6")) ||
        (identical(input$nivel_selection, "REGIÃO DE SAÚDE") && identical(input$secondary_filter, "SÃO PAULO")) ||
        (identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) ||
        (identical(input$nivel_selection, "MUNICIPAL") && identical(input$analisar_muni_sp, "SIM"))
    })

    nascidos_display_years <- reactive({
      if (isTRUE(is_nascidos_sp_context())) {
        return(nascidos_sp_years)
      }
      nascidos_municipal_years
    })

    nascidos_caption <- reactive({
      years <- nascidos_display_years()
      preliminary <- if (nascidos_preliminary_year %in% years) nascidos_preliminary_year else NA_integer_
      consolidated <- if (length(preliminary) == 1L && !is.na(preliminary)) {
        years[years != preliminary]
      } else {
        years
      }
      build_nascidos_caption_legend(consolidated, preliminary)
    })

    nascidos_context_long <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      municipal <- nascidos_vivos_aps$municipal
      supervisao <- nascidos_vivos_aps$supervisao
      empty <- data.frame(
        ano = integer(),
        LOCALIDADE = character(),
        nascidos_vivos = numeric(),
        stringsAsFactors = FALSE
      )

      if (isTRUE(is_nascidos_sp_context())) {
        if (is.null(supervisao) || !is.data.frame(supervisao) || nrow(supervisao) == 0L) {
          return(empty)
        }

        df <- supervisao
        if (identical(level, "DRS")) {
          if (!valid_choice(input$secondary_filter)) return(empty)
          df <- dplyr::filter(df, .data$coordenadoria_de_saude == input$secondary_filter)
        } else if (identical(level, "MUNICIPAL")) {
          if (!valid_choice(input$secondary_filter)) return(empty)
          selected_key <- normalize_municipio_key(input$secondary_filter)
          df <- dplyr::filter(df, .data$supervisao_key == selected_key)
        }

        return(
          df |>
            dplyr::transmute(
              ano = as.integer(.data$ano),
              LOCALIDADE = as.character(.data$supervisao_de_saude),
              nascidos_vivos = suppressWarnings(as.numeric(.data$nascidos_vivos))
            )
        )
      }

      if (is.null(municipal) || !is.data.frame(municipal) || nrow(municipal) == 0L) {
        return(empty)
      }

      df <- municipal |>
        dplyr::filter(.data$ano %in% nascidos_municipal_years)

      if (identical(level, "RRAS")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$rras == input$secondary_filter)
      } else if (identical(level, "DRS")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$drs == input$secondary_filter)
      } else if (identical(level, "REGIÃO DE SAÚDE")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter)
      } else if (identical(level, "MUNICIPAL")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        selected_key <- normalize_municipio_key(input$secondary_filter)
        df <- dplyr::filter(df, .data$municipal_key == selected_key)
      }

      localidade_col <- if (identical(level, "ESTADUAL")) "rras" else "municipal"
      df |>
        dplyr::transmute(
          ano = as.integer(.data$ano),
          LOCALIDADE = as.character(.data[[localidade_col]]),
          nascidos_vivos = suppressWarnings(as.numeric(.data$nascidos_vivos))
        )
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, nascidos_municipal_years, nascidos_sp_years, cache = "app")

    nascidos_vivos_multiyear_data <- reactive({
      years <- nascidos_display_years()
      df <- nascidos_context_long()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !length(years)) {
        return(data.frame())
      }

      df |>
        dplyr::filter(
          .data$ano %in% years,
          normalize_municipio_key(.data$LOCALIDADE) != "IGNORADO"
        ) |>
        dplyr::group_by(.data$LOCALIDADE, .data$ano) |>
        dplyr::summarise(nascidos_vivos = sum(.data$nascidos_vivos, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = .data$ano,
          values_from = .data$nascidos_vivos,
          names_prefix = "valor_"
        ) |>
        as.data.frame()
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, nascidos_display_years(), cache = "app")

    nascidos_summary_year_choices <- reactive({
      if (isTRUE(is_nascidos_sp_context()) && !is.na(nascidos_preliminary_year)) {
        return(sort(unique(stats::na.omit(c(2023L, nascidos_default_summary_year, nascidos_preliminary_year)))))
      }
      sort(unique(stats::na.omit(c(2023L, nascidos_default_summary_year))))
    })

    selected_nascidos_summary_year <- reactive({
      choices <- nascidos_summary_year_choices()
      selected <- suppressWarnings(as.integer(input$nascidos_vivos_summary_year))
      if (length(selected) == 1L && !is.na(selected) && selected %in% choices) {
        return(selected)
      }
      nascidos_default_summary_year
    })

    nascidos_summary_total <- reactive({
      year <- selected_nascidos_summary_year()
      df <- nascidos_context_long()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        return(NA_real_)
      }
      sum(df$nascidos_vivos[df$ano == year], na.rm = TRUE)
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, selected_nascidos_summary_year(), cache = "app")

    ubs_cnes_caption <- reactive({
      build_nascidos_caption_legend(
        consolidated_years = ubs_cnes_display_years,
        preliminary_year = NA_integer_,
        year_color = ubs_cnes_year_color
      )
    })

    ubs_cnes_context_long <- reactive({
      req(input$nivel_selection)

      empty <- data.frame(
        ano = integer(),
        LOCALIDADE = character(),
        n_ubs = numeric(),
        stringsAsFactors = FALSE
      )

      if (!isTRUE(is_updated_ubs_context())) {
        return(empty)
      }

      municipal <- ubs_cnes_aps$municipal
      if (is.null(municipal) || !is.data.frame(municipal) || nrow(municipal) == 0L) {
        return(empty)
      }

      df <- municipal |>
        dplyr::filter(.data$ano %in% ubs_cnes_display_years)

      level <- input$nivel_selection
      if (identical(level, "RRAS")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$rras == input$secondary_filter)
      } else if (identical(level, "DRS")) {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_sp, "SIM")) return(empty)
        df <- dplyr::filter(df, .data$drs == input$secondary_filter)
      } else if (identical(level, "REGIÃO DE SAÚDE")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter)
      } else if (identical(level, "MUNICIPAL")) {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_muni_sp, "SIM")) return(empty)
        selected_key <- normalize_municipio_key(input$secondary_filter)
        df <- dplyr::filter(df, .data$municipal_key == selected_key)
      } else if (!identical(level, "ESTADUAL")) {
        return(empty)
      }

      localidade_col <- if (identical(level, "ESTADUAL")) "rras" else "municipal"
      df |>
        dplyr::transmute(
          ano = as.integer(.data$ano),
          LOCALIDADE = as.character(.data[[localidade_col]]),
          n_ubs = suppressWarnings(as.numeric(.data$n_ubs))
        )
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, ubs_cnes_display_years, cache = "app")

    ubs_cnes_multiyear_data <- reactive({
      years <- ubs_cnes_display_years
      df <- ubs_cnes_context_long()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !length(years)) {
        return(data.frame())
      }

      df |>
        dplyr::filter(
          .data$ano %in% years,
          normalize_municipio_key(.data$LOCALIDADE) != "IGNORADO"
        ) |>
        dplyr::group_by(.data$LOCALIDADE, .data$ano) |>
        dplyr::summarise(n_ubs = sum(.data$n_ubs, na.rm = TRUE), .groups = "drop") |>
        tidyr::pivot_wider(
          names_from = .data$ano,
          values_from = .data$n_ubs,
          names_prefix = "valor_"
        ) |>
        as.data.frame()
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, ubs_cnes_display_years, cache = "app")

    ubs_cnes_summary_year_choices <- reactive({
      ubs_cnes_display_years
    })

    selected_ubs_cnes_summary_year <- reactive({
      choices <- ubs_cnes_summary_year_choices()
      selected <- suppressWarnings(as.integer(input$ubs_cnes_summary_year))
      if (length(selected) == 1L && !is.na(selected) && selected %in% choices) {
        return(selected)
      }
      ubs_cnes_default_summary_year
    })

    ubs_cnes_summary_total <- reactive({
      year <- selected_ubs_cnes_summary_year()
      df <- ubs_cnes_context_long()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || length(year) != 1L || is.na(year)) {
        return(NA_real_)
      }

      sum(df$n_ubs[df$ano == year], na.rm = TRUE)
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, selected_ubs_cnes_summary_year(), cache = "app")

    susdependente_caption <- reactive({
      build_nascidos_caption_legend(susdependente_display_years, NA_integer_)
    })

    build_summary_year_dropdown <- function(input_id, choices, selected_year) {
      choices <- sort(unique(stats::na.omit(as.integer(choices))))
      if (!length(choices)) {
        return(NULL)
      }

      tags$div(
        style = "position:absolute; top:6px; left:6px; width:72px;",
        tags$select(
          id = ns(input_id),
          class = "form-control",
          style = "height:25px; padding:1px 18px 1px 6px; font-size:11px; border-radius:4px; border:1px solid rgba(10,30,60,0.25); background-color:rgba(255,255,255,0.92); color:#0A1E3C;",
          lapply(choices, function(year) {
            tags$option(
              value = as.character(year),
              selected = if (identical(as.integer(year), as.integer(selected_year))) "selected" else NULL,
              as.character(year)
            )
          })
        )
      )
    }

    susdependentes_context_long <- reactive({
      req(input$nivel_selection)

      empty <- data.frame(
        ano = integer(),
        LOCALIDADE = character(),
        nascidos_susdependentes = numeric(),
        gestantes_susdependentes = numeric(),
        stringsAsFactors = FALSE
      )

      if (!isTRUE(is_updated_susdependente_context())) {
        return(empty)
      }

      nascidos <- nascidos_vivos_aps$municipal
      ans <- cobertura_ans_aps$municipal
      if (is.null(nascidos) || !is.data.frame(nascidos) || nrow(nascidos) == 0L ||
          is.null(ans) || !is.data.frame(ans) || nrow(ans) == 0L) {
        return(empty)
      }

      years <- susdependente_display_years
      if (!length(years)) {
        return(empty)
      }

      nascidos_base <- nascidos |>
        dplyr::filter(.data$ano %in% years) |>
        dplyr::transmute(
          ano = as.integer(.data$ano),
          municipal_key = normalize_municipio_key(.data$municipal),
          municipal = canonicalize_municipio_display(.data$municipal),
          rras = as.character(.data$rras),
          drs = as.character(.data$drs),
          regiao_de_saude = as.character(.data$regiao_de_saude),
          nascidos_vivos = suppressWarnings(as.numeric(.data$nascidos_vivos))
        )

      ans_base <- ans |>
        dplyr::filter(.data$ano %in% years) |>
        dplyr::transmute(
          ano = as.integer(.data$ano),
          municipal_key = normalize_municipio_key(.data$municipal),
          cobertura_ans = suppressWarnings(as.numeric(.data$cobertura_ans))
        )

      df <- nascidos_base |>
        dplyr::left_join(ans_base, by = c("ano", "municipal_key")) |>
        dplyr::mutate(
          fator_sus = 1 - (.data$cobertura_ans / 100),
          nascidos_susdependentes = .data$nascidos_vivos * .data$fator_sus,
          gestantes_susdependentes = (.data$nascidos_vivos * 1.10) * .data$fator_sus
        )

      level <- input$nivel_selection
      if (identical(level, "RRAS")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$rras == input$secondary_filter)
      } else if (identical(level, "DRS")) {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_sp, "SIM")) return(empty)
        df <- dplyr::filter(df, .data$drs == input$secondary_filter)
      } else if (identical(level, "REGIÃO DE SAÚDE")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter)
      } else if (identical(level, "MUNICIPAL")) {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_muni_sp, "SIM")) return(empty)
        selected_key <- normalize_municipio_key(input$secondary_filter)
        df <- dplyr::filter(df, .data$municipal_key == selected_key)
      } else if (!identical(level, "ESTADUAL")) {
        return(empty)
      }

      localidade_col <- if (identical(level, "ESTADUAL")) "rras" else "municipal"
      df |>
        dplyr::transmute(
          ano = .data$ano,
          LOCALIDADE = as.character(.data[[localidade_col]]),
          nascidos_susdependentes = .data$nascidos_susdependentes,
          gestantes_susdependentes = .data$gestantes_susdependentes
        )
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, susdependente_display_years, cache = "app")

    build_susdependente_multiyear_data <- function(value_col) {
      reactive({
        years <- susdependente_display_years
        df <- susdependentes_context_long()
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !length(years) ||
            !(value_col %in% names(df))) {
          return(data.frame())
        }

        out <- df |>
          dplyr::filter(
            .data$ano %in% years,
            normalize_municipio_key(.data$LOCALIDADE) != "IGNORADO"
          ) |>
          dplyr::group_by(.data$LOCALIDADE, .data$ano) |>
          dplyr::summarise(
            valor = {
              values <- suppressWarnings(as.numeric(.data[[value_col]]))
              if (length(values) && any(!is.na(values))) sum(values, na.rm = TRUE) else NA_real_
            },
            .groups = "drop"
          ) |>
          tidyr::pivot_wider(
            names_from = .data$ano,
            values_from = .data$valor,
            names_prefix = "valor_"
          ) |>
          as.data.frame()

        value_cols <- paste0("valor_", years)
        for (col in value_cols) {
          if (!(col %in% names(out))) {
            out[[col]] <- NA_real_
          }
        }

        out |>
          dplyr::select(LOCALIDADE, dplyr::all_of(value_cols))
      }) %>%
        shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, value_col, susdependente_display_years, cache = "app")
    }

    nascidos_susdependentes_multiyear_data <- build_susdependente_multiyear_data("nascidos_susdependentes")
    gestantes_susdependentes_multiyear_data <- build_susdependente_multiyear_data("gestantes_susdependentes")

    susdependente_summary_year_choices <- reactive({
      susdependente_display_years
    })

    selected_susdependente_summary_year <- function(input_id) {
      reactive({
        choices <- susdependente_summary_year_choices()
        selected <- suppressWarnings(as.integer(input[[input_id]]))
        if (length(selected) == 1L && !is.na(selected) && selected %in% choices) {
          return(selected)
        }
        susdependente_default_summary_year
      })
    }

    selected_nascidos_susdependentes_summary_year <- selected_susdependente_summary_year("nascidos_susdependentes_summary_year")
    selected_gestantes_susdependentes_summary_year <- selected_susdependente_summary_year("gestantes_susdependentes_summary_year")

    susdependente_summary_total <- function(value_col, year) {
      df <- susdependentes_context_long()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
          !(value_col %in% names(df)) || length(year) != 1L || is.na(year)) {
        return(NA_real_)
      }

      values <- suppressWarnings(as.numeric(df[[value_col]][df$ano == year]))
      if (length(values) && any(!is.na(values))) {
        sum(values, na.rm = TRUE)
      } else {
        NA_real_
      }
    }

    make_scroll_card_options <- function(data, var_numeric, is_percentage = FALSE, bars_per_group = 1L) {
      if (!isTRUE(is_scrollable_municipal_chart_context()) ||
          is.null(data) || !is.data.frame(data) || nrow(data) <= 20L) {
        return(NULL)
      }

      height_cfg <- calc_multiyear_grouped_height(
        n_groups = nrow(data),
        visible_groups = 20L,
        bars_per_group = bars_per_group
      )

      list(
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height,
        fixed_axis = build_fixed_axis_legend(
          compute_axis_spec(data, var_numeric = var_numeric, is_percentage = is_percentage)
        )
      )
    }

    cobertura_ab_plot_caption <- reactive({
      if (isTRUE(is_updated_ab_context()) && input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL")) {
        paste0(
          "Anos: ",
          cobertura_ab_legacy_year,
          " (histórico), ",
          format_year_sequence(cobertura_consolidated_years),
          " (consolidados) e ",
          cobertura_preliminary_year,
          " (preliminar)"
        )
      } else {
        "Ano: 2020"
      }
    })

    cobertura_esf_plot_caption <- reactive({
      if (isTRUE(is_updated_ab_context()) && input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL")) {
        paste0(
          "Anos: ",
          cobertura_esf_legacy_year,
          " (histórico), ",
          format_year_sequence(cobertura_consolidated_years),
          " (consolidados) e ",
          cobertura_preliminary_year,
          " (preliminar)"
        )
      } else {
        "Ano: 2020"
      }
    })

    cobertura_ans_caption <- reactive({
      years <- cobertura_ans_display_years
      preliminary <- if (cobertura_ans_preliminary_year %in% years) cobertura_ans_preliminary_year else NA_integer_
      consolidated <- if (length(preliminary) == 1L && !is.na(preliminary)) {
        years[years != preliminary]
      } else {
        years
      }

      build_cobertura_ans_caption_legend(
        consolidated_years = consolidated,
        preliminary_year = preliminary
      )
    })

    secondary_filter_config <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (identical(level, "ESTADUAL")) {
        return(NULL)
      }

      if (identical(level, "DRS")) {
        if (identical(input$analisar_sp, "SIM")) {
          return(list(
            label = "Selecione a coordenadoria de saúde:",
            choices = aps_choices$coordenadoria
          ))
        }

        return(list(
          label = "Selecione a DRS:",
          choices = aps_choices$drs
        ))
      }

      if (identical(level, "RRAS")) {
        return(list(
          label = "Selecione a RRAS:",
          choices = aps_choices$rras
        ))
      }

      if (identical(level, "REGIÃO DE SAÚDE")) {
        return(list(
          label = "Selecione a região de saúde:",
          choices = aps_choices$regiao
        ))
      }

      if (identical(level, "MUNICIPAL")) {
        if (identical(input$analisar_muni_sp, "SIM")) {
          return(list(
            label = "Selecione a supervisão de saúde:",
            choices = aps_choices$supervisao
          ))
        }

        return(list(
          label = "Selecione o município:",
          choices = aps_choices$municipal
        ))
      }

      list(label = "Selecione:", choices = character())
    })

    aps_plot_cache_key <- reactive({
      list(
        modulo = "aps2_linhas",
        nivel = input$nivel_selection,
        filtro = input$secondary_filter,
        analisar_sp = input$analisar_sp,
        analisar_muni_sp = input$analisar_muni_sp,
        ab_consolidados = cobertura_consolidated_years,
        ab_preliminar = cobertura_preliminary_year,
        ans_anos = cobertura_ans_display_years,
        ans_preliminar = cobertura_ans_preliminary_year,
        nascidos_anos = nascidos_display_years(),
        susdependentes_anos = susdependente_display_years,
        ubs_anos = ubs_cnes_display_years
      )
    })

    # Atualiza o filtro secundário conforme o nível selecionado
    output$secondary_filter_ui <- renderUI({
      cfg <- secondary_filter_config()
      if (is.null(cfg)) {
        return(NULL)
      }

      shinyWidgets::pickerInput(
        inputId = ns("secondary_filter"),
        label = cfg$label,
        choices = cfg$choices,
        selected = if (length(cfg$choices)) cfg$choices[1] else NULL,
        options = list("live-search" = TRUE)
      )
    })

    # Filtra dados conforme o nível e o filtro secundário
    filtered_data <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "ESTADUAL") {
        return(tabela_APS)
      }

      if (level == "RRAS") {
        return(get_split_data(aps_by_rras, input$secondary_filter))
      }

      if (level == "DRS") {
        if (identical(input$analisar_sp, "SIM")) {
          return(get_split_data(aps_by_coord, input$secondary_filter))
        } else {
          return(get_split_data(aps_by_drs, input$secondary_filter))
        }
      }

      if (level == "REGIÃO DE SAÚDE") {
        return(get_split_data(aps_by_regiao, input$secondary_filter))
      }

      if (level == "MUNICIPAL") {
        if (identical(input$analisar_muni_sp, "SIM")) {
          return(get_split_data(aps_by_supervisao, input$secondary_filter))
        } else {
          return(get_split_data(aps_by_municipio, input$secondary_filter))
        }
      }

      empty_tabela_APS
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, cache = "app")

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

      if (level == "ESTADUAL") {
        return(aps_estado_plot)
      }

      if (level == "DRS" && !identical(input$analisar_sp, "SIM")) {
        if (!valid_choice(input$secondary_filter)) {
          return(empty_tabela_APS)
        }
        df <- aps_drs_municipal_plot_by_drs[[as.character(input$secondary_filter)]]
        return(if (is.null(df)) empty_tabela_APS else df)
      }

      # Demais níveis usam o filtro pronto
      filtered_data()
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, cache = "app")

    # Versão robusta que injeta "SÃO PAULO" na DRS Grande SP quando necessário
    plot_data_main <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection

      if (level == "DRS" && !identical(input$analisar_sp, "SIM")) {
        agg <- plot_data()

        if (identical(input$secondary_filter, "GRANDE SÃO PAULO") && !("SÃO PAULO" %in% agg$MUNICIPAL)) {
          df2 <- get_split_data(aps_by_municipio, "SÃO PAULO")

          sp <- data.frame(
            v1 = "SÃO PAULO",
            v2 = sum(df2[[aps_col_nascidos]], na.rm = TRUE),
            v3 = sum(df2[[aps_col_nascidos_sus]], na.rm = TRUE),
            v4 = sum(df2[[aps_col_ubs]], na.rm = TRUE),
            v5 = sum(df2[[aps_col_gestantes]], na.rm = TRUE),
            v6 = sum(total_sp_values$cobertura_ans, na.rm = TRUE),
            v7 = NA_real_,
            v8 = sum(total_sp_values$cobertura_ab, na.rm = TRUE),
            check.names = FALSE
          )
          names(sp) <- c(
            aps_col_municipio,
            aps_col_nascidos,
            aps_col_nascidos_sus,
            aps_col_ubs,
            aps_col_gestantes,
            aps_col_cobertura_ans,
            aps_col_cobertura_esf,
            aps_col_cobertura_ab
          )
          agg <- rbind(agg, sp)
        }
        return(agg)
      }

      if (level == "ESTADUAL") {
        return(plot_data())
      }

      filtered_data()
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, cache = "app")

    # Plot para não considerar SP (casos específicos como cobertura ESF)
    plot_data_cov <- reactive({
      df <- plot_data_main()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)
      # Se a DRS selecionada for GRANDE SÃO PAULO, remove SP apenas quando houver coluna MUNICIPAL
      if (identical(input$nivel_selection, "DRS") &&
          identical(input$secondary_filter, "GRANDE SÃO PAULO") &&
          "MUNICIPAL" %in% names(df)) {
        df <- dplyr::filter(df, MUNICIPAL != "SÃO PAULO")
      }
      df
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, cache = "app")

    cobertura_ab_atual_municipal <- reactive({
      req(input$nivel_selection)

      df <- cobertura_ab_aps$municipal
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        return(data.frame())
      }

      df <- dplyr::filter(df, .data$ano == cobertura_ab_latest_year)
      level <- input$nivel_selection

      if (level == "RRAS") {
        if (!valid_choice(input$secondary_filter)) return(df[0, , drop = FALSE])
        return(dplyr::filter(df, .data$rras == input$secondary_filter))
      }

      if (level == "DRS") {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_sp, "SIM")) {
          return(df[0, , drop = FALSE])
        }
        return(dplyr::filter(df, .data$drs == input$secondary_filter))
      }

      if (level == "REGIÃO DE SAÚDE") {
        if (!valid_choice(input$secondary_filter)) return(df[0, , drop = FALSE])
        return(dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter))
      }

      if (level == "MUNICIPAL") {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_muni_sp, "SIM")) {
          return(df[0, , drop = FALSE])
        }
        return(dplyr::filter(df, .data$municipal == input$secondary_filter))
      }

      df[0, , drop = FALSE]
    })

    cobertura_ab_legado_municipal <- reactive({
      req(input$nivel_selection)

      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"))) {
        return(data.frame(MUNICIPAL = character(), cobertura_ab_legado = numeric()))
      }

      if (identical(input$nivel_selection, "MUNICIPAL")) {
        if (identical(input$analisar_muni_sp, "SIM") || !valid_choice(input$secondary_filter)) {
          return(data.frame(MUNICIPAL = character(), cobertura_ab_legado = numeric()))
        }

        if (identical(input$secondary_filter, "SÃO PAULO")) {
          total_sp <- data_list$total_sp
          legacy_value <- suppressWarnings(as.numeric(gsub(",", ".", total_sp$`COBERTURA AB %`)))
          return(data.frame(MUNICIPAL = "SÃO PAULO", cobertura_ab_legado = legacy_value))
        }

        data <- filtered_data()
        legacy_value <- round(mean(data$`COBERTURA AB %`, na.rm = TRUE), 1)
        return(data.frame(MUNICIPAL = input$secondary_filter, cobertura_ab_legado = legacy_value))
      }

      df <- plot_data_main()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
          !("MUNICIPAL" %in% names(df)) || !("COBERTURA AB %" %in% names(df))) {
        return(data.frame(MUNICIPAL = character(), cobertura_ab_legado = numeric()))
      }

      df |>
        dplyr::transmute(
          MUNICIPAL = toupper(as.character(.data$MUNICIPAL)),
          cobertura_ab_legado = suppressWarnings(as.numeric(.data$`COBERTURA AB %`))
        )
    })

    cobertura_ab_comparison_data <- reactive({
      req(input$nivel_selection)

      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE")) || !isTRUE(is_updated_ab_context())) {
        return(data.frame())
      }

      legado <- cobertura_ab_legado_municipal()
      atual <- cobertura_ab_atual_municipal() |>
        dplyr::transmute(
          MUNICIPAL = toupper(as.character(.data$municipal)),
          cobertura_ab_atual = .data$cobertura_ab
        )

      dplyr::full_join(legado, atual, by = "MUNICIPAL") |>
        dplyr::filter(!is.na(.data$MUNICIPAL)) |>
        dplyr::arrange(.data$MUNICIPAL)
    })
    cobertura_ab_card_values <- reactive({
      req(input$nivel_selection == "MUNICIPAL")

      if (!isTRUE(is_updated_ab_context())) {
        return(NULL)
      }

      legado <- cobertura_ab_legado_municipal()
      atual <- cobertura_ab_atual_municipal()

      list(
        legacy = if (nrow(legado)) legado$cobertura_ab_legado[1] else NA_real_,
        current = if (nrow(atual)) atual$cobertura_ab[1] else NA_real_
      )
    })

    parse_legacy_coverage_value <- function(x) {
      suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
    }

    get_updated_coverage_current <- function(current_column) {
      reactive({
        req(input$nivel_selection)

        df <- coverage_year_data(cobertura_ab_latest_year, current_column)
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
          return(data.frame())
        }

        level <- input$nivel_selection

        if (level == "RRAS") {
          if (!valid_choice(input$secondary_filter)) return(df[0, , drop = FALSE])
          return(dplyr::filter(df, .data$rras == input$secondary_filter))
        }

        if (level == "DRS") {
          if (!valid_choice(input$secondary_filter) || identical(input$analisar_sp, "SIM")) {
            return(df[0, , drop = FALSE])
          }
          return(dplyr::filter(df, .data$drs == input$secondary_filter))
        }

        if (level == "REGIÃO DE SAÚDE") {
          if (!valid_choice(input$secondary_filter)) return(df[0, , drop = FALSE])
          return(dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter))
        }

        if (level == "MUNICIPAL") {
          if (!valid_choice(input$secondary_filter) || identical(input$analisar_muni_sp, "SIM")) {
            return(df[0, , drop = FALSE])
          }
          municipio_key <- normalize_municipio_key(input$secondary_filter)
          return(
            df |>
              dplyr::filter(.data$municipal_key == municipio_key)
          )
        }

        df[0, , drop = FALSE]
      }) %>%
        shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, current_column, cobertura_ab_latest_year, cache = "app")
    }

    get_legacy_coverage_data <- function(legacy_column) {
      reactive({
        req(input$nivel_selection)

        if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL"))) {
          return(data.frame(MUNICIPAL = character(), valor_legado = numeric()))
        }

        if (identical(input$nivel_selection, "MUNICIPAL")) {
          if (identical(input$analisar_muni_sp, "SIM") || !valid_choice(input$secondary_filter)) {
            return(data.frame(MUNICIPAL = character(), valor_legado = numeric()))
          }

          if (identical(input$secondary_filter, "SÃO PAULO")) {
            total_sp <- data_list$total_sp
            legacy_value <- if (legacy_column %in% names(total_sp)) {
              parse_legacy_coverage_value(total_sp[[legacy_column]])
            } else {
              NA_real_
            }

            if (!length(legacy_value) || all(is.na(legacy_value))) {
              legacy_value <- NA_real_
            } else {
              legacy_value <- legacy_value[1]
            }

            return(data.frame(MUNICIPAL = "SÃO PAULO", valor_legado = legacy_value))
          }

          data <- filtered_data()
          legacy_values <- if (legacy_column %in% names(data)) {
            parse_legacy_coverage_value(data[[legacy_column]])
          } else {
            numeric()
          }

          legacy_value <- if (!length(legacy_values) || all(is.na(legacy_values))) {
            NA_real_
          } else {
            round(mean(legacy_values, na.rm = TRUE), 1)
          }

          return(data.frame(MUNICIPAL = input$secondary_filter, valor_legado = legacy_value))
        }

        df <- plot_data_main()
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
            !("MUNICIPAL" %in% names(df)) || !(legacy_column %in% names(df))) {
          return(data.frame(MUNICIPAL = character(), valor_legado = numeric()))
        }

        df |>
          dplyr::transmute(
            MUNICIPAL = toupper(as.character(.data$MUNICIPAL)),
            valor_legado = parse_legacy_coverage_value(.data[[legacy_column]])
          )
      }) %>%
        shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, legacy_column, cache = "app")
    }

    build_coverage_comparison_data <- function(current_reactive, current_column, legacy_reactive) {
      reactive({
        req(input$nivel_selection)

        if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE")) || !isTRUE(is_updated_ab_context())) {
          return(data.frame())
        }

        legado <- legacy_reactive()
        atual <- current_reactive() |>
          dplyr::transmute(
            MUNICIPAL = toupper(as.character(.data$municipal)),
            valor_atual = suppressWarnings(as.numeric(.data[[current_column]]))
          )

        dplyr::full_join(legado, atual, by = "MUNICIPAL") |>
          dplyr::filter(!is.na(.data$MUNICIPAL)) |>
          dplyr::arrange(.data$MUNICIPAL)
      }) %>%
        shiny::bindCache(
          input$nivel_selection,
          input$secondary_filter,
          input$analisar_sp,
          input$analisar_muni_sp,
          current_column,
          cobertura_ab_latest_year,
          cache = "app"
        )
    }

    build_coverage_card_values <- function(current_reactive, current_column, legacy_reactive) {
      reactive({
        req(input$nivel_selection == "MUNICIPAL")

        if (!isTRUE(is_updated_ab_context())) {
          return(NULL)
        }

        legado <- legacy_reactive()
        atual <- current_reactive()

        list(
          legacy = if (nrow(legado)) legado$valor_legado[1] else NA_real_,
          current = if (nrow(atual)) suppressWarnings(as.numeric(atual[[current_column]][1])) else NA_real_
        )
      })
    }

    cobertura_ab_atual_municipal <- get_updated_coverage_current("cobertura_ab")
    cobertura_esf_atual_municipal <- get_updated_coverage_current("cobertura_esf")

    cobertura_ab_legado_municipal <- get_legacy_coverage_data("COBERTURA AB %")
    cobertura_esf_legado_municipal <- get_legacy_coverage_data("COBERTURA ESF %")

    cobertura_ab_comparison_data <- build_coverage_comparison_data(
      current_reactive = cobertura_ab_atual_municipal,
      current_column = "cobertura_ab",
      legacy_reactive = cobertura_ab_legado_municipal
    )

    cobertura_esf_comparison_data <- build_coverage_comparison_data(
      current_reactive = cobertura_esf_atual_municipal,
      current_column = "cobertura_esf",
      legacy_reactive = cobertura_esf_legado_municipal
    )

    cobertura_ab_card_values <- build_coverage_card_values(
      current_reactive = cobertura_ab_atual_municipal,
      current_column = "cobertura_ab",
      legacy_reactive = cobertura_ab_legado_municipal
    )

    cobertura_esf_card_values <- build_coverage_card_values(
      current_reactive = cobertura_esf_atual_municipal,
      current_column = "cobertura_esf",
      legacy_reactive = cobertura_esf_legado_municipal
    )

    get_updated_coverage_year <- function(current_column, year_value) {
      reactive({
        req(input$nivel_selection)

        df <- coverage_year_data(year_value, current_column)
        if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
          return(data.frame())
        }

        level <- input$nivel_selection

        if (level == "RRAS") {
          if (!valid_choice(input$secondary_filter)) return(df[0, , drop = FALSE])
          return(dplyr::filter(df, .data$rras == input$secondary_filter))
        }

        if (level == "DRS") {
          if (!valid_choice(input$secondary_filter) || identical(input$analisar_sp, "SIM")) {
            return(df[0, , drop = FALSE])
          }
          return(dplyr::filter(df, .data$drs == input$secondary_filter))
        }

        if (level == "REGIÃO DE SAÚDE") {
          if (!valid_choice(input$secondary_filter)) return(df[0, , drop = FALSE])
          return(dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter))
        }

        if (level == "MUNICIPAL") {
          if (!valid_choice(input$secondary_filter) || identical(input$analisar_muni_sp, "SIM")) {
            return(df[0, , drop = FALSE])
          }
          municipio_key <- normalize_municipio_key(input$secondary_filter)
          return(
            df |>
              dplyr::filter(.data$municipal_key == municipio_key)
          )
        }

        df[0, , drop = FALSE]
      }) %>%
        shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, current_column, year_value, cache = "app")
    }

    build_coverage_multiyear_data <- function(legacy_reactive, updated_reactives, current_column) {
      reactive({
        req(input$nivel_selection)

        if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL")) || !isTRUE(is_updated_ab_context())) {
          return(data.frame())
        }

        first_text_value <- function(x) {
          x <- x[!is.na(x) & nzchar(x)]
          if (length(x)) x[[1]] else NA_character_
        }

        first_num_value <- function(x) {
          x <- x[!is.na(x)]
          if (length(x)) x[[1]] else NA_real_
        }

        collapse_coverage_values <- function(data, display_source, value_source, display_col, value_col) {
          empty <- data.frame(
            municipio_key = character(),
            display_tmp = character(),
            value_tmp = numeric(),
            stringsAsFactors = FALSE
          )
          names(empty)[2:3] <- c(display_col, value_col)

          if (is.null(data) || !is.data.frame(data) || nrow(data) == 0L ||
              !(display_source %in% names(data)) || !(value_source %in% names(data))) {
            return(empty)
          }

          out <- data.frame(
            municipio_key = normalize_municipio_key(data[[display_source]]),
            display_tmp = canonicalize_municipio_display(data[[display_source]]),
            value_tmp = suppressWarnings(as.numeric(data[[value_source]])),
            stringsAsFactors = FALSE
          )

          out |>
            dplyr::filter(!is.na(.data$municipio_key), nzchar(.data$municipio_key)) |>
            dplyr::group_by(.data$municipio_key) |>
            dplyr::summarise(
              display_tmp = first_text_value(.data$display_tmp),
              value_tmp = first_num_value(.data$value_tmp),
              .groups = "drop"
            ) |>
            stats::setNames(c("municipio_key", display_col, value_col))
        }

        pieces <- list(
          collapse_coverage_values(
            legacy_reactive(),
            display_source = "MUNICIPAL",
            value_source = "valor_legado",
            display_col = paste0("MUNICIPAL_", cobertura_ab_legacy_year),
            value_col = paste0("valor_", cobertura_ab_legacy_year)
          )
        )

        for (year_name in names(updated_reactives)) {
          year_value <- as.integer(year_name)
          pieces[[paste0("ano_", year_name)]] <- collapse_coverage_values(
            updated_reactives[[year_name]](),
            display_source = "municipal",
            value_source = current_column,
            display_col = paste0("MUNICIPAL_", year_value),
            value_col = paste0("valor_", year_value)
          )
        }

        combined <- Reduce(
          function(x, y) dplyr::full_join(x, y, by = "municipio_key"),
          pieces
        )

        display_cols <- paste0("MUNICIPAL_", cobertura_display_years)
        value_cols <- paste0("valor_", cobertura_display_years)
        for (col in display_cols) {
          if (!(col %in% names(combined))) {
            combined[[col]] <- NA_character_
          }
        }
        for (col in value_cols) {
          if (!(col %in% names(combined))) {
            combined[[col]] <- NA_real_
          }
        }

        municipal <- rep(NA_character_, nrow(combined))
        for (col in display_cols) {
          replace_idx <- is.na(municipal) | !nzchar(municipal)
          municipal[replace_idx] <- combined[[col]][replace_idx]
        }

        out <- data.frame(
          MUNICIPAL = municipal,
          combined[, value_cols, drop = FALSE],
          check.names = FALSE
        )
        out <- out[!is.na(out$MUNICIPAL) & nzchar(out$MUNICIPAL), , drop = FALSE]
        sort_key <- iconv(toupper(as.character(out$MUNICIPAL)), from = "", to = "ASCII//TRANSLIT")
        sort_key[is.na(sort_key)] <- toupper(as.character(out$MUNICIPAL)[is.na(sort_key)])
        out[order(sort_key, out$MUNICIPAL, na.last = TRUE), , drop = FALSE]
      }) %>%
        shiny::bindCache(
          input$nivel_selection,
          input$secondary_filter,
          input$analisar_sp,
          input$analisar_muni_sp,
          current_column,
          names(updated_reactives),
          cobertura_display_years,
          cache = "app"
        )
    }

    build_coverage_multiyear_card_values <- function(legacy_reactive,
                                                     consolidated_reactive,
                                                     consolidated_column,
                                                     preliminary_reactive,
                                                     preliminary_column) {
      reactive({
        req(input$nivel_selection == "MUNICIPAL")

        if (!isTRUE(is_updated_ab_context())) {
          return(NULL)
        }

        historico <- legacy_reactive()
        consolidado <- consolidated_reactive()
        preliminar <- preliminary_reactive()

        list(
          historico = if (nrow(historico)) historico$valor_legado[1] else NA_real_,
          consolidado = if (nrow(consolidado)) suppressWarnings(as.numeric(consolidado[[consolidated_column]][1])) else NA_real_,
          preliminar = if (nrow(preliminar)) suppressWarnings(as.numeric(preliminar[[preliminary_column]][1])) else NA_real_
        )
      }) %>%
        shiny::bindCache(
          input$nivel_selection,
          input$secondary_filter,
          input$analisar_muni_sp,
          consolidated_column,
          preliminary_column,
          cobertura_consolidated_year,
          cobertura_preliminary_year,
          cache = "app"
        )
    }

    cobertura_year_names <- as.character(cobertura_egestor_years)
    cobertura_ab_updated_municipal_by_year <- stats::setNames(
      lapply(cobertura_egestor_years, function(year) get_updated_coverage_year("cobertura_ab", year)),
      cobertura_year_names
    )
    cobertura_esf_updated_municipal_by_year <- stats::setNames(
      lapply(cobertura_egestor_years, function(year) get_updated_coverage_year("cobertura_esf", year)),
      cobertura_year_names
    )

    cobertura_ab_consolidated_municipal <- get_updated_coverage_year("cobertura_ab", cobertura_consolidated_year)
    cobertura_ab_preliminary_municipal <- get_updated_coverage_year("cobertura_ab", cobertura_preliminary_year)
    cobertura_esf_consolidated_municipal <- get_updated_coverage_year("cobertura_esf", cobertura_consolidated_year)
    cobertura_esf_preliminary_municipal <- get_updated_coverage_year("cobertura_esf", cobertura_preliminary_year)

    cobertura_ab_comparison_data <- build_coverage_multiyear_data(
      legacy_reactive = cobertura_ab_legado_municipal,
      updated_reactives = cobertura_ab_updated_municipal_by_year,
      current_column = "cobertura_ab"
    )

    cobertura_esf_comparison_data <- build_coverage_multiyear_data(
      legacy_reactive = cobertura_esf_legado_municipal,
      updated_reactives = cobertura_esf_updated_municipal_by_year,
      current_column = "cobertura_esf"
    )

    cobertura_ab_card_values <- build_coverage_multiyear_card_values(
      legacy_reactive = cobertura_ab_legado_municipal,
      consolidated_reactive = cobertura_ab_consolidated_municipal,
      consolidated_column = "cobertura_ab",
      preliminary_reactive = cobertura_ab_preliminary_municipal,
      preliminary_column = "cobertura_ab"
    )

    cobertura_esf_card_values <- build_coverage_multiyear_card_values(
      legacy_reactive = cobertura_esf_legado_municipal,
      consolidated_reactive = cobertura_esf_consolidated_municipal,
      consolidated_column = "cobertura_esf",
      preliminary_reactive = cobertura_esf_preliminary_municipal,
      preliminary_column = "cobertura_esf"
    )

    cobertura_ans_context_long <- reactive({
      req(input$nivel_selection)

      empty <- data.frame(
        ano = integer(),
        MUNICIPAL = character(),
        cobertura_ans = numeric(),
        stringsAsFactors = FALSE
      )

      if (!isTRUE(is_updated_ans_context())) {
        return(empty)
      }

      df <- cobertura_ans_aps$municipal
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L) {
        return(empty)
      }

      df <- dplyr::filter(df, .data$ano %in% cobertura_ans_display_years)
      level <- input$nivel_selection

      if (identical(level, "RRAS")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$rras == input$secondary_filter)
      } else if (identical(level, "DRS")) {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_sp, "SIM")) return(empty)
        df <- dplyr::filter(df, .data$drs == input$secondary_filter)
      } else if (identical(level, "REGIÃO DE SAÚDE")) {
        if (!valid_choice(input$secondary_filter)) return(empty)
        df <- dplyr::filter(df, .data$regiao_de_saude == input$secondary_filter)
      } else if (identical(level, "MUNICIPAL")) {
        if (!valid_choice(input$secondary_filter) || identical(input$analisar_muni_sp, "SIM")) return(empty)
        selected_key <- normalize_municipio_key(input$secondary_filter)
        df <- dplyr::filter(df, .data$municipal_key == selected_key)
      } else {
        return(empty)
      }

      df |>
        dplyr::transmute(
          ano = as.integer(.data$ano),
          MUNICIPAL = as.character(.data$municipal),
          cobertura_ans = suppressWarnings(as.numeric(.data$cobertura_ans))
        )
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, cobertura_ans_display_years, cache = "app")

    cobertura_ans_multiyear_data <- reactive({
      years <- cobertura_ans_display_years
      df <- cobertura_ans_context_long()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0L || !length(years)) {
        return(data.frame())
      }

      out <- df |>
        dplyr::filter(.data$ano %in% years) |>
        dplyr::group_by(.data$MUNICIPAL, .data$ano) |>
        dplyr::summarise(
          cobertura_ans = {
            values <- .data$cobertura_ans[!is.na(.data$cobertura_ans)]
            if (length(values)) values[[1]] else NA_real_
          },
          .groups = "drop"
        ) |>
        tidyr::pivot_wider(
          names_from = .data$ano,
          values_from = .data$cobertura_ans,
          names_prefix = "valor_"
        ) |>
        as.data.frame()

      value_cols <- paste0("valor_", years)
      for (col in value_cols) {
        if (!(col %in% names(out))) {
          out[[col]] <- NA_real_
        }
      }

      out |>
        dplyr::select(MUNICIPAL, dplyr::all_of(value_cols))
    }) %>%
      shiny::bindCache(input$nivel_selection, input$secondary_filter, input$analisar_sp, input$analisar_muni_sp, cobertura_ans_display_years, cache = "app")

    ## Tabelas de estabelecimentos antigas
    # # Dados para tabelas de AAE
    # filtered_data_aae <- reactive({
    #   req(input$nivel_selection)
    #   level <- input$nivel_selection
    #
    #   if (level == "RRAS") {
    #     req(isTruthy(input$secondary_filter))
    #     switch(
    #       input$secondary_filter,
    #       "RRAS 1"  = data_list$tabela_1_APS_AAE,
    #       "RRAS 2"  = data_list$tabela_2_APS_AAE,
    #       "RRAS 3"  = data_list$tabela_3_APS_AAE,
    #       "RRAS 4"  = data_list$tabela_4_APS_AAE,
    #       "RRAS 5"  = data_list$tabela_5_APS_AAE,
    #       "RRAS 6"  = data_list$tabela_6_APS_AAE,
    #       "RRAS 7"  = data_list$tabela_7_APS_AAE,
    #       "RRAS 8"  = data_list$tabela_8_APS_AAE,
    #       "RRAS 9"  = data_list$tabela_9_APS_AAE,
    #       "RRAS 10" = data_list$tabela_10_APS_AAE,
    #       "RRAS 11" = data_list$tabela_11_APS_AAE,
    #       "RRAS 12" = data_list$tabela_12_APS_AAE,
    #       "RRAS 13" = data_list$tabela_13_APS_AAE,
    #       "RRAS 14" = data_list$tabela_14_APS_AAE,
    #       "RRAS 15" = data_list$tabela_15_APS_AAE,
    #       "RRAS 16" = data_list$tabela_16_APS_AAE,
    #       "RRAS 17" = data_list$tabela_17_APS_AAE,
    #       "RRAS 18" = data_list$tabela_18_APS_AAE
    #     )
    #   } else {
    #     table_aae_all <- dplyr::bind_rows(
    #       data_list$tabela_1_APS_AAE,  data_list$tabela_2_APS_AAE,
    #       data_list$tabela_3_APS_AAE,  data_list$tabela_4_APS_AAE,
    #       data_list$tabela_5_APS_AAE,  data_list$tabela_6_APS_AAE,
    #       data_list$tabela_7_APS_AAE,  data_list$tabela_8_APS_AAE,
    #       data_list$tabela_9_APS_AAE,  data_list$tabela_10_APS_AAE,
    #       data_list$tabela_11_APS_AAE, data_list$tabela_12_APS_AAE,
    #       data_list$tabela_13_APS_AAE, data_list$tabela_14_APS_AAE,
    #       data_list$tabela_15_APS_AAE, data_list$tabela_16_APS_AAE,
    #       data_list$tabela_17_APS_AAE, data_list$tabela_18_APS_AAE
    #     )
    #
    #     if (level == "ESTADUAL") {
    #       return(table_aae_all)
    #     }
    #
    #     req(isTruthy(input$secondary_filter))
    #
    #     if (level == "DRS") {
    #       if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
    #         # Preferência: filtrar pela própria coluna se ela existir;
    #         # fallback: mapear municípios da coordenadoria via tabela_APS
    #         if ("COORDENADORIA DE SAÚDE" %in% names(table_aae_all)) {
    #           dados <- table_aae_all[table_aae_all$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
    #         } else {
    #           munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`COORDENADORIA DE SAÚDE` == input$secondary_filter])
    #           dados <- table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` %in% munis, ]
    #         }
    #       } else {
    #         dados <- table_aae_all[table_aae_all$DRS == input$secondary_filter, ]
    #       }
    #       dados <- dados |>
    #         dplyr::rename(`MUNICÍPIO DA DRS` = `MUNICÍPIO DA RRAS`) |>
    #         dplyr::filter(!if_all(dplyr::everything(), is.na))
    #       return(dados)
    #     }
    #
    #     if (level == "REGIÃO DE SAÚDE") {
    #       dados <- table_aae_all[table_aae_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ] |>
    #         dplyr::rename(`MUNICÍPIO DA REGIÃO DE SAÚDE` = `MUNICÍPIO DA RRAS`)
    #       return(dados)
    #     }
    #
    #     if (level == "MUNICIPAL") {
    #       if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
    #         # secondary_filter é uma SUPERVISÃO; mapeia p/ municípios e filtra
    #         munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`SUPERVISÃO DE SAÚDE` == input$secondary_filter])
    #         dados <- table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` %in% munis, ]
    #       } else {
    #         dados <- table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
    #       }
    #       return(dados)
    #     }
    #   }
    # })
    #
    #
    # # Dados para tabelas de BAIXO RISCO
    # filtered_data_bxr <- reactive({
    #   req(input$nivel_selection)
    #   level <- input$nivel_selection
    #
    #   if (level == "RRAS") {
    #     req(isTruthy(input$secondary_filter))
    #     switch(
    #       input$secondary_filter,
    #       "RRAS 1"  = data_list$tabela_1_APS_BXRISCO,
    #       "RRAS 2"  = data_list$tabela_2_APS_BXRISCO,
    #       "RRAS 3"  = data_list$tabela_3_APS_BXRISCO,
    #       "RRAS 4"  = data_list$tabela_4_APS_BXRISCO,
    #       "RRAS 5"  = data_list$tabela_5_APS_BXRISCO,
    #       "RRAS 6"  = data_list$tabela_6_APS_BXRISCO,
    #       "RRAS 7"  = data_list$tabela_7_APS_BXRISCO,
    #       "RRAS 8"  = data_list$tabela_8_APS_BXRISCO,
    #       "RRAS 9"  = data_list$tabela_9_APS_BXRISCO,
    #       "RRAS 10" = data_list$tabela_10_APS_BXRISCO,
    #       "RRAS 11" = data_list$tabela_11_APS_BXRISCO,
    #       "RRAS 12" = data_list$tabela_12_APS_BXRISCO,
    #       "RRAS 13" = data_list$tabela_13_APS_BXRISCO,
    #       "RRAS 14" = data_list$tabela_14_APS_BXRISCO,
    #       "RRAS 15" = data_list$tabela_15_APS_BXRISCO,
    #       "RRAS 16" = data_list$tabela_16_APS_BXRISCO,
    #       "RRAS 17" = data_list$tabela_17_APS_BXRISCO,
    #       "RRAS 18" = data_list$tabela_18_APS_BXRISCO
    #     )
    #   } else {
    #     table_bxr_all <- dplyr::bind_rows(
    #       data_list$tabela_1_APS_BXRISCO,  data_list$tabela_2_APS_BXRISCO,
    #       data_list$tabela_3_APS_BXRISCO,  data_list$tabela_4_APS_BXRISCO,
    #       data_list$tabela_5_APS_BXRISCO,  data_list$tabela_6_APS_BXRISCO,
    #       data_list$tabela_7_APS_BXRISCO,  data_list$tabela_8_APS_BXRISCO,
    #       data_list$tabela_9_APS_BXRISCO,  data_list$tabela_10_APS_BXRISCO,
    #       data_list$tabela_11_APS_BXRISCO, data_list$tabela_12_APS_BXRISCO,
    #       data_list$tabela_13_APS_BXRISCO, data_list$tabela_14_APS_BXRISCO,
    #       data_list$tabela_15_APS_BXRISCO, data_list$tabela_16_APS_BXRISCO,
    #       data_list$tabela_17_APS_BXRISCO, data_list$tabela_18_APS_BXRISCO
    #     )
    #
    #     if (level == "ESTADUAL") {
    #       return(table_bxr_all)
    #     }
    #
    #     req(isTruthy(input$secondary_filter))
    #
    #     if (level == "DRS") {
    #       if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
    #         if ("COORDENADORIA DE SAÚDE" %in% names(table_bxr_all)) {
    #           dados <- table_bxr_all[table_bxr_all$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
    #         } else {
    #           munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`COORDENADORIA DE SAÚDE` == input$secondary_filter])
    #           dados <- table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` %in% munis, ]
    #         }
    #       } else {
    #         dados <- table_bxr_all[table_bxr_all$DRS == input$secondary_filter, ]
    #       }
    #       dados <- dados |>
    #         dplyr::rename(`MUNICÍPIO DA DRS` = `MUNICÍPIO DA RRAS`) |>
    #         dplyr::filter(!if_all(dplyr::everything(), is.na))
    #       return(dados)
    #     }
    #
    #     if (level == "REGIÃO DE SAÚDE") {
    #       dados <- table_bxr_all[table_bxr_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ] |>
    #         dplyr::rename(`MUNICÍPIO DA REGIÃO DE SAÚDE` = `MUNICÍPIO DA RRAS`)
    #       return(dados)
    #     }
    #
    #     if (level == "MUNICIPAL") {
    #       if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
    #         munis <- unique(tabela_APS$MUNICIPAL[tabela_APS$`SUPERVISÃO DE SAÚDE` == input$secondary_filter])
    #         dados <- table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` %in% munis, ]
    #       } else {
    #         dados <- table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
    #       }
    #       return(dados)
    #     }
    #   }
    # })
    #
    # # Renderiza as tabelas
    # output$table_aae <- DT::renderDT({
    #   data <- filtered_data_aae()
    #   validate(
    #     need(!is.null(data) && ncol(data) > 0, "Dados não disponíveis para exibição")
    #   )
    #   DT::datatable(
    #     data,
    #     options = list(
    #       pageLength = -1,
    #       autoWidth  = TRUE,
    #       scrollX    = TRUE,
    #       scrollY    = "400px",
    #       scrollCollapse = TRUE,
    #       paging     = FALSE,
    #       dom        = 't',
    #       columnDefs = list(
    #         list(className = "dt-center", targets = "_all"),
    #         list(width = '10%', targets = "_all")
    #       )
    #     ),
    #     rownames = FALSE,
    #     class = "compact stripe hover nowrap"
    #   ) |> DT::formatStyle(
    #     columns = names(filtered_data_aae()),
    #     `padding-left` = '0px',
    #     `padding-right` = '0px'
    #   )
    # })
    #
    # output$table_bxr <- DT::renderDT({
    #   data <- filtered_data_bxr()
    #   validate(
    #     need(!is.null(data) && ncol(data) > 0, "Dados não disponíveis para exibição")
    #   )
    #   DT::datatable(
    #     data,
    #     options = list(
    #       pageLength = -1,
    #       autoWidth  = TRUE,
    #       scrollX    = TRUE,
    #       scrollY    = "400px",
    #       scrollCollapse = TRUE,
    #       paging     = FALSE,
    #       dom        = 't',
    #       columnDefs = list(
    #         list(className = "dt-center", targets = "_all"),
    #         list(width = '10%', targets = "_all")
    #       )
    #     ),
    #     rownames = FALSE,
    #     class = "compact stripe hover nowrap"
    #   ) |> DT::formatStyle(
    #     columns = names(filtered_data_bxr()),
    #     `padding-left` = '0px',
    #     `padding-right` = '0px'
    #   )
    # })

    output$aps_graph_tabs <- renderUI({
      req(input$nivel_selection)

      plot_col <- function(output_id, width = 4L, offset = 0L) {
        column(
          width = width,
          offset = offset,
          shinycssloaders::withSpinner(uiOutput(ns(output_id)))
        )
      }

      is_municipal <- identical(input$nivel_selection, "MUNICIPAL")
      is_municipal_sp <- is_municipal && identical(input$analisar_muni_sp, "SIM")
      is_sp_supervisao_context <- (
        (identical(input$nivel_selection, "RRAS") && identical(input$secondary_filter, "RRAS 6")) ||
          (identical(input$nivel_selection, "REGIÃO DE SAÚDE") && identical(input$secondary_filter, "SÃO PAULO")) ||
          (identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM"))
      )

      nascidos_sus_card <- if (identical(input$nivel_selection, "ESTADUAL")) {
        "card_plot_nascidos_susdependentes_estadual"
      } else if (is_sp_supervisao_context) {
        "card_plot_nascidos_susdependentes_rras6"
      } else {
        "card_plot_nascidos_susdependentes_outros"
      }

      dependencia_sus <- if (is_municipal) {
        if (is_municipal_sp) {
          tagList(
            fluidRow(plot_col("card_plot_nascidos_vivos_municipal", width = 12L))
          )
        } else {
          tagList(
            fluidRow(
              plot_col("card_plot_nascidos_vivos_municipal"),
              plot_col("card_plot_nascidos_susdependentes_municipal"),
              plot_col("card_plot_gestantes_susdependentes_municipal")
            )
          )
        }
      } else {
        tagList(
          fluidRow(
            plot_col("card_plot_nascidos_vivos"),
            plot_col(nascidos_sus_card),
            plot_col("card_plot_gestantes_susdependentes")
          )
        )
      }

      cobertura_assistencial <- if (is_municipal) {
        if (is_municipal_sp) {
          tagList()
        } else {
          tagList(
            fluidRow(
              plot_col("card_plot_ubs_municipal", width = 6L),
              plot_col("card_plot_cobertura_ans_municipal", width = 6L)
            ),
            br(),
            fluidRow(
              plot_col("card_plot_cobertura_esf_municipal", width = 6L),
              plot_col("card_plot_cobertura_ab_municipal", width = 6L)
            )
          )
        }
      } else if (is_sp_supervisao_context) {
        tagList(
          fluidRow(
            plot_col("card_plot_ubs"),
            plot_col("card_plot_cobertura_ans_rras6"),
            plot_col("card_plot_cobertura_ab_rras6")
          )
        )
      } else if (identical(input$nivel_selection, "ESTADUAL")) {
        tagList(
          fluidRow(plot_col("card_plot_ubs", width = 12L))
        )
      } else {
        tagList(
          fluidRow(
            plot_col("card_plot_ubs", width = 6L),
            plot_col("card_plot_cobertura_ans", width = 6L)
          ),
          br(),
          fluidRow(
            plot_col("card_plot_cobertura_esf", width = 6L),
            plot_col("card_plot_cobertura_ab", width = 6L)
          )
        )
      }

      fluidRow(
        column(
          width = 12,
          tags$div(
            class = "estab-tabs-prenatal aps2-graph-tabs",
            bs4Dash::tabBox(
              id = ns("aps_graph_tabbox"),
              title = NULL,
              side = "left",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              type = "tabs",
              selected = "Dependência do SUS",
              shiny::tabPanel("Dependência do SUS", dependencia_sus),
              shiny::tabPanel("Cobertura assistencial", cobertura_assistencial)
            )
          )
        )
      )
    })

    # Caixas resumo principais
    output$summary_boxes_ui <- renderUI({
      req(input$nivel_selection)

      if (identical(input$nivel_selection, "MUNICIPAL") &&
          !identical(input$analisar_muni_sp, "SIM")) {
        return(NULL)
      }

      fluidRow(
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_1")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_3")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_4")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_2"))))
      )
    })

    output$summary_box_1 <- renderUI({
      total_nascidos <- if (identical(input$nivel_selection, "MUNICIPAL")) {
        df <- nascidos_context_long()
        year <- nascidos_default_summary_year
        round(sum(df$nascidos_vivos[df$ano == year], na.rm = TRUE))
      } else {
        round(nascidos_summary_total())
      }
      summary_year <- if (identical(input$nivel_selection, "MUNICIPAL")) {
        nascidos_default_summary_year
      } else {
        selected_nascidos_summary_year()
      }
      caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
        tags$div(
          paste0("Ano de atualização dos dados: ", summary_year),
          style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
        )
      } else {
        NULL
      }
      dropdown <- if (!identical(input$nivel_selection, "MUNICIPAL")) {
        choices <- nascidos_summary_year_choices()
        tags$div(
          style = "position:absolute; top:6px; left:6px; width:72px;",
          tags$select(
            id = ns("nascidos_vivos_summary_year"),
            class = "form-control",
            style = "height:25px; padding:1px 18px 1px 6px; font-size:11px; border-radius:4px; border:1px solid rgba(10,30,60,0.25); background-color:rgba(255,255,255,0.92); color:#0A1E3C;",
            lapply(choices, function(year) {
              tags$option(
                value = as.character(year),
                selected = if (identical(as.integer(year), as.integer(summary_year))) "selected" else NULL,
                as.character(year)
              )
            })
          )
        )
      } else {
        NULL
      }
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4("Nascidos vivos"),
        h3(format_number(total_nascidos)),
        caption
      )
    })

    output$summary_box_2 <- renderUI({
      if (isTRUE(is_updated_susdependente_context())) {
        summary_year <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          susdependente_default_summary_year
        } else {
          selected_nascidos_susdependentes_summary_year()
        }
        total_sus_nasc <- susdependente_summary_total("nascidos_susdependentes", summary_year)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          tags$div(
            paste0("Ano de atualização dos dados: ", summary_year),
            style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
          )
        } else {
          NULL
        }
        dropdown <- if (!identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_dropdown(
            "nascidos_susdependentes_summary_year",
            susdependente_summary_year_choices(),
            summary_year
          )
        } else {
          NULL
        }
      } else {
        total_sus_nasc <- sum(filtered_data()[["NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO"]], na.rm = TRUE)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          tags$div(
            "Ano de atualização dos dados: 2023",
            style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
          )
        } else {
          NULL
        }
        dropdown <- NULL
      }
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4("Nascidos vivos SUSdependentes estimados/ano"),
        h3(format_number(round(total_sus_nasc, 0))),
        caption
      )
    })

    output$summary_box_3 <- renderUI({
      if (isTRUE(is_updated_ubs_context())) {
        summary_year <- selected_ubs_cnes_summary_year()
        total_ubs <- ubs_cnes_summary_total()
        dropdown <- build_summary_year_dropdown(
          "ubs_cnes_summary_year",
          ubs_cnes_summary_year_choices(),
          summary_year
        )
        caption <- NULL
      } else {
        total_ubs <- sum(filtered_data()[["Nº DE UBS"]], na.rm = TRUE)
        dropdown <- NULL
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          tags$div(
            "Ano de atualização dos dados: 2022/2023",
            style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
          )
        } else {
          NULL
        }
      }
      div(
        class = "custom-box box-danger",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4(
          "Unidade Básica de Saúde (UBS)",
          style = "font-size:16px; line-height:1.2; text-align:center; margin:0 8px 6px 8px;"
        ),
        h3(format_number(round(total_ubs, 0))),
        caption
      )
    })

    output$summary_box_4 <- renderUI({
      if (isTRUE(is_updated_susdependente_context())) {
        summary_year <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          susdependente_default_summary_year
        } else {
          selected_gestantes_susdependentes_summary_year()
        }
        total_gestantes <- susdependente_summary_total("gestantes_susdependentes", summary_year)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          tags$div(
            paste0("Ano de atualização dos dados: ", summary_year),
            style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
          )
        } else {
          NULL
        }
        dropdown <- if (!identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_dropdown(
            "gestantes_susdependentes_summary_year",
            susdependente_summary_year_choices(),
            summary_year
          )
        } else {
          NULL
        }
      } else {
        total_gestantes <- sum(filtered_data()[["GESTANTES SUSDEPENDENTES ESTIMADAS/ANO"]], na.rm = TRUE)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          tags$div(
            "Ano de atualização dos dados: 2023",
            style = "position: absolute; bottom: 0px; left: 0; right: 0; font-size: 12px; color: #FFFFFF; background-color: #0A1E3C; padding: 3px 10px; border-radius: 0 0 3px 3px; text-align: center; box-sizing: border-box;"
          )
        } else {
          NULL
        }
        dropdown <- NULL
      }
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4("Gestantes SUSdependentes estimadas/ano"),
        h3(format_number(round(total_gestantes, 0))),
        caption
      )
    })

    output$municipal_extras <- renderUI({
      if(input$nivel_selection != "MUNICIPAL") return(NULL)

      # Condição: se o usuário selecionou supervisão (analisar_muni_sp = "SIM")
      # OU se o filtro secundário for "SÃO PAULO" em contexto legado,
      # então não exibe o box de Cobertura ESF.
      if(identical(input$analisar_muni_sp, "SIM") ||
         (identical(input$secondary_filter, "SÃO PAULO") && !isTRUE(is_updated_ab_context()))) {
        tagList(
          fluidRow(
            column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
            column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
          ),
          br(),
          fluidRow(
            column(width = 12, shinycssloaders::withSpinner(uiOutput(ns("card_plot_nascidos_vivos_municipal"))))
          )
        )
      } else if (isTRUE(is_updated_ab_context())) {
        tagList(
          fluidRow(
            column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_nascidos_vivos_municipal")))),
            column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_ubs_municipal")))),
            column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_gestantes_susdependentes_municipal"))))
          ),
          br(),
          fluidRow(
            column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_nascidos_susdependentes_municipal")))),
            column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_cobertura_ans_municipal")))),
            column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_cobertura_esf_municipal"))))
          ),
          br(),
          fluidRow(
            column(width = 4, offset = 4, shinycssloaders::withSpinner(uiOutput(ns("card_plot_cobertura_ab_municipal"))))
          )
        )
      } else {
        tagList(
          fluidRow(
            column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_1")))),
            column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_2")))),
            column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("extra_summary_box_3"))))
          ),
          br(),
          fluidRow(
            column(width = 12, shinycssloaders::withSpinner(uiOutput(ns("card_plot_nascidos_vivos_municipal"))))
          )
        )
      }
    })

    # Caixas resumo extras para nível MUNICIPAL
    output$extra_summary_box_1 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (identical(input$secondary_filter, "SÃO PAULO")) {
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
      if (isTRUE(is_updated_ab_context())) {
        return(
          build_multiyear_coverage_box(
            title = "Cobertura ESF (%)",
            box_class = "box-success",
            values = cobertura_esf_card_values()
          )
        )
      }

      if (identical(input$secondary_filter, "SÃO PAULO")) {
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
      if (isTRUE(is_updated_ab_context())) {
        return(
          build_multiyear_coverage_box(
            title = "Cobertura AB (%)",
            box_class = "box-warning",
            values = cobertura_ab_card_values()
          )
        )
      }
      req(input$nivel_selection == "MUNICIPAL")
      if (identical(input$secondary_filter, "SÃO PAULO")) {
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

    output$card_plot_nascidos_vivos <- renderUI({
      req(input$nivel_selection)
      if(identical(input$nivel_selection, "MUNICIPAL")) return(NULL)
      data_source <- nascidos_vivos_multiyear_data()
      years <- nascidos_display_years()
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        bars_per_group = length(years)
      )
      target_visible_height <- if (identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) {
        400L
      } else if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(data_source) > 20L) {
        calc_multiyear_grouped_height(
          nrow(data_source),
          visible_groups = 20L,
          bars_per_group = 1L
        )$visible_height
      } else {
        calc_dynamic_height(nrow(data_source))
      }
      if (isTRUE(is_nascidos_sp_context())) {
        target_visible_height <- target_visible_height - 20L
      }
      target_visible_height <- max(320L, as.integer(target_visible_height))

      build_plot_card(
        "Nascidos vivos",
        "plot_nascidos_vivos",
        data_source,
        caption = nascidos_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = target_visible_height
      )
    })

    output$card_plot_ubs <- renderUI({
      req(input$nivel_selection)
      if(identical(input$nivel_selection, "MUNICIPAL")) return(NULL)
      if (isTRUE(is_updated_ubs_context())) {
        data_source <- ubs_cnes_multiyear_data()
        years <- ubs_cnes_display_years
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(years)
        )
        target_visible_height <- if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(data_source) > 20L) {
          calc_multiyear_grouped_height(
            nrow(data_source),
            visible_groups = 20L,
            bars_per_group = 1L
          )$visible_height
        } else {
          calc_dynamic_height(nrow(data_source))
        }

        return(
          build_plot_card(
            "Unidade Básica de Saúde (UBS)",
            "plot_ubs",
            data_source,
            caption = ubs_cnes_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(target_visible_height))
          )
        )
      }

      data_source <- plot_data_main()
      if(identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Unidade Básica de Saúde (UBS)", "plot_ubs", plot_data(), caption = "Ano: 2022/2023", height_override = 400)
      } else {
        opts <- make_scroll_card_options(data_source, "NÂº DE UBS")
        if (!is.null(opts)) {
          return(do.call(build_plot_card, c(list("Unidade Básica de Saúde (UBS)", "plot_ubs", data_source, caption = "Ano: 2022/2023"), opts)))
        }
        build_plot_card("Unidade Básica de Saúde (UBS)", "plot_ubs", data_source, caption = "Ano: 2022/2023")
      }
    })
    output$card_plot_gestantes_susdependentes <- renderUI({
      req(input$nivel_selection)
      if(identical(input$nivel_selection, "MUNICIPAL")) return(NULL)
      if (isTRUE(is_updated_susdependente_context())) {
        data_source <- gestantes_susdependentes_multiyear_data()
        years <- susdependente_display_years
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(years)
        )
        target_visible_height <- if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(data_source) > 20L) {
          calc_multiyear_grouped_height(
            nrow(data_source),
            visible_groups = 20L,
            bars_per_group = 1L
          )$visible_height
        } else {
          calc_dynamic_height(nrow(data_source))
        }

        return(
          build_plot_card(
            "Gestantes SUSdependentes",
            "plot_gestantes_susdependentes",
            data_source,
            caption = susdependente_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(target_visible_height))
          )
        )
      }

      data_source <- plot_data_main()
      if(identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        opts <- make_scroll_card_options(data_source, "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO")
        if (!is.null(opts)) {
          return(do.call(build_plot_card, c(list("Gestantes SUSdependentes", "plot_gestantes_susdependentes", data_source, caption = "Ano: 2023"), opts)))
        }
        build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", data_source, caption = "Ano: 2023")
      }
    })
    # Para nível ESTADUAL
    output$card_plot_nascidos_susdependentes_estadual <- renderUI({
      req(input$nivel_selection)
      if(!identical(input$nivel_selection, "ESTADUAL")) return(NULL)
      if (isTRUE(is_updated_susdependente_context())) {
        data_source <- nascidos_susdependentes_multiyear_data()
        years <- susdependente_display_years
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(years)
        )

        return(
          build_plot_card(
            "Nascidos vivos SUSdependentes",
            "plot_nascidos_susdependentes_estado",
            data_source,
            caption = susdependente_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(calc_dynamic_height(nrow(data_source))))
          )
        )
      }
      build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_estado", plot_data(), caption = "Ano: 2023")
    })

    # Para níveis RRAS (- RRAS 6), DRS ou REGIÃO DE SAÚDE
    output$card_plot_nascidos_susdependentes_outros <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if (isTRUE(is_updated_susdependente_context())) {
        data_source <- nascidos_susdependentes_multiyear_data()
        years <- susdependente_display_years
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(years)
        )
        target_visible_height <- if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(data_source) > 20L) {
          calc_multiyear_grouped_height(
            nrow(data_source),
            visible_groups = 20L,
            bars_per_group = 1L
          )$visible_height
        } else {
          calc_dynamic_height(nrow(data_source))
        }

        return(
          build_plot_card(
            "Nascidos vivos SUSdependentes",
            "plot_nascidos_susdependentes_outros",
            data_source,
            caption = susdependente_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(target_visible_height))
          )
        )
      }

      data_source <- plot_data_main()
      opts <- make_scroll_card_options(data_source, "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO")
      if (!is.null(opts)) {
        return(do.call(build_plot_card, c(list("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_outros", data_source, caption = "Ano: 2023"), opts)))
      }
      build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_outros", data_source, caption = "Ano: 2023")
    })

    # Para níveis RRAS 6
    output$card_plot_nascidos_susdependentes_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) {
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

      if (isTRUE(is_updated_ans_context())) {
        data_source <- cobertura_ans_multiyear_data()
        years <- cobertura_ans_display_years
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(years)
        )
        target_visible_height <- if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(data_source) > 20L) {
          calc_multiyear_grouped_height(
            nrow(data_source),
            visible_groups = 20L,
            bars_per_group = 1L
          )$visible_height
        } else {
          calc_dynamic_height(nrow(data_source))
        }

        return(
          build_plot_card(
            "Cobertura da Saúde Suplementar (%)",
            "plot_cobertura_ans",
            data_source,
            caption = cobertura_ans_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(target_visible_height))
          )
        )
      }

      data_source <- plot_data_main()
      opts <- make_scroll_card_options(data_source, "COBERTURA ANS %", is_percentage = TRUE)
      if (!is.null(opts)) {
        return(do.call(build_plot_card, c(list("Cobertura da Saúde Suplementar (%)", "plot_cobertura_ans", data_source, caption = "Ano: 2023"), opts)))
      }
      build_plot_card("Cobertura da Saúde Suplementar (%)", "plot_cobertura_ans", data_source, caption = "Ano: 2023")
    })
    output$card_plot_cobertura_esf <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      caption_value <- if (isTRUE(is_updated_ab_context())) {
        build_multiyear_caption_legend(
          legacy_year = cobertura_esf_legacy_year,
          consolidated_years = cobertura_consolidated_years,
          preliminary_year = cobertura_preliminary_year
        )
      } else {
        cobertura_esf_plot_caption()
      }
      if (isTRUE(is_updated_ab_context())) {
        height_cfg <- calc_multiyear_grouped_height(
          nrow(cobertura_esf_comparison_data()),
          bars_per_group = length(cobertura_display_years)
        )
        return(
          build_plot_card(
            "Cobertura da Estratégia Saúde da Família (%)",
            "plot_cobertura_esf",
            cobertura_esf_comparison_data(),
            caption = caption_value,
            height_override = height_cfg$full_height,
            scroll_max_height = max(
              320L,
              as.integer(
                if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(cobertura_esf_comparison_data()) > 20L) {
                  calc_multiyear_grouped_height(
                    nrow(cobertura_esf_comparison_data()),
                    visible_groups = 20L,
                    bars_per_group = 1L
                  )$visible_height
                } else {
                  calc_dynamic_height(nrow(cobertura_esf_comparison_data()))
                }
              )
            ),
            fixed_axis = build_fixed_axis_legend(
              compute_axis_spec(cobertura_esf_comparison_data(), var_numeric = "valor_consolidado", is_percentage = TRUE)
            )
          )
        )
      }
      build_plot_card("Cobertura da Estratégia Saúde da Família (%)", "plot_cobertura_esf", plot_data(), caption = caption_value)
    })
    output$card_plot_cobertura_ab <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      caption_value <- if (isTRUE(is_updated_ab_context())) {
        build_multiyear_caption_legend(
          legacy_year = cobertura_ab_legacy_year,
          consolidated_years = cobertura_consolidated_years,
          preliminary_year = cobertura_preliminary_year
        )
      } else {
        cobertura_ab_plot_caption()
      }
      if (isTRUE(is_updated_ab_context())) {
        height_cfg <- calc_multiyear_grouped_height(
          nrow(cobertura_ab_comparison_data()),
          bars_per_group = length(cobertura_display_years)
        )
        return(
          build_plot_card(
            "Cobertura da Atenção Básica (%)",
            "plot_cobertura_ab",
            cobertura_ab_comparison_data(),
            caption = caption_value,
            height_override = height_cfg$full_height,
            scroll_max_height = max(
              320L,
              as.integer(
                if (isTRUE(is_scrollable_municipal_chart_context()) && nrow(cobertura_ab_comparison_data()) > 20L) {
                  calc_multiyear_grouped_height(
                    nrow(cobertura_ab_comparison_data()),
                    visible_groups = 20L,
                    bars_per_group = 1L
                  )$visible_height
                } else {
                  calc_dynamic_height(nrow(cobertura_ab_comparison_data()))
                }
              )
            ),
            fixed_axis = build_fixed_axis_legend(
              compute_axis_spec(cobertura_ab_comparison_data(), var_numeric = "valor_consolidado", is_percentage = TRUE)
            )
          )
        )
      }
      build_plot_card("Cobertura da Atenção Básica (%)", "plot_cobertura_ab", plot_data(), caption = caption_value)
    })

    output$card_plot_nascidos_vivos_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      data_source <- nascidos_vivos_multiyear_data()
      years <- nascidos_display_years()
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(years)
      )

      build_plot_card(
        "Nascidos vivos",
        "plot_nascidos_vivos_municipal",
        data_source,
        caption = nascidos_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height
      )
    })

    output$card_plot_ubs_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ubs_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      data_source <- ubs_cnes_multiyear_data()
      years <- ubs_cnes_display_years
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(years)
      )

      build_plot_card(
        "Unidade Básica de Saúde (UBS)",
        "plot_ubs_municipal",
        data_source,
        caption = ubs_cnes_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height
      )
    })

    output$card_plot_gestantes_susdependentes_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_susdependente_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      data_source <- gestantes_susdependentes_multiyear_data()
      years <- susdependente_display_years
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(years)
      )

      build_plot_card(
        "Gestantes SUSdependentes",
        "plot_gestantes_susdependentes_municipal",
        data_source,
        caption = susdependente_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height
      )
    })

    output$card_plot_nascidos_susdependentes_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_susdependente_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      data_source <- nascidos_susdependentes_multiyear_data()
      years <- susdependente_display_years
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(years)
      )

      build_plot_card(
        "Nascidos vivos SUSdependentes",
        "plot_nascidos_susdependentes_municipal",
        data_source,
        caption = susdependente_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height
      )
    })

    output$card_plot_cobertura_esf_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ab_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      data_source <- cobertura_esf_comparison_data()
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(cobertura_display_years)
      )

      build_plot_card(
        "Cobertura da Estratégia Saúde da Família (%)",
        "plot_cobertura_esf_municipal",
        data_source,
        caption = build_multiyear_caption_legend(
          legacy_year = cobertura_esf_legacy_year,
          consolidated_years = cobertura_consolidated_years,
          preliminary_year = cobertura_preliminary_year
        ),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height,
        fixed_axis = build_fixed_axis_legend(
          compute_axis_spec(data_source, var_numeric = paste0("valor_", cobertura_preliminary_year), is_percentage = TRUE)
        )
      )
    })

    output$card_plot_cobertura_ab_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ab_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      data_source <- cobertura_ab_comparison_data()
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(cobertura_display_years)
      )

      build_plot_card(
        "Cobertura da Atenção Básica (%)",
        "plot_cobertura_ab_municipal",
        data_source,
        caption = build_multiyear_caption_legend(
          legacy_year = cobertura_ab_legacy_year,
          consolidated_years = cobertura_consolidated_years,
          preliminary_year = cobertura_preliminary_year
        ),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height,
        fixed_axis = build_fixed_axis_legend(
          compute_axis_spec(data_source, var_numeric = paste0("valor_", cobertura_preliminary_year), is_percentage = TRUE)
        )
      )
    })

    output$card_plot_cobertura_ans_municipal <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ans_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      data_source <- cobertura_ans_multiyear_data()
      years <- cobertura_ans_display_years
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(years)
      )

      build_plot_card(
        "Cobertura da Saúde Suplementar (%)",
        "plot_cobertura_ans_municipal",
        data_source,
        caption = cobertura_ans_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = height_cfg$visible_height
      )
    })

    # NOVOS CARDS DE COBERTURA (para RRAS 6)
    output$card_plot_cobertura_ans_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Cobertura da Saúde Suplementar (%)", "plot_cobertura_ans_rras6", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        build_plot_card("Cobertura da Saúde Suplementar (%)", "plot_cobertura_ans_rras6", plot_data(), caption = "Ano: 2020")
      }
    })
    output$card_plot_cobertura_ab_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Cobertura da Atenção Básica (%)", "plot_cobertura_ab_rras6", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("Cobertura da Atenção Básica (%)", "plot_cobertura_ab_rras6", plot_data(), caption = "Ano: 2020")
      }
    })

    output$plot_nascidos_vivos <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      build_nascidos_multiyear_plot(
        data = nascidos_vivos_multiyear_data(),
        years = nascidos_display_years(),
        metric_title = "Nascidos vivos"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_nascidos_vivos_multiyear", cache = "app")

    output$plot_nascidos_vivos_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")

      build_nascidos_multiyear_plot(
        data = nascidos_vivos_multiyear_data(),
        years = nascidos_display_years(),
        metric_title = "Nascidos vivos"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_nascidos_vivos_municipal", cache = "app")

    output$plot_gestantes_susdependentes_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_susdependente_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_nascidos_multiyear_plot(
        data = gestantes_susdependentes_multiyear_data(),
        years = susdependente_display_years,
        metric_title = "Gestantes SUSdependentes",
        axis_title = "Nº ESTIMADO"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_gestantes_susdependentes_municipal", cache = "app")

    output$plot_nascidos_susdependentes_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_susdependente_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_nascidos_multiyear_plot(
        data = nascidos_susdependentes_multiyear_data(),
        years = susdependente_display_years,
        metric_title = "Nascidos vivos SUSdependentes",
        axis_title = "Nº ESTIMADO"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_nascidos_susdependentes_municipal", cache = "app")

    output$plot_ubs <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      if (isTRUE(is_updated_ubs_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = ubs_cnes_multiyear_data(),
            years = ubs_cnes_display_years,
            metric_title = "Unidade Básica de Saúde (UBS)",
            axis_title = "Nº DE UBS",
            year_color = ubs_cnes_year_color,
            preliminary_year = NA_integer_
          )
        )
      }

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data_main(),
        var_numeric = "Nº DE UBS",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_ubs", cache = "app")

    output$plot_ubs_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ubs_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_nascidos_multiyear_plot(
        data = ubs_cnes_multiyear_data(),
        years = ubs_cnes_display_years,
        metric_title = "Unidade Básica de Saúde (UBS)",
        axis_title = "Nº DE UBS",
        year_color = ubs_cnes_year_color,
        preliminary_year = NA_integer_
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_ubs_municipal", cache = "app")

    output$plot_gestantes_susdependentes <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = gestantes_susdependentes_multiyear_data(),
            years = susdependente_display_years,
            metric_title = "Gestantes SUSdependentes",
            axis_title = "Nº ESTIMADO"
          )
        )
      }

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data_main(),
        var_numeric = "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_gestantes_susdependentes", cache = "app")

    output$plot_nascidos_susdependentes_estado <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = nascidos_susdependentes_multiyear_data(),
            years = susdependente_display_years,
            metric_title = "Nascidos vivos SUSdependentes",
            axis_title = "Nº ESTIMADO"
          )
        )
      }

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
      build_bar_plot(
        data = plot_data(),  # mantém sua fonte original
        var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_nascidos_susdependentes_estado", cache = "app")

    output$plot_nascidos_susdependentes_outros <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = nascidos_susdependentes_multiyear_data(),
            years = susdependente_display_years,
            metric_title = "Nascidos vivos SUSdependentes",
            axis_title = "Nº ESTIMADO"
          )
        )
      }

      cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)

      build_bar_plot(
        data         = plot_data_main(),
        var_numeric  = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO",
        var_category = cfg$cat_var,
        force_vertical = cfg$force_v
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_nascidos_susdependentes_outros", cache = "app")

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
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_nascidos_susdependentes_rras6", cache = "app")

    # GRÁFICOS DE COBERTURA (para RRAS, DRS e REGIÃO DE SAÚDE)
    output$plot_cobertura_ans <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      if (isTRUE(is_updated_ans_context())) {
        return(
          build_cobertura_ans_multiyear_plot(
            data = cobertura_ans_multiyear_data(),
            years = cobertura_ans_display_years,
            metric_title = "Cobertura da Saúde Suplementar (%)"
          )
        )
      }

      build_bar_plot(data = plot_data_main(), var_numeric = "COBERTURA ANS %", var_category = "MUNICIPAL", is_percentage = TRUE)
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ans", cache = "app")

    output$plot_cobertura_esf <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if (isTRUE(is_updated_ab_context())) {
        cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
        return(
          build_coverage_multiyear_plot(
            data = cobertura_esf_comparison_data(),
            metric_title = "Cobertura ESF (%)",
            force_vertical = cfg$force_v
          )
        )
      }
      build_bar_plot(data = plot_data_cov(), var_numeric = "COBERTURA ESF %", var_category = "MUNICIPAL", is_percentage = TRUE)
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_esf", cache = "app")

    output$plot_cobertura_ab <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if (isTRUE(is_updated_ab_context())) {
        cfg <- get_cat_config(input$nivel_selection, input$secondary_filter, input$analisar_sp)
        return(
          build_coverage_multiyear_plot(
            data = cobertura_ab_comparison_data(),
            metric_title = "Cobertura AB (%)",
            force_vertical = cfg$force_v
          )
        )
      }

      build_bar_plot(data = plot_data_main(), var_numeric = "COBERTURA AB %", var_category = "MUNICIPAL", is_percentage = TRUE)
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ab", cache = "app")

    output$plot_cobertura_esf_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ab_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_coverage_multiyear_plot(
        data = cobertura_esf_comparison_data(),
        metric_title = "Cobertura da Estratégia Saúde da Família (%)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_esf_municipal", cache = "app")

    output$plot_cobertura_ab_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ab_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_coverage_multiyear_plot(
        data = cobertura_ab_comparison_data(),
        metric_title = "Cobertura da Atenção Básica (%)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ab_municipal", cache = "app")

    output$plot_cobertura_ans_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ans_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_cobertura_ans_multiyear_plot(
        data = cobertura_ans_multiyear_data(),
        years = cobertura_ans_display_years,
        metric_title = "Cobertura da Saúde Suplementar (%)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ans_municipal", cache = "app")

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
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ans_rras6", cache = "app")

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
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ab_rras6", cache = "app")
  })
}

