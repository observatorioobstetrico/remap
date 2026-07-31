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

    show_aps_indicator_help <- function(title, content) {
      shiny::showModal(
        shiny::modalDialog(
          title = tagList(shiny::icon("circle-question"), paste0(" ", title)),
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Fechar"),
          tags$div(
            style = "font-size: 15px; line-height: 1.55;",
            content
          )
        )
      )
    }

    observeEvent(input$help_cobertura_esf, {
      show_aps_indicator_help(
        "Cobertura da Estratégia Saúde da Família (ESF)",
        tagList(
          tags$p(
            "A Estratégia Saúde da Família é o principal modelo de organização da Atenção Primária no Sistema Único de Saúde (SUS)."
          ),
          tags$p(
            "A cobertura da ESF representa a estimativa da população acompanhada por equipes de Saúde da Família em determinado território."
          ),
          tags$p(
            "Esse indicador auxilia na avaliação do acesso da população aos serviços básicos de saúde."
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$help_cobertura_ab, {
      show_aps_indicator_help(
        "Cobertura da Atenção Básica (AB)",
        tagList(
          tags$p(
            "A cobertura da Atenção Básica representa a estimativa da população atendida pelas equipes de Atenção Primária à Saúde, incluindo equipes de Saúde da Família e outras modalidades de atenção básica."
          ),
          tags$p(
            "O indicador permite monitorar a oferta de serviços básicos de saúde nos municípios e regiões."
          )
        )
      )
    }, ignoreInit = TRUE)

    observeEvent(input$help_cobertura_ans, {
      show_aps_indicator_help(
        "Cobertura de saúde suplementar (ANS)",
        tagList(
          tags$p(
            "A cobertura de saúde suplementar corresponde à proporção da população com vínculo a planos privados de assistência à saúde, segundo dados da ANS (Agência Nacional de Saúde Suplementar)."
          ),
          tags$p(
            "Esse indicador auxilia na compreensão da distribuição entre utilização do sistema público e da saúde suplementar."
          )
        )
      )
    }, ignoreInit = TRUE)

    # Helpers de validação e update seguro
    valid_choice <- function(x) {
      isTruthy(x) && length(x) == 1L && !is.na(x) && nzchar(x)
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

    # Título territorial usado no eixo categórico dos gráficos horizontais.
    # Os recortes especiais da capital são exibidos por supervisão; os demais
    # recortes regionais detalham municípios e o panorama estadual detalha RRAS.
    get_locality_axis_title <- function(level,
                                        secondary_filter = NULL,
                                        analisar_sp = NULL,
                                        analisar_muni_sp = NULL) {
      if (identical(level, "ESTADUAL")) {
        return("RRAS")
      }

      is_supervisao <-
        (identical(level, "DRS") && identical(analisar_sp, "SIM")) ||
        (identical(level, "RRAS") && identical(secondary_filter, "RRAS 6")) ||
        (identical(level, "REGIÃO DE SAÚDE") && identical(secondary_filter, "SÃO PAULO")) ||
        (identical(level, "MUNICIPAL") && identical(analisar_muni_sp, "SIM"))

      if (is_supervisao) "Supervisão de Saúde" else "Município"
    }

    current_locality_axis_title <- function() {
      get_locality_axis_title(
        level = input$nivel_selection,
        secondary_filter = input$secondary_filter,
        analisar_sp = input$analisar_sp,
        analisar_muni_sp = input$analisar_muni_sp
      )
    }

    # As bases atualizadas de UBS, ANS, AB/ESF e estimativas SUS são
    # municipalizadas. No panorama estadual elas são agregadas por RRAS;
    # nos demais recortes em que essas bases estão disponíveis, as barras
    # representam municípios. Recortes submunicipais da capital são tratados
    # separadamente e nunca recebem rateios artificiais de totais municipais.
    current_municipal_source_axis_title <- function() {
      if (identical(input$nivel_selection, "ESTADUAL")) "RRAS" else "Município"
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

    # Calcula a altura dos gráficos de barras anuais agrupadas. Cada grupo ocupa
    # uma única posição no eixo categórico; reservar uma altura por barra criava
    # canvases excessivamente altos e deixava os gráficos visualmente deformados.
    calc_multiyear_grouped_height <- function(n_groups, visible_groups = 12L, bars_per_group = 1L) {
      visible_groups <- max(1L, as.integer(visible_groups))
      n_groups <- max(0L, as.integer(n_groups))
      bars_per_group <- max(1L, as.integer(bars_per_group))
      # A altura cresce com o número de séries para manter cada barra com área
      # confortável de hover. A folga restante separa visualmente localidades.
      per_group_height <- max(88L, 30L + (bars_per_group * 14L))

      full_height <- 110 + (n_groups * per_group_height)
      visible_height <- 110 + (min(n_groups, visible_groups) * per_group_height)

      list(
        full_height = max(320L, full_height),
        visible_height = max(320L, visible_height)
      )
    }

    # Mantém uma folga clara entre localidades e uma separação leve também entre
    # as barras anuais que compõem cada localidade.
    calc_multiyear_bar_width <- function(n_years) {
      n_years <- max(1L, as.integer(n_years))
      if (n_years == 1L) 0.26 else 0.78 / n_years
    }

    # Gráficos antigos por supervisão possuem apenas uma série. Para eles, o
    # canvas interno usa faixas mais compactas que as séries anuais agrupadas,
    # sem alterar a altura visível nem o alinhamento dos cards.
    calc_single_series_supervision_height <- function(n_groups) {
      n_groups <- max(0L, as.integer(n_groups))

      list(
        full_height = max(320L, 110L + (n_groups * 40L)),
        visible_height = max(320L, as.integer(calc_dynamic_height(n_groups)))
      )
    }

    single_series_supervision_bar_width <- 0.60
    standardized_plot_levels <- c("ESTADUAL", "DRS", "RRAS", "REGIÃO DE SAÚDE")
    standardized_plot_viewport_height <- 520L

    summary_box_class <- function(legacy_class) {
      if (isTRUE(input$nivel_selection %in% standardized_plot_levels)) {
        return("custom-box aps-summary-card")
      }

      paste("custom-box", legacy_class)
    }

    build_plot_card <- function(card_title,
                                plot_output_id,
                                data_to_plot,
                                caption = NULL,
                                height_override = NULL,
                                scroll_max_height = NULL,
                                fixed_axis = NULL,
                                locality_filter = NULL) {
      n_bars <- if (is.null(data_to_plot) || !is.data.frame(data_to_plot)) 0L else nrow(data_to_plot)
      # Se height_override não for NULL, usa-o; caso contrário, calcula dinamicamente
      height_val <- if (is.null(height_override)) calc_dynamic_height(n_bars) else height_override
      height_val <- max(1L, as.integer(round(height_val)))

      use_standardized_viewport <- isTRUE(input$nivel_selection %in% standardized_plot_levels)
      has_requested_viewport <- !is.null(scroll_max_height) &&
        length(scroll_max_height) == 1L &&
        is.finite(scroll_max_height)
      has_viewport <- use_standardized_viewport || has_requested_viewport
      viewport_height <- if (use_standardized_viewport) {
        standardized_plot_viewport_height
      } else if (has_requested_viewport) {
        max(1L, as.integer(round(scroll_max_height)))
      } else {
        height_val
      }
      plot_height <- max(height_val, viewport_height)
      plot_tag <- plotly::plotlyOutput(ns(plot_output_id), height = paste0(plot_height, "px"))

      # A viewport recebe sempre a altura visível combinada, inclusive quando o
      # canvas é menor. Isso evita diferenças entre cards vizinhos com números
      # distintos de anos e mantém o conteúdo excedente acessível por rolagem.
      if (has_viewport) {
        plot_tag <- tags$div(
          class = "aps-plot-viewport",
          style = paste0(
            "height:", viewport_height, "px; max-height:", viewport_height, "px;"
          ),
          plot_tag
        )
      }

      help_input_id <- switch(
        as.character(card_title),
        "Cobertura da Saúde Suplementar (ANS %)" = "help_cobertura_ans",
        "Cobertura da Estratégia Saúde da Família (ESF %)" = "help_cobertura_esf",
        "Cobertura da Atenção Básica (AB %)" = "help_cobertura_ab",
        NULL
      )
      card_title_tag <- if (is.null(help_input_id)) {
        card_title
      } else {
        tags$div(
          class = "aps-card-title-with-help",
          tags$span(class = "aps-card-title-text", card_title),
          shiny::actionButton(
            inputId = ns(help_input_id),
            label = NULL,
            icon = shiny::icon("circle-question"),
            class = "aps-card-help-btn",
            `aria-label` = paste0("Informações sobre ", card_title),
            title = paste0("Informações sobre ", card_title)
          )
        )
      }
      if (!is.null(locality_filter)) {
        card_title_tag <- tags$div(
          class = "aps-card-title-with-locality-filter",
          tags$span(class = "aps-card-title-text", card_title_tag),
          build_locality_filter_control(locality_filter)
        )
      }

      tags$div(
        class = "aps-plot-card-host",
        bs4Dash::bs4Card(
          tagList(
            plot_tag,
            # A área é reservada em todos os cards para manter o alinhamento.
            # Somente a legenda cromática é fornecida pelos chamadores.
            tags$div(class = "aps-plot-caption", caption)
          ),
          title = card_title_tag,
          height = "100%",
          width = NULL,
          collapsible = FALSE
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

    aps_min_year <- 2021L
    aps_max_year <- 2025L

    aps_display_year_filter <- function(years) {
      years <- sort(unique(stats::na.omit(as.integer(years))))
      years[years >= aps_min_year & years <= aps_max_year]
    }

    aps_year_color <- function(year, years) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }

      all_years <- aps_display_year_filter(years)
      if (!length(all_years)) {
        return("#0a1e3c")
      }

      ramp_length <- length(all_years)
      if (aps_max_year %in% all_years) {
        ramp_length <- ramp_length + 1L
      }

      palette <- grDevices::colorRampPalette(c("#bfe7ff", "#32a0ff", "#0a1e3c"))(ramp_length)
      palette <- palette[seq_along(all_years)]
      year_index <- match(year, all_years)
      if (is.na(year_index)) {
        return("#0a1e3c")
      }

      palette[[year_index]]
    }

    cobertura_year_color <- function(year) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }
      aps_year_color(year, cobertura_display_years)
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

      rows <- list()
      if (length(legacy_year) == 1L && !is.na(legacy_year)) {
        rows <- c(rows, list(
          legend_row("Histórico:", legend_item(cobertura_year_color(legacy_year), as.character(legacy_year)))
        ))
      }
      rows <- c(rows, list(
        legend_row("Consolidados:", lapply(consolidated_years, function(year) {
          legend_item(cobertura_year_color(year), as.character(year))
        }))
      ))
      if (length(preliminary_year) == 1L && !is.na(preliminary_year)) {
        rows <- c(rows, list(
          legend_row("Preliminar:", legend_item(cobertura_year_color(preliminary_year), as.character(preliminary_year)))
        ))
      }

      do.call(
        tags$div,
        c(list(style = "display:flex; flex-direction:column; gap:4px;"), rows)
      )
    }

    nascidos_year_color <- function(year) {
      year <- as.integer(year)
      if (length(year) != 1L || is.na(year)) {
        return("#0a1e3c")
      }
      aps_year_color(year, c(nascidos_consolidated_years, nascidos_preliminary_year))
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
      aps_year_color(year, c(cobertura_ans_consolidated_years, cobertura_ans_preliminary_year))
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

    stabilize_aps_plot_size <- function(p) {
      if (is.null(p)) {
        return(NULL)
      }

      htmlwidgets::onRender(
        p,
        "function(el, x) {
          function resizeWhenReady() {
            if (window.Plotly && el && el.offsetParent !== null) {
              window.Plotly.Plots.resize(el);
            }
          }
          [0, 80, 250, 700].forEach(function(delay) {
            window.setTimeout(resizeWhenReady, delay);
          });
          if (window.ResizeObserver && el.parentElement) {
            if (el.__apsResizeObserver) {
              el.__apsResizeObserver.disconnect();
            }
            var observer = new ResizeObserver(function() {
              resizeWhenReady();
            });
            observer.observe(el.parentElement);
            el.__apsResizeObserver = observer;
          }
          if (window.jQuery && !window.__apsPlotResizeTabHandler) {
            window.__apsPlotResizeTabHandler = true;
            window.jQuery(document).on('shown.bs.tab', function() {
              [0, 120].forEach(function(delay) {
                window.setTimeout(function() {
                  window.jQuery('.aps-graph-tabs .js-plotly-plot:visible').each(function() {
                    if (window.Plotly) {
                      window.Plotly.Plots.resize(this);
                    }
                  });
                }, delay);
              });
            });
          }
        }"
      )
    }

    hide_aps_modebar <- function(p) {
      if (is.null(p)) {
        return(NULL)
      }

      p <- plotly::config(
        p,
        displayModeBar = FALSE,
        displaylogo = FALSE
      )
      stabilize_aps_plot_size(p)
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

    build_bar_plot <- function(data,
                               var_numeric,
                               var_category,
                               is_percentage = FALSE,
                               force_vertical = FALSE,
                               bar_width = NULL) {
      percent_tick_vals <- c(0, 25, 50, 75, 100)
      percent_tick_text <- paste0(percent_tick_vals, "%")
      display_category <- switch(
        var_category,
        "MUNICIPAL" = "Município",
        "SUPERVISÃO DE SAÚDE" = "Supervisão de Saúde",
        var_category
      )

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

        # `plot_ly()` ignora silenciosamente o argumento `width` quando ele é
        # informado no traço inicial. Adicionar o traço explicitamente garante
        # que a espessura configurada seja preservada no JSON enviado ao browser.
        p <- plotly::plot_ly(data = trace_data) |>
          plotly::add_trace(
            x = as.formula(paste0("~`", var_numeric, "`")),
            y = ~row_id,
            type = "bar",
            orientation = "h",
            width = bar_width,
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
          bargap = 0.18,
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

        p <- plotly::plot_ly(data = trace_data) |>
          plotly::add_trace(
            x = as.formula(paste0("~`", var_category, "`")),
            y = as.formula(paste0("~`", var_numeric, "`")),
            type = "bar",
            width = bar_width,
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
      if (!length(x) || is.na(x[1]) || !is.finite(as.numeric(x[1]))) {
        return("Não disponível")
      }
      format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }


    build_ab_comparison_plot <- function(data, current_year, legacy_year = NA_integer_, force_vertical = FALSE) {
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
              title = list(text = "Cobertura da Atenção Básica (AB %)", standoff = 0L),
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
          title = list(text = "Cobertura da Atenção Básica (AB %)", standoff = 20L, size = 1),
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

    build_coverage_comparison_plot <- function(data, metric_title, current_year, legacy_year = NA_integer_, force_vertical = FALSE) {
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

    build_updated_coverage_box <- function(title, box_class, values, current_year, legacy_year = NA_integer_) {
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

    build_coverage_multiyear_plot <- function(data,
                                              metric_title,
                                              force_vertical = FALSE,
                                              locality_axis_title = "Município") {
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

      p <- plotly::plot_ly()
      bar_width <- calc_multiyear_bar_width(length(years))
      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        trace_data <- data[is.finite(data[[value_col]]), , drop = FALSE]
        if (!nrow(trace_data)) {
          next
        }
        marker_cfg <- list(color = year_color(year))
        if (year %in% cobertura_preliminary_year) {
          marker_cfg$line <- list(color = "#0a1e3c", width = 1)
        }

        p <- plotly::add_bars(
          p,
          x = trace_data[[value_col]],
          y = trace_data$row_id,
          orientation = "h",
          width = bar_width,
          name = paste0("Ano ", year),
          offsetgroup = as.character(year),
          marker = marker_cfg,
          opacity = if (year %in% cobertura_preliminary_year) 0.9 else 1,
          textposition = "none",
          customdata = I(Map(c, as.character(trace_data$MUNICIPAL), trace_data[[formatted_col]])),
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
          title = list(text = locality_axis_title, standoff = 10L),
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

    build_cobertura_ans_multiyear_plot <- function(data,
                                                   years,
                                                   metric_title = "Cobertura da Saúde Suplementar (ANS %)",
                                                   locality_axis_title = "Município") {
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

      p <- plotly::plot_ly()
      bar_width <- calc_multiyear_bar_width(length(years))
      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        trace_data <- data[is.finite(data[[value_col]]), , drop = FALSE]
        if (!nrow(trace_data)) {
          next
        }
        marker_cfg <- list(color = cobertura_ans_year_color(year))
        if (year %in% cobertura_ans_preliminary_year) {
          marker_cfg$line <- list(color = "#0a1e3c", width = 1)
        }

        p <- plotly::add_bars(
          p,
          x = trace_data[[value_col]],
          y = trace_data$row_id,
          orientation = "h",
          width = bar_width,
          name = paste0("Ano ", year),
          offsetgroup = as.character(year),
          marker = marker_cfg,
          opacity = if (year %in% cobertura_ans_preliminary_year) 0.9 else 1,
          textposition = "none",
          customdata = I(Map(c, as.character(trace_data$MUNICIPAL), trace_data[[formatted_col]])),
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
          title = list(text = locality_axis_title, standoff = 10L),
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
                                              preliminary_year = nascidos_preliminary_year,
                                              locality_axis_title = current_locality_axis_title()) {
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

      p <- plotly::plot_ly()
      bar_width <- calc_multiyear_bar_width(length(years))
      for (year in years) {
        value_col <- paste0("valor_", year)
        formatted_col <- paste0("formatted_", year)
        trace_data <- data[is.finite(data[[value_col]]), , drop = FALSE]
        if (!nrow(trace_data)) {
          next
        }
        marker_cfg <- list(color = year_color(year))
        if (year %in% preliminary_year) {
          marker_cfg$line <- list(color = "#0a1e3c", width = 1)
        }

        p <- plotly::add_bars(
          p,
          x = trace_data[[value_col]],
          y = trace_data$row_id,
          orientation = "h",
          width = bar_width,
          name = paste0("Ano ", year),
          offsetgroup = as.character(year),
          marker = marker_cfg,
          opacity = if (year %in% preliminary_year) 0.9 else 1,
          textposition = "none",
          customdata = I(Map(c, as.character(trace_data$LOCALIDADE), trace_data[[formatted_col]])),
          hovertemplate = paste0("%{customdata[0]}<br>Ano ", year, " (", year_status(year), "): %{customdata[1]}<extra></extra>"),
          showlegend = FALSE
        )
      }

      p <- plotly::layout(
        p,
        barmode = "group",
        bargap = 0.16,
        xaxis = list(
          title = list(text = axis_title, standoff = 0L),
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
          title = list(text = locality_axis_title, standoff = 10L),
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
            year_label = paste0("Ano ", cobertura_consolidated_year),
            subtitle = "Consolidado",
            value = values$consolidado,
            color = cobertura_year_color(cobertura_consolidated_year),
            background = "#eef3f9"
          ),
          build_tile(
            year_label = paste0("Ano ", cobertura_preliminary_year),
            subtitle = "Preliminar",
            value = values$preliminar,
            color = cobertura_year_color(cobertura_preliminary_year),
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

    # As fontes historicas e atuais nem sempre usam a mesma quantidade de
    # espacos nos nomes territoriais. A normalizacao em um unico ponto evita
    # que filtros visualmente identicos retornem conjuntos diferentes.
    aps_text_cols <- c(
      aps_col_drs,
      aps_col_regiao,
      aps_col_municipio,
      aps_col_rras,
      aps_col_coord,
      aps_col_supervisao
    )
    for (column in aps_text_cols) {
      tabela_APS[[column]] <- canonicalize_municipio_display(tabela_APS[[column]])
    }
    empty_tabela_APS <- tabela_APS[0, , drop = FALSE]

    aps_cols_sum <- c(
      aps_col_nascidos,
      aps_col_nascidos_sus,
      aps_col_ubs,
      aps_col_gestantes,
      aps_col_cobertura_ans,
      aps_col_cobertura_esf,
      aps_col_cobertura_ab
    )

    clean_choice_values <- function(x) {
      values <- trimws(as.character(x))
      unique(values[!is.na(values) & nzchar(values)])
    }

    alphabetical_choices <- function(x) {
      values <- clean_choice_values(x)
      sort_key <- iconv(toupper(values), from = "", to = "ASCII//TRANSLIT")
      sort_key[is.na(sort_key)] <- toupper(values[is.na(sort_key)])
      values[order(sort_key, values, na.last = TRUE)]
    }

    natural_rras_choices <- function(x) {
      values <- clean_choice_values(x)
      sort_key <- iconv(toupper(values), from = "", to = "ASCII//TRANSLIT")
      sort_key[is.na(sort_key)] <- toupper(values[is.na(sort_key)])
      rras_number <- suppressWarnings(as.integer(sub("^RRAS\\s+([0-9]+)$", "\\1", sort_key)))
      values[order(is.na(rras_number), rras_number, sort_key, values, na.last = TRUE)]
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
      rras = natural_rras_choices(tabela_APS[[aps_col_rras]]),
      drs = alphabetical_choices(tabela_APS[[aps_col_drs]]),
      coordenadoria = alphabetical_choices(tabela_APS[[aps_col_coord]]),
      regiao = alphabetical_choices(tabela_APS[[aps_col_regiao]]),
      supervisao = alphabetical_choices(tabela_APS[[aps_col_supervisao]]),
      municipal = alphabetical_choices(tabela_APS[[aps_col_municipio]])
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
      dplyr::mutate(
        municipal = canonicalize_municipio_display(.data$municipal),
        municipal_key = normalize_municipio_key(.data$municipal),
        rras = canonicalize_municipio_display(.data$rras),
        regiao_de_saude = canonicalize_municipio_display(.data$regiao_de_saude),
        drs = canonicalize_municipio_display(.data$drs)
      )
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
    cobertura_ab_legacy_year <- NA_integer_
    max_year_or_na <- function(years) {
      years <- stats::na.omit(as.integer(years))
      years <- years[is.finite(years)]
      if (length(years)) max(years) else NA_integer_
    }

    available_cobertura_years <- aps_display_year_filter(cobertura_ab_aps$available_years)
    cobertura_egestor_years <- available_cobertura_years
    cobertura_ab_latest_year <- max_year_or_na(cobertura_egestor_years)
    cobertura_preliminary_year <- cobertura_ab_latest_year
    cobertura_consolidated_years <- if (is.na(cobertura_preliminary_year)) {
      integer()
    } else {
      cobertura_egestor_years[cobertura_egestor_years < cobertura_preliminary_year]
    }
    cobertura_consolidated_year <- max_year_or_na(cobertura_consolidated_years)
    cobertura_display_years <- sort(unique(stats::na.omit(c(
      cobertura_consolidated_years,
      cobertura_preliminary_year
    ))))
    cobertura_esf_latest_year <- cobertura_ab_latest_year
    cobertura_esf_legacy_year <- cobertura_ab_legacy_year

    cobertura_ans_aps <- data_list$cobertura_ans_aps
    if (is.null(cobertura_ans_aps) || !is.list(cobertura_ans_aps)) {
      cobertura_ans_aps <- list(
        municipal = data.frame(),
        consolidated_years = 2021:2024,
        preliminary_year = 2025L
      )
    }
    if (is.data.frame(cobertura_ans_aps$municipal) && nrow(cobertura_ans_aps$municipal)) {
      cobertura_ans_aps$municipal <- cobertura_ans_aps$municipal |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          municipal = dplyr::if_else(.data$municipal_key == "SAO PAULO", "SÃO PAULO", .data$municipal),
          rras = canonicalize_municipio_display(.data$rras),
          regiao_de_saude = canonicalize_municipio_display(.data$regiao_de_saude),
          drs = canonicalize_municipio_display(.data$drs)
        )
    }
    cobertura_ans_consolidated_years <- aps_display_year_filter(cobertura_ans_aps$consolidated_years)
    cobertura_ans_preliminary_year <- suppressWarnings(as.integer(cobertura_ans_aps$preliminary_year))
    if (length(cobertura_ans_preliminary_year) != 1L || !is.finite(cobertura_ans_preliminary_year)) {
      cobertura_ans_preliminary_year <- NA_integer_
    }
    if (!(cobertura_ans_preliminary_year %in% aps_min_year:aps_max_year)) {
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
        consolidated_years = 2021:2024,
        preliminary_year = NA_integer_
      )
    }
    if (is.data.frame(nascidos_vivos_aps$municipal) && nrow(nascidos_vivos_aps$municipal)) {
      nascidos_vivos_aps$municipal <- nascidos_vivos_aps$municipal |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          municipal = dplyr::if_else(.data$municipal_key == "SAO PAULO", "SÃO PAULO", .data$municipal),
          rras = canonicalize_municipio_display(.data$rras),
          regiao_de_saude = canonicalize_municipio_display(.data$regiao_de_saude),
          drs = canonicalize_municipio_display(.data$drs)
        )
    }
    if (is.data.frame(nascidos_vivos_aps$supervisao) && nrow(nascidos_vivos_aps$supervisao)) {
      nascidos_vivos_aps$supervisao <- nascidos_vivos_aps$supervisao |>
        dplyr::mutate(
          municipal = canonicalize_municipio_display(.data$municipal),
          municipal_key = normalize_municipio_key(.data$municipal),
          supervisao_key = normalize_municipio_key(.data$supervisao_de_saude),
          coordenadoria_de_saude = canonicalize_municipio_display(.data$coordenadoria_de_saude),
          regiao_de_saude = canonicalize_municipio_display(.data$regiao_de_saude),
          drs = canonicalize_municipio_display(.data$drs)
        )
    }
    nascidos_consolidated_years <- aps_display_year_filter(nascidos_vivos_aps$consolidated_years)
    nascidos_preliminary_year <- suppressWarnings(as.integer(nascidos_vivos_aps$preliminary_year))
    if (length(nascidos_preliminary_year) != 1L || !is.finite(nascidos_preliminary_year)) {
      nascidos_preliminary_year <- NA_integer_
    }
    if (!(nascidos_preliminary_year %in% aps_min_year:aps_max_year)) {
      nascidos_preliminary_year <- NA_integer_
    }
    nascidos_municipal_years <- aps_display_year_filter(c(nascidos_vivos_aps$municipal_years, nascidos_consolidated_years))
    nascidos_sp_years <- aps_display_year_filter(c(nascidos_vivos_aps$sp_years, nascidos_consolidated_years, nascidos_preliminary_year))
    nascidos_default_summary_year <- if (2024L %in% nascidos_consolidated_years) 2024L else max_year_or_na(nascidos_consolidated_years)
    susdependente_display_years <- sort(unique(stats::na.omit(as.integer(intersect(
      aps_min_year:aps_max_year,
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
          rras = canonicalize_municipio_display(.data$rras),
          regiao_de_saude = canonicalize_municipio_display(.data$regiao_de_saude),
          drs = canonicalize_municipio_display(.data$drs),
          n_ubs = suppressWarnings(as.numeric(.data$n_ubs))
        )
    }
    ubs_cnes_consolidated_years <- aps_display_year_filter(ubs_cnes_aps$consolidated_years)
    ubs_cnes_display_years <- sort(unique(stats::na.omit(as.integer(intersect(aps_min_year:aps_max_year, ubs_cnes_consolidated_years)))))
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
      aps_year_color(year, ubs_cnes_display_years)
    }

    is_submunicipal_sp_context <- reactive({
      req(input$nivel_selection)

      (identical(input$nivel_selection, "RRAS") && identical(input$secondary_filter, "RRAS 6")) ||
        (identical(input$nivel_selection, "REGIÃO DE SAÚDE") && identical(input$secondary_filter, "SÃO PAULO")) ||
        (identical(input$nivel_selection, "DRS") && identical(input$analisar_sp, "SIM")) ||
        (identical(input$nivel_selection, "MUNICIPAL") && identical(input$analisar_muni_sp, "SIM"))
    })

    locality_filter_config <- function(data, plot_output_id) {
      if (isTRUE(is_submunicipal_sp_context()) ||
          is.null(data) || !is.data.frame(data) || !nrow(data) ||
          !("LOCALIDADE" %in% names(data))) {
        return(NULL)
      }

      level <- input$nivel_selection
      if (identical(level, "MUNICIPAL")) {
        return(NULL)
      }

      if (!(level %in% c("ESTADUAL", "DRS", "RRAS", "REGIÃO DE SAÚDE", "MUNICIPAL"))) {
        return(NULL)
      }

      choices <- if (identical(level, "ESTADUAL")) {
        natural_rras_choices(data$LOCALIDADE)
      } else {
        alphabetical_choices(data$LOCALIDADE)
      }
      if (!length(choices)) {
        return(NULL)
      }

      list(
        input_id = paste0(plot_output_id, "_localities"),
        choices = choices,
        territory_label = if (identical(level, "ESTADUAL")) "RRAS" else "municípios"
      )
    }

    build_locality_filter_control <- function(config) {
      tags$div(
        class = "aps-locality-filter-control",
        title = paste0("Selecionar ", config$territory_label, " exibidos no gráfico"),
        tags$label(
          class = "sr-only",
          `for` = ns(config$input_id),
          paste0("Selecionar ", config$territory_label, " exibidos no gráfico")
        ),
        shinyWidgets::pickerInput(
          inputId = ns(config$input_id),
          label = NULL,
          choices = config$choices,
          selected = config$choices,
          multiple = TRUE,
          width = "28px",
          inline = TRUE,
          options = list(
            "actions-box" = TRUE,
            "live-search" = TRUE,
            "selected-text-format" = "count > 1",
            "count-selected-text" = "{0} selecionados",
            "none-selected-text" = "Nenhum selecionado",
            "select-all-text" = "Selecionar todos",
            "deselect-all-text" = "Remover todos",
            "size" = 10
          )
        ),
        tags$span(
          class = "aps-locality-filter-icon",
          shiny::icon("filter")
        )
      )
    }

    filter_locality_plot_data <- function(data, plot_output_id) {
      config <- locality_filter_config(data, plot_output_id)
      if (is.null(config)) {
        return(data)
      }

      selected <- input[[config$input_id]]
      picker_state <- input[[paste0(config$input_id, "_open")]]
      if (is.null(selected) && is.null(picker_state)) {
        return(data)
      }

      data[data$LOCALIDADE %in% as.character(selected), , drop = FALSE]
    }

    locality_filter_cache_key <- function(plot_output_id) {
      input_id <- paste0(plot_output_id, "_localities")
      list(
        selected = input[[input_id]],
        picker_open = input[[paste0(input_id, "_open")]]
      )
    }

    # As bases anuais atualizadas são municipais e não possuem desagregação por
    # supervisão. Para esses recortes específicos, preserva-se a granularidade
    # submunicipal já existente no painel, sem ratear totais municipais nem
    # substituir os dados atualizados nos demais níveis de análise.
    legacy_supervision_context_data <- reactive({
      req(input$nivel_selection)
      if (!isTRUE(is_submunicipal_sp_context())) {
        return(empty_tabela_APS)
      }

      level <- input$nivel_selection
      if (identical(level, "RRAS")) {
        if (!valid_choice(input$secondary_filter)) return(empty_tabela_APS)
        data <- tabela_APS[tabela_APS[[aps_col_rras]] == input$secondary_filter, , drop = FALSE]
      } else if (identical(level, "REGIÃO DE SAÚDE")) {
        if (!valid_choice(input$secondary_filter)) return(empty_tabela_APS)
        data <- tabela_APS[tabela_APS[[aps_col_regiao]] == input$secondary_filter, , drop = FALSE]
      } else if (identical(level, "DRS") && identical(input$analisar_sp, "SIM")) {
        if (!valid_choice(input$secondary_filter)) return(empty_tabela_APS)
        data <- tabela_APS[tabela_APS[[aps_col_coord]] == input$secondary_filter, , drop = FALSE]
      } else if (identical(level, "MUNICIPAL") && identical(input$analisar_muni_sp, "SIM")) {
        if (!valid_choice(input$secondary_filter)) return(empty_tabela_APS)
        data <- tabela_APS[tabela_APS[[aps_col_supervisao]] == input$secondary_filter, , drop = FALSE]
      } else {
        return(empty_tabela_APS)
      }

      supervision <- as.character(data[[aps_col_supervisao]])
      data[!is.na(supervision) & nzchar(trimws(supervision)), , drop = FALSE]
    })

    legacy_supervision_plot_data <- function(value_column) {
      data <- legacy_supervision_context_data()
      if (!is.data.frame(data) || !nrow(data) ||
          !(value_column %in% names(data)) || !(aps_col_supervisao %in% names(data))) {
        return(data.frame())
      }

      out <- data[, c(aps_col_supervisao, value_column), drop = FALSE]
      names(out)[1] <- "SUPERVISÃO DE SAÚDE"
      out
    }

    legacy_supervision_has_values <- function(value_column) {
      data <- legacy_supervision_plot_data(value_column)
      nrow(data) > 0L && any(is.finite(suppressWarnings(as.numeric(data[[value_column]]))))
    }

    legacy_supervision_total <- function(value_column) {
      data <- legacy_supervision_plot_data(value_column)
      if (!nrow(data)) return(NA_real_)
      values <- suppressWarnings(as.numeric(data[[value_column]]))
      if (!any(is.finite(values))) NA_real_ else sum(values[is.finite(values)])
    }

    legacy_supervision_metric <- function(value_column) {
      data <- legacy_supervision_plot_data(value_column)
      if (!nrow(data)) return(NA_real_)
      values <- suppressWarnings(as.numeric(data[[value_column]]))
      if (!any(is.finite(values))) NA_real_ else mean(values[is.finite(values)])
    }

    legacy_supervision_year_labels <- stats::setNames(
      c("2023", "2023", "2022/2023", "2020", "2020", "2023", "2023"),
      c(
        aps_col_nascidos,
        aps_col_cobertura_ans,
        aps_col_ubs,
        aps_col_cobertura_esf,
        aps_col_cobertura_ab,
        aps_col_gestantes,
        aps_col_nascidos_sus
      )
    )

    legacy_supervision_year_label <- function(value_column) {
      year_label <- unname(legacy_supervision_year_labels[[value_column]])
      if (is.null(year_label) || !length(year_label) || is.na(year_label)) {
        return(NULL)
      }
      year_label
    }

    legacy_supervision_year_caption <- function(value_column) {
      year_label <- legacy_supervision_year_label(value_column)
      if (is.null(year_label)) return(NULL)
      paste0("Ano: ", year_label)
    }

    legacy_supervision_height <- function(data) {
      calc_single_series_supervision_height(
        if (is.data.frame(data)) nrow(data) else 0L
      )
    }

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
        dplyr::filter(.data$ano %in% years) |>
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
      nascidos_display_years()
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

    build_summary_unavailable_caption <- function() {
      tags$div(
        "Sem dado atualizado para este recorte",
        style = paste0(
          "position:absolute; bottom:0; left:0; right:0; font-size:11px; ",
          "color:#FFFFFF; background-color:#0A1E3C; padding:3px 8px; ",
          "border-radius:0 0 3px 3px; text-align:center; box-sizing:border-box;"
        )
      )
    }

    build_summary_year_caption <- function(year_label) {
      tags$div(
        paste0("Ano de atualização dos dados: ", year_label),
        style = paste0(
          "position:absolute; bottom:0; left:0; right:0; font-size:12px; ",
          "color:#FFFFFF; background-color:#0A1E3C; padding:3px 10px; ",
          "border-radius:0 0 3px 3px; text-align:center; box-sizing:border-box;"
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

      ans_coverage_col <- if ("cobertura_ans_exata" %in% names(ans)) {
        "cobertura_ans_exata"
      } else {
        "cobertura_ans"
      }

      ans_base <- ans |>
        dplyr::filter(.data$ano %in% years) |>
        dplyr::transmute(
          ano = as.integer(.data$ano),
          municipal_key = normalize_municipio_key(.data$municipal),
          cobertura_ans = suppressWarnings(as.numeric(.data[[ans_coverage_col]]))
        )

      df <- nascidos_base |>
        dplyr::left_join(ans_base, by = c("ano", "municipal_key")) |>
        dplyr::mutate(
          # Regra do painel: a parcela SUSdependente é o complemento da
          # cobertura ANS. O acréscimo de 10% estima gestações que não chegam
          # ao registro de nascido vivo. O arredondamento ocorre apenas na
          # apresentação, preservando a precisão durante as agregações.
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
        visible_groups = 12L,
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

    unavailable_data_caption <- function() {
      tags$span(
        "Dados atualizados não estão disponíveis para este recorte territorial.",
        style = "color:#555; line-height:1.4;"
      )
    }

    build_unavailable_plot_card <- function(card_title, plot_output_id, height = 400L) {
      build_plot_card(
        card_title,
        plot_output_id,
        data.frame(),
        caption = NULL,
        height_override = height,
        scroll_max_height = height
      )
    }

    cobertura_ab_plot_caption <- reactive({
      if (isTRUE(is_updated_ab_context()) && input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL")) {
        paste0(
          "Anos: ",
          format_year_sequence(cobertura_consolidated_years),
          " (consolidados) e ",
          cobertura_preliminary_year,
          " (preliminar)"
        )
      } else {
        paste0("Anos: ", aps_min_year, "-", aps_max_year)
      }
    })

    cobertura_esf_plot_caption <- reactive({
      if (isTRUE(is_updated_ab_context()) && input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE", "MUNICIPAL")) {
        paste0(
          "Anos: ",
          format_year_sequence(cobertura_consolidated_years),
          " (consolidados) e ",
          cobertura_preliminary_year,
          " (preliminar)"
        )
      } else {
        paste0("Anos: ", aps_min_year, "-", aps_max_year)
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

    # Enquanto o picker secundário é reconstruído, o valor antigo ainda pode
    # pertencer ao nível anterior. Suspender a atualização nesse intervalo
    # evita cards intermediários com "Não disponível".
    aps_filter_ready <- reactive({
      req(input$nivel_selection)
      cfg <- secondary_filter_config()

      if (is.null(cfg)) {
        return(TRUE)
      }

      valid_choice(input$secondary_filter) &&
        input$secondary_filter %in% as.character(cfg$choices)
    })

    aps_plot_cache_key <- reactive({
      list(
        modulo = "aps_barras",
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

    build_coverage_multiyear_data <- function(updated_reactives, current_column) {
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

        pieces <- list()

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

        if (!length(pieces)) {
          return(data.frame())
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

    build_coverage_multiyear_card_values <- function(consolidated_reactive,
                                                     consolidated_column,
                                                     preliminary_reactive,
                                                     preliminary_column) {
      reactive({
        req(input$nivel_selection == "MUNICIPAL")

        if (!isTRUE(is_updated_ab_context())) {
          return(NULL)
        }

        consolidado <- consolidated_reactive()
        preliminar <- preliminary_reactive()

        list(
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
      updated_reactives = cobertura_ab_updated_municipal_by_year,
      current_column = "cobertura_ab"
    )

    cobertura_esf_comparison_data <- build_coverage_multiyear_data(
      updated_reactives = cobertura_esf_updated_municipal_by_year,
      current_column = "cobertura_esf"
    )

    cobertura_ab_card_values <- build_coverage_multiyear_card_values(
      consolidated_reactive = cobertura_ab_consolidated_municipal,
      consolidated_column = "cobertura_ab",
      preliminary_reactive = cobertura_ab_preliminary_municipal,
      preliminary_column = "cobertura_ab"
    )

    cobertura_esf_card_values <- build_coverage_multiyear_card_values(
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

    register_locality_plot_resize <- function(plot_output_id, data_reactive, years_reactive) {
      observe({
        data <- data_reactive()
        if (is.null(locality_filter_config(data, plot_output_id))) {
          return()
        }

        filtered_data <- filter_locality_plot_data(data, plot_output_id)
        years <- years_reactive()
        height_cfg <- calc_multiyear_grouped_height(
          nrow(filtered_data),
          bars_per_group = length(years)
        )
        plot_height <- height_cfg$full_height
        if (input$nivel_selection %in% standardized_plot_levels) {
          plot_height <- max(plot_height, standardized_plot_viewport_height)
        }

        session$sendCustomMessage(
          "aps-resize-plot",
          list(id = ns(plot_output_id), height = as.integer(plot_height))
        )
      })
    }

    register_locality_plot_resize(
      "plot_nascidos_vivos",
      nascidos_vivos_multiyear_data,
      nascidos_display_years
    )
    register_locality_plot_resize(
      "plot_nascidos_vivos_municipal",
      nascidos_vivos_multiyear_data,
      nascidos_display_years
    )
    register_locality_plot_resize(
      "plot_ubs",
      ubs_cnes_multiyear_data,
      function() ubs_cnes_display_years
    )
    register_locality_plot_resize(
      "plot_ubs_municipal",
      ubs_cnes_multiyear_data,
      function() ubs_cnes_display_years
    )
    register_locality_plot_resize(
      "plot_gestantes_susdependentes",
      gestantes_susdependentes_multiyear_data,
      function() susdependente_display_years
    )
    register_locality_plot_resize(
      "plot_gestantes_susdependentes_municipal",
      gestantes_susdependentes_multiyear_data,
      function() susdependente_display_years
    )
    register_locality_plot_resize(
      "plot_nascidos_susdependentes_estado",
      nascidos_susdependentes_multiyear_data,
      function() susdependente_display_years
    )
    register_locality_plot_resize(
      "plot_nascidos_susdependentes_outros",
      nascidos_susdependentes_multiyear_data,
      function() susdependente_display_years
    )
    register_locality_plot_resize(
      "plot_nascidos_susdependentes_municipal",
      nascidos_susdependentes_multiyear_data,
      function() susdependente_display_years
    )

    output$aps_graph_tabs <- renderUI({
      req(input$nivel_selection)
      req(aps_filter_ready(), cancelOutput = TRUE)

      plot_col <- function(output_id, width = 4L, offset = 0L) {
        column(
          width = width,
          offset = offset,
          class = "aps-plot-col",
          shinycssloaders::withSpinner(uiOutput(ns(output_id)))
        )
      }

      is_municipal <- identical(input$nivel_selection, "MUNICIPAL")
      is_municipal_sp <- is_municipal && identical(input$analisar_muni_sp, "SIM")

      if (is_municipal_sp) {
        return(NULL)
      }

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
            class = "estab-tabs-prenatal aps-graph-tabs",
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
      req(aps_filter_ready(), cancelOutput = TRUE)

      if (identical(input$nivel_selection, "MUNICIPAL") &&
          !identical(input$analisar_muni_sp, "SIM")) {
        return(NULL)
      }

      main_boxes <- fluidRow(
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_1")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_3")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_4")))),
        column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_2"))))
      )

      if (identical(input$nivel_selection, "MUNICIPAL") &&
          identical(input$analisar_muni_sp, "SIM")) {
        return(
          tagList(
            main_boxes,
            br(),
            fluidRow(
              column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_cobertura_ans")))),
              column(width = 3, shinycssloaders::withSpinner(uiOutput(ns("summary_box_cobertura_ab"))))
            )
          )
        )
      }

      main_boxes
    })

    output$summary_box_1 <- renderUI({
      req(aps_filter_ready(), cancelOutput = TRUE)
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
        build_summary_year_caption(summary_year)
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
        class = summary_box_class("box-primary"),
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4("Nascidos vivos"),
        h3(format_number(total_nascidos)),
        caption
      )
    })

    output$summary_box_2 <- renderUI({
      req(aps_filter_ready(), cancelOutput = TRUE)
      if (isTRUE(is_updated_susdependente_context())) {
        summary_year <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          susdependente_default_summary_year
        } else {
          selected_nascidos_susdependentes_summary_year()
        }
        total_sus_nasc <- susdependente_summary_total("nascidos_susdependentes", summary_year)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_caption(summary_year)
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
      } else if (legacy_supervision_has_values(aps_col_nascidos_sus)) {
        total_sus_nasc <- legacy_supervision_total(aps_col_nascidos_sus)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_caption("2023")
        } else {
          NULL
        }
        dropdown <- NULL
      } else {
        total_sus_nasc <- NA_real_
        caption <- build_summary_unavailable_caption()
        dropdown <- NULL
      }
      div(
        class = summary_box_class("box-success"),
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4("Nascidos vivos SUSdependentes"),
        h3(format_number(round(total_sus_nasc, 0))),
        caption
      )
    })

    output$summary_box_3 <- renderUI({
      req(aps_filter_ready(), cancelOutput = TRUE)
      if (isTRUE(is_updated_ubs_context())) {
        summary_year <- selected_ubs_cnes_summary_year()
        total_ubs <- ubs_cnes_summary_total()
        dropdown <- build_summary_year_dropdown(
          "ubs_cnes_summary_year",
          ubs_cnes_summary_year_choices(),
          summary_year
        )
        caption <- NULL
      } else if (legacy_supervision_has_values(aps_col_ubs)) {
        total_ubs <- legacy_supervision_total(aps_col_ubs)
        dropdown <- NULL
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_caption("2022/2023")
        } else {
          NULL
        }
      } else {
        total_ubs <- NA_real_
        dropdown <- NULL
        caption <- build_summary_unavailable_caption()
      }
      div(
        class = summary_box_class("box-danger"),
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
      req(aps_filter_ready(), cancelOutput = TRUE)
      if (isTRUE(is_updated_susdependente_context())) {
        summary_year <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          susdependente_default_summary_year
        } else {
          selected_gestantes_susdependentes_summary_year()
        }
        total_gestantes <- susdependente_summary_total("gestantes_susdependentes", summary_year)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_caption(summary_year)
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
      } else if (legacy_supervision_has_values(aps_col_gestantes)) {
        total_gestantes <- legacy_supervision_total(aps_col_gestantes)
        caption <- if (identical(input$nivel_selection, "MUNICIPAL")) {
          build_summary_year_caption("2023")
        } else {
          NULL
        }
        dropdown <- NULL
      } else {
        total_gestantes <- NA_real_
        caption <- build_summary_unavailable_caption()
        dropdown <- NULL
      }
      div(
        class = summary_box_class("box-warning"),
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        dropdown,
        h4("Gestantes SUSdependentes"),
        h3(format_number(round(total_gestantes, 0))),
        caption
      )
    })

    output$summary_box_cobertura_ans <- renderUI({
      req(aps_filter_ready(), cancelOutput = TRUE)
      req(
        identical(input$nivel_selection, "MUNICIPAL"),
        identical(input$analisar_muni_sp, "SIM")
      )

      metric <- legacy_supervision_metric(aps_col_cobertura_ans)
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        h4(
          "Cobertura da Saúde Suplementar (ANS %)",
          style = "font-size:16px; line-height:1.2; text-align:center; margin:0 8px 6px 8px;"
        ),
        h3(if (is.finite(metric)) paste0(format_metric_percent(metric), "%") else "Não disponível"),
        build_summary_year_caption(legacy_supervision_year_label(aps_col_cobertura_ans))
      )
    })

    output$summary_box_cobertura_ab <- renderUI({
      req(aps_filter_ready(), cancelOutput = TRUE)
      req(
        identical(input$nivel_selection, "MUNICIPAL"),
        identical(input$analisar_muni_sp, "SIM")
      )

      metric <- legacy_supervision_metric(aps_col_cobertura_ab)
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center; position:relative;",
        h4(
          "Cobertura da Atenção Básica (AB %)",
          style = "font-size:16px; line-height:1.2; text-align:center; margin:0 8px 6px 8px;"
        ),
        h3(if (is.finite(metric)) paste0(format_metric_percent(metric), "%") else "Não disponível"),
        build_summary_year_caption(legacy_supervision_year_label(aps_col_cobertura_ab))
      )
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
          visible_groups = 12L,
          bars_per_group = 1L
        )$visible_height
      } else {
        calc_dynamic_height(nrow(data_source))
      }
      target_visible_height <- max(320L, as.integer(target_visible_height))

      build_plot_card(
        "Nascidos vivos",
        "plot_nascidos_vivos",
        data_source,
        caption = nascidos_caption(),
        height_override = height_cfg$full_height,
        scroll_max_height = target_visible_height,
        locality_filter = locality_filter_config(data_source, "plot_nascidos_vivos")
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
            visible_groups = 12L,
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
            scroll_max_height = max(320L, as.integer(target_visible_height)),
            locality_filter = locality_filter_config(data_source, "plot_ubs")
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_ubs)) {
        data_source <- legacy_supervision_plot_data(aps_col_ubs)
        height_cfg <- legacy_supervision_height(data_source)
        return(
          build_plot_card(
            "Unidade Básica de Saúde (UBS)",
            "plot_ubs",
            data_source,
            caption = legacy_supervision_year_caption(aps_col_ubs),
            height_override = height_cfg$full_height,
            scroll_max_height = height_cfg$visible_height
          )
        )
      }

      build_unavailable_plot_card("Unidade Básica de Saúde (UBS)", "plot_ubs")
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
            visible_groups = 12L,
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
            scroll_max_height = max(320L, as.integer(target_visible_height)),
            locality_filter = locality_filter_config(data_source, "plot_gestantes_susdependentes")
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_gestantes)) {
        data_source <- legacy_supervision_plot_data(aps_col_gestantes)
        height_cfg <- legacy_supervision_height(data_source)
        return(
          build_plot_card(
            "Gestantes SUSdependentes",
            "plot_gestantes_susdependentes",
            data_source,
            caption = legacy_supervision_year_caption(aps_col_gestantes),
            height_override = height_cfg$full_height,
            scroll_max_height = height_cfg$visible_height
          )
        )
      }

      build_unavailable_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes")
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
            scroll_max_height = max(320L, as.integer(calc_dynamic_height(nrow(data_source)))),
            locality_filter = locality_filter_config(data_source, "plot_nascidos_susdependentes_estado")
          )
        )
      }
      build_unavailable_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_estado")
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
            visible_groups = 12L,
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
            scroll_max_height = max(320L, as.integer(target_visible_height)),
            locality_filter = locality_filter_config(data_source, "plot_nascidos_susdependentes_outros")
          )
        )
      }

      build_unavailable_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_outros")
    })

    # Para níveis RRAS 6
    output$card_plot_nascidos_susdependentes_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if (isTRUE(is_updated_susdependente_context())) {
        data_source <- nascidos_susdependentes_multiyear_data()
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(susdependente_display_years)
        )

        return(
          build_plot_card(
            "Nascidos vivos SUSdependentes",
            "plot_nascidos_susdependentes_rras6",
            data_source,
            caption = susdependente_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(calc_dynamic_height(nrow(data_source))))
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_nascidos_sus)) {
        data_source <- legacy_supervision_plot_data(aps_col_nascidos_sus)
        height_cfg <- legacy_supervision_height(data_source)
        return(
          build_plot_card(
            "Nascidos vivos SUSdependentes",
            "plot_nascidos_susdependentes_rras6",
            data_source,
            caption = legacy_supervision_year_caption(aps_col_nascidos_sus),
            height_override = height_cfg$full_height,
            scroll_max_height = height_cfg$visible_height
          )
        )
      }

      build_unavailable_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_rras6")
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
            visible_groups = 12L,
            bars_per_group = 1L
          )$visible_height
        } else {
          calc_dynamic_height(nrow(data_source))
        }

        return(
          build_plot_card(
            "Cobertura da Saúde Suplementar (ANS %)",
            "plot_cobertura_ans",
            data_source,
            caption = cobertura_ans_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(target_visible_height))
          )
        )
      }

      build_unavailable_plot_card("Cobertura da Saúde Suplementar (ANS %)", "plot_cobertura_ans")
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
            "Cobertura da Estratégia Saúde da Família (ESF %)",
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
                    visible_groups = 12L,
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
      build_unavailable_plot_card("Cobertura da Estratégia Saúde da Família (ESF %)", "plot_cobertura_esf")
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
            "Cobertura da Atenção Básica (AB %)",
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
                    visible_groups = 12L,
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
      build_unavailable_plot_card("Cobertura da Atenção Básica (AB %)", "plot_cobertura_ab")
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
        scroll_max_height = height_cfg$visible_height,
        locality_filter = locality_filter_config(data_source, "plot_nascidos_vivos_municipal")
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
        scroll_max_height = height_cfg$visible_height,
        locality_filter = locality_filter_config(data_source, "plot_ubs_municipal")
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
        scroll_max_height = height_cfg$visible_height,
        locality_filter = locality_filter_config(data_source, "plot_gestantes_susdependentes_municipal")
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
        scroll_max_height = height_cfg$visible_height,
        locality_filter = locality_filter_config(data_source, "plot_nascidos_susdependentes_municipal")
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
        "Cobertura da Estratégia Saúde da Família (ESF %)",
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
      if (identical(input$analisar_muni_sp, "SIM")) {
        if (legacy_supervision_has_values(aps_col_cobertura_ab)) {
          data_source <- legacy_supervision_plot_data(aps_col_cobertura_ab)
          height_cfg <- legacy_supervision_height(data_source)
          return(
            build_plot_card(
              "Cobertura da Atenção Básica (AB %)",
              "plot_cobertura_ab_municipal",
              data_source,
              height_override = height_cfg$full_height,
              scroll_max_height = height_cfg$visible_height
            )
          )
        }

        return(
          build_unavailable_plot_card(
            "Cobertura da Atenção Básica (AB %)",
            "plot_cobertura_ab_municipal"
          )
        )
      }
      if (!isTRUE(is_updated_ab_context())) return(NULL)

      data_source <- cobertura_ab_comparison_data()
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(cobertura_display_years)
      )

      build_plot_card(
        "Cobertura da Atenção Básica (AB %)",
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
      if (identical(input$analisar_muni_sp, "SIM")) {
        if (legacy_supervision_has_values(aps_col_cobertura_ans)) {
          data_source <- legacy_supervision_plot_data(aps_col_cobertura_ans)
          height_cfg <- legacy_supervision_height(data_source)
          return(
            build_plot_card(
              "Cobertura da Saúde Suplementar (ANS %)",
              "plot_cobertura_ans_municipal",
              data_source,
              height_override = height_cfg$full_height,
              scroll_max_height = height_cfg$visible_height
            )
          )
        }

        return(
          build_unavailable_plot_card(
            "Cobertura da Saúde Suplementar (ANS %)",
            "plot_cobertura_ans_municipal"
          )
        )
      }
      if (!isTRUE(is_updated_ans_context())) return(NULL)

      data_source <- cobertura_ans_multiyear_data()
      years <- cobertura_ans_display_years
      height_cfg <- calc_multiyear_grouped_height(
        nrow(data_source),
        visible_groups = 1L,
        bars_per_group = length(years)
      )

      build_plot_card(
        "Cobertura da Saúde Suplementar (ANS %)",
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
      if (isTRUE(is_updated_ans_context())) {
        data_source <- cobertura_ans_multiyear_data()
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(cobertura_ans_display_years)
        )

        return(
          build_plot_card(
            "Cobertura da Saúde Suplementar (ANS %)",
            "plot_cobertura_ans_rras6",
            data_source,
            caption = cobertura_ans_caption(),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(calc_dynamic_height(nrow(data_source))))
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_cobertura_ans)) {
        data_source <- legacy_supervision_plot_data(aps_col_cobertura_ans)
        height_cfg <- legacy_supervision_height(data_source)
        return(
          build_plot_card(
            "Cobertura da Saúde Suplementar (ANS %)",
            "plot_cobertura_ans_rras6",
            data_source,
            caption = legacy_supervision_year_caption(aps_col_cobertura_ans),
            height_override = height_cfg$full_height,
            scroll_max_height = height_cfg$visible_height
          )
        )
      }

      build_unavailable_plot_card("Cobertura da Saúde Suplementar (ANS %)", "plot_cobertura_ans_rras6")
    })
    output$card_plot_cobertura_ab_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if (isTRUE(is_updated_ab_context())) {
        data_source <- cobertura_ab_comparison_data()
        height_cfg <- calc_multiyear_grouped_height(
          nrow(data_source),
          bars_per_group = length(cobertura_display_years)
        )

        return(
          build_plot_card(
            "Cobertura da Atenção Básica (AB %)",
            "plot_cobertura_ab_rras6",
            data_source,
            caption = build_multiyear_caption_legend(
              legacy_year = cobertura_ab_legacy_year,
              consolidated_years = cobertura_consolidated_years,
              preliminary_year = cobertura_preliminary_year
            ),
            height_override = height_cfg$full_height,
            scroll_max_height = max(320L, as.integer(calc_dynamic_height(nrow(data_source))))
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_cobertura_ab)) {
        data_source <- legacy_supervision_plot_data(aps_col_cobertura_ab)
        height_cfg <- legacy_supervision_height(data_source)
        return(
          build_plot_card(
            "Cobertura da Atenção Básica (AB %)",
            "plot_cobertura_ab_rras6",
            data_source,
            caption = legacy_supervision_year_caption(aps_col_cobertura_ab),
            height_override = height_cfg$full_height,
            scroll_max_height = height_cfg$visible_height
          )
        )
      }

      build_unavailable_plot_card("Cobertura da Atenção Básica (AB %)", "plot_cobertura_ab_rras6")
    })

    output$plot_nascidos_vivos <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      build_nascidos_multiyear_plot(
        data = filter_locality_plot_data(nascidos_vivos_multiyear_data(), "plot_nascidos_vivos"),
        years = nascidos_display_years(),
        metric_title = "Nascidos vivos"
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_nascidos_vivos"),
        "plot_nascidos_vivos_multiyear",
        cache = "app"
      )

    output$plot_nascidos_vivos_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")

      build_nascidos_multiyear_plot(
        data = filter_locality_plot_data(nascidos_vivos_multiyear_data(), "plot_nascidos_vivos_municipal"),
        years = nascidos_display_years(),
        metric_title = "Nascidos vivos"
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_nascidos_vivos_municipal"),
        "plot_nascidos_vivos_municipal",
        cache = "app"
      )

    output$plot_gestantes_susdependentes_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_susdependente_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_nascidos_multiyear_plot(
        data = filter_locality_plot_data(
          gestantes_susdependentes_multiyear_data(),
          "plot_gestantes_susdependentes_municipal"
        ),
        years = susdependente_display_years,
        metric_title = "Gestantes SUSdependentes",
        axis_title = "Nº ESTIMADO",
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_gestantes_susdependentes_municipal"),
        "plot_gestantes_susdependentes_municipal",
        cache = "app"
      )

    output$plot_nascidos_susdependentes_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_susdependente_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_nascidos_multiyear_plot(
        data = filter_locality_plot_data(
          nascidos_susdependentes_multiyear_data(),
          "plot_nascidos_susdependentes_municipal"
        ),
        years = susdependente_display_years,
        metric_title = "Nascidos vivos SUSdependentes",
        axis_title = "Nº ESTIMADO",
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_nascidos_susdependentes_municipal"),
        "plot_nascidos_susdependentes_municipal",
        cache = "app"
      )

    output$plot_ubs <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      if (isTRUE(is_updated_ubs_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = filter_locality_plot_data(ubs_cnes_multiyear_data(), "plot_ubs"),
            years = ubs_cnes_display_years,
            metric_title = "Unidade Básica de Saúde (UBS)",
            axis_title = "Nº DE UBS",
            year_color = ubs_cnes_year_color,
            preliminary_year = NA_integer_,
            locality_axis_title = current_municipal_source_axis_title()
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_ubs)) {
        return(
          build_bar_plot(
            data = legacy_supervision_plot_data(aps_col_ubs),
            var_numeric = aps_col_ubs,
            var_category = "SUPERVISÃO DE SAÚDE",
            bar_width = single_series_supervision_bar_width
          )
        )
      }

      build_nascidos_multiyear_plot(
        data = data.frame(),
        years = ubs_cnes_display_years,
        metric_title = "Unidade Básica de Saúde (UBS)",
        axis_title = "Nº DE UBS",
        year_color = ubs_cnes_year_color,
        preliminary_year = NA_integer_,
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_ubs"),
        "plot_ubs",
        cache = "app"
      )

    output$plot_ubs_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ubs_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_nascidos_multiyear_plot(
        data = filter_locality_plot_data(ubs_cnes_multiyear_data(), "plot_ubs_municipal"),
        years = ubs_cnes_display_years,
        metric_title = "Unidade Básica de Saúde (UBS)",
        axis_title = "Nº DE UBS",
        year_color = ubs_cnes_year_color,
        preliminary_year = NA_integer_,
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_ubs_municipal"),
        "plot_ubs_municipal",
        cache = "app"
      )

    output$plot_gestantes_susdependentes <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = filter_locality_plot_data(
              gestantes_susdependentes_multiyear_data(),
              "plot_gestantes_susdependentes"
            ),
            years = susdependente_display_years,
            metric_title = "Gestantes SUSdependentes",
            axis_title = "Nº ESTIMADO",
            locality_axis_title = current_municipal_source_axis_title()
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_gestantes)) {
        return(
          build_bar_plot(
            data = legacy_supervision_plot_data(aps_col_gestantes),
            var_numeric = aps_col_gestantes,
            var_category = "SUPERVISÃO DE SAÚDE",
            bar_width = single_series_supervision_bar_width
          )
        )
      }

      build_nascidos_multiyear_plot(
        data = data.frame(),
        years = susdependente_display_years,
        metric_title = "Gestantes SUSdependentes",
        axis_title = "Nº ESTIMADO",
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_gestantes_susdependentes"),
        "plot_gestantes_susdependentes",
        cache = "app"
      )

    output$plot_nascidos_susdependentes_estado <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (identical(input$nivel_selection, "MUNICIPAL")) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = filter_locality_plot_data(
              nascidos_susdependentes_multiyear_data(),
              "plot_nascidos_susdependentes_estado"
            ),
            years = susdependente_display_years,
            metric_title = "Nascidos vivos SUSdependentes",
            axis_title = "Nº ESTIMADO",
            locality_axis_title = current_municipal_source_axis_title()
          )
        )
      }

      build_nascidos_multiyear_plot(
        data = data.frame(),
        years = susdependente_display_years,
        metric_title = "Nascidos vivos SUSdependentes",
        axis_title = "Nº ESTIMADO",
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_nascidos_susdependentes_estado"),
        "plot_nascidos_susdependentes_estado",
        cache = "app"
      )

    output$plot_nascidos_susdependentes_outros <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = filter_locality_plot_data(
              nascidos_susdependentes_multiyear_data(),
              "plot_nascidos_susdependentes_outros"
            ),
            years = susdependente_display_years,
            metric_title = "Nascidos vivos SUSdependentes",
            axis_title = "Nº ESTIMADO",
            locality_axis_title = current_municipal_source_axis_title()
          )
        )
      }

      build_nascidos_multiyear_plot(
        data = data.frame(),
        years = susdependente_display_years,
        metric_title = "Nascidos vivos SUSdependentes",
        axis_title = "Nº ESTIMADO",
        locality_axis_title = current_municipal_source_axis_title()
      )
    }) %>%
      shiny::bindCache(
        aps_plot_cache_key(),
        locality_filter_cache_key("plot_nascidos_susdependentes_outros"),
        "plot_nascidos_susdependentes_outros",
        cache = "app"
      )

    # RRAS 6
    output$plot_nascidos_susdependentes_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      if (isTRUE(is_updated_susdependente_context())) {
        return(
          build_nascidos_multiyear_plot(
            data = nascidos_susdependentes_multiyear_data(),
            years = susdependente_display_years,
            metric_title = "Nascidos vivos SUSdependentes",
            axis_title = "Nº ESTIMADO",
            locality_axis_title = current_municipal_source_axis_title()
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_nascidos_sus)) {
        return(
          build_bar_plot(
            data = legacy_supervision_plot_data(aps_col_nascidos_sus),
            var_numeric = aps_col_nascidos_sus,
            var_category = "SUPERVISÃO DE SAÚDE",
            bar_width = single_series_supervision_bar_width
          )
        )
      }

      build_nascidos_multiyear_plot(
        data = data.frame(),
        years = susdependente_display_years,
        metric_title = "Nascidos vivos SUSdependentes",
        axis_title = "Nº ESTIMADO",
        locality_axis_title = current_municipal_source_axis_title()
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
            metric_title = "Cobertura da Saúde Suplementar (ANS %)"
          )
        )
      }

      build_cobertura_ans_multiyear_plot(
        data = data.frame(),
        years = cobertura_ans_display_years,
        metric_title = "Cobertura da Saúde Suplementar (ANS %)"
      )
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
            metric_title = "Cobertura da Estratégia Saúde da Família (ESF %)",
            force_vertical = cfg$force_v
          )
        )
      }
      build_coverage_multiyear_plot(
        data = data.frame(),
        metric_title = "Cobertura da Estratégia Saúde da Família (ESF %)"
      )
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
            metric_title = "Cobertura da Atenção Básica (AB %)",
            force_vertical = cfg$force_v
          )
        )
      }

      build_coverage_multiyear_plot(
        data = data.frame(),
        metric_title = "Cobertura da Atenção Básica (AB %)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ab", cache = "app")

    output$plot_cobertura_esf_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (!isTRUE(is_updated_ab_context()) || identical(input$analisar_muni_sp, "SIM")) return(NULL)

      build_coverage_multiyear_plot(
        data = cobertura_esf_comparison_data(),
        metric_title = "Cobertura da Estratégia Saúde da Família (ESF %)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_esf_municipal", cache = "app")

    output$plot_cobertura_ab_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (identical(input$analisar_muni_sp, "SIM")) {
        if (legacy_supervision_has_values(aps_col_cobertura_ab)) {
          return(
            build_bar_plot(
              data = legacy_supervision_plot_data(aps_col_cobertura_ab),
              var_numeric = aps_col_cobertura_ab,
              var_category = "SUPERVISÃO DE SAÚDE",
              is_percentage = TRUE,
              bar_width = single_series_supervision_bar_width
            )
          )
        }
        return(build_coverage_multiyear_plot(data.frame(), "Cobertura da Atenção Básica (AB %)"))
      }
      if (!isTRUE(is_updated_ab_context())) return(NULL)

      build_coverage_multiyear_plot(
        data = cobertura_ab_comparison_data(),
        metric_title = "Cobertura da Atenção Básica (AB %)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ab_municipal", cache = "app")

    output$plot_cobertura_ans_municipal <- plotly::renderPlotly({
      req(input$nivel_selection == "MUNICIPAL")
      if (identical(input$analisar_muni_sp, "SIM")) {
        if (legacy_supervision_has_values(aps_col_cobertura_ans)) {
          return(
            build_bar_plot(
              data = legacy_supervision_plot_data(aps_col_cobertura_ans),
              var_numeric = aps_col_cobertura_ans,
              var_category = "SUPERVISÃO DE SAÚDE",
              is_percentage = TRUE,
              bar_width = single_series_supervision_bar_width
            )
          )
        }
        return(build_cobertura_ans_multiyear_plot(data.frame(), cobertura_ans_display_years))
      }
      if (!isTRUE(is_updated_ans_context())) return(NULL)

      build_cobertura_ans_multiyear_plot(
        data = cobertura_ans_multiyear_data(),
        years = cobertura_ans_display_years,
        metric_title = "Cobertura da Saúde Suplementar (ANS %)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ans_municipal", cache = "app")

    # GRÁFICOS DE COBERTURA (para RRAS 6)
    output$plot_cobertura_ans_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      if (isTRUE(is_updated_ans_context())) {
        return(
          build_cobertura_ans_multiyear_plot(
            data = cobertura_ans_multiyear_data(),
            years = cobertura_ans_display_years,
            metric_title = "Cobertura da Saúde Suplementar (ANS %)"
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_cobertura_ans)) {
        return(
          build_bar_plot(
            data = legacy_supervision_plot_data(aps_col_cobertura_ans),
            var_numeric = aps_col_cobertura_ans,
            var_category = "SUPERVISÃO DE SAÚDE",
            is_percentage = TRUE,
            bar_width = single_series_supervision_bar_width
          )
        )
      }

      build_cobertura_ans_multiyear_plot(
        data = data.frame(),
        years = cobertura_ans_display_years,
        metric_title = "Cobertura da Saúde Suplementar (ANS %)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ans_rras6", cache = "app")

    output$plot_cobertura_ab_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if (!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)

      if (isTRUE(is_updated_ab_context())) {
        return(
          build_coverage_multiyear_plot(
            data = cobertura_ab_comparison_data(),
            metric_title = "Cobertura da Atenção Básica (AB %)"
          )
        )
      }

      if (legacy_supervision_has_values(aps_col_cobertura_ab)) {
        return(
          build_bar_plot(
            data = legacy_supervision_plot_data(aps_col_cobertura_ab),
            var_numeric = aps_col_cobertura_ab,
            var_category = "SUPERVISÃO DE SAÚDE",
            is_percentage = TRUE,
            bar_width = single_series_supervision_bar_width
          )
        )
      }

      build_coverage_multiyear_plot(
        data = data.frame(),
        metric_title = "Cobertura da Atenção Básica (AB %)"
      )
    }) %>%
      shiny::bindCache(aps_plot_cache_key(), "plot_cobertura_ab_rras6", cache = "app")
  })
}
