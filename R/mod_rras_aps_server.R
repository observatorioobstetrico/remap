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

    # Função auxiliar para construir o card que conterá o gráfico
    build_plot_card <- function(card_title, plot_output_id, data_to_plot, caption = NULL, height_override = NULL) {
      n_bars <- nrow(data_to_plot)
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
      n_bars <- nrow(data)

      # Pré-formata os valores para o hover (customdata)
      if(is_percentage){
        data$formatted_value <- sapply(data[[var_numeric]], function(x) {
          x_num <- as.numeric(x)
          format(round(x_num, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
        })
      } else {
        data$formatted_value <- sapply(data[[var_numeric]], function(x) {
          x_num <- as.numeric(x)
          # Arredonda para inteiro e formata com separador de milhar como ponto
          format_integer(x_num)
        })
      }

      if(force_vertical) {
        orientation <- "v"
      } else if(n_bars <= 10) {
        orientation <- "v"
      } else {
        tick_names <- as.character(data[[var_category]])
        all_short <- all(nchar(tick_names) <= 12 & sapply(tick_names, function(x) length(unlist(strsplit(x, "\\s+")))) <= 2)
        orientation <- if(all_short) "v" else "h"
      }

      # Define o hovertemplate para mostrar a categoria e o valor formatado (customdata)
      if (orientation == "h") {
        # Em orientação horizontal, x é o valor e y é a categoria.
        hovertemplate <- paste0("%{y}<br>%{customdata}<extra></extra>")
      } else {
        # Em orientação vertical, x é a categoria e y é o valor.
        hovertemplate <- paste0("%{x}<br>%{customdata}<extra></extra>")
      }

      # Configuração padrão do hoverlabel
      hl <- list(bgcolor = "white",
                 bordercolor = "#0A1E3C",
                 font = list(color = "black", size = 14, family = "Arial Black"))

      # Para os eixos, se não for porcentagem, cria rótulos customizados com separador de milhar.
      if (!is_percentage) {
        # Para o eixo que exibe os valores (dependendo da orientação)
        tick_vals_numeric <- pretty(range(c(0, data[[var_numeric]]), na.rm = TRUE))
        tick_text_numeric <- sapply(tick_vals_numeric, format_integer)
      }

      if (orientation == "h") {
        # Para orientação horizontal: x é o valor, y é a categoria.
        # Configura o eixo x com os ticks customizados se não for porcentagem.
        xaxis_config <- list(
          title = list(text = wrap_vertical_title(var_numeric), standoff = 0L),
          tickfont = list(size = 12, color = "#000000")
        )
        if(is_percentage){
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
        p <- plotly::layout(p,
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
        # Para orientação vertical: x é a categoria, y é o valor.
        original_categories <- data[[var_category]]
        categories <- ifelse(
          grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
          sub("\\s+", "<br>", original_categories),
          gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
        )

        # Configura o eixo y com ticks customizados se não for porcentagem.
        yaxis_config <- list(
          title = list(text = wrap_after_second(var_numeric, threshold = 19), standoff = 20L, size = 1),
          tickfont = list(size = 12, color = "#000000")
        )
        if(is_percentage){
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
        p <- plotly::layout(p,
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
                            hoverlabel = hl)
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
    observe({
      req(input$nivel_selection)
      level <- input$nivel_selection
      if(level == "RRAS"){
        choices <- sort(unique(tabela_APS$RRAS))
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      } else if(level == "DRS"){
        if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
          choices <- sort(unique(tabela_APS$`COORDENADORIA DE SAÚDE`))
        } else {
          choices <- sort(unique(tabela_APS$DRS))
        }
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      } else if(level == "REGIÃO DE SAÚDE"){
        choices <- sort(unique(tabela_APS$`REGIÃO DE SAÚDE`))
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      } else if(level == "MUNICIPAL"){
        if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
          choices <- sort(unique(tabela_APS$`SUPERVISÃO DE SAÚDE`))
        } else {
          choices <- sort(unique(tabela_APS$MUNICIPAL))
        }
        shinyWidgets::updatePickerInput(session, "secondary_filter", choices = choices, selected = choices[1])
      }
    })


    # Filtra dados conforme o nível e o filtro secundário
    filtered_data <- reactive({
      req(input$nivel_selection)
      level <- input$nivel_selection
      if(level == "ESTADUAL"){
        tabela_APS
      } else if(level == "RRAS"){
        req(input$secondary_filter)
        tabela_APS[tabela_APS$RRAS == input$secondary_filter, ]
      } else if(level == "DRS"){
        req(input$secondary_filter)
        if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
          tabela_APS[tabela_APS$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
        } else {
          tabela_APS[tabela_APS$DRS == input$secondary_filter, ]
        }
      } else if(level == "REGIÃO DE SAÚDE"){
        req(input$secondary_filter)
        tabela_APS[tabela_APS$`REGIÃO DE SAÚDE` == input$secondary_filter, ]
      } else if(level == "MUNICIPAL"){
        req(input$secondary_filter)
        if (!is.null(input$analisar_muni_sp) && input$analisar_muni_sp == "SIM") {
          tabela_APS[tabela_APS$`SUPERVISÃO DE SAÚDE` == input$secondary_filter, ]
        } else {
          tabela_APS[tabela_APS$MUNICIPAL == input$secondary_filter, ]
        }
      }
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

    # Plot geral sem dor de cabeça com SP
    plot_data <- reactive({
      level <- input$nivel_selection
      if (level == "ESTADUAL") {
        aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
                        `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                        `Nº DE UBS`,
                        `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`) ~ RRAS,
                  data = tabela_APS, FUN = sum, na.rm = TRUE)
      } else if (level == "DRS" && (!is.null(input$analisar_sp) && input$analisar_sp == "NÃO")) {
        # Para DRS quando NÃO especifica SP, agrega os dados por MUNICIPAL
        df <- tabela_APS[tabela_APS$DRS == input$secondary_filter, ]
        aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
                        `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                        `Nº DE UBS`,
                        `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`,
                        `COBERTURA ANS %`,
                        `COBERTURA ESF %`,
                        `COBERTURA AB %`) ~ MUNICIPAL,
                  data = df, FUN = sum, na.rm = TRUE)
      } else {
        filtered_data()
      }
    })

    # Plot considerando SP (menos cobertura ESF) ao adicionae a linha com seus totais
    plot_data_main <- reactive({
      level <- input$nivel_selection
      if (level == "DRS" && (!is.null(input$analisar_sp) && input$analisar_sp == "NÃO")) {
        df <- tabela_APS[tabela_APS$DRS == input$secondary_filter, ]
        if (input$secondary_filter == "GRANDE SÃO PAULO") {
          # Agrega por município para os demais (SP é deletada pelo agg)
          agg <- aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
                                 `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                                 `Nº DE UBS`,
                                 `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`,
                                 `COBERTURA ANS %`,
                                 `COBERTURA ESF %`,
                                 `COBERTURA AB %`) ~ MUNICIPAL,
                           data = df, FUN = sum, na.rm = TRUE)
          # Converte a coluna MUNICIPAL para character
          agg$MUNICIPAL <- as.character(agg$MUNICIPAL)
          # Se a linha "SÃO PAULO" não estiver presente, cria-a agregando todas as linhas correspondentes
          if (!("SÃO PAULO" %in% agg$MUNICIPAL)) {
            df2 <- df[df$MUNICIPAL == "SÃO PAULO", ] # Subset só da cidade de SP
            total_sp <- data_list$total_sp # Carrega linha de totais de SP
            # Valors da planilha estão com separador decimal em vírgula (passa pra ponto)
            total_sp$`COBERTURA ANS %` <- as.numeric(gsub(",", ".", total_sp$`COBERTURA ANS %`))
            total_sp$`COBERTURA AB %` <- as.numeric(gsub(",", ".", total_sp$`COBERTURA AB %`))

            # Define dataframe dos totais de SP
            sp <- data.frame(
              MUNICIPAL = "SÃO PAULO",
              `Nº DE NASCIDOS VIVOS` = sum(df2$`Nº DE NASCIDOS VIVOS`, na.rm = TRUE),
              `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO` = sum(df2$`NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`, na.rm = TRUE),
              `Nº DE UBS` = sum(df2$`Nº DE UBS`, na.rm = TRUE),
              `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO` = sum(df2$`GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`, na.rm = TRUE),
              `COBERTURA ANS %` = sum(total_sp$`COBERTURA ANS %`, na.rm = TRUE),
              `COBERTURA ESF %` = sum(df$`COBERTURA ESF %`, na.rm = TRUE),
              `COBERTURA AB %` = sum(total_sp$`COBERTURA AB %`, na.rm = TRUE),
              stringsAsFactors = FALSE
            )
            # Se necessário, alinhe os nomes
            names(sp) <- names(agg)
            agg <- rbind(agg, sp) # Adiciona a linha de SP ao dataframe agregado
          }
          agg
        } else {
          aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
                          `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                          `Nº DE UBS`,
                          `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`,
                          `COBERTURA ANS %`,
                          `COBERTURA ESF %`,
                          `COBERTURA AB %`) ~ MUNICIPAL,
                    data = df, FUN = sum, na.rm = TRUE)
        }
      } else if (level == "ESTADUAL") {
        aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
                        `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                        `Nº DE UBS`,
                        `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`) ~ RRAS,
                  data = tabela_APS, FUN = sum, na.rm = TRUE)
      } else {
        filtered_data()
      }
    })

    # Plot para não considerar SP (casos específicos como a cobertura ESF, onde SP não possui dados)
    plot_data_cov <- reactive({
      df <- plot_data_main()
      # Se a DRS selecionada for GRANDE SÃO PAULO, remove a linha "SÃO PAULO" para os gráficos de cobertura
      if (input$nivel_selection == "DRS" && input$secondary_filter == "GRANDE SÃO PAULO") {
        df <- dplyr::filter(df, MUNICIPAL != "SÃO PAULO")
      }
      df
    })


    # Dados para tabelas de AAE
    filtered_data_aae <- reactive({
      level <- input$nivel_selection
      if (level == "RRAS") {
        req(input$secondary_filter)
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
          data_list$tabela_1_APS_AAE,
          data_list$tabela_2_APS_AAE,
          data_list$tabela_3_APS_AAE,
          data_list$tabela_4_APS_AAE,
          data_list$tabela_5_APS_AAE,
          data_list$tabela_6_APS_AAE,
          data_list$tabela_7_APS_AAE,
          data_list$tabela_8_APS_AAE,
          data_list$tabela_9_APS_AAE,
          data_list$tabela_10_APS_AAE,
          data_list$tabela_11_APS_AAE,
          data_list$tabela_12_APS_AAE,
          data_list$tabela_13_APS_AAE,
          data_list$tabela_14_APS_AAE,
          data_list$tabela_15_APS_AAE,
          data_list$tabela_16_APS_AAE,
          data_list$tabela_17_APS_AAE,
          data_list$tabela_18_APS_AAE
        )
        if (level == "ESTADUAL") {
          table_aae_all
        } else if (level == "DRS") {
          req(input$secondary_filter)
          if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
            dados <- table_aae_all[table_aae_all$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
          } else {
            dados <- table_aae_all[table_aae_all$DRS == input$secondary_filter, ]
          }
          dados <- dados %>%
            rename("MUNICÍPIO DA DRS" = "MUNICÍPIO DA RRAS")
          dados <- dados %>%
            dplyr::filter(!if_all(everything(), is.na))
        } else if (level == "REGIÃO DE SAÚDE") {
          req(input$secondary_filter)
          dados <- table_aae_all[table_aae_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ]
          dados <- dados %>%
            rename("MUNICÍPIO DA REGIÃO DE SAÚDE" = "MUNICÍPIO DA RRAS")
        } else if (level == "MUNICIPAL") {
          req(input$secondary_filter)
          table_aae_all[table_aae_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
        }
      }
    })

    # Dados para tabelas de BAIXO RISCO
    filtered_data_bxr <- reactive({
      level <- input$nivel_selection
      if(level == "RRAS"){
        req(input$secondary_filter)
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
          data_list$tabela_1_APS_BXRISCO,
          data_list$tabela_2_APS_BXRISCO,
          data_list$tabela_3_APS_BXRISCO,
          data_list$tabela_4_APS_BXRISCO,
          data_list$tabela_5_APS_BXRISCO,
          data_list$tabela_6_APS_BXRISCO,
          data_list$tabela_7_APS_BXRISCO,
          data_list$tabela_8_APS_BXRISCO,
          data_list$tabela_9_APS_BXRISCO,
          data_list$tabela_10_APS_BXRISCO,
          data_list$tabela_11_APS_BXRISCO,
          data_list$tabela_12_APS_BXRISCO,
          data_list$tabela_13_APS_BXRISCO,
          data_list$tabela_14_APS_BXRISCO,
          data_list$tabela_15_APS_BXRISCO,
          data_list$tabela_16_APS_BXRISCO,
          data_list$tabela_17_APS_BXRISCO,
          data_list$tabela_18_APS_BXRISCO
        )
        if(level == "ESTADUAL"){
          table_bxr_all
        } else if(level == "DRS"){
          req(input$secondary_filter)
          if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
            dados <- table_bxr_all[table_bxr_all$`COORDENADORIA DE SAÚDE` == input$secondary_filter, ]
          } else {
            dados <- table_bxr_all[table_bxr_all$DRS == input$secondary_filter, ]
          }
          dados <- dados %>%
            rename("MUNICÍPIO DA DRS" = "MUNICÍPIO DA RRAS")
          dados <- dados %>%
            dplyr::filter(!if_all(everything(), is.na))
        } else if(level == "REGIÃO DE SAÚDE"){
          req(input$secondary_filter)
          dados <- table_bxr_all[table_bxr_all$`REGIÃO DE SAÚDE` == input$secondary_filter, ]
          dados <- dados %>%
            rename("MUNICÍPIO DA REGIÃO DE SAÚDE" = "MUNICÍPIO DA RRAS")
        } else if(level == "MUNICIPAL"){
          req(input$secondary_filter)
          table_bxr_all[table_bxr_all$`MUNICÍPIO DA RRAS` == input$secondary_filter, ]
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
          "Ano de atualização dos dados: 2020",
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
          "Ano de atualização dos dados: 2020",
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
          "Ano de atualização dos dados: 2020",
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
        build_plot_card("UBS", "plot_ubs", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("UBS", "plot_ubs", plot_data(), caption = "Ano: 2020")
      }
    })
    output$card_plot_gestantes_susdependentes <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("Gestantes SUSdependentes", "plot_gestantes_susdependentes", plot_data(), caption = "Ano: 2020")
      }
    })
    # Para nível ESTADUAL
    output$card_plot_nascidos_susdependentes_estadual <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection != "ESTADUAL") return(NULL)
      build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_estado", plot_data(), caption = "Ano: 2020")
    })

    # Para níveis RRAS (- RRAS 6), DRS ou REGIÃO DE SAÚDE
    output$card_plot_nascidos_susdependentes_outros <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_outros", plot_data(), caption = "Ano: 2020")
    })

    # Para níveis RRAS 6
    output$card_plot_nascidos_susdependentes_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_rras6", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("Nascidos vivos SUSdependentes", "plot_nascidos_susdependentes_rras6", plot_data(), caption = "Ano: 2020")
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
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      force_v <- FALSE
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6" || input$nivel_selection == "REGIÃO DE SAÚDE" && input$secondary_filter == "SÃO PAULO"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data_main(), var_numeric = "Nº DE NASCIDOS VIVOS", var_category = cat_var, force_vertical = force_v)
    })
    output$plot_ubs <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      force_v <- FALSE
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6" || input$nivel_selection == "REGIÃO DE SAÚDE" && input$secondary_filter == "SÃO PAULO"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data_main(), var_numeric = "Nº DE UBS", var_category = cat_var, force_vertical = force_v)
    })
    output$plot_gestantes_susdependentes <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      force_v <- FALSE
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6" || input$nivel_selection == "REGIÃO DE SAÚDE" && input$secondary_filter == "SÃO PAULO"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data_main(), var_numeric = "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO", var_category = cat_var, force_vertical = force_v)
    })
    output$plot_nascidos_susdependentes_estado <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data(), var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO", var_category = cat_var)
    })
    output$plot_nascidos_susdependentes_outros <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data_main(), var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO", var_category = cat_var)
    })
    # RRAS 6
    output$plot_nascidos_susdependentes_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      force_v <- FALSE
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6" || input$nivel_selection == "REGIÃO DE SAÚDE" && input$secondary_filter == "SÃO PAULO"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data(), var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO", var_category = "SUPERVISÃO DE SAÚDE", force_vertical = force_v)
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
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      force_v <- FALSE
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6" || input$nivel_selection == "REGIÃO DE SAÚDE" && input$secondary_filter == "SÃO PAULO"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA ANS %", var_category = cat_var, is_percentage = TRUE, force_vertical = force_v)
    })
    output$plot_cobertura_ab_rras6 <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      force_v <- FALSE
      if(input$nivel_selection == "RRAS" && input$secondary_filter == "RRAS 6" || input$nivel_selection == "REGIÃO DE SAÚDE" && input$secondary_filter == "SÃO PAULO"){
        cat_var <- "SUPERVISÃO DE SAÚDE"
      } else if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        cat_var <- "SUPERVISÃO DE SAÚDE"
        force_v <- TRUE
      } else {
        cat_var <- if(input$nivel_selection == "ESTADUAL") "RRAS" else "MUNICIPAL"
      }
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA AB %", var_category = cat_var, is_percentage = TRUE, force_vertical = force_v)
    })
  })
}
