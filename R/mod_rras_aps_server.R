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

    # Função auxiliar para construir o gráfico de barras com orientação dinâmica.
    # Refatorada para usar chamadas aninhadas em vez de pipe.
    build_bar_plot <- function(data, var_numeric, var_category, is_percentage = FALSE, force_vertical = FALSE) {
      n_bars <- nrow(data)

      if(force_vertical) {
        orientation <- "v"
      } else if(n_bars <= 10) {
        orientation <- "v"
      } else {
        tick_names <- as.character(data[[var_category]])
        all_short <- all(nchar(tick_names) <= 12 & sapply(tick_names, function(x) length(unlist(strsplit(x, "\\s+")))) <= 2)
        orientation <- if(all_short) "v" else "h"
      }

      if (orientation == "h") {
        p <- plotly::plot_ly(
          data = data,
          x = as.formula(paste0("~`", var_numeric, "`")),
          y = as.formula(paste0("~`", var_category, "`")),
          type = "bar",
          orientation = "h",
          marker = list(color = "#0A1E3C")
        )
        p <- plotly::layout(p,
                            xaxis = c(
                              list(
                                title = list(text = wrap_vertical_title(var_numeric), standoff = 0L),
                                tickformat = "d",
                                tickfont = list(size = 12, color = "#000000")
                              ),
                              if(is_percentage) list(range = c(0, 100), dtick = 20) else list()
                            ),
                            yaxis = list(
                              title = list(text = var_category, standoff = 0L),
                              tickfont = list(size = 12, color = "#000000"),
                              categoryorder = "category ascending",
                              autorange = "reversed"
                            )
        )
      } else {
        original_categories <- data[[var_category]]
        categories <- ifelse(
          grepl("^[[:alpha:]]+\\s+[[:alpha:]]+$", original_categories),
          sub("\\s+", "<br>", original_categories),
          gsub("^((\\S+\\s+\\S+))\\s+", "\\1<br>", original_categories)
        )
        p <- plotly::plot_ly(
          data = data,
          x = as.formula(paste0("~`", var_category, "`")),
          y = as.formula(paste0("~`", var_numeric, "`")),
          type = "bar",
          marker = list(color = "#0A1E3C")
        )
        p <- plotly::layout(p,
                            xaxis = list(
                              title = list(text = var_category, standoff = 20L),
                              tickmode = "array",
                              tickvals = original_categories,
                              ticktext = categories,
                              tickangle = 90,
                              automargin = TRUE,
                              tickfont = list(size = 12, color = "#000000")
                            ),
                            yaxis = c(
                              list(
                                title = list(text = wrap_after_second(var_numeric, threshold = 19), standoff = 20L, size = 1),
                                tickfont = list(size = 12, color = "#000000"),
                                tickformat = "d"
                              ),
                              if(is_percentage) list(range = c(0, 100), dtick = 20) else list()
                            ),
                            margin = list(b = 90)
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
        label_text <- "Selecione:"  # label padrão
        if (input$nivel_selection == "DRS") {
          if (!is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
            label_text <- "Selecione a coordenadoria de saúde:"
          } else {
            label_text <- "Selecione a DRS:"
          }
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
        choices <- sort(unique(tabela_APS$MUNICIPAL))
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
        tabela_APS[tabela_APS$MUNICIPAL == input$secondary_filter, ]
      }
    })

    # Dados para gráficos: se ESTADUAL, agregação por RRAS; caso contrário, usa os dados filtrados
    plot_data <- reactive({
      if (input$nivel_selection == "ESTADUAL") {
        aggregate(cbind(`Nº DE NASCIDOS VIVOS`,
                        `NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO`,
                        `Nº DE UBS`,
                        `GESTANTES SUSDEPENDENTES ESTIMADAS/ANO`
                        # `COBERTURA ANS %`,
                        # `COBERTURA ESF %`,
                        # `COBERTURA AB %`
                        ) ~ RRAS,
                  data = tabela_APS, FUN = sum, na.rm = TRUE)
      } else {
        filtered_data()
      }
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
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nascidos Vivos"),
        h3(format_number(total_nascidos))
      )
    })

    output$summary_box_2 <- renderUI({
      total_sus_nasc <- sum(filtered_data()[["NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO"]], na.rm = TRUE)
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nascidos vivos SUSdependentes estimados/ano"),
        h3(format_number(ceiling(total_sus_nasc)))
      )
    })

    output$summary_box_3 <- renderUI({
      total_ubs <- sum(filtered_data()[["Nº DE UBS"]], na.rm = TRUE)
      div(
        class = "custom-box box-danger",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Nº de UBS"),
        h3(format_number(total_ubs))
      )
    })

    output$summary_box_4 <- renderUI({
      total_gestantes <- sum(filtered_data()[["GESTANTES SUSDEPENDENTES ESTIMADAS/ANO"]], na.rm = TRUE)
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Gestantes SUSdependentes estimadas/ano"),
        h3(format_number(ceiling(total_gestantes)))
      )
    })

    # Caixas resumo extras para nível MUNICIPAL
    output$extra_summary_box_1 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA ANS %`, na.rm = TRUE), 2)
      div(
        class = "custom-box box-primary",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura ANS (%)"),
        h3(format_number(metric))
      )
    })

    output$extra_summary_box_2 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA ESF %`, na.rm = TRUE), 2)
      div(
        class = "custom-box box-success",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura ESF (%)"),
        h3(format_number(metric))
      )
    })

    output$extra_summary_box_3 <- renderUI({
      req(input$nivel_selection == "MUNICIPAL")
      data <- filtered_data()
      metric <- round(mean(data$`COBERTURA AB %`, na.rm = TRUE), 2)
      div(
        class = "custom-box box-warning",
        style = "height:125px; display:flex; flex-direction:column; justify-content:center; align-items:center;",
        h4("Cobertura AB (%)"),
        h3(format_number(metric))
      )
    })

    # Renderização dos cards com gráficos, usando a função auxiliar build_plot_card
    output$card_plot_nascidos_vivos <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Nascidos Vivos", "plot_nascidos_vivos", plot_data(), caption = "Ano: 2023", height_override = 400)
      } else {
        build_plot_card("Nascidos Vivos", "plot_nascidos_vivos", plot_data(), caption = "Ano: 2023")
      }
    })
    output$card_plot_ubs <- renderUI({
      req(input$nivel_selection)
      if(input$nivel_selection == "MUNICIPAL") return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Número de UBS", "plot_ubs", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("Número de UBS", "plot_ubs", plot_data(), caption = "Ano: 2020")
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
      build_plot_card("Nascidos Vivos SUSdependentes", "plot_nascidos_susdependentes_estado", plot_data(), caption = "Ano: 2020")
    })

    # Para níveis RRAS (- RRAS 6), DRS ou REGIÃO DE SAÚDE
    output$card_plot_nascidos_susdependentes_outros <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Nascidos Vivos SUSdependentes", "plot_nascidos_susdependentes_outros", plot_data(), caption = "Ano: 2020")
    })

    # Para níveis RRAS 6
    output$card_plot_nascidos_susdependentes_rras6 <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      if(input$nivel_selection == "DRS" && !is.null(input$analisar_sp) && input$analisar_sp == "SIM") {
        # Força uma altura menor para os gráficos de coordenadoria de saúde
        build_plot_card("Nascidos Vivos SUSdependentes", "plot_nascidos_susdependentes_rras6", plot_data(), caption = "Ano: 2020", height_override = 400)
      } else {
        build_plot_card("Nascidos Vivos SUSdependentes", "plot_nascidos_susdependentes_rras6", plot_data(), caption = "Ano: 2020")
      }
    })

    # NOVOS CARDS DE COBERTURA (para RRAS, DRS e REGIÃO DE SAÚDE)
    output$card_plot_cobertura_ans <- renderUI({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_plot_card("Cobertura ANS (%)", "plot_cobertura_ans", plot_data(), caption = "Ano: 2020")
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
        build_plot_card("Cobertura ANS (%)", "plot_cobertura_ans_rras6", plot_data(), caption = "Ano: 2020", height_override = 400)
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
      build_bar_plot(data = plot_data(), var_numeric = "Nº DE NASCIDOS VIVOS", var_category = cat_var, force_vertical = force_v)
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
      build_bar_plot(data = plot_data(), var_numeric = "Nº DE UBS", var_category = cat_var, force_vertical = force_v)
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
      build_bar_plot(data = plot_data(), var_numeric = "GESTANTES SUSDEPENDENTES ESTIMADAS/ANO", var_category = cat_var, force_vertical = force_v)
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
      build_bar_plot(data = plot_data(), var_numeric = "NASCIDOS VIVOS SUSDEPENDENTES ESTIMADOS/ANO", var_category = cat_var)
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
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA ANS %", var_category = "MUNICIPAL", is_percentage = TRUE)
    })
    output$plot_cobertura_esf <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA ESF %", var_category = "MUNICIPAL", is_percentage = TRUE)
    })
    output$plot_cobertura_ab <- plotly::renderPlotly({
      req(input$nivel_selection)
      if(!(input$nivel_selection %in% c("RRAS", "DRS", "REGIÃO DE SAÚDE"))) return(NULL)
      build_bar_plot(data = plot_data(), var_numeric = "COBERTURA AB %", var_category = "MUNICIPAL", is_percentage = TRUE)
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
