# R/mod_obitos_nao_considerados_server.R
#' Server: Óbitos de Gestantes e Puérperas Não Considerados (São Paulo)
#'
#' @param id módulo id
#' @param data_list lista de dados de óbitos (saída de load_obitos_data())
#' @import shiny
#' @importFrom dplyr filter group_by summarise mutate if_else case_when arrange
#' @importFrom magrittr %>%
#' @importFrom reactable renderReactable reactable colDef colFormat
#' @importFrom openxlsx write.xlsx
#' @importFrom htmlwidgets JS
#' @noRd
#' @export
mod_obitos_nao_considerados_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Inicializa input de ano (removendo NAs)
    observe({
      anos <- sort(unique(na.omit(data_list$nao_considerados$ano)))
      updateNumericInput(session, "ano",
                         value = max(anos), min = min(anos), max = max(anos))
    })

    # 2) Filtros locais por nível
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(
        input$nivel,
        "ESTADUAL" = NULL,
        "RRAS" = {
          rras_choices <- unique(data_list$nao_considerados$rras)
          rras_choices <- rras_choices[order(as.numeric(gsub("\\D", "", rras_choices)))]
          # selectInput(ns("rras"), "Selecione a RRAS:", choices = rras_choices)
          shinyWidgets::pickerInput(
            inputId = ns("rras"),
            label = "Selecione a RRAS:",
            choices = rras_choices,
            options = list("live-search" = TRUE)
          )
        },
        # "DRS" = selectInput(ns("drs"), "Selecione a DRS:",
        #                     choices = sort(unique(data_list$oficiais$drs)))
        "DRS" = shinyWidgets::pickerInput(
          inputId = ns("drs"),
          label = "Selecione a DRS:",
          choices = sort(unique(data_list$nao_considerados$drs)),
          options = list("live-search" = TRUE)
        ),
        # "REGIÃO DE SAÚDE" = selectInput(ns("regiao_de_saude"),
        #                                 "Selecione a Região de Saúde:",
        #                                 choices = sort(unique(data_list$oficiais$regiao_de_saude)))
        "REGIÃO DE SAÚDE" = shinyWidgets::pickerInput(
          inputId = ns("regiao_de_saude"),
          label = "Selecione a Região de Saúde:",
          choices = sort(unique(data_list$nao_considerados$regiao_de_saude)),
          options = list("live-search" = TRUE)
        ),
        # "MUNICIPAL" = selectInput(ns("municipio_sp"), "Selecione o Município:",
        #                           choices = sort(unique(data_list$oficiais$municipio_sp)))
        "MUNICIPAL" = shinyWidgets::pickerInput(
          inputId = ns("municipio_sp"),
          label = "Selecione o Município:",
          choices = sort(unique(data_list$nao_considerados$municipio_sp)),
          options = list("live-search" = TRUE)
        )
      )
    })

    # 3) Filtros fixos e normalização
    observe({
      updateCheckboxGroupInput(
        session, "raca",
        choices  = sort(unique(data_list$nao_considerados$racacor)),
        selected = sort(unique(data_list$nao_considerados$racacor))
      )

      periodos_raw <- unique(data_list$nao_considerados$periodo_do_obito)
      periodos <- ifelse(periodos_raw %in% c("Período inconsistente", "Inconsistente"),
                         "Período inconsistente", periodos_raw)
      periodos <- sort(unique(periodos))
      updateCheckboxGroupInput(session, "periodo", choices = periodos, selected = periodos)

      inv_vals <- data_list$nao_considerados$investigacao_cmm
      inv_vals[is.na(inv_vals)] <- "Sem informação"
      named_inv <- c(
        "Investigado por Comitê de Morte Materna"     = "Sim",
        "Não investigado por Comitê de Morte Materna" = "Não",
        "Sem informação"                               = "Sem informação"
      )
      present <- unique(inv_vals)
      choices_keep <- named_inv[named_inv %in% present]
      updateCheckboxGroupInput(session, "investigacao", choices = choices_keep, selected = choices_keep)
    })

    # 4) Base com filtros gerais
    dados_nm_base <- reactive({
      req(input$ano, input$nivel, input$idade, input$raca, input$periodo, input$investigacao)

      df <- data_list$nao_considerados %>%
        dplyr::mutate(
          periodo_do_obito = ifelse(periodo_do_obito %in% c("Período inconsistente", "Inconsistente"),
                                    "Período inconsistente", periodo_do_obito),
          investigacao_cmm = ifelse(is.na(investigacao_cmm), "Sem informação", investigacao_cmm),
          causabas_categoria = trimws(causabas_categoria)
        ) %>%
        dplyr::filter(
          ano == input$ano,
          idade >= input$idade[1] & idade <= input$idade[2],
          racacor %in% input$raca,
          periodo_do_obito %in% input$periodo,
          investigacao_cmm %in% input$investigacao
        )

      if ("Excluir óbitos por causas externas" %in% input$externos) {
        df <- df %>%
          dplyr::filter(capitulo_cid10 != "XX - Causas externas de morbidade e de mortalidade")
      }

      if (input$nivel == "RRAS") {
        req(input$rras); df <- df %>% dplyr::filter(rras == input$rras)
      } else if (input$nivel == "DRS") {
        req(input$drs); df <- df %>% dplyr::filter(drs == input$drs)
      } else if (input$nivel == "REGIÃO DE SAÚDE") {
        req(input$regiao_de_saude); df <- df %>% dplyr::filter(regiao_de_saude == input$regiao_de_saude)
      } else if (input$nivel == "MUNICIPAL") {
        req(input$municipio_sp); df <- df %>% dplyr::filter(municipio_sp == input$municipio_sp)
      }

      df
    })

    # 5) Normalização + filtro Sim/Não
    dados_nm_norm <- reactive({
      dados_nm_base() %>%
        dplyr::mutate(
          capitulo_cid10 = dplyr::if_else(
            is.na(capitulo_cid10) | trimws(capitulo_cid10) == "",
            "Sem informação", capitulo_cid10
          ),
          causabas_categoria = dplyr::if_else(
            is.na(causabas_categoria) | trimws(causabas_categoria) == "",
            "Sem informação", causabas_categoria
          )
        ) %>%
        dplyr::mutate(
          causabas_categoria = dplyr::if_else(
            capitulo_cid10 == "Sem informação", "Sem informação", causabas_categoria
          )
        ) %>%
        {
          if (identical(input$mostrar_sem_info, "Não")) {
            dplyr::filter(., capitulo_cid10 != "Sem informação",
                          causabas_categoria != "Sem informação")
          } else .
        }
    })

    # 6) Agregado final (linhas visíveis)
    dados_nao_final <- reactive({
      dados_nm_norm() %>%
        {
          if (!"municipio_ocorrencia" %in% names(.)) {
            dplyr::mutate(., municipio_ocorrencia = NA_character_)
          } else .
        } %>%
        {
          if (!"municipio_residencia" %in% names(.)) {
            dplyr::mutate(., municipio_residencia = municipio_sp)
          } else .
        } %>%
        dplyr::mutate(
          municipio_residencia = dplyr::if_else(
            is.na(municipio_residencia) | trimws(municipio_residencia) == "",
            municipio_sp,
            municipio_residencia
          ),
          municipio_ocorrencia = as.character(municipio_ocorrencia),
          municipio_ocorrencia = dplyr::if_else(
            is.na(municipio_ocorrencia) | trimws(municipio_ocorrencia) == "",
            "Não disponível na base",
            municipio_ocorrencia
          )
        ) %>%
        dplyr::group_by(
          capitulo_cid10, causabas_categoria,
          municipio_residencia, municipio_ocorrencia,
          periodo_do_obito, racacor, investigacao_cmm
        ) %>%
        dplyr::summarise(obitos = sum(as.numeric(obitos), na.rm = TRUE), .groups = "drop")
    })

    # 7) Total do rodapé
    total_nm <- reactive({
      sum(as.numeric(dados_nao_final()$obitos), na.rm = TRUE)
    })

    # 8) Tabela
    output$tabela_nao <- reactable::renderReactable({
      df <- dados_nao_final()
      validate(need(nrow(df) > 0, "Não existem registros para os filtros selecionados."))
      tem_municipio_ocorrencia <- any(df$municipio_ocorrencia != "Não disponível na base", na.rm = TRUE)

      reactable::reactable(
        df,
        groupBy = c("capitulo_cid10", "causabas_categoria"),
        columns = list(
          capitulo_cid10   = reactable::colDef(name = "Capítulo CID10", aggregate = "unique", footer = "Total"),
          causabas_categoria = reactable::colDef(
            name = "Categoria CID10",
            aggregate = "count",
            grouped = htmlwidgets::JS("function(cellInfo, state) { return cellInfo.value; }"),
            aggregated = htmlwidgets::JS("
              function(cellInfo, state) {
                var ri = cellInfo.rowInfo;
                if (ri && ri.level === 0) {
                  return cellInfo.value + ' ocorrência(s)';
                }
                return cellInfo.value;
              }
            ")
          ),
          obitos = reactable::colDef(
            name = "Nº de óbitos",
            aggregate = "sum",
            footer   = total_nm()
          ),
          municipio_residencia = reactable::colDef(
            name = "Município de residência",
            aggregate = htmlwidgets::JS("function() { return '' }"),
            format   = list(aggregated = reactable::colFormat(prefix = "Todos"))
          ),
          municipio_ocorrencia = reactable::colDef(
            name = "Município de ocorrência",
            aggregate = if (tem_municipio_ocorrencia) {
              htmlwidgets::JS("function() { return '' }")
            } else {
              htmlwidgets::JS("function() { return 'Não disponível na base' }")
            },
            format = if (tem_municipio_ocorrencia) {
              list(aggregated = reactable::colFormat(prefix = "Todos"))
            } else {
              NULL
            }
          ),
          periodo_do_obito = reactable::colDef(
            name = "Período do óbito",
            aggregate = htmlwidgets::JS("function() { return '' }"),
            format   = list(aggregated = reactable::colFormat(prefix = "Todos"))
          ),
          racacor = reactable::colDef(
            name = "Raça/Cor",
            aggregate = htmlwidgets::JS("function() { return '' }"),
            format   = list(aggregated = reactable::colFormat(prefix = "Todas"))
          ),
          investigacao_cmm = reactable::colDef(
            name = "Investigação por CMM",
            aggregate = htmlwidgets::JS("function() { return '' }"),
            format   = list(aggregated = reactable::colFormat(prefix = "Todas as categorias"))
          )
        ),
        searchable = TRUE, sortable = TRUE, filterable = TRUE,
        highlight  = TRUE, striped  = TRUE, bordered = FALSE, pagination = FALSE,
        defaultColDef = reactable::colDef(footerStyle = list(fontWeight = "bold")),
        rowStyle = htmlwidgets::JS("function(r){ if(r.aggregated) return({fontWeight:'bold'}); }")
      )
    })

    # 9) Download
    slug_download_obitos <- function(x) {
      x <- x[!is.na(x) & nzchar(x)]
      slug <- paste(as.character(x), collapse = "_")
      slug_ascii <- iconv(slug, from = "", to = "ASCII//TRANSLIT", sub = "")
      if (!is.na(slug_ascii) && nzchar(slug_ascii)) {
        slug <- slug_ascii
      }
      slug <- tolower(gsub("[^[:alnum:]]+", "_", slug))
      slug <- gsub("^_+|_+$", "", slug)
      if (!nzchar(slug)) "tabela" else slug
    }

    download_suffix_nm <- reactive({
      req(input$ano, input$nivel)

      suffix <- switch(
        input$nivel,
        "ESTADUAL" = c("estadual", input$ano),
        "RRAS" = c("rras", req(input$rras), input$ano),
        "DRS" = c("drs", req(input$drs), input$ano),
        "REGIÃO DE SAÚDE" = c("regiao_saude", req(input$regiao_de_saude), input$ano),
        "MUNICIPAL" = c("municipal", req(input$municipio_sp), input$ano)
      )

      slug_download_obitos(suffix)
    })

    dados_download_nm <- reactive({
      df <- dados_nao_final()
      validate(need(nrow(df) > 0, "Não há dados para baixar com os filtros selecionados."))

      df %>%
        dplyr::transmute(
          `Capítulo CID10` = capitulo_cid10,
          `Categoria CID10` = causabas_categoria,
          `Município de residência` = municipio_residencia,
          `Município de ocorrência` = municipio_ocorrencia,
          `Período do óbito` = periodo_do_obito,
          `Raça/Cor` = racacor,
          `Investigação por CMM` = investigacao_cmm,
          `Nº de óbitos` = as.numeric(obitos)
        )
    })

    write_obitos_xlsx <- function(df, file) {
      wb <- openxlsx::createWorkbook()
      sheet_name <- "Tabela"
      header_style <- openxlsx::createStyle(
        textDecoration = "bold",
        fgFill = "#EAF0F7",
        border = "bottom"
      )
      integer_style <- openxlsx::createStyle(numFmt = "#,##0")

      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, df, headerStyle = header_style, withFilter = TRUE)
      openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
      openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")

      obitos_col <- match("Nº de óbitos", names(df))
      if (!is.na(obitos_col) && nrow(df) > 0) {
        openxlsx::addStyle(
          wb,
          sheet_name,
          integer_style,
          rows = 2:(nrow(df) + 1),
          cols = obitos_col,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }

    output$download_NM_xlsx <- downloadHandler(
      filename = function() {
        paste0("obitos_nao_classificados_como_morte_materna_", download_suffix_nm(), ".xlsx")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        write_obitos_xlsx(dados_download_nm(), file)
      }
    )

    outputOptions(output, "download_NM_xlsx", suspendWhenHidden = FALSE)
  })
}
