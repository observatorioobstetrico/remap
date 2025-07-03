# R/mod_obitos_nao_considerados_server.R
#' Server: Óbitos de Gestantes e Puérperas Não Considerados (São Paulo)
#'
#' @param id módulo id
#' @param data_list lista de dados de óbitos (saída de load_obitos_data())
#' @import shiny
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom reactable renderReactable reactable colDef colFormat JS
#' @importFrom openxlsx write.xlsx
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

    # 2) Monta UI de filtros locais conforme nível (São Paulo)
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "ESTADUAL" = NULL,
             "RRAS" = {
               rras_choices <- unique(data_list$nao_considerados$rras)
               rras_choices <- rras_choices[order(as.numeric(gsub("\\D", "", rras_choices)))]
               selectInput(ns("rras"), "Selecione a RRAS:", choices = rras_choices)
             },
             "DRS" = selectInput(ns("drs"), "Selecione a DRS:",
                                 choices = sort(unique(data_list$nao_considerados$drs))),
             "REGIÃO DE SAÚDE" = selectInput(
               ns("regiao_de_saude"), "Selecione a Região de Saúde:",
               choices = sort(unique(data_list$nao_considerados$regiao_de_saude))
             ),
             "MUNICIPAL" = selectInput(
               ns("municipio_sp"), "Selecione o Município:",
               choices = sort(unique(data_list$nao_considerados$municipio_sp))
             )
      )
    })

    # 3) Define escolhas fixas de filtros (independentes de localidade ou tempo)
    observe({
      # Raça/Cor
      updateCheckboxGroupInput(
        session, "raca",
        choices  = sort(unique(data_list$nao_considerados$racacor)),
        selected = sort(unique(data_list$nao_considerados$racacor))
      )

      # Períodos de óbito — unifica rótulos inconsistentes
      periodos_raw <- unique(data_list$nao_considerados$periodo_do_obito)
      periodos <- ifelse(periodos_raw %in% c("Período inconsistente", "Inconsistente"),
                         "Período inconsistente", periodos_raw)
      periodos <- sort(unique(periodos))
      updateCheckboxGroupInput(
        session, "periodo",
        choices  = periodos,
        selected = periodos
      )

      # Investigação por CMM — considerando "Sem informação"
      inv_vals <- data_list$nao_considerados$investigacao_cmm
      inv_vals[is.na(inv_vals)] <- "Sem informação"
      named_inv <- c(
        "Investigado por Comitê de Morte Materna"   = "Sim",
        "Não investigado por Comitê de Morte Materna" = "Não",
        "Sem informação"                             = "Sem informação"
      )
      present <- unique(inv_vals)
      choices_keep <- named_inv[named_inv %in% present]
      updateCheckboxGroupInput(
        session, "investigacao",
        choices  = choices_keep,
        selected = choices_keep
      )
    })

    # 4) Filtra, agrega e retorna
    dados_nao_filtrados <- reactive({
      req(input$ano, input$nivel,
          input$idade, input$raca,
          input$periodo, input$investigacao)

      df <- data_list$nao_considerados |>
        filter(
          ano == input$ano,
          idade >= input$idade[1] & idade <= input$idade[2],
          racacor %in% input$raca,
          periodo_do_obito %in% input$periodo
        ) |>
        mutate(
          investigacao_cmm = ifelse(is.na(investigacao_cmm),
                                    "Sem informação", investigacao_cmm)
        ) |>
        filter(investigacao_cmm %in% input$investigacao)

      # Excluir óbitos por causas externas
      if ("Excluir óbitos por causas externas" %in% input$externos) {
        df <- df |> filter(
          capitulo_cid10 != "XX - Causas externas de morbidade e de mortalidade"
        )
      }

      # Filtragem por nível (com req para garantir valor válido)
      if (input$nivel == "RRAS") {
        req(input$rras)
        df <- df |> filter(rras == input$rras)
      } else if (input$nivel == "DRS") {
        req(input$drs)
        df <- df |> filter(drs == input$drs)
      } else if (input$nivel == "REGIÃO DE SAÚDE") {
        req(input$regiao_de_saude)
        df <- df |> filter(regiao_de_saude == input$regiao_de_saude)
      } else if (input$nivel == "MUNICIPAL") {
        req(input$municipio_sp)
        df <- df |> filter(municipio_sp == input$municipio_sp)
      }

      df |> group_by(
        capitulo_cid10, causabas_categoria,
        periodo_do_obito, racacor, investigacao_cmm
      ) |>
        summarise(obitos = sum(as.numeric(obitos), na.rm = TRUE), .groups = "drop")
    })

    # 5) Renderiza tabela
    output$tabela_nao <- renderReactable({
      df <- dados_nao_filtrados()
      validate(need(nrow(df) > 0, "Não existem registros para os filtros selecionados."))
      reactable::reactable(
        df,
        groupBy = c("capitulo_cid10", "causabas_categoria"),
        columns = list(
          capitulo_cid10   = colDef(name = "Capítulo CID10", aggregate = "unique", footer = "Total"),
          causabas_categoria = colDef(name = "Categoria CID10", aggregate = "count", format = colFormat(suffix = " ocorrência(s)")),
          obitos           = colDef(name = "Nº de óbitos", aggregate = "sum",
                                    footer = JS("function(ci){return ci.data.reduce((t,r)=>t+r['obitos'],0);}")),
          periodo_do_obito = colDef(name = "Período do óbito", aggregate = JS("()=>''"), format = colFormat(prefix = "Todos ")),
          racacor          = colDef(name = "Raça/Cor", aggregate = JS("()=>''"), format = colFormat(prefix = "Todas ")),
          investigacao_cmm = colDef(name = "Investigação por CMM", aggregate = JS("()=>''"), format = colFormat(prefix = "Todas as categorias "))
        ),
        searchable = TRUE, sortable = TRUE, filterable = TRUE,
        highlight  = TRUE, striped  = TRUE, bordered = FALSE, pagination = FALSE,
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
        rowStyle      = JS("function(r){if(r.aggregated) return({fontWeight:'bold'});}")
      )
    })

    # 6) Download
    output$download_ui <- renderUI({
      if (input$download_choice == "Sim") {
        tagList(
          selectInput(ns("filetype"), "Tipo de arquivo:", c("CSV", "XLSX")),
          downloadButton(ns("download_NM"), "Baixar")
        )
      }
    })

    output$download_NM <- downloadHandler(
      filename = function() {
        ext <- tolower(input$filetype)
        suffix <- switch(
          input$nivel,
          "ESTADUAL"          = paste0("estadual_", input$ano),
          "RRAS"              = paste0("rras_", gsub("\\s+", "_", input$rras), "_", input$ano),
          "DRS"               = paste0("drs_", gsub("\\s+", "_", input$drs), "_", input$ano),
          "REGIÃO DE SAÚDE"  = paste0("regsaude_", gsub("\\s+", "_", input$regiao_de_saude), "_", input$ano),
          "MUNICIPAL"        = paste0("muni_", gsub("\\s+", "_", input$municipio_sp), "_", input$ano)
        )
        paste0("obitos_nao_considerados_", suffix, ".", ext)
      },
      content = function(file) {
        df <- dados_nao_filtrados()
        if (input$filetype == "CSV") {
          write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
        } else {
          write.xlsx(df, file, rowNames = FALSE)
        }
      }
    )
  })
}
