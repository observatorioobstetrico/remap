# R/mod_obitos_oficiais_server.R
#' Server: Óbitos Maternos Oficiais (São Paulo)
#'
#' @param id módulo id
#' @param data_list lista de dados de óbitos (load_obitos_data())
#' @import shiny
#' @importFrom dplyr filter group_by summarise mutate
#' @importFrom reactable renderReactable reactable colDef colFormat
#' @importFrom openxlsx write.xlsx
#' @noRd
#' @export
mod_obitos_oficiais_server <- function(id, data_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 1) Inicializa input de ano
    observe({
      anos <- sort(unique(data_list$oficiais$ano))
      updateNumericInput(session, "ano",
                         value = max(anos), min = min(anos), max = max(anos))
    })

    # 2) Monta UI de filtros locais conforme nível
    output$filtros_locais <- renderUI({
      req(input$nivel)
      switch(input$nivel,
             "ESTADUAL" = NULL,
             "RRAS" = {
               rras_choices <- unique(data_list$oficiais$rras)
               rras_choices <- rras_choices[order(as.numeric(gsub("\\D", "", rras_choices)))]
               selectInput(ns("rras"), "Selecione a RRAS:", choices = rras_choices)
             },
             "DRS"      = selectInput(ns("drs"),  "Selecione a DRS:",   choice = sort(unique(data_list$oficiais$drs))),
             "REGIÃO DE SAÚDE" = selectInput(ns("regiao_de_saude"), "Selecione a Região de Saúde:", choice = sort(unique(data_list$oficiais$regiao_de_saude))),
             "MUNICIPAL" = selectInput(ns("municipio_sp"), "Selecione o Município:", choice = sort(unique(data_list$oficiais$municipio_sp)))
      )
    })

    # 3) Define escolhas fixas de filtros (independentes de localidade ou tempo)
    observe({
      # Raça/Cor
      updateCheckboxGroupInput(
        session, "raca",
        choices  = sort(unique(data_list$oficiais$racacor)),
        selected = sort(unique(data_list$oficiais$racacor))
      )

      # Causas obstétricas
      updateCheckboxGroupInput(
        session, "causas",
        choices  = sort(unique(data_list$oficiais$tipo_de_morte_materna)),
        selected = sort(unique(data_list$oficiais$tipo_de_morte_materna))
      )

      # Períodos de óbito — unifica rótulos inconsistentes
      periodos_raw <- unique(data_list$oficiais$periodo_do_obito)
      # Normaliza qualquer variante de inconsistente
      periodos <- ifelse(periodos_raw %in% c("Período inconsistente", "Inconsistente"),
                         "Período inconsistente", periodos_raw)
      periodos <- sort(unique(periodos))
      updateCheckboxGroupInput(
        session, "periodo",
        choices  = periodos,
        selected = periodos
      )

      # Investigação por CMM — considerando "Sem informação"
      inv_vals <- data_list$oficiais$investigacao_cmm
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

    # 4) Filtra e agrega
    dados_om_filtrados <- reactive({
      req(input$ano, input$nivel,
          input$idade, input$raca,
          input$causas, input$periodo,
          input$investigacao)

      df <- data_list$oficiais |>
        filter(
          ano == input$ano,
          idade >= input$idade[1] & idade <= input$idade[2],
          racacor %in% input$raca,
          tipo_de_morte_materna %in% input$causas,
          periodo_do_obito %in% input$periodo
        ) |>
        mutate(
          investigacao_cmm = ifelse(is.na(investigacao_cmm), "Sem informação", investigacao_cmm)
        ) |>
        filter(investigacao_cmm %in% input$investigacao)

      if (input$nivel == "RRAS")         df <- df |> filter(rras == input$rras)
      if (input$nivel == "DRS")          df <- df |> filter(drs == input$drs)
      if (input$nivel == "REGIÃO DE SAÚDE") df <- df |> filter(regiao_de_saude == input$regiao_de_saude)
      if (input$nivel == "MUNICIPAL")    df <- df |> filter(municipio_sp == input$municipio_sp)

      df |> group_by(
        capitulo_cid10, causabas_categoria,
        tipo_de_morte_materna, periodo_do_obito,
        racacor, investigacao_cmm
      ) |>
        summarise(obitos = sum(as.numeric(obitos), na.rm = TRUE), .groups = "drop")
    })

    # 5) Renderiza tabela
    output$tabela_oficiais <- renderReactable({
      df <- dados_om_filtrados()
      validate(need(nrow(df)>0, "Não existem registros para os filtros selecionados."))
      reactable(
        df,
        groupBy = c("capitulo_cid10","causabas_categoria"),
        columns = list(
          capitulo_cid10       = colDef(name="Capítulo CID10", aggregate="unique", footer="Total"),
          causabas_categoria   = colDef(name="Categoria CID10", aggregate="count", format=colFormat(suffix=" ocorrência(s)")),
          obitos               = colDef(name="Nº de óbitos", aggregate="sum", footer=JS("function(ci){return ci.data.reduce((t,r)=>t+r['obitos'],0);}")),
          tipo_de_morte_materna= colDef(name="Tipo de morte materna", aggregate="unique"),
          periodo_do_obito     = colDef(name="Período do óbito", aggregate=JS("()=>''"), format=colFormat(prefix="Todos ")),
          racacor              = colDef(name="Raça/Cor", aggregate=JS("()=>''"), format=colFormat(prefix="Todas ")),
          investigacao_cmm     = colDef(name="Investigação por CMM", aggregate=JS("()=>''"), format=colFormat(prefix="Todas as categorias "))
        ),
        searchable=TRUE, sortable=TRUE, filterable=TRUE,
        highlight=TRUE, striped=TRUE, bordered=FALSE, pagination=FALSE,
        defaultColDef=colDef(footerStyle=list(fontWeight="bold")),
        rowStyle=JS("function(r){if(r.aggregated)return{fontWeight:'bold'}}")
      )
    })

    # 6) Download
    output$download_ui <- renderUI({
      if(input$download_choice == "Sim") {
        tagList(
          selectInput(ns("filetype"), "Tipo de arquivo:", c("CSV","XLSX")),
          downloadButton(ns("download_OM"), "Baixar")
        )
      }
    })
    output$download_OM <- downloadHandler(
      filename = function() {
        ext <- tolower(input$filetype)
        paste0("obitos_maternos_sp_", tolower(gsub("[[:space:]]+","_",input$nivel)), "_", input$ano, ".", ext)
      },
      content = function(file) {
        df <- dados_om_filtrados()
        if(input$filetype == "CSV") write.csv(df, file, row.names=FALSE)
        else openxlsx::write.xlsx(df, file, rowNames=FALSE)
      }
    )
  })
}
