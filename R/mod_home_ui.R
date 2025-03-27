# R/mod_home_ui.R
#' Home UI
#'
#' @param id Module id
#'
#' @export
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # Título
    fluidRow(
      column(
        width = 12,
        tags$div(
          class = "panel-title-custom",
          "Painel ReMaP"
        )
      )
    ),
    # Texto informativo centralizado com margens iguais
    fluidRow(
      column(
        width = 12,
        tags$div(
          style = "
            max-width: 1700px;
            margin: 20px auto;
            font-size: 25px;
            text-align: justify;
            line-height: 1.5;
            padding: 15px;
            border: 1px solid #ddd;
            border-radius: 5px;
            background-color: #f9f9f9;
          ",
          "O Painel (ReMaP) tem como objetivo apresentar dados públicos relacionados à saúde que possam ser utilizados para reestruturação e redesenho da rede de atenção materna e infantil (componentes fetal e neonatal) no Estado de São Paulo. A implementação de painéis de monitoramento em tempo real será crucial para gestores, imprensa e cidadãos acompanharem a evolução de indicadores importantes como mortalidade materna e neonatal, disponibilidade de leitos, cobertura de atenção primária e especializada no SUS, cobertura de saúde suplementar e demais indicadores a serem definidos pela Rede Alyne. O projeto utilizará dados de sistemas como SINASC, SIM, CNES, SIHSUS e IBGE, TabNEt, aplicando ferramentas de ciência de dados e epidemiologia para apoiar decisões de gestão. A transparência é garantida pela documentação clara sobre a forma de extração e tratamento dos dados e a publicização dos painéis permitirá que eventuais ajustes na rede de atenção possam ser feitos com maior agilidade."
        )
      )
    )
  )
}
