# R/mod_home_ui.R
#' Home UI
#'
#' @param id Module id
#' @export
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    # Wrapper vertical para CTA no fundo
    div(class = "home-page",

        # --- HERO (logo central) ---
        fluidRow(
          column(
            width = 12,
            div(class = "home-hero",
                img(
                  src   = "www/logotipo/Logo_ReMaP Completo_FundoClaro-2.png",
                  class = "home-hero-logo",
                  alt   = "Painel ReMaP"
                )
            )
          )
        ),

        # # --- SEÇÃO: Sobre o Painel ReMaP ---
        # # fluidRow(column(12, div(class = "section-title", "Sobre o Painel ReMaP"))),
        # fluidRow(column(12, div(class = "section-divider"))),
        # fluidRow(
        #   column(
        #     width = 12,
        #     div(
        #       class = "home-info home-info--plain",
        #       HTML("O Painel (ReMaP) tem como objetivo apresentar dados públicos relacionados à saúde que possam ser utilizados para reestruturação e redesenho da rede de atenção materna e infantil (componentes fetal e neonatal) no Estado de São Paulo. A implementação de painéis de monitoramento em tempo real será crucial para gestores, imprensa e cidadãos acompanharem a evolução de indicadores importantes como mortalidade materna e neonatal, disponibilidade de leitos, cobertura de atenção primária e especializada no SUS, cobertura de saúde suplementar e demais indicadores a serem definidos pela Rede Alyne.")
        #     )
        #   )
        # ),
        #
        # # --- SEÇÃO: Funcionalidades ---
        # # fluidRow(column(12, div(class = "section-title", "Funcionalidades"))),
        # fluidRow(column(12, div(class = "section-divider"))),

        # GRID 1: 3 CARDS (um por menu principal)
        fluidRow(
          column(
            width = 12,
            div(class = "home-card-grid home-card-grid--three",
                # Card 1 — Estatísticas RRAS
                div(class = "home-card home-card--lg blue2",
                    tags$i(class = "fas fa-chart-bar home-card-icon", aria_hidden = "true"),
                    div(class = "home-card-text",
                        h4("Estatísticas RRAS"),
                        p("Indicadores de Atenção Primária organizados por RRAS, DRS, Região de Saúde e Municípios, para suporte ao planejamento e à avaliação regional.")
                    )
                ),
                # Card 2 — Óbitos de Gestantes e Puérperas
                div(class = "home-card home-card--lg blue2",
                    tags$i(class = "fas fa-heartbeat home-card-icon", aria_hidden = "true"),
                    div(class = "home-card-text",
                        h4("Óbitos de Gestantes e Puérperas"),
                        p("Monitoramento da mortalidade materna, incluindo séries históricas, informações sobre óbitos oficiais e não considerados, além da possibilidade de análise de dados cruzados.")
                    )
                ),
                # Card 3 — Indicadores Obstétricos
                div(class = "home-card home-card--lg blue2",
                    tags$i(class = "fas fa-chart-area home-card-icon", aria_hidden = "true"),
                    div(class = "home-card-text",
                        h4("Indicadores Obstétricos"),
                        p("Acesso aos indicadores obstétricos do estado de São Paulo, como nascimentos, prematuridade, cesáreas, anomalias congênitas, entre outros, em diferentes níveis de análise.")
                    )
                )
            )
          )
        ),

        # GRID 2: 2 CARDS CENTRALIZADOS — “Dados” e “Transparência”
        fluidRow(
          column(
            width = 12,
            div(class = "home-card-grid home-card-grid--two",
                # Card 4 — Dados
                div(class = "home-card home-card--lg blue2",
                    tags$i(class = "fas fa-database home-card-icon", aria_hidden = "true"),
                    div(class = "home-card-text",
                        h4("Dados"),
                        p("Integração de bases públicas como SINASC, SIM, CNES, SIHSUS e IBGE/TabNet, aplicando métodos de ciência de dados e epidemiologia para apoiar decisões de gestão.")
                    )
                ),
                # Card 5 — Transparência
                div(class = "home-card home-card--lg blue2",
                    tags$i(class = "fas fa-file-alt home-card-icon", aria_hidden = "true"),
                    div(class = "home-card-text",
                        h4("Transparência"),
                        p("Documentação clara de extração e tratamento, com publicização dos painéis, permitindo que eventuais ajustes na rede de atenção possam ser feitos com maior agilidade conforme novas evidências.")
                    )
                )
            )
          )
        ),

        # --- CTA no fundo, seta inline (sem JS) ---
        div(class = "home-cta-row",
            div(class = "home-cta",
                span(class = "home-cta-arrow", tags$i(class = "fas fa-arrow-left", aria_hidden = "true")),
                p(HTML('Para começar, utilize o <span class="cta-strong">menu lateral</span> para selecionar o módulo de análise.'))
            )
        )
    )
  )
}
