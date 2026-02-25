# R/app_ui.R

#' The application User-Interface
#'
#' Este arquivo constrói toda a interface do painel, agregando os
#' recursos externos (CSS, JS e favicon), a estrutura da página
#' principal e os itens de menu. Nesta versão foram incluídos novos
#' elementos no menu lateral e no corpo da aplicação para suportar a
#' tela de "Estabelecimentos de Referência", que permite aos
#' usuários visualizar tabelas de estabelecimentos de referência
#' conforme filtros selecionados.
#'
#' @param request Internal parameter for {shiny}. DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @import golem
#' @importFrom magrittr %>%
#' @noRd
#' @export
app_ui <- function(request) {
  tagList(
    # 1. Recursos externos (CSS, JS, favicon, bundle)
    golem_add_external_resources(),

    # 2. Página bs4Dash
    bs4Dash::bs4DashPage(
      title = "Painel ReMaP",

      # --- Cabeçalho (TOP NAVBAR) ---
      header = bs4Dash::bs4DashNavbar(
        fixed   = TRUE,
        skin    = "dark",
        status  = "primary",
        title   = bs4Dash::bs4DashBrand(
          # Texto ao lado do logo (estilizado via CSS)
          title = HTML('<span class="brand-text-top"></span>'),
          color = "primary",
          href  = "https://observatorioobstetricobr.org/projetos/remap-reestruturacao-e-redesenho-da-rede-de-atencao-materna-e-perinatal/",
          # **IMPORTANTE**: usar image = (faz a marca ficar fixa/estável no topo)
          image = "www/logotipo/Logo_ReMaP Curto_FundoEscuro-4_Vertical.png"
        )
      ),

      # --- Barra lateral ---
      sidebar = bs4Dash::bs4DashSidebar(
        skin      = "light",
        collapsed = TRUE,
        width     = "370px",
        # Não definir 'title' aqui evita brand duplicada na sidebar
        bs4Dash::bs4SidebarMenu(
          id = "menu",

          # Home
          bs4Dash::bs4SidebarMenuItem(
            "Início", tabName = "home_painel", icon = icon("info-circle")
          ),

          # Estatísticas RRAS
          bs4Dash::bs4SidebarMenuItem(
            "Estatísticas RRAS",
            icon = icon("chart-bar"),

            bs4Dash::bs4SidebarMenuSubItem(
              "Atenção Primária à Saúde",
              tabName = "tabela_1_APS"
            ),

            bs4Dash::bs4SidebarMenuSubItem(
              "Estabelecimentos de Referência",
              tabName = "estabelecimentos"
            )
          ),

          # Óbitos de Gestantes e Puérperas
          bs4Dash::bs4SidebarMenuItem(
            "Óbitos de Gestantes e Puérperas",
            icon = icon("heartbeat"),
            bs4Dash::bs4SidebarMenuSubItem("Séries de Mortalidade e Morbidade", tabName = "series_obitos"),
            bs4Dash::bs4SidebarMenuSubItem("Oficiais",                          tabName = "obitos_oficiais"),
            bs4Dash::bs4SidebarMenuSubItem("Não considerados",                  tabName = "obitos_nao_considerados"),
            bs4Dash::bs4SidebarMenuSubItem("Análise cruzada",                   tabName = "analise_cruzada")
          ),

          # Indicadores Obstétricos
          bs4Dash::bs4SidebarMenuItem(
            "Indicadores Obstétricos",
            icon = icon("chart-area"),
            bs4Dash::bs4SidebarMenuSubItem("Nascimentos",         tabName = "nascimentos"),
            bs4Dash::bs4SidebarMenuSubItem("Partos Prematuros",   tabName = "prematuros"),
            bs4Dash::bs4SidebarMenuSubItem("Partos Cesáreas",     tabName = "cesarias"),
            bs4Dash::bs4SidebarMenuSubItem("Anomalias Congênitas",tabName = "anomalias"),
            bs4Dash::bs4SidebarMenuSubItem("Consultas Pré-natal", tabName = "prenatal"),
            bs4Dash::bs4SidebarMenuSubItem("Robson",              tabName = "robson"),
            bs4Dash::bs4SidebarMenuSubItem("Robson & Cesáreas",   tabName = "robson_cesarea")
          )
        )
      ),

      # --- Corpo (Body) com abas ---
      body = bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(
          # Aba Home
          bs4Dash::bs4TabItem(
            tabName = "home_painel",
            mod_home_ui("home")
          ),

          # Aba APS
          bs4Dash::bs4TabItem(tabName = "tabela_1_APS", mod_rras_aps_ui("rras_aps")),

          # Placeholder AGAR
          bs4Dash::bs4TabItem(
            tabName = "tabela_2_AGAR",
            fluidRow(column(12, tags$div(class = "panel-title-custom","Conteúdo para AGAR (em desenvolvimento)")))
          ),

          # Placeholder Mortalidade Materna
          bs4Dash::bs4TabItem(
            tabName = "mortalidade_materna",
            fluidRow(column(12, tags$div(class = "panel-title-custom","Mortalidade Materna (conteúdo a ser desenvolvido)")))
          ),

          # Abas de Óbitos
          bs4Dash::bs4TabItem(tabName = "obitos_oficiais",        mod_obitos_oficiais_ui("oficiais")),
          bs4Dash::bs4TabItem(tabName = "obitos_nao_considerados",mod_obitos_nao_considerados_ui("nao_cons")),
          bs4Dash::bs4TabItem(tabName = "analise_cruzada",        mod_analise_cruzada_ui("cruzada")),

          # Aba Séries de Mortalidade e Morbidade
          bs4Dash::bs4TabItem(tabName = "series_obitos",          mod_series_obitos_ui("series_obitos")),

          # Abas de Indicadores Obstétricos
          bs4Dash::bs4TabItem(tabName = "nascimentos",            mod_nascimentos_ui("nasc")),
          bs4Dash::bs4TabItem(tabName = "prematuros",             mod_prematuros_ui("pp")),
          bs4Dash::bs4TabItem(tabName = "cesarias",               mod_cesarias_ui("pc")),
          bs4Dash::bs4TabItem(tabName = "anomalias",              mod_anomalias_ui("an")),
          bs4Dash::bs4TabItem(tabName = "prenatal",               mod_prenatal_ui("cpn")),
          bs4Dash::bs4TabItem(tabName = "robson",                 mod_robson_ui("robson")),
          bs4Dash::bs4TabItem(tabName = "robson_cesarea",         mod_robson_cesareas_ui("rc")),

          # NOVA ABA: Estabelecimentos de Referência
          bs4Dash::bs4TabItem(tabName = "estabelecimentos", mod_estabelecimentos_ui("estabelecimentos"))
        )
      )
    )
  )
}

#' Adiciona recursos externos à aplicação
#'
#' Esta função configura os caminhos para os recursos estáticos
#' (imagens, CSS e JavaScript) e inclui o favicon e folhas de estilo
#' personalizadas. Não é necessário alterar esta função ao adicionar
#' novas telas, desde que os arquivos permaneçam na pasta `inst/app/www`.
#'
#' @import shiny
#' @importFrom golem add_resource_path bundle_resources favicon
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app", "www"))
  tags$head(
    favicon(ext = "png"),
    bundle_resources(path = app_sys("app", "www"), app_title = "Painel ReMaP"),
    includeCSS(app_sys("app", "www", "styles.css")),
    includeScript(app_sys("app", "www", "scripts.js"))
  )
}
