#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @noRd
#' @export
app_ui <- function(request) {
  tagList(
    # Recursos Externos (CSS, JS, etc.)
    golem_add_external_resources(),

    # bs4Dash Page
    bs4Dash::bs4DashPage(
      title = "Projeto ReMaP",

      # Cabeçalho
      header = bs4Dash::bs4DashNavbar(
        fixed = TRUE,
        title = bs4Dash::bs4DashBrand(
          title = HTML("<b> Painel ReMap </b>"),
          color = "primary",
          href = "https://observatorioobstetricobr.org/",
          image = "www/logotipo/logo-oobr-curto.png"
        ),
        skin   = "dark",
        status = "primary",
        rightUi = NULL
      ),

      # Sidebar
      sidebar = bs4Dash::bs4DashSidebar(
        skin      = "light",
        collapsed = TRUE,
        width     = "300px",
        bs4Dash::bs4SidebarMenu(
          id = "menu",
          bs4Dash::bs4SidebarMenuItem(
            "Home",
            tabName = "home_painel",
            icon = icon("info-circle")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Estatísticas RRAS",
            icon = icon("chart-bar"),
            bs4Dash::bs4SidebarMenuSubItem(
              "Atenção Primária à Saúde",
              tabName = "tabela_1_APS",
              icon = icon("table")
            )
            # ,
            # bs4Dash::bs4SidebarMenuSubItem(
            #   "Atenção Ambulatorial Especializada",
            #   tabName = "tabela_2_AGAR",
            #   icon = icon("table")
            # )
          )
          # ,
          # bs4Dash::bs4SidebarMenuItem(
          #   "Mortalidade Materna",
          #   tabName = "mortalidade_materna",
          #   icon = icon("heartbeat")
          # )
        )
      ),

      # Corpo (Body) com Tabs
      body = bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(
          # Aba Home
          bs4Dash::bs4TabItem(
            tabName = "home_painel",
            mod_home_ui("home")
          ),
          # Aba APS
          bs4Dash::bs4TabItem(
            tabName = "tabela_1_APS",
            mod_rras_aps_ui("rras_aps")
          ),
          # Placeholder Aba AGAR
          bs4Dash::bs4TabItem(
            tabName = "tabela_2_AGAR",
            fluidRow(
              column(12, tags$div(class = "panel-title-custom",
                                  "Conteúdo para AGAR (em desenvolvimento)"))
            )
          ),
          # Placeholder Mortalidade Materna
          bs4Dash::bs4TabItem(
            tabName = "mortalidade_materna",
            fluidRow(
              column(12, tags$div(class = "panel-title-custom",
                                  "Mortalidade Materna (conteúdo a ser desenvolvido)"))
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyremap"
    ),
    shiny::includeCSS(app_sys("app", "www/styles.css")),
    shiny::includeScript(app_sys("app", "www/scripts.js"))
  )
}
