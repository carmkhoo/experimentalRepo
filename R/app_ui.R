#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(title = 'Simulations'),
      sidebar = shinydashboard::dashboardSidebar(
        bsplus::use_bs_popover(),
        width = 250,
        disable = FALSE,
        shinydashboard::sidebarMenu(
          id = 'tabs',
          shinydashboard::menuItem('Mean/Variance Effect Tests', tabName = 'meanvar'),
          shinydashboard::menuItem('Bimodality Tests', tabName = 'bimodal'),
          shinydashboard::menuItem('Distribution Tests', tabName = 'distribdiff'),
          shinydashboard::menuItem('Analyze Data', tabName = 'analysis')
        ) # END sidebarMenu
      ), # END dashboardSidebar,
      body = shinydashboard::dashboardBody(
        # tags$head(
        #   tags$link(rel = 'stylesheet', type = 'text/css', href = '~/TR01/style.css')
        # ),


        dashboardthemes::shinyDashboardThemes(theme = 'poor_mans_flatly'),

        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = 'meanvar', mod_meanvarModule_ui('meanvarModule')),
          shinydashboard::tabItem(tabName = 'bimodal', mod_bimodalityModule_ui('bimodalityModule')),
          shinydashboard::tabItem(tabName = 'distribdiff', mod_distribDiffModule_ui('distribDiffModule')),
          shinydashboard::tabItem(tabName = 'analysis', mod_analysisModule_ui('analysisModule'))

        ),

        tags$style(HTML('.popover-title {color:black;}
                         .popover-content {color:black;}
                         .main-sidebar {z-index:auto;}
                         .popover{width:200px;height:250px;}'))

      ) # END dashboardBody
    )
    # fluidPage(
    #   h1("experimentalRepo"),
    #   mod_bimodalityModule_ui("bimodalityModule_1")
    # )
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
      app_title = "experimentalRepo"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
