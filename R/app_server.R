#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  observeEvent(input$tabs, {

    if (input$tabs == 'meanvar') {
      mod_meanvarModule_server("meanvarModule")
    }

    if (input$tabs == 'bimodal') {
      mod_bimodalityModule_server("bimodalityModule")
      # callModule(meanvarServer, 'meanvarMod')
    }

    if (input$tabs == 'distribdiff') {
      mod_distribDiffModule_server("distribDiffModule")
    }

    if (input$tabs == 'analysis') {
      mod_analysisModule_server("analysisModule")
    }
  })

  output$tabs <- shinydashboard::renderMenu({
    shinydashboard::dashboardSidebar(
      width = 250,
      disable = FALSE,
      shinydashboard::sidebarMenu(
        id = 'tabs',
        shinydashboard::menuItem('Mean/Variance Effect Tests', tabName = 'meanvar'),
        shinydashboard::menuItem('Bimodality Tests', tabName = 'bimodal'),
        shinydashboard::menuItem('Distribution Tests', tabName = 'distribdiff'),
        shinydashboard::menuItem('Analyze Data', tabName = 'analysis')

      ) # END sidebarMenu
    ) # END dashboardSidebar
  })
}
