#' bimodalityModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tag
mod_bimodalityModule_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = 'bimodal',
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinydashboard::box(
          width = NULL,
          title = 'Parameters',
          sliderInput(
            ns("n"),
            label = "Total number of samples",
            min = 3,
            max = 5000,
            value = 32,
            step = 1
          ),

          numericInput(
            ns("alpha"),
            label =  "Significance level adjusted for multiple testing",
            value = 0.05,
            min = 0.00000001,
            max = 0.2
          ),

          selectInput(ns("dist"), label = "Distribution",
                      c(
                        "Gaussian" = "norm",
                        "Beta" = "beta"
                      )),

          conditionalPanel(
            condition = sprintf('input["%s"] == "norm"', ns("dist")),
            numericInput(
              ns("mu1"),
              label = "Mean of mode 1",
              value = 0,
              min = NA,
              max = NA
            ),
            numericInput(
              ns("sd1"),
              label = "SD of mode 1",
              value = 1,
              min = NA,
              max = NA
            ),
            numericInput(
              ns("mu2"),
              label = "Mean of mode 2",
              value = 3,
              min = NA,
              max = NA
            ),
            numericInput(
              ns("sd2"),
              label = "SD of mode 2",
              value = 1,
              min = NA,
              max = NA
            ),
            sliderInput(
              ns("p"),
              label = "Proportion in mode 1",
              min = 0.01,
              max = 0.99,
              value = .5,
              step = .01
            )
          ),

          conditionalPanel(
            condition = sprintf('input["%s"] == "beta"', ns("dist")),
            numericInput(
              ns("s1"),
              label = "Shape parameter 1",
              value = .5,
              min = NA,
              max = NA
            ),
            numericInput(
              ns("s2"),
              label = "Shape parameter 2",
              value = .5,
              min = NA,
              max = NA
            )
          ),


          ##Commented out because sigclust is being a bear
          # checkboxGroupInput("checkGroup2", label = ("Must select one or more Test"), choices = list("Hartigans' dip test"="dip","Mclust"="mclust","2-Mean cluster"="sigclust","Laplace"="isbimo","Mouse Trap"="mt"),selected="dip"),
          #

          checkboxGroupInput(
            ns("checkGroup2"),
            label = ("Must select one or more Test"),
            choices = list(
              "Hartigans' dip test" = "dip",
              "Mclust" = "mclust",
              "Laplace" = "isbimo",
              "Mouse Trap" = "mt"
            ),
            selected = "dip"
          ),

          numericInput(
            ns("nsim"),
            label = "Number of simulations",
            min = 10,
            max = 5000,
            value = 10
          ),
        ) # END box
      ),
      # END column

      shiny::column(
        width = 9,
        shinydashboard::box(width = NULL,
                            title = 'Detecting Bimodality',
                            # verbatimTextOutput(ns('description')),
                            plotly::plotlyOutput(ns("pplt"))) # END box
      ) # END column
    ) # END fluidRow
  ) # END tabItem
}

#' bimodalityModule Server Functions
#'
#' @noRd
mod_bimodalityModule_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 # observeEvent(input$dist, {
                 #   output$inputdist = renderUI({
                 #     input_list <- if(input$dist == "norm"){
                 #       list(
                 #         numericInput(ns("mu1"),
                 #                      label="Mean of mode 1", value=0, min = NA, max = NA),
                 #         numericInput(ns("sd1"),
                 #                      label="SD of mode 1", value=1, min = NA, max = NA),
                 #         numericInput(ns("mu2"),
                 #                      label="Mean of mode 2", value=3, min = NA, max = NA),
                 #         numericInput(ns("sd2"),
                 #                      label="SD of mode 2", value=1, min = NA, max = NA),
                 #         sliderInput(ns("p"),
                 #                     label = "Proportion in mode 1",min = 0.01, max = 0.99, value = .5, step = .01))
                 #     } else {
                 #       if(input$dist == "beta"){
                 #         list(
                 #           numericInput(ns("s1"),
                 #                        label="Shape parameter 1", value=.5, min = NA, max = NA),
                 #           numericInput(ns("s2"),
                 #                        label="Shape parameter 2", value=.5, min = NA, max = NA))}
                 #     }
                 #     do.call(tagList, input_list)
                 #   })
                 # })

                 ss <- reactive({
                   if (input$dist == "norm") {
                     calcs =  reshape2::melt(as.data.frame(
                       bifurcatoR::est_pow(
                         input$n,
                         input$alpha,
                         input$nsim,
                         input$dist,
                         list(
                           p = input$p,
                           mu1 = input$mu1,
                           sd1 = input$sd1,
                           mu2 = input$mu2,
                           sd2 = input$sd2
                         ),
                         tests = input$checkGroup2
                       )
                     ), id.vars = c("N", "Test"))

                     dens.plot = data.frame(var = c(
                       stats::rnorm(ceiling(input$p * 2000), input$mu1, input$sd1),
                       stats::rnorm(ceiling((1 - input$p) * 2000), input$mu2, input$sd2)
                     ))


                   } else {
                     if (input$dist == "beta") {
                       dens.plot =  data.frame(var = stats::rbeta(2000, input$s1, input$s2))
                       calcs =  reshape2::melt(as.data.frame(
                         bifurcatoR::est_pow(
                           input$n,
                           input$alpha,
                           input$nsim,
                           input$dist,
                           list(s1 = input$s1, s2 = input$s2),
                           tests = input$checkGroup2
                         )
                       ), id.vars = c("N", "Test"))
                     }
                   }
                   list(dens.plot = dens.plot, calcs = calcs)
                 })


                 output$pplt <- plotly::renderPlotly({
                   p1 = plotly::ggplotly(
                     ggplot2::ggplot() +
                       ggplot2::geom_density(data = ss()[["dens.plot"]], ggplot2::aes(x = var)) +
                       ggplot2::theme_classic(14) +
                       ggplot2::ylab("Population density") +
                       ggplot2::xlab("Modes") +
                       ggplot2::theme(legend.text = ggplot2::element_text(10))
                   )
                   fig1 = plotly::ggplotly(p1)

                   p2 = plotly::ggplotly(
                     ggplot2::ggplot(data = ss()[["calcs"]], ggplot2::aes(
                       x = Test, y = value, color = variable
                     )) +
                       ggplot2::geom_point(position = ggplot2::position_dodge(width = .25)) +
                       ggplot2::theme_classic(14) +
                       ggplot2::ylab("Probability") +
                       ggplot2::xlab("Test") +
                       ggplot2::theme(
                         legend.title = ggplot2::element_blank(),
                         legend.text = ggplot2::element_text(10),
                         axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                       ) +
                       ggplot2::scale_color_manual(values = c("black", "red")) +
                       ggplot2::coord_cartesian(ylim = c(-.01, 1.01))
                   )
                   fig2 = plotly::ggplotly(p2)

                   plotly::subplot(fig1,
                           fig2,
                           nrows = 1,
                           margin = c(0.02, 0.02, .21, .21))

                 })

               })
}

## To be copied in the UI
# mod_bimodalityModule_ui("bimodalityModule_1")

## To be copied in the server
# mod_bimodalityModule_server("bimodalityModule_1")
