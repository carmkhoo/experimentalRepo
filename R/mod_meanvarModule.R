#' meanvarModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#source("./est_pow_2samp.R")
mod_meanvarModule_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = 'meanvar',
                          shiny::fluidRow(
                            column(
                              width = 3,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Parameters',
                                shinyWidgets::pickerInput(
                                  inputId = ns('dist'),
                                  label = 'Distribution',
                                  selected = 'Gaussian',
                                  choices = c('Weibull', 'Gaussian')
                                ),

                                shiny::sliderInput(
                                  inputId = ns('gaussmean'),
                                  label = 'Mean',
                                  min = 0,
                                  max = 10,
                                  value = 2
                                ),

                                shiny::sliderInput(
                                  inputId = ns('nsize'),
                                  label = 'Sample Sizes',
                                  min = 3,
                                  max = 1000,
                                  value = c(25, 25)
                                ),

                                shiny::sliderInput(
                                  inputId = ns('gaussvar'),
                                  label = 'Variance',
                                  min = 0,
                                  max = 10,
                                  value = 1
                                ),

                                shiny::numericInput(
                                  inputId = ns('nperm'),
                                  label = 'Number of Permutations',
                                  min = 10,
                                  max = 5000,
                                  value = 10
                                ),

                                shiny::numericInput(
                                  inputId = ns('nsim'),
                                  label = 'Number of Simulations',
                                  min = 10,
                                  max = 5000,
                                  value = 10
                                ),

                                shiny::numericInput(
                                  inputId = ns('alpha'),
                                  label = 'Significance level',
                                  min = 0.00000000000000001,
                                  max = 0.5,
                                  value = 0.05
                                ),

                                shinyWidgets::pickerInput(
                                  inputId = ns('method'),
                                  label = 'Effect to Test',
                                  selected = 'Mean Effect',
                                  choices = c('Mean Effect', 'Variance Effect')
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean Effect"', ns('method')),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
                                                'Permutation (Raw)'),
                                    selected = 'ANOVA'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Variance Effect"', ns('method')),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('vareff'),
                                    label = 'Variance Methods',
                                    choices = c('Levene',
                                                'Permutation (MAD)',
                                                'Permutation (Gini Index)'),
                                    selected = 'Levene'
                                  )
                                ),

                                shiny::sliderInput(
                                  inputId = ns('maf'),
                                  label = 'Minor Allele Frequency',
                                  min = 0,
                                  max = 1,
                                  value = .4
                                ),

                                shiny::h5("To save parameters, enter file name and click the Download button:"),

                                shiny::textInput(
                                  inputId = ns("filename"),
                                  label = "File Name",
                                  value = "params2"
                                ),

                                shinyWidgets::downloadBttn(
                                  outputId = ns("downloadParams"),
                                  label = "Download",
                                  style = "gradient",
                                  color = "primary",
                                  size = "sm"
                                )
                              ) # END box
                            ),
                            # END column

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  title = 'Testing Mean or Variance Effects',
                                                  # verbatimTextOutput(ns('description')),
                                                  plotly::plotlyOutput(ns("pplt")),
                                                  DT::dataTableOutput(ns("paramsTable"))
                                                  ) # END box
                            ) # END column
                          ) # END fluidRow
                        ) # END tabItem
}

#' meanvarModule Server Functions
#'
#' @noRd
mod_meanvarModule_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ss <- reactive({
                   if (input$dist == "Gaussian") {
                     calcs =  reshape2::melt(as.data.frame(
                       est_pow_2samp(
                         input$nsize[1],
                         input$nsize[2],
                         input$alpha,
                         input$nsim,
                         1,
                         "norm",
                         list(
                           mean = input$gaussmean,
                           v_scale = input$gaussvar
                         ),
                         c(input$meaneff, input$vareff),
                         input$nperm
                       )
                     ), id.vars = c("Test"))


                     dens.plot = data.frame(var = c(
                       rnorm(3000, 0, 1),
                       rnorm(3000, input$gaussmean, input$gaussvar)
                     ),
                     Group = c(rep("Genotype 1", 3000), rep("Genotype 2", 3000)))


                   } else {
                     if (input$dist == "Weibull") {
                       # dens.plot =  data.frame(var = rbeta(2000,input$s1,input$s2))
                       # calcs =  reshape2::melt(as.data.frame(est_pow(input$n,input$alpha,input$nsim,input$dist,list(s1=input$s1,s2=input$s2),tests=input$checkGroup2)),id.vars=c("N","Test"))
                     }
                   }
                   list(dens.plot = dens.plot, calcs = calcs)
                 })

                 output$pplt <- plotly::renderPlotly({
                   p1 = plotly::ggplotly(
                     ggplot2::ggplot() +
                       ggplot2::geom_density(data =
                                               ss()[["dens.plot"]], ggplot2::aes(x = var, color = Group)) +
                       ggplot2::theme_classic(14) +
                       ggplot2::ylab("Population density") +
                       ggplot2::xlab("Modes") +
                       ggplot2::theme(legend.title = ggplot2::element_blank()) +
                       ggplot2::scale_color_manual(values =
                                                     c("black", "blue"))
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

                 paramsTable <- shiny::reactive({

                   calcOutput <- ss()[['calcs']]
                   # calcOutput$Row <- 1:nrow(calcOutput)

                   results <- data.frame(
                     Distribution = input$dist,
                     nSim = input$nsim
                   )

                   calcOutput <- tidyr::pivot_wider(calcOutput, names_from = c('Test', 'variable'), values_from = 'value')
                   tidyr::expand_grid(results, calcOutput)
                 })

                 output$paramsTable <- DT::renderDataTable( paramsTable() )

                 output$downloadParams <- shiny::downloadHandler(

                   filename = function() {
                     paste0(input$filename, ".csv")
                   },

                   content = function(file) {
                     write.csv(paramsTable(), file, row.names = FALSE)
                   }
                 )

               })
}
