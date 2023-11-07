#' bimodalityModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
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
                        "Beta" = "beta",
                        "Weibull" = "weib"
                      )),

          conditionalPanel(
            condition = sprintf('input["%s"] == "norm"', ns("dist")),

            sliderInput(
              ns("mu"),
              label = "Means of Mode 1 and Mode 2",
              value = c(0,3),
              min = 0,
              max = 50
            ),

            sliderInput(
              ns("sd"),
              label = "SDs of Mode 1 and Mode 2",
              value = c(1,3),
              min = 0.01,
              max = 50
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
            condition = sprintf('input["%s"] == "weib"', ns("dist")),

            sliderInput(
              ns("sp"),
              label = "Shape parameters of Mode 1 and Mode 2",
              value = c(0.5,3),
              min = 0.01,
              max = 50
            ),

            sliderInput(
              ns("sc"),
              label = "Scale parameters of Mode 1 and Mode 2",
              value = c(1,3),
              min = 0.01,
              max = 50
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
            sliderInput(
              ns("s"),
              label = "Shape parameters 1 and 2",
              value = c(0.5,0.5),
              min = 0.01,
              max = 50
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
              "Gaussian mixR" = "GmixR",
              "Weibull mixR" = "WmixR",
              "Bimodality Coefficient" = "mt",
              "Silverman Bandwidth test" = "SI",
              "Hall and York Bandwidth test" = "HY",
              "Cheng and Hall Excess Mass" = "CH",
              "Ameijeiras-Alonso et al. Excess Mass" = "ACR",
              "Fisher and Marron Carmer-von Mises" = "FM"
            ),
            selected = "dip"
          ),

          numericInput(
            ns("nsim"),
            label = "Number of simulations",
            min = 10,
            max = 5000,
            value = 100
          ),

          numericInput(
            ns("nboot"),
            label = "Number of bootstraps and/or permutations",
            min = 10,
            max = 5000,
            value = 100
          ),

         shiny::h5("To run simulations, be sure all of your parameters are set as desired and hit the 'Run Simulation' button. Please be patient as simualtions can take awhile to run."),

         shinyWidgets::actionBttn(inputId = ns("runsim2"),
                                  label = "Run Simulation",
                                  style = "gradient",
                                  color = "primary",
                                  size = "sm"),


          shiny::h5("To save parameters, enter file name and click the Download button:"),

          shiny::textInput(
            inputId = ns("filename"),
            label = "File Name",
            value = "params1"
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
                            title = 'Detecting Bimodality',
                            # verbatimTextOutput(ns('description')),
                            plotly::plotlyOutput(ns("pplt")),
                            DT::dataTableOutput(ns("paramsTable"))
                            ) # END box
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
                 vals <- reactiveValues(ss = data.frame(Test=rep("Example of selected tests",2),N=2,p=0.5,value=c(1,0),variable=c("Power","FP")))

                 observeEvent(input$runsim2,{
                   if (input$dist == "norm") {
                     calcs =  reshape2::melt(data.frame(as.data.frame(
                       bifurcatoR::est_pow(
                         input$n,
                         input$alpha,
                         input$nsim,
                         input$dist,
                         list(
                           p = input$p,
                           mu1 = input$mu[1],
                           sd1 = input$sd[1],
                           mu2 = input$mu[2],
                           sd2 = input$sd[2]
                         ),
                         tests = input$checkGroup2,
                         nboot = input$nboot
                       )), Proportion = input$p
                     ), id.vars = c("N","Proportion", "Test"))

                     # dens.plot = data.frame(var = c(
                     #   rnorm(ceiling(input$p * 2000), input$mu[1], input$sd[1]),
                     #   rnorm(ceiling((1 - input$p) * 2000), input$mu[2], input$sd[2])
                     # ))


                   } else {
                     if (input$dist == "beta") {
                       # dens.plot =  data.frame(var = rbeta(2000, input$s[1], input$s[2]))
                       calcs =  reshape2::melt(data.frame(as.data.frame(
                         bifurcatoR::est_pow(
                           input$n,
                           input$alpha,
                           input$nsim,
                           input$dist,
                           list(s1 = input$s[1], s2 = input$s[2]),
                           tests = input$checkGroup2,
                           nboot = input$nboot
                         )), Proportion = input$p
                       ), id.vars = c("N","Proportion", "Test"))
                     } else {
                      if (input$dist == "weib") {
                       calcs =  reshape2::melt(data.frame(as.data.frame(
                           bifurcatoR::est_pow(
                            input$n,
                            input$alpha,
                            input$nsim,
                            input$dist,
                            list(
                              p = input$p,
                              sp1 = input$sp[1],
                              sc1 = input$sc[1],
                              sp2 = input$sp[2],
                              sc2 = input$sc[2]
                             ),
                            tests = input$checkGroup2,
                            nboot = input$nboot
                           )), Proportion = input$p
                         ), id.vars = c("N","Proportion", "Test"))

                      #   dens.plot = data.frame(var = c(
                      #   rweibull(ceiling(input$p * 2000), input$sp[1], input$sc[1]),
                      #   rweibull(ceiling((1 - input$p) * 2000), input$sp[2], input$sc[2])
                      # ))
                      }
                    }
                   }
                     vals$ss = vals$ss[vals$ss$Test != "Example of selected tests",]
                     vals$ss <-  rbind(vals$ss,calcs)
                   })


                 output$pplt <- plotly::renderPlotly({
                   p1 = ggplot2::ggplot() +
                       ggplot2::theme_classic(14) +
                       ggplot2::ylab("Population density") +
                       ggplot2::xlab("Modes") +
                       ggplot2::theme(legend.text = ggplot2::element_text(10))
                   if(input$dist == "norm"){
                     p1 = p1 + ggplot2::stat_function(fun = function(x) {(dnorm(x, mean = input$mu[1], sd = input$sd[1]) * input$p)  +
                         (dnorm(x, mean = input$mu[2], sd = input$sd[2]) * (1-input$p)) },
                       alpha=0.5,linewidth=0.7,color = "black") +

                       ggplot2::scale_x_continuous(limits=c(min(input$mu[1]) - 3*input$sd[1]  , min(input$mu[2]) + 3*input$sd[2]))

                   } else if(input$dist == "beta"){
                     p1 = p1 + ggplot2::stat_function(fun = function(x) {(dbeta(x, mean = input$s[1], sd = input$s[2]))  },
                         alpha=0.5,linewidth=0.7,color = "black")
                   } else {
                     p1 = p1 + ggplot2::stat_function(fun = function(x) {(dweibull(x, shape = input$sp[1], scale = input$sc[1]) * input$p)  +
                         (dweibull(x, shape = input$sp[2], scale = input$sc[2]) * (1-input$p)) },
                         alpha=0.5,linewidth=0.7,color = "black")
                   }


                   fig1 = plotly::ggplotly(p1)

                   p2 = plotly::ggplotly(
                     ggplot2::ggplot(data = vals$ss , ggplot2::aes(
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



                 output$paramsTable <- DT::renderDataTable( vals$ss )

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

## To be copied in the UI
# mod_bimodalityModule_ui("bimodalityModule_1")

## To be copied in the server
# mod_bimodalityModule_server("bimodalityModule_1")
