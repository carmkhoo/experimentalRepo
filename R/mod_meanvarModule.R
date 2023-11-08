#' meanvarModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import magrittr bsplus
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
                                  label ="Distribution",
                                  selected = 'Gaussian',
                                  choices = c('Weibull', 'Gaussian','Log-normal')
                                  ) %>%
                                    bsplus::shinyInput_label_embed(
                                      bsplus::shiny_iconlink() %>%
                                        bsplus::bs_embed_popover(
                                          title = "More info", content = "Parent distribution from which samples are drawn. Gaussian (AKA normal) and Weibull", placement ="right"
                                        )
                                    ),

                                shiny::sliderInput(
                                  inputId = ns('gaussmean'),
                                  label = 'Mean difference',
                                  min = 0,
                                  max = 10,
                                  value = 2
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Mean differnece between two groups of interest. For simplicity it's assumed that both groups have been scaled and translated such that one group is the standard normal for Gaussian or, a Weibull with mean and sd of 1", placement ="right"
                                      )
                                  ),

                                shiny::sliderInput(
                                  inputId = ns('nsize'),
                                  label = 'Sample Sizes',
                                  min = 3,
                                  max = 1000,
                                  value = c(20, 30)
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Sample sizes for the two groups", placement ="right"
                                      )
                                  ),

                                shiny::numericInput(
                                  inputId = ns('gaussvar'),
                                  label = 'Standard deviation fold-difference',
                                  min = 1,
                                  max = 10,
                                  value = 1.5
                                )%>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "How many times larger is the variance in the second group relative to the first", placement ="right"
                                      )
                                  ),

                                shiny::numericInput(
                                  inputId = ns('nperm'),
                                  label = 'Number of Permutations',
                                  min = 10,
                                  max = 5000,
                                  value = 100
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Number of permutations to run for permutation tests. The higher this is the more accurate the results but the longer the run time", placement ="right"
                                      )
                                  ),

                                shiny::numericInput(
                                  inputId = ns('nsim'),
                                  label = 'Number of Simulations',
                                  min = 10,
                                  max = 5000,
                                  value = 100
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Number of simulations to run for estimation of power and false positive rate. The more simulations the more accurate and precise the estimates will be, but run time will increase", placement ="right"
                                      )
                                  ),

                                shiny::numericInput(
                                  inputId = ns('alpha'),
                                  label = 'Significance level',
                                  min = 0.00000000000000001,
                                  max = 0.5,
                                  value = 0.05
                                )%>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Significance level or p-value cut-off level. Please note this app does not account for any multiple testing; corrections should be made outside of the app (e.g. manually apply a Bonferroni correction)", placement ="right"
                                      )
                                  ),

                                shinyWidgets::pickerInput(
                                  inputId = ns('method'),
                                  label = 'Effect to Test',
                                  selected = 'Mean Effect',
                                  choices = c("Mean Dfferences" = 'Mean Effect', "Unequal variances" = 'Variance Effect')
                                ) %>%
                                  bsplus::shinyInput_label_embed(
                                    bsplus::shiny_iconlink() %>%
                                      bsplus::bs_embed_popover(
                                        title = "More info", content = "Drop down menu that will bring up options to test for mean differences and/or variance differneces", placement ="right"
                                      )
                                  ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean Effect"', ns('method')),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
                                                "Ranked ANOVA" = 'Non-parametric ANOVA',
                                                "Permutation test of the mean" = 'Permutations (Raw)'),
                                    selected = 'ANOVA'
                                  )
                                ) ,

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Variance Effect"', ns('method')),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('vareff'),
                                    label = 'Variance Methods',
                                    choices = c("Levene's Test" = "Levene",
                                                'Permutations (MAD)',
                                                'Permutations (Gini Index)',
                                                'Permutations (SD)'),
                                    selected = 'Levene'
                                  )
                                ),

                                shiny::h5("To run simulations, be sure all of your parameters are set as desired and hit the 'Run Simulation' button. Please be patient as simualtions can take awhile to run."),

                                shinyWidgets::actionBttn(inputId = ns("runsim1"),
                                                         label = "Run Simulation",
                                                         style = "gradient",
                                                         color = "primary",
                                                         size = "sm"),

                                shiny::h5("To save parameters, enter file name and click the Download button:"),

                                shiny::textInput(
                                  inputId = ns("filename"),
                                  label = "File Name for download",
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
                                                  DT::dataTableOutput(ns("paramsTable")),
                                                  shiny::textOutput(ns("testPrint"))
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

                 vals <- reactiveValues(ss = data.frame(Test=rep("Example of selected tests",2), "Sample.Sizes" = "[n1, n2]",value=c(1,0),variable=c("Power","FP")),
                                        tbl = data.frame(Distribution = "TBD",Mean.Diff = NA,SD_FC = NA,Test="Example of selected tests", "Sample.Sizes" = "[n1, n2]",Power=1,FP = 0))

                 observeEvent(input$runsim1,{
                   # print("button works")

                  if (input$dist == "Gaussian") {
                     calcs =  data.frame(as.data.frame(
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
                       )), "Sample.Sizes" = paste0("[",input$nsize[1],", ", input$nsize[2],"]")
                     )


                     # dens.plot = data.frame(var = c(
                     #   rnorm(3000, 0, 1),
                     #   rnorm(3000, input$gaussmean, input$gaussvar)
                     # ),
                     # Group = c(rep("Genotype 1", 3000), rep("Genotype 2", 3000)))


                   } else {
                     if (input$dist == "Weibull") {
                           calcs = data.frame(as.data.frame(est_pow_2samp(input$nsize[1],
                                                              input$nsize[2], input$alpha, input$nsim, 1,
                                                              "weib", list(mean = input$gaussmean, v_scale = input$gaussvar),
                                                              c(input$meaneff, input$vareff), input$nperm)), "Sample.Sizes" = paste0("[",input$nsize[1],", ", input$nsize[2],"]")
                           )
                            ## Distirbution for the reference group is weibull(1,1), user still enters mean eff and var eff
                            ## the R package mixdist is then used to get shape and scale paramters from new mean and sd
                            # shape = mixdist::weibullpar(1+input$gaussmean,1*input$gaussvar, loc = 0)$shape
                            # scale = mixdist::weibullpar(1+input$gaussmean,1*input$gaussvar, loc = 0)$scale
                            # dens.plot = data.frame(var = c(rweibull(3000, 1, 1), rweibull(3000,shape=shape,scale=scale )),
                            #                        Group = c(rep("Genotype 1", 3000), rep("Genotype 2", 3000)))
                     } else {
                       if(input$dist == "Log-normal"){

                       calcs = data.frame(as.data.frame(est_pow_2samp(input$nsize[1],
                                                                                     input$nsize[2], input$alpha, input$nsim, 1,
                                                                                     "lnorm", list(mean = input$gaussmean, v_scale = input$gaussvar),
                                                                                     c(input$meaneff, input$vareff), input$nperm)), "Sample.Sizes" = paste0("[",input$nsize[1],", ", input$nsize[2],"]")
                       )

                     }
                     }
                   }
                   vals$ss = vals$ss[vals$ss$Test != "Example of selected tests",]
                   vals$tbl = vals$tbl[vals$tbl$Test != "Example of selected tests",]

                   vals$ss <- reshape2::melt(calcs,id.vars=c("Test","Sample.Sizes"))
                   calcs$Distribution = input$dist
                   calcs$Mean.Diff = input$gaussmean
                   calcs$SD_FC = input$gaussvar

                   vals$tbl <-  rbind(vals$tbl,calcs)
                 })

                 output$pplt <- plotly::renderPlotly({
                   p1 = ggplot2::ggplot() +
                       # ggplot2::geom_density(data =
                       #                         ss()[["dens.plot"]], ggplot2::aes(x = var, color = Group)) +
                       ggplot2::theme_classic(14) +
                       ggplot2::ylab("Population density") +
                       ggplot2::xlab("Modes") +
                       ggplot2::theme(legend.title = ggplot2::element_blank()) +
                       ggplot2::scale_color_manual(values =
                                                     c("black", "blue"))

                   if (input$dist == "Gaussian"){
                     p1 = p1 + ggplot2::stat_function(fun = function(x) {
                      (dnorm(x, mean = 0, sd = 1))},
                      alpha=0.5,linewidth=0.7,color = "black") +

                     ggplot2::stat_function(fun = function(x) {
                       (dnorm(x, mean = input$gaussmean, sd = input$gaussvar))},
                       alpha=0.5,linewidth=0.7,color = "blue") +
                       ggplot2::scale_x_continuous(limits=c(-1 * input$gaussvar*3  , input$gaussvar*3 + input$gaussmean))

                   } else {
                     if (input$dist == "Weibull"){
                       p1 = p1 + ggplot2::stat_function(fun = function(x) {
                         (dweibull(x, 1, 1))},
                         alpha=0.5,linewidth=0.7,color = "black") +

                         ggplot2::stat_function(fun = function(x) {
                           (dweibull(x, shape = mixdist::weibullpar(1+input$gaussmean,1*input$gaussvar, loc = 0)$shape,
                                     scale = mixdist::weibullpar(1+input$gaussmean,1*input$gaussvar, loc = 0)$scale))},
                           alpha=0.5,linewidth=0.7,color = "blue")+
                         ggplot2::scale_x_continuous(limits=c(0  , input$gaussvar*5 + input$gaussmean))

                     } else{
                       if (input$dist == "Log-normal"){
                         p1 = p1 + ggplot2::stat_function(fun = function(x) {
                           (dlnorm(x, 0, 1))},
                           alpha=0.5,linewidth=0.7,color = "black") +

                           ggplot2::stat_function(fun = function(x) {
                             (dlnorm(x,
                                     meanlog = EpiNow2::convert_to_logmean(exp(0.5) + input$gaussmean, input$gaussvar*exp((1/2)*1)*sqrt(exp(1) - 1) ),
                                     sdlog = EpiNow2::convert_to_logsd(exp(0.5) +  input$gaussmean, input$gaussvar*exp((1/2)*1)*sqrt(exp(1) - 1) )))},
                             alpha=0.5,linewidth=0.7,color = "blue")+
                           ggplot2::scale_x_continuous(limits=c(0 ,
                                                                EpiNow2::convert_to_logmean(exp(0.5) + input$gaussmean, input$gaussvar*exp((1/2)*1)*sqrt(exp(1) - 1)) * 5 +
                                                                                              EpiNow2::convert_to_logsd(exp(0.5) +  input$gaussmean, input$gaussvar*exp((1/2)*1)*sqrt(exp(1) - 1) )))

                       }
                     }
                   }

                   fig1 = plotly::ggplotly(p1)
                   if(length(vals$ss) > 0){
                   p2 = ggplot2::ggplot(data =vals$ss, ggplot2::aes(
                       x = Test, y = value, color = variable
                     ))
                   }  else {
                     p2 = ggplot2::ggplot()
                   }

                   p2 = p2 +
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

                   fig2 = plotly::ggplotly(p2)

                   plotly::subplot(fig1,
                                   fig2,
                                   nrows = 1,
                                   margin = c(0.02, 0.02, .21, .21))


                 })

                 # # Initialize table of parameters
                 # init_tbl <- data.frame(
                 #   Distribution = character(),
                 #   nSim = numeric(),
                 #   Test = character(),
                 #   Variable = character(),
                 #   Value = numeric()
                 # )
                 #
                 # paramsTable <- shiny::reactive({
                 #   # observeEvent(input$meaneff, {
                 #     if(length(vals)>0){
                 #     calcOutput <- vals$ss
                 #     # calcOutput
                 #     tbl_row <- nrow(calcOutput)
                 #     tbl <- data.frame(
                 #       Distribution = rep(input$dist, tbl_row),
                 #       nSim = rep(input$nsim, tbl_row),
                 #       Test = calcOutput$Test,
                 #       Variable = calcOutput$variable,
                 #       Value = calcOutput$value
                 #     )
                 #     tbl <- tbl[order(tbl$Test),]
                 #     rbind(init_tbl, tbl)
                 #   # })
                 #     }
                 # })

                 output$testPrint <- shiny::renderPrint( c(input$meaneff, input$vareff) ) #vals$ss

                 output$paramsTable <- DT::renderDataTable( vals$tbl )

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
