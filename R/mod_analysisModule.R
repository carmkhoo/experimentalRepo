#' mod_analysisModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_meanvarModule_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = 'Analysis',
                          shiny::fluidRow(
                            column(
                              width = 3,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Parameters',

                                fileInput("Dataset","Please input a two column csv file (One column 'group' names, second column 'value'"),

                                numericInput("Alpha",
                                             label = "Significance Level (adjusted for multiple testing)",min = 0.0000000001, max = 0.999, value =0.05),


                                shiny::numericInput(
                                  inputId = ns('nboot'),
                                  label = 'Number of Permutation or Bootstrap Resamples',
                                  min = 10,
                                  max = 5000,
                                  value = 100
                                ),

                                shinyWidgets::pickerInput(
                                  inputId = ns('effect'),
                                  label = 'Select Effect:',
                                  choices = c('Mean', 'Variance','Bimodality', 'Mean-Variance'),
                                  selected = 'Mean'
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean-Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meanvar'),
                                    label = 'Select Test(s):',
                                    choices = c('Anderson-Darling' = "ad",
                                                'Kolmogorov–Smirnov' = "ks",
                                                'Cramer-Von Mises' = "cvm",
                                                'DTS' = "dts"),
                                    selected = 'Kolmogorov–Smirnov'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
                                                'Non-parametric ANOVA',
                                                'Permutations (Raw)'),
                                    selected = 'ANOVA'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('vareff'),
                                    label = 'Variance Methods',
                                    choices = c('Levene',
                                                'Permutations (MAD)',
                                                'Permutations (Gini Index)',
                                                'Permutations (SD)'),
                                    selected = 'Levene'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Bimodality"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('bimode'),
                                    label = ("Bimodality Tests"),
                                    choices = list(
                                      "Hartigans dip test" = "dip",
                                      "Mclust" = "mclust",
                                      "Bimodality Coefficient" = "mt",
                                      "Silverman Bandwidth test" = "SI",
                                      "Hall and York Bandwidth test" = "HY",
                                      "Cheng and Hall Excess Mass" = "CH",
                                      "Ameijeiras-Alonso et al. Excess Mass" = "ACR",
                                      "Fisher and Marron Carmer-von Mises" = "FM"),
                                    selected = "dip")
                                ),
                                actionButton("analysisButton", "Analyze data")
                              )
                            ),

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  title = 'Testing Mean or Variance Effects',
                                                  verbatimTextOutput(ns('description')),
                                                  plotly::plotlyOutput(ns("pplt")),
                                                  DT::dataTableOutput(ns("resTable")),
                                                  shiny::textOutput(ns("testPrint"))
                              ) # END box
                            ) # END column
                          ) # END fluidRow
  ) # END tabItem
}


mod_meanvarModule_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {

                 vals <- reactiveValues(upld.file = NULL)

                 observeEvent(input$annotation,{
                   file <- input$annotation
                   data <- read.csv(file$datapath)
                   vals$upld.file <- data
                   data = na.omit(data)
                   data$value = as.numeric(as.character(data$value))
                   output$pplt <- shiny::renderPlot({
                     p1 = ggplot(data, aes(x = group, y = value, color = group, fill = group)) +
                       ggplot2::scale_y_continuous() +
                       ggplot2::scale_color_manual(values =c("darkblue","orange")) +
                       ggplot2::scale_fill_manual(values =c("darkblue","orange")) +
                       ggplot2::geom_boxplot(
                         width = .15, fill = "white", outlier.shape = 17, notch = T
                       ) +
                       ggdist::stat_halfeye(
                         color = NA, ## remove slab interval
                         position = position_nudge(x = .15),
                         trim=FALSE,
                         alpha=0.75,
                         width = .67,
                       ) +
                       gghalves::geom_half_point(
                         side = "l", range_scale = .25,
                         alpha = .5, size = 2
                       ) +
                       ggplot2::theme_classic(16) +
                       ggplot2::facet_wrap(~group,scales="free_x") +
                       ggplot2::theme(legend.text = ggplot2::element_text(10),
                                      strip.background = element_blank(),
                                      axis.text.x = element_blank(),
                                      axis.ticks.x = element_blank(),
                                      legend.title = element_blank(),
                                      axis.line.x = element_blank(),
                                      legend.position = "none")+
                       xlab("") + ylab("Value")

                     if(length(unique(data$group))==1){
                       p2 = decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group[[1]])],boot=input$nboot,boot.col="darkblue")+
                         theme_classic(14) + theme(strip.background = element_blank(),strip.text = element_blank())
                     } else {

                       p2.1 = decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group[[1]])],boot=input$nboot,boot.col="darkblue",obs.col="darkblue",obs_geom_size = 5)+
                         theme_classic(14) + theme(strip.background = element_blank(),strip.text = element_blank())+ggtitle(unique(data$group[[1]]))+
                         theme(legend.position = "bottom")

                       p2.2 = decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group[[2]])],boot=input$nboot,boot.col="orange",obs.col = "orange",obs_geom_size = 5)+
                         theme_classic(14) + theme(strip.background = element_blank(),strip.text = element_blank())+ggtitle(unique(data$group[[2]]))+
                         theme(legend.position = "bottom")


                       p2 = p2.1 + p2.2

                     }

                     print(p1/p2)

                   })

                   # Initialize table of parameters
                   init_tbl <- data.frame(
                     Test = character(),
                     nboot = numeric(),
                     p.value = numeric(),
                     Stat = numeric(),
                     CI = character()
                   )
                 })

                 observeEvent(input$analysisButton,{

                   ss = Analyze_Data(data = vals$upld.file, c(input$vareff,input$bimode,input$meanvar,input$vareff),input$nboot,input$alpha)
                   rownames(ss) = NULL
                   paramsTable <- shiny::reactive({
                     calcOutput <- ss
                     # calcOutput
                     tbl_row <- nrow(calcOutput)
                     tbl <- ss
                     tbl <- tbl[order(tbl$Test),]
                     rbind(init_tbl, tbl)
                     # })

                   })

                   output$testPrint <- shiny::renderPrint( c(input$meaneff, input$vareff) ) # ss()[["calcs"]]

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
               })
}

## To be copied in the UI
# mod_analysisModule_ui("bimodalityModule_1")

## To be copied in the server
# mod_analysisModule_server("bimodalityModule_1")
