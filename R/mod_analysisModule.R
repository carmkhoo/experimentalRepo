#' analysisModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysisModule_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = 'analysis',
                          shiny::fluidRow(
                            column(
                              width = 3,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Settings',

                                shiny::fileInput(ns("file_name"),"Please input a two column csv file (One column 'group' names, second column 'value'",accept=".csv"),

                                shiny::numericInput(ns("Alpha"),
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
                                                'DTS' = "dts")
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
                                                'Non-parametric ANOVA',
                                                'Permutations (Raw)')
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
                                                'Permutations (SD)')
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
                                      "Fisher and Marron Carmer-von Mises" = "FM"))
                                ),
                                actionButton(ns("analysisButton"), "Analyze data")
                              )
                            ),

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  title = 'Descriptive Plots of Your Data',
                                                  shiny::plotOutput(ns("pplt")),
                                                  DT::dataTableOutput(ns("resTable"))
                              ) # END box
                            ) # END column
                          ) # END fluidRow
  ) # END tabItem
}


mod_analysisModule_server <- function(id) {
  ns <- shiny::NS(id)

  moduleServer(id,
               function(input, output, session) {

              vals <- reactiveValues(tbl=NULL,upld.file=NULL)

              observeEvent(input$file_name,{
                   vals$tbl = NULL
                   output$resTable <- DT::renderDataTable({
                     vals$tbl
                   })
                   file = input$file_name
                   upld.file = read.csv(file$datapath,header=T)
                   upld.file = na.omit(upld.file)
                   upld.file$value = as.numeric(as.character(upld.file$value))
                   vals$upld.file <- upld.file
                   data = upld.file
                   if(length(unique(data$group)) <2){
                   shinyWidgets::updatePickerInput(
                       session = session,
                       inputId = 'effect',
                       label = 'Select Effect:',
                       choices = c('Bimodality'),
                       selected = 'Bimodality'
                     )
                   } else {
                     shinyWidgets::updatePickerInput(
                       session = session,
                       inputId = 'effect',
                       label = 'Select Effect:',
                       choices = c('Mean', 'Variance', 'Mean-Variance'),
                       selected = 'Mean'
                     )
                   }

              output$pplt <- shiny::renderPlot({

                if(length(unique(data$group))>1){
                  p1 = ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, color = group, fill = group)) +
                    ggplot2::scale_y_continuous() +
                    ggplot2::scale_color_manual(values =c("darkblue","orange")) +
                    ggplot2::scale_fill_manual(values =c("darkblue","orange"))
                } else{
                  p1 = ggplot2::ggplot(data, ggplot2::aes(x = group, y = value, color = group, fill = group)) +
                    ggplot2::scale_y_continuous() +
                    ggplot2::scale_color_manual(values =c("darkorchid4")) +
                    ggplot2::scale_fill_manual(values =c("darkorchid4"))

                }

                p1 = p1 +  ggplot2::geom_boxplot(
                    width = .15, fill = "white", outlier.shape = 17, notch = F
                  ) +
                  ggdist::stat_halfeye(
                    color = NA, ## remove slab interval
                    position = ggplot2::position_nudge(x = .15),
                    trim=FALSE,
                    alpha=0.75,
                    width = .67,
                  ) +
                  gghalves::geom_half_point(
                    side = "l", range_scale = .25,
                    alpha = .5, size = 2
                  ) +
                  ggplot2::theme_classic(16) +
                  ggplot2::theme(legend.text = ggplot2::element_text(10),
                                 strip.background = ggplot2::element_blank(),
                                 axis.text.x = ggplot2::element_blank(),
                                 axis.ticks.x = ggplot2::element_blank(),
                                 legend.title = ggplot2::element_blank(),
                                 axis.line.x = ggplot2::element_blank(),
                                 legend.position = "none")+
                  ggplot2::xlab("") + ggplot2::ylab("Value")

                if(length(unique(data$group))==1){

                  p1 = p1 + ggplot2::facet_wrap(~group) +
                    ggplot2::theme(legend.text = ggplot2::element_text(10),
                                   strip.background = ggplot2::element_blank(),
                                   axis.text.x = ggplot2::element_blank(),
                                   axis.ticks.x = ggplot2::element_blank(),
                                   legend.title = ggplot2::element_blank(),
                                   axis.line.x = ggplot2::element_blank(),
                                   legend.position = "none")+
                    ggplot2::xlab("") + ggplot2::ylab("Value")

                  p2 = decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group[[1]])],boot=input$nboot,boot.col="darkorchid",obs.col="darkorchid4" , obs_geom_size = 4,boot_geom_size = 0.02)+
                    ggplot2::theme_minimal(12) + ggplot2::theme(strip.background = ggplot2::element_blank(),
                                                                strip.text = ggplot2::element_blank(),
                                                                legend.text = ggplot2::element_text(size=ggplot2::rel(.5)))

                  p2.l = cowplot::get_legend(p2)
                  # p2 = cowplot::plot_grid(p2+ggplot2::theme(legend.position = "none"),p2.l,ncol=1)
                  p2 = p2+ggplot2::theme(legend.position = "none")

                } else {
                  p1 = p1 + ggplot2::facet_wrap(~ group,scales="free_y") +
                    ggplot2::theme(legend.text = ggplot2::element_text(10),
                                   strip.background = ggplot2::element_blank(),
                                   axis.text.y = ggplot2::element_blank(),
                                   axis.ticks.y = ggplot2::element_blank(),
                                   legend.title = ggplot2::element_blank(),
                                   axis.line.y = ggplot2::element_blank(),
                                   legend.position = "none")+
                    ggplot2::ylab("") + ggplot2::xlab("Value") + ggplot2::coord_flip()

                  p2.1 = decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group)[[1]]],boot=input$nboot,boot.col="blue",obs.col="darkblue",obs_geom_size = 5)+
                    ggplot2::theme_minimal(12) + ggplot2::theme(strip.background = ggplot2::element_blank(),strip.text = ggplot2::element_blank(),
                                                                legend.text = ggplot2::element_text(size=8))+ggplot2::ggtitle(unique(data$group)[[1]])

                  p2.1 = p2.1+ggplot2::theme(legend.position = "none")



                  p2.2 = decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group)[[2]]],boot=input$nboot,boot.col="orange",obs.col = "darkorange",obs_geom_size = 5)+
                    ggplot2::theme_minimal(12) + ggplot2::theme(strip.background = ggplot2::element_blank(),strip.text = ggplot2::element_blank(),
                                                                legend.text = ggplot2::element_text(size=8))+ggplot2::ggtitle(unique(data$group)[[2]])

                  p2.2 = p2.2+ggplot2::theme(legend.position = "none")

                  p2 = cowplot::plot_grid(p2.1 , p2.2)

                  p2.l = cowplot::get_legend(decisionSupportExtra::ggplot_descdist(data$value[data$group == unique(data$group)[[2]]],boot=input$nboot,boot.col="black",obs.col = "black" , obs_geom_size = 4,boot_geom_size = 0.02)+
                                               ggplot2::theme_minimal(12) + ggplot2::theme(strip.background = ggplot2::element_blank(),strip.text = ggplot2::element_blank(),
                                                                                           legend.text = ggplot2::element_text(size=8),
                                                                                           legend.position="bottom")+ggplot2::ggtitle(unique(data$group)[[2]])
                  )

                }

                if(length(unique(data$group))<2){
                  cowplot::plot_grid(cowplot::plot_grid(p1,p2,ncol=2,scale=c(0.9,0.8)),
                                     cowplot::plot_grid(p2.l),rel_widths = c(1,.35))

                } else {
                  cowplot::plot_grid(cowplot::plot_grid(p1,p2,ncol=1,rel_heights=c(.75,1)),
                  cowplot::plot_grid(p2.l),rel_heights = c(1,.25),ncol=1)
                }


               }) #close renderPlot
              })


             observeEvent(input$analysisButton,{
                   if(length(unique(vals$upld.file$group))>1){
                    vals$tbl = rbind(vals$tbl,bifurcatoR_Analysis(data = vals$upld.file, c(input$vareff,input$meanvar,input$meaneff),input$nboot,input$alpha))
                   } else {
                    vals$tbl = rbind(vals$tbl,bifurcatoR_Analysis(data = vals$upld.file, c(input$bimode),input$nboot,input$alpha))
                   }
                   output$resTable <- DT::renderDataTable({
                     vals$tbl
                   })
              })
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
# mod_analysisModule_ui("bimodalityModule_1")

## To be copied in the server
# mod_analysisModule_server("bimodalityModule_1")
