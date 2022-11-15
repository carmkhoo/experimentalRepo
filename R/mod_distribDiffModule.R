#' distribDiffModule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_distribDiffModule_ui <- function(id){
  ns <- NS(id)
  
  shinydashboard::tabItem(tabName = 'distribdiff',
                          shiny::fluidRow(
                            shinyjs::useShinyjs(),
                            shiny::column(
                              width = 3,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Parameters',
                                
                                shinyWidgets::pickerInput(
                                  inputId = ns('dist'),
                                  label = 'Select Distribution:',
                                  choices = c(
                                    'Beta' = "beta",
                                    'Gaussian' = "norm",
                                    'Weibull' = "weib"
                                  ),
                                  selected = 'norm'
                                ),
                                
                                shiny::numericInput(
                                  inputId = ns('alpha'),
                                  label = 'Significance level',
                                  min = 0.00000000000000001,
                                  max = 0.5,
                                  value = 0.05
                                ),
                                
                                shiny::sliderInput(
                                  ns("n"),
                                  label = "Total number of samples in group A and group B",
                                  min = 5,
                                  max = 500,
                                  value = c(20, 40),
                                  step = 5
                                ),
                                
                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "norm"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("p1"),
                                    label = "Proportion in mode 1 group A",
                                    min = 0.01,
                                    max = 0.99,
                                    value = .5,
                                    step = .01
                                  ),
                                  shiny::sliderInput(
                                    ns("p2"),
                                    label = "Proportion in mode 1 group B",
                                    min = 0.01,
                                    max = 0.99,
                                    value = .5,
                                    step = .01
                                  ),
                                  shiny::sliderInput(
                                    ns("mus1"),
                                    label = "Mean of mode 1 and mode 2 group A",
                                    value = c(0, 3),
                                    min = 0,
                                    max = 100
                                  ),
                                  shiny::sliderInput(
                                    ns("sds1"),
                                    label = "SD of mode 1 and mode 2 group A",
                                    value = c(.5, .5),
                                    min = .5,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("mus2"),
                                    label = "Mean of mode 1 and mode 2 group B",
                                    value = c(2, 10),
                                    min = 0,
                                    max = 100
                                  ),
                                  shiny::sliderInput(
                                    ns("sds2"),
                                    label = "SD of mode 1 and mode 2 group B",
                                    value = c(.5, 2),
                                    min = .5,
                                    max = 10
                                  )
                                ),
                                
                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "beta"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("sps1"),
                                    label = "Shape parameters group A",
                                    value = c(.5, .75),
                                    min = .05,
                                    max = 10,
                                    step = 0.05
                                  ),
                                  shiny::sliderInput(
                                    ns("sps2"),
                                    label = "Shape parameters group B",
                                    value = c(.25, 1),
                                    min = .05,
                                    max = 10,
                                    step = 0.05
                                  )
                                ),
                                
                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "weib"', ns("dist")),
                                  shiny::sliderInput(
                                    ns("p1"),
                                    label = "Proportion in mode 1 group A",
                                    min = 0.01,
                                    max = 0.99,
                                    value = 0.5,
                                    step = 0.01
                                  ),
                                  shiny::sliderInput(
                                    ns("p2"),
                                    label = "Proportion in mode 1 group B",
                                    min = 0.01,
                                    max = 0.99,
                                    value = 0.5,
                                    step = 0.01
                                  ),
                                  shiny::sliderInput(
                                    ns("sps1"),
                                    label = "Shape parameter of mode 1 and mode 2 group A",
                                    value = c(1, 1.5),
                                    min = 0.01,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("scs1"),
                                    label = "Scale parameter of mode 1 and mode 2 group A",
                                    value = c(1, 5),
                                    min = 0.01,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("sps2"),
                                    label = "Shape parameter of mode 1 and mode 2 group B",
                                    value = c(2, 3),
                                    min = 0.01,
                                    max = 10
                                  ),
                                  shiny::sliderInput(
                                    ns("scs2"),
                                    label = "Scale parameter of mode 1 and mode 2 group B",
                                    value = c(2, 10),
                                    min = 0.01,
                                    max = 10
                                  )
                                ),
                                
                                
                                
                                shiny::numericInput(
                                  inputId = ns('nsim'),
                                  label = 'Number of Simulations',
                                  min = 5,
                                  max = 5000,
                                  value = 10
                                ),
                                
                                shiny::numericInput(
                                  inputId = ns('nperm'),
                                  label = 'Number of Permutations',
                                  min = 5,
                                  max = 5000,
                                  value = 10
                                ),
                                
                                shinyWidgets::pickerInput(
                                  inputId = ns('effect'),
                                  label = 'Select Effect:',
                                  choices = c('Mean', 'Variance', 'Mean-Variance'),
                                  selected = 'Mean'
                                ),
                                
                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean-Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meanvar'),
                                    label = 'Select Test(s):',
                                    choices = c('Anderson-Darling',
                                                'Kolmogorov–Smirnov'),
                                    selected = 'Kolmogorov–Smirnov'
                                  )
                                ),
                                
                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
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
                                                'Permutations (MAD)'),
                                    # 'Permutation (Gini Index)'),
                                    selected = 'Levene'
                                  )
                                ),
                                
                                shiny::h5("To save parameters, enter file name and click the Download button:"),
                                
                                shiny::textInput(
                                  inputId = ns("filename"),
                                  label = "File Name",
                                  value = "params3"
                                ),
                                
                                shinyWidgets::downloadBttn(
                                  outputId = ns("downloadParams"),
                                  label = "Download",
                                  style = "gradient",
                                  color = "primary",
                                  size = "sm"
                                )
                                
                              )
                            ), # END FIRST COLUMN
                            
                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  title = 'Comparing two Bimodal Groups',
                                                  # verbatimTextOutput(ns('description')),
                                                  plotly::plotlyOutput(ns("pplt")),
                                                  DT::dataTableOutput(ns("paramsTable"))
                              ) # END box
                            ) # END column
                          ) # END fluidRow
  ) # END tabItem
}

#' distribDiffModule Server Functions
#'
#' @noRd
mod_distribDiffModule_server <- function(id){
  moduleServer( id, function(input, output, session){
    # ns <- session$ns
    
    ss <- reactive({
      
      if(input$dist == "norm"){
        param = list(p_1 = input$p1,p_2=input$p2,
                     mu1_1 = input$mus1[1],
                     mu2_1 = input$mus1[2],
                     mu1_2 = input$mus2[1],
                     mu2_2 = input$mus2[2],
                     sd1_1 = input$sds1[1],
                     sd2_1 = input$sds1[2],
                     sd1_2 = input$sds2[1],
                     sd2_2 = input$sds2[2])
        
        calcs = reshape2::melt(as.data.frame(est_pow_2samp(input$n[1],
                                                           input$n[2],
                                                           input$alpha,
                                                           input$nsim,
                                                           modes = 2,
                                                           dist = 'norm',
                                                           params = param,
                                                           c(input$meaneff,
                                                             input$vareff,
                                                             input$meanvar),
                                                           input$nperm),
                                             id.vars=c("Test")))
        
        
        dens.plot = data.frame(Group = rep("Group A"),
                               var = c(rnorm(ceiling(input$p1*2000),input$mus1[1],input$sds1[1]),rnorm(ceiling((1-input$p1)*2000),input$mus1[2],input$sds1[2])))
        dens.plot = rbind(dens.plot,data.frame(Group=rep("Group B"),
                                               var = c(rnorm(ceiling(input$p2*2000),input$mus2[1],input$sds2[1]),rnorm(ceiling((1-input$p2)*2000),input$mus2[2],input$sds2[2]))))
        
      } else {
        if(input$dist == "beta"){
          dens.plot =  data.frame(Group=rep("Group A"),var = rbeta(2000,input$sps1[1],input$sps1[2]))
          dens.plot =  rbind(dens.plot,data.frame(Group=rep("Group B"),var = rbeta(2000,input$sps2[1],input$sps2[2])))
          param = list(s1_1 = input$sps1[1],s2_1 = input$sps1[2],
                       s1_2 = input$sps2[1],s2_2 = input$sps2[2])
          
          calcs =  reshape2::melt(as.data.frame(est_pow_2samp(input$n[1],
                                                              input$n[2],
                                                              input$alpha,
                                                              input$nsim,
                                                              modes = 2,
                                                              dist= 'beta',
                                                              params=param,
                                                              c(input$meaneff,input$vareff,input$meanvar),
                                                              input$nperm),id.vars=c("Test")))
        } else {
          if(input$dist == "weib"){
            param = list(p_1 = input$p1,p_2=input$p2,
                         sp1_1 = input$sps1[1],
                         sp2_1 = input$sps1[2],
                         sc1_1 = input$scs1[1],
                         sc2_1 = input$scs1[2],
                         sp1_2 = input$sps2[1],
                         sp2_2 = input$sps2[2],
                         sc1_2 = input$scs2[1],
                         sc2_2 = input$scs2[2])
            
            calcs = reshape2::melt(as.data.frame(est_pow_2samp(input$n[1],
                                                               input$n[2],
                                                               input$alpha,
                                                               input$nsim,
                                                               modes = 2,
                                                               dist = 'weib',
                                                               params = param,
                                                               c(input$meaneff,
                                                                 input$vareff,
                                                                 input$meanvar),
                                                               input$nperm),
                                                 id.vars=c("Test")))
            
            
            dens.plot = data.frame(Group = rep("Group A"),
                                   var = c(rweibull(ceiling(input$p1*2000),shape = input$sps1[1],scale = input$scs1[1]),rweibull(ceiling((1-input$p1)*2000),shape = input$sps1[2],scale = input$scs1[2])))
            dens.plot = rbind(dens.plot,data.frame(Group=rep("Group B"),
                                                   var = c(rweibull(ceiling(input$p2*2000),shape = input$sps2[1],scale = input$scs2[1]),rweibull(ceiling((1-input$p2)*2000),shape = input$sps2[2],scale = input$scs2[2]))))
            print(max(dens.plot$var))
            print(min(dens.plot$var))
          }
        }
      }
      list(dens.plot=dens.plot,calcs=calcs)
    })
    
    
    output$pplt <- plotly::renderPlotly({
      p1 = plotly::ggplotly(ggplot2::ggplot() +
                              ggplot2::geom_density(data=ss()[["dens.plot"]],
                                                    ggplot2::aes(x=var,color=Group)) +
                              ggplot2::theme_classic(14) +
                              ggplot2::ylab("Population density") +
                              ggplot2::xlab("Modes") +
                              ggplot2::theme(legend.title = ggplot2::element_blank()) +
                              ggplot2::scale_color_manual(values=c("black","blue")))
      fig1 = plotly::ggplotly(p1)
      
      p2 = plotly::ggplotly(ggplot2::ggplot(data=ss()[["calcs"]],
                                            ggplot2::aes(x=Test,y=value,color=variable)) +
                              ggplot2::geom_point(position = ggplot2::position_dodge(width = .25)) +
                              ggplot2::theme_classic(14) +
                              ggplot2::ylab("Probability") +
                              ggplot2::xlab("Test") +
                              ggplot2::theme(legend.title=ggplot2::element_blank(),
                                             legend.text = ggplot2::element_text(10),
                                             axis.text.x = ggplot2::element_text(angle=45,hjust=1)) +
                              ggplot2::scale_color_manual(values=c("black","red")) +
                              ggplot2::coord_cartesian(ylim=c(-.02,1.01)))
      fig2 = plotly::ggplotly(p2)
      
      
      plotly::subplot(fig1, fig2, nrows=1,margin=c(0.02,0.02,.21,.21))
      
    })
    
    # Initialize table of parameters
    init_tbl <- data.frame(
      Distribution = character(),
      nSim = numeric(),
      Test = character(),
      Variable = character(),
      Value = numeric()
    )
    
    paramsTable <- shiny::reactive({
      
      calcOutput <- ss()[['calcs']]
      # observeEvent(input$meaneff, {
      
      calcOutput <- ss()[['calcs']]
      # calcOutput
      tbl_row <- nrow(calcOutput)
      tbl <- data.frame(
        Distribution = rep(input$dist, tbl_row),
        nSim = rep(input$nsim, tbl_row),
        Test = calcOutput$Test,
        Variable = calcOutput$variable,
        Value = calcOutput$value
      )
      tbl <- tbl[order(tbl$Test),]
      rbind(init_tbl, tbl)
      # })
      
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

## To be copied in the UI
# mod_distribDiffModule_ui("distribDiffModule_1")

## To be copied in the server
# mod_distribDiffModule_server("distribDiffModule_1")
