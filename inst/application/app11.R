############################
## REVAvis v0.9           ##
## Author: Matt McCoy     ##
## Date: February 3, 2020 ##
############################

# This is a Shiny web application that visualizes REVA output. You can run the application by clicking
# the 'Run App' button above.

# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
# library(DT) # overwrites shiny's renderDataTable and dataTableOutput to work properly

# Load functions
source(file = "../../R/read.input.files.R")
source(file = "../../R/chromPlot.R")
source(file = "../../R/agg_chromPlot.R")
source(file = "../../R/multi_chromPlot.R")
source(file = "../../R/2DPlot.R")

# Define UI for application
ui <- navbarPage("REVA Visualization",
                 tabPanel("Data Input",
                          fluidRow(
                            # Input Condition 1 files:
                            column(4, wellPanel(
                              fileInput(
                                inputId = "condition1_files",
                                label = "Choose REVA Output Files for Condition 1",
                                multiple = TRUE,
                                accept = c(".tdv")
                              ),
                              textInput(
                                inputId = "condition1_name",
                                label = "Name for Condition 1",
                                value = "Condition1"),
                              checkboxInput("condition1_norm", "RPM normalize", TRUE)
                            )),
                            # Output Condition 1 file names:
                            column(4,
                                   tableOutput("condition1_files")
                            )
                          ),
                          fluidRow(
                            # Input Condition 2 files:
                            column(4, wellPanel(
                              fileInput(
                                inputId = "condition2_files",
                                label = "Choose REVA Output Files for Condition 2",
                                multiple = TRUE,
                                accept = c(".tdv")
                              ),
                              textInput(
                                inputId = "condition2_name",
                                label = "Name for Condition 2",
                                value = "Condition2"),
                              checkboxInput("condition2_norm", "RPM normalize", TRUE)
                            )),
                            # Outout Condition 2 file names:
                            column(4,
                              tableOutput("condition2_files")
                            )
                          ),
                          fluidRow(
                            column(4,
                              submitButton("Submit")
                            )
                          )
                 ),
                 tabPanel("Individual Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("chr_ui_1"),
                                   uiOutput("feature_ui_1"),
                                   submitButton("Submit")
                            ),
                            column(10,
                                   plotlyOutput("condition1_chromPlot")
                            )
                          ),
                          fluidRow(
                            column(2),
                            column(10,
                                   plotlyOutput("condition2_chromPlot")
                            )
                          )
                 ),
                 tabPanel("Aggregate Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("chr_ui_2"),
                                   uiOutput("feature_ui_2"),
                                   checkboxInput("mean_se", "Mean + SE", TRUE),
                                   submitButton("Submit")
                            ),
                            column(10,
                                   plotlyOutput("agg_chromPlot")
                            )
                          )
                 ),
                 tabPanel("Multi-Chromosomal Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("chr_ui_3"),
                                   uiOutput("feature_ui_3"),
                                   submitButton("Submit")
                            ),
                            column(10,
                                   plotlyOutput("condition1_multi_chromPlot")
                            )
                          ),
                          fluidRow(
                            column(2),
                            column(10,
                                   plotlyOutput("condition2_multi_chromPlot")
                            )
                          ),
                          fluidRow(
                            column(2),
                            column(10,
                                   plotlyOutput("multi_chromPlot_ratio")
                            )
                          )
                 ),
                 tabPanel("2D Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("chr_ui_4"),
                                   uiOutput("feature_ui_4"),
                                   submitButton("Submit")
                            ),
                            column(5,
                                   plotlyOutput("TwoDPlot")
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {

  # Limit file size
  options(shiny.maxRequestSize=10000*1024^2) # ~10 Gb

  ###############################
  ## Define reactive variables ##
  ###############################

  # condition files
  condition1_data <- reactive({
    req(input$condition1_files)
    read.input.files(
      file.list = input$condition1_files,
      condition = input$condition1_name,
      normalized = input$condition1_norm
    )
  })
  condition2_data <- reactive({
    req(input$condition2_files)
    read.input.files(
      file.list = input$condition2_files,
      condition = input$condition2_name,
      normalized = input$condition2_norm)
  })

  # chr and feature values
  vals <- reactiveValues()
  observe({
    vals$chr_1 <- as.factor(input$chr_1)
    vals$chr_2 <- as.factor(input$chr_2)
    vals$chr_3 <- as.factor(input$chr_3)
    vals$chr_4 <- as.factor(input$chr_4)
    vals$feature_1 <- input$feature_1
    vals$feature_2 <- input$feature_2
    vals$feature_3 <- input$feature_3
    vals$feature_4 <- input$feature_4
  })

  ####################
  ## Define outputs ##
  ####################

  # condition file names
  output$condition1_files <- renderTable({
    out.table <- input$condition1_files[['name']]
    if(is.null(out.table)){
      return(NULL)
    }
    out.table <- as.data.frame(out.table)
    names(out.table) <- input$condition1_name
    return(out.table)
  })
  output$condition2_files <- renderTable({
    out.table <- input$condition2_files[['name']]
    if(is.null(out.table)){
      return(NULL)
    }
    out.table <- as.data.frame(out.table)
    names(out.table) <- input$condition2_name
    return(out.table)
  })

  # chr
  output$chr_ui_1 <- renderUI({
    selectInput("chr_1", "Chromosome name:",
                choices = unique(as.character(condition1_data()$Chr)))
  })
  output$chr_ui_2 <- renderUI({
    selectInput("chr_2", "Chromosome name:",
                choices = unique(as.character(condition1_data()$Chr)))
  })
  output$chr_ui_3 <- renderUI({
    selectInput("chr_3", "Chromosome name:",
                multiple = T,
                choices = unique(as.character(condition1_data()$Chr)))
  })
  output$chr_ui_4 <- renderUI({
    selectInput("chr_4", "Chromosome name:",
                multiple = T,
                choices = unique(as.character(condition1_data()$Chr)))
  })

  # feature
  output$feature_ui_1 <- renderUI({
    selectInput('feature_1', 'Feature', names(condition1_data())[5:(length(names(condition1_data())) - 1)])
  })
  output$feature_ui_2 <- renderUI({
    selectInput('feature_2', 'Feature', names(condition1_data())[5:(length(names(condition1_data())) - 1)])
  })
  output$feature_ui_3 <- renderUI({
    selectInput('feature_3', 'Feature', names(condition1_data())[5:(length(names(condition1_data())) - 1)])
  })
  output$feature_ui_4 <- renderUI({
    selectInput('feature_4', 'Feature', names(condition1_data())[5:(length(names(condition1_data())) - 1)])
  })

  ###########
  ## Plots ##
  ###########

  # condition1_chromPlot
  output$condition1_chromPlot <- renderPlotly({
    req(vals$chr_1)
    chromPlot(
      data = condition1_data(),
      chr = vals$chr_1,
      feature = vals$feature_1
    )
  })

  # condition2_chromPlot
  output$condition2_chromPlot <- renderPlotly({
    req(vals$chr_1)
    chromPlot(
      data = condition2_data(),
      chr = vals$chr_1,
      feature = vals$feature_1
    )
  })

  # agg_chromPlot
  output$agg_chromPlot <- renderPlotly({
    req(vals$chr_2)
    agg_chromPlot(
      data1 = condition1_data(),
      data2 = condition2_data(),
      chr = vals$chr_2,
      feature = vals$feature_2,
      mean_se = input$mean_se
    )
  })

  # condition1_multi_chromPlot
  output$condition1_multi_chromPlot <- renderPlotly({
    req(vals$chr_3)
    multi_chromPlot(
      data = condition1_data(),
      chr = vals$chr_3,
      feature = vals$feature_3)
  })

  # condition2_multi_chromPlot
  output$condition2_multi_chromPlot <- renderPlotly({
    req(vals$chr_3)
    multi_chromPlot(
      data = condition2_data(),
      chr = vals$chr_3,
      feature = vals$feature_3)
  })

  # multi_chromPlot_ratio
  output$multi_chromPlot_ratio <- renderPlotly({
    req(vals$chr_3)
    multi_chromPlot(
      data = condition1_data(),
      data2 = condition2_data(),
      chr = vals$chr_3,
      feature = vals$feature_3,
      condition1_name = input$condition1_name,
      condition2_name = input$condition2_name)
  })

  # multi_chromPlot_ratio
  output$TwoDPlot <- renderPlotly({
    req(vals$chr_4)
    TwoDPlot(
      data1 = condition1_data(),
      data2 = condition2_data(),
      chr = vals$chr_4,
      feature = vals$feature_4,
      condition1_name = input$condition1_name,
      condition2_name = input$condition2_name)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
