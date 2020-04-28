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
source(file = "../../R/ManhattanPlot.R")
source(file = "../../R/2DPlot.R")
source(file = "../../R/PlotDownload.R")

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
                                   uiOutput("render_ui_1"),
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
                                   uiOutput("render_ui_2"),
                                   submitButton("Submit")
                            ),
                            column(10,
                                   plotlyOutput("agg_chromPlot")
                            )
                          )
                 ),
                 tabPanel("Manhattan Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("render_ui_3"),
                                   submitButton("Submit")
                            ),
                            column(10,
                                   plotOutput("condition1_ManhattanPlot"),
                                   uiOutput("render_download_condition1_ManhattanPlot")
                            )
                          ),
                          fluidRow(
                            column(2),
                            column(10,
                                   plotOutput("condition2_ManhattanPlot")
                            )
                          ),
                          fluidRow(
                            column(2),
                            column(10,
                                   plotOutput("ManhattanPlot_ratio")
                            )
                          )
                 ),
                 tabPanel("2D Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("render_ui_4"),
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
    vals$condition1_name <- input$condition1_name
    vals$condition2_name <- input$condition2_name
    vals$chr_1 <- as.factor(input$chr_1)
    vals$chr_2 <- as.factor(input$chr_2)
    vals$chr_3 <- as.factor(input$chr_3)
    vals$chr_4 <- as.factor(input$chr_4)
    vals$feature_1 <- input$feature_1
    vals$feature_2 <- input$feature_2
    vals$feature_3 <- input$feature_3
    vals$feature_4 <- input$feature_4
    vals$plot_height_1 <- input$plot_height_1
    vals$plot_width_1 <- input$plot_width_1
    vals$plot_height_2 <- input$plot_height_2
    vals$plot_width_2 <- input$plot_width_2
    vals$plot_height_3 <- input$plot_height_3
    vals$plot_width_3 <- input$plot_width_3
    vals$plot_height_4 <- input$plot_height_4
    vals$plot_width_4 <- input$plot_width_4
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

  # Render UI
  output$render_ui_1 <- renderUI({
    list(
      selectInput(
        inputId = "chr_1",
        label = "Chromosome name:",
        choices = unique(as.character(condition1_data()$Chr))),
      selectInput(
        inputId = 'feature_1',
        label = 'Feature',
        choices = names(condition1_data())[5:(length(names(condition1_data())) - 1)])
    )
  })

  output$render_ui_2 <- renderUI({
    list(
      selectInput(
        inputId = "chr_2",
        label = "Chromosome name:",
        choices = unique(as.character(condition1_data()$Chr))),
      selectInput(
        inputId = 'feature_2',
        label = 'Feature',
        choices = names(condition1_data())[5:(length(names(condition1_data())) - 1)]),
      checkboxInput("mean_se", "Mean + SE", TRUE)
    )
  })

  output$render_ui_3 <- renderUI({
    list(
      selectInput(
        inputId = "chr_3",
        label = "Chromosome name:",
        choices = unique(as.character(condition1_data()$Chr)),
        multiple = T),
      selectInput(
        inputId = 'feature_3',
        label = 'Feature',
        choices = names(condition1_data())[5:(length(names(condition1_data())) - 1)]),
      checkboxInput("log_scale", "log10(x + 1)", TRUE),
      numericInput('plot_height_3', 'Download plot height (in)', 3,
                   min = 1, max = 48),
      numericInput('plot_width_3', 'Download plot width (in)', 8,
                   min = 1, max = 48)
    )
  })

  output$render_ui_4 <- renderUI({
    list(
      selectInput(
        inputId = "chr_4",
        label = "Chromosome name:",
        choices = unique(as.character(condition1_data()$Chr)),
        multiple = T),
      selectInput(
        inputId = 'feature_4',
        label = 'Feature',
        choices = names(condition1_data())[5:(length(names(condition1_data())) - 1)])
    )
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

  # condition1_ManhattanPlot
  output$condition1_ManhattanPlot <- renderPlot({
    req(vals$chr_3)
    ManhattanPlot(
      data = condition1_data(),
      chr = vals$chr_3,
      feature = vals$feature_3,
      log_scale = input$log_scale)
  })

  # condition2_ManhattanPlot
  output$condition2_ManhattanPlot <- renderPlot({
    req(vals$chr_3)
    ManhattanPlot(
      data = condition2_data(),
      chr = vals$chr_3,
      feature = vals$feature_3,
      log_scale = input$log_scale)
  })

  # ManhattanPlot_ratio
  output$ManhattanPlot_ratio <- renderPlot({
    req(vals$chr_3)
    ManhattanPlot(
      data = condition1_data(),
      data2 = condition2_data(),
      chr = vals$chr_3,
      feature = vals$feature_3,
      log_scale = input$log_scale,
      condition1_name = input$condition1_name,
      condition2_name = input$condition2_name)
  })

  # ManhattanPlot_ratio
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

  # Download Plots
  output$download_condition1_ManhattanPlot <- PlotDownload(
      condition = vals$condition1_name,
      plotType = "Manhattan",
      input.plot = ManhattanPlot(
        data = condition1_data(),
        chr = vals$chr_3,
        feature = vals$feature_3,
        log_scale = input$log_scale),
      width = vals$plot_width_3,
      height = vals$plot_height_3
  )

  output$render_download_condition1_ManhattanPlot <- renderUI({
    req(vals$chr_3, condition1_data())
    downloadButton("download_condition1_ManhattanPlot", "Download plot")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
