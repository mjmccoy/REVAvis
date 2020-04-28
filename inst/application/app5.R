#
# This is a Shiny web application that visualizes REVA output. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
# library(DT) # overwrites shiny's renderDataTable and dataTableOutput to work properly

# Load functions
source(file = "../../R/read.input.files.R")

# Load REVA output data. Currently, there are issues with the input file.
# For example, some of the columns have extra quotation marks that throw that row out of wack.
# data.df <- read.table(file = "../extdata/FeatureSummary_REVA_CATALLNavy_allHuman38_unOrdered_random_EBV_NsReplacedWithA_D_12_02_19_T_13_55_12.unix.tdv", sep = c("\t"), skip = 53, header = T, fill = T, quote = c(""))
# names(data.df) <- gsub(pattern = "\\__.*", "", names(data.df))
# data.df <- data.df[,!(names(data.df) %in% "GFF_ColumnUnlabeled")]

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
                                value = "Condition1")
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
                                value = "Condition2")
                            )),
                            # Outout Condition 2 file names:
                            column(4,
                              tableOutput("condition2_files")
                            )
                          ),
                          actionButton(inputId = "submit_button",
                                       label = "Submit")
                 ),
                 tabPanel("Individual Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("chr_ui_1"),
                                   uiOutput("feature_ui_1")
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
                 # tabPanel("Multi-Chromosomal Plots",
                 #          fluidRow(
                 #            column(2,
                 #                   uiOutput("chr_ui_2"),
                 #                   uiOutput("feature_ui_2")
                 #            ),
                 #            column(10,
                 #                   plotlyOutput("condition1_multichromPlot")
                 #            )
                 #          ),
                 #          fluidRow(
                 #            column(2),
                 #            column(10,
                 #                   plotlyOutput("condition2_multichromPlot")
                 #            )
                 #          )
                 # ),
                 tabPanel("Aggregate Plots",
                          fluidRow(
                            column(2,
                                   uiOutput("chr_ui_2"),
                                   uiOutput("feature_ui_2")
                            ),
                            column(10,
                                   plotlyOutput("agg_chromPlot")
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {

  # Limit file size
  options(shiny.maxRequestSize=10000*1024^2)

  # Read Condition 1 files
  condition1_data <- reactive({
    req(input$condition1_files)
    read.input.files(file.list = input$condition1_files, condition = input$condition1_name)
  })

  # Output Condition 1 file names
  output$condition1_files <- renderTable({
    out.table <- input$condition1_files[['name']]
    if(is.null(out.table)){
      return(NULL)
    }
    out.table <- as.data.frame(out.table)
    names(out.table) <- input$condition1_name
    return(out.table)
  })

  # Read Condition 2 files
  condition2_data <- reactive({
    req(input$condition2_files)
    read.input.files(file.list = input$condition2_files, condition = input$condition2_name)
  })

  # Output Condition 2 file names
  output$condition2_files <- renderTable({
    out.table <- input$condition2_files[['name']]
    if(is.null(out.table)){
      return(NULL)
    }
    out.table <- as.data.frame(out.table)
    names(out.table) <- input$condition2_name
    return(out.table)
  })

  # Define reactive variables
  output$chr_ui_1 <- renderUI({
    selectInput("chr_1", "Chromosome name:",
                choices = unique(as.character(condition1_data()$Chr)))
  })

  output$chr_ui_2 <- renderUI({
    selectInput("chr_2", "Chromosome name:",
                choices = unique(as.character(condition1_data()$Chr)))
  })

  chr_1 <- reactive({
    as.factor(input$chr_1)
  })

  chr_2 <- reactive({
    as.factor(input$chr_2)
  })

  output$feature_ui_1 <- renderUI({
    selectInput('feature_1', 'Feature', names(condition1_data())[5:(length(names(condition1_data())) - 1)])
  })

  output$feature_ui_2 <- renderUI({
    selectInput('feature_2', 'Feature', names(condition1_data())[5:(length(names(condition1_data())) - 1)])
  })

  feature_1 <- reactive({
    input$feature_1
  })

  feature_2 <- reactive({
    input$feature_2
  })

  # condition1_chromPlot
  output$condition1_chromPlot <- renderPlotly({
    if (is.null(condition1_data())){
      return(NULL)
    }
    g1 <- subset(condition1_data(), Chr %in% chr_1()) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) %>% # removes the first instance of duplicated values (summary of each chr)
      ggplot(aes_string(x = "BinStart", y = feature_1(), col = "Condition")) +
      geom_point() +
      ylab(feature_1()) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
    ggplotly(g1)
  })

  # condition2_chromPlot
  output$condition2_chromPlot <- renderPlotly({
    if (is.null(condition2_data())){
      return(NULL)
    }
    g2 <- subset(condition2_data(), Chr %in% chr_1()) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) %>% # removes the first instance of duplicated values (summary of each chr)
      ggplot(aes_string(x = "BinStart", y = feature_1(), col = "Condition")) +
      geom_point() +
      ylab(feature_1()) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
    ggplotly(g2)
  })

  # agg_chromPlot
  output$agg_chromPlot <- renderPlotly({
    if(is.null(condition1_data())){
      return(NULL)
    }
    if(is.null(condition2_data())){
      return(NULL)
    }
    g3 <- rbind(condition1_data(), condition2_data()) %>%
      subset(Chr %in% chr_2()) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) %>%
      mutate(group = gsub("\\_.*", "", Condition)) %>%
      ggplot(aes_string(x = "BinStart", y = feature_2(), col = "group")) +
      geom_point() +
      ylab(feature_2()) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
    ggplotly(g3)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
