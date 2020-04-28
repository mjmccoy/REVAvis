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
                 ),
                 tabPanel("Aggregate Plots",
                 )
)

# Define server logic
server <- function(input, output) {

  # Limit file size
  options(shiny.maxRequestSize=10000*1024^2)

  # Read Condition 1 files
  condition1_data <- reactive({
    req(input$condition1_files)
    upload = list()
    for(nr in 1:length(input$condition1_files[, 1])){
      temp.data.df <- read.table(
        file = input$condition1_files[[nr, 'datapath']],
        sep = c("\t"),
        header = T,
        fill = T,
        quote = c(""),
        blank.lines.skip = T,
        strip.white = T,
        flush = T,
        allowEscapes = T,
        stringsAsFactors = F,
        skip = max(grep(pattern = "<!", readLines(input$condition1_files[[nr, 'datapath']]), value = FALSE)) - 1
      )
      names(temp.data.df) <- gsub(pattern = "\\__.*", "", names(temp.data.df))
      upload[[nr]] <- temp.data.df
    }
    return(upload)
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
    upload = list()
    for(nr in 1:length(input$condition2_files[, 1])){
      temp.data.df <- read.table(
        file = input$condition2_files[[nr, 'datapath']],
        sep = c("\t"),
        header = T,
        fill = T,
        quote = c(""),
        blank.lines.skip = T,
        strip.white = T,
        flush = T,
        allowEscapes = T,
        stringsAsFactors = F,
        skip = max(grep(pattern = "<!", readLines(input$condition2_files[[nr, 'datapath']]), value = FALSE)) - 1
      )
      names(temp.data.df) <- gsub(pattern = "\\__.*", "", names(temp.data.df))
      upload[[nr]] <- temp.data.df
    }
    return(upload)
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
}

# Run the application
shinyApp(ui = ui, server = server)
