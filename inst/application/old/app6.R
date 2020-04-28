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

  # chr
  chr_1 <- reactive({
    as.factor(input$chr_1)
  })
  chr_2 <- reactive({
    as.factor(input$chr_2)
  })
  chr_3 <- reactive({
    as.factor(input$chr_3)
  })

  # feature
  feature_1 <- reactive({
    input$feature_1
  })
  feature_2 <- reactive({
    input$feature_2
  })
  feature_3 <- reactive({
    input$feature_3
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

  ###########
  ## Plots ##
  ###########

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

  # condition1_multi_chromPlot
  output$condition1_multi_chromPlot <- renderPlotly({
    if (is.null(condition1_data())){
      return(NULL)
    }
    data.df <- subset(condition1_data(), Chr %in% chr_3()) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
    data.df <- data.df[with(data.df, order(Condition, Chr, BinStart)), ]
    data.df$Chr <- factor(data.df$Chr, levels = chr_3()) # force chr order by selection
    g4 <- data.df %>% ggplot(aes_string(x = "Chr", y = feature_3(), col = "Condition")) +
      geom_point() +
      ylab(feature_3()) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
    ggplotly(g4)
  })

  # condition2_multi_chromPlot
  output$condition2_multi_chromPlot <- renderPlotly({
    if (is.null(condition2_data())){
      return(NULL)
    }
    data.df <- subset(condition2_data(), Chr %in% chr_3()) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
    data.df <- data.df[with(data.df, order(Condition, Chr, BinStart)), ]
    data.df$Chr <- factor(data.df$Chr, levels = chr_3()) # force chr order by selection
    g5 <- data.df %>% ggplot(aes_string(x = "Chr", y = feature_3(), col = "Condition")) +
      geom_point() +
      ylab(feature_3()) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
    ggplotly(g5)
  })

  # multi_chromPlot_ratio
  output$multi_chromPlot_ratio <- renderPlotly({
    if (is.null(condition1_data())){
      return(NULL)
    }
    if (is.null(condition2_data())){
      return(NULL)
    }
    data.df <- condition1_data() %>% subset(Chr %in% chr_3()) %>%
      select(c("Condition", "Chr", feature_3())) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
    data2.df <- condition2_data() %>% subset(Chr %in% chr_3()) %>%
      select(c("Condition", "Chr", feature_3())) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
    data.df[,3] <- data.df[,3]/data2.df[,3] # calculate ratio condition1/condition2
    # data.df <- data.df[with(data.df, order(Condition, Chr)), ]
    data.df$Chr <- factor(data.df$Chr, levels = chr_3()) # force chr order by selection
    g6 <- data.df %>% ggplot(aes_string(x = "Chr", y = feature_3(), col = "Condition")) +
      stat_summary() +
      ylab(paste(feature_3(), "\n", input$condition1_name, "/", input$condition2_name, sep = "")) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
    ggplotly(g6)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
