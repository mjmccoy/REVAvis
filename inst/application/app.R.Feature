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
               tabPanel("chromPlot",
                        sidebarLayout(
                            sidebarPanel(
                                fluidRow(
                                    # Input: Select a file ----
                                    fileInput("file1", "Choose REVA Output File",
                                              multiple = FALSE,
                                              accept = c("text/tdv",
                                                         "text/tab-delimited-values",
                                                         ".tdv")),
                                    # Horizontal line ----
                                    tags$hr(),
                                    uiOutput("chr_ui"),
                                    uiOutput("start_ui"),
                                    uiOutput("end_ui"),
                                    uiOutput("feature_ui")
                                )
                            ),
                            mainPanel(
                                plotlyOutput("chromPlot")
                                )
                            )
                        )
               # tabPanel("manPlot",
               #          sidebarPanel(
               #              fluidRow(
               #                  selectInput('feature2', 'Feature', names(data.df)[10:37])
               #              )
               #          ),
               #          mainPanel(
               #                  plotlyOutput("manPlot")
               #              )
               #          ),
               # tabPanel("boxPlot",
               #          sidebarPanel(
               #              fluidRow(
               #                  selectInput('feature3', 'Feature', names(data.df)[10:37])
               #              )
               #          ),
               #          mainPanel(
               #              plotlyOutput("boxPlot")
               #          )
               # )
)

# Define server logic
server <- function(input, output) {
    options(shiny.maxRequestSize=10000*1024^2)
    data.df <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        temp.data.df <- read.table(file = inFile$datapath, sep = c("\t"), skip = 53, header = T, fill = T, quote = c(""))
        # temp.data.df <- read.table(file = inFile$datapath, sep = c("\t"), header = T, fill = T, quote = c(""), blank.lines.skip = T, strip.white = T, flush = T, allowEscapes = T, skip = 83)
        names(temp.data.df) <- gsub(pattern = "\\__.*", "", names(temp.data.df))
        temp.data.df[,!(names(temp.data.df) %in% "GFF_ColumnUnlabeled")]
    })

    # define reactive variables
    output$chr_ui <- renderUI({
        selectInput("chr", "Chromosome name:",
                    choices = unique(as.character(data.df()$Seqname)))
    })
    chr <- reactive({
        as.factor(input$chr)
    })
    output$start_ui <- renderUI({
        numericInput("start",
                     h3("Chromosome start:"),
                     value = min(data.df()$Start))
    })
    start <- reactive({
        input$start
    })
    output$end_ui <- renderUI({
        numericInput("end",
                     h3("Chromosome end:"),
                     value = max(data.df()$End))
    })
    end <- reactive({
        input$end
    })
    output$feature_ui <- renderUI({
        selectInput('feature', 'Feature', names(data.df())[10:37])
    })
    feature <- reactive({
        input$feature
    })
    feature2 <- reactive({
        input$feature2
    })
    feature3 <- reactive({
        input$feature3
    })

    # chromPlot
    output$chromPlot <- renderPlotly({
        if (is.null(data.df()))
            return(NULL)
        g1 <- subset(data.df(), Seqname %in% chr() & Start >= start() & End <= end()) %>% ggplot(aes(x = Start, y = eval(parse(text = feature())))) + geom_point(aes(col = Attribute)) + ylab(feature()) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) + geom_hline(yintercept = 0)
        ggplotly(g1)
    })
    # manPlot
    output$manPlot <- renderPlotly({
        if (is.null(data.df()))
            return(NULL)
        g2 <- data.df()[with(data.df(), order(data.df()$Seqname, data.df()$Start)), ] %>% ggplot() + geom_point(aes(x = as.factor(Start), y = eval(parse(text = feature2())), col = Seqname)) + ylab(paste("log10(", feature2(), ")", "")) + scale_y_log10() + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.title.x=element_blank(), axis.text.x=element_blank()) + geom_hline(yintercept = 0)
        # g2 <- data.df[with(data.df, order(data.df$Seqname, data.df$Start)), ] %>% ggplot() + geom_point(aes(x = as.factor(Start), y = eval(parse(text = feature2()))^(1/3), col = Seqname)) + ylab(paste("cube root(", feature2(), ")", "")) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.title.x=element_blank(), axis.text.x=element_blank()) + geom_hline(yintercept = 0)
        ggplotly(g2)
    })
    # boxPlot
    output$boxPlot <- renderPlotly({
        if (is.null(data.df()))
            return(NULL)
        g3 <- ggplot(data.df()) + geom_boxplot(aes(x = as.factor(Seqname), y = eval(parse(text = feature3())), fill = Seqname)) + ylab(paste("log10(", feature3(), ")", "")) + scale_y_log10() + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.title.x=element_blank(), axis.text.x=element_blank())
        # g3 <- ggplot(data.df) + geom_boxplot(aes(x = as.factor(Seqname), y = eval(parse(text = feature3()))^(1/3), fill = Seqname)) + ylab(paste("cube root(", feature2(), ")", "")) + theme_classic() + theme(panel.border = element_rect(colour = "black", fill=NA, size=1), axis.title.x=element_blank(), axis.text.x=element_blank())
        ggplotly(g3)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
