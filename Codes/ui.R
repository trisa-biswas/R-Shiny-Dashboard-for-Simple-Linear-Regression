library(shiny)
# Define UI for data upload app ----
ui <- fluidPage(
  # App title ----
  titlePanel("Simple Linear Regression"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Horizontal line ----
      tags$hr(),
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      # Horizontal line ----
      tags$hr(),
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("contents")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Corelogram", plotOutput("corrplot")),
        tabPanel("Histogram", plotOutput("Hist1"),
                              plotOutput("Hist2")),
        tabPanel("Prediction", verbatimTextOutput("modelSummary")),
        tabPanel("Vizualising Test Results", plotOutput("Viz"))
        
                )
    
              )
      )
    )