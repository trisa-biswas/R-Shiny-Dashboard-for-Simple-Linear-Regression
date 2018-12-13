
library(shiny)


# Define server logic to read selected file ----
server <- function(input, output) 
  {
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1
  })
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
  
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
    })
    
  output$summary <- renderPrint({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
      print(summary(df))
    
  })
  
  output$corrplot<- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    library(PerformanceAnalytics)
    chart.Correlation(df, histogram=TRUE, pch=19)
  })
  
  output$Hist1<- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    hist(df$YearsExperience,
         main="Histogram for Years of Experience",
         xlab = "YearsExperience",
         border = "blue")
  })
  
  output$Hist2<- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    hist(df$Salary,
         main = "Histogram for Salary",
         xlab = "Salary",
         border = "blue")
  })
  
  
  
  
  output$modelSummary <- renderPrint({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    model <- load(file = "SimpleLinearRegression.rda")
    YE <- data.frame(YearsExperience = df$YearsExperience)
    #y_pred = predict(get(model), newdata = YE)
    #print(y_pred)
    dat2 <- data.frame(YE, Predicted = predict(get(model), YE))
    print(merge(df, dat2, by = 'YearsExperience', all.y = TRUE))
  })
  
  output$Viz<- renderPlot({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    library(ggplot2)
    ggplot() +
      geom_point(aes(x = df$YearsExperience, y = df$Salary),
                 colour = 'red') +
      geom_line(aes(x = df$YearsExperience, y = predict(regressor, newdata = df)),
                colour = 'blue') +
      ggtitle('Salary vs Experience') +
      xlab('Years of experience') +
      ylab('Salary')
  })
}