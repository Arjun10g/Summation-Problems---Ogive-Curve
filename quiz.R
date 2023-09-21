library(shiny)
library(ggplot2)
library(tidyverse)
library(janitor)
library(shinyjs)
library(papaja)

createOgivePlot <- function(data, variable) {
  # Create the ECDF data
  ecdf_data <- ecdf(data[[variable]])
  
  # Generate a range of x values for the curve
  x_values <- seq(min(data[[variable]]), max(data[[variable]]), length.out = 100)
  
  # Calculate the cumulative probabilities using the ECDF
  y_values <- ecdf_data(x_values)
  
  # Create a data frame for the plot
  ogive_data <- data.frame(x = x_values, y = y_values)
  
  # Create the ggplot2 plot
  plot <- ggplot(ogive_data, aes(x, y)) +
    geom_step() +
    labs(x = variable, y = "Cumulative Probability") +
    scale_x_continuous(breaks = seq(min(x_values), max(x_values)))
  ggtitle(paste("Ogive Curve for", variable)) +
    papaja::theme_apa()
  
  return(plot)
}



ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(includeCSS("www/quiz.css")),
  tabsetPanel(
    tabPanel("Quiz", 
             tags$h2("Quiz time!!"),
             
             # Question 1
             h4("Question 1: What type of data is characterized by distinct, separate values often representing counts or whole numbers?"),
             radioButtons("question1", "Select an answer:",
                          c("A) Nominal data" = "incorrect", 
                            "B) Ratio data" = "incorrect", 
                            "C) Continuous data" = "incorrect", 
                            "D) Discrete data" = "correct")),
             
             # Question 2
             h4("Question 2: Which of the following represents data with a meaningful order or ranking but with uneven intervals between categories?"),
             radioButtons("question2", "Select an answer:",
                          c("A) Nominal data" = "incorrect", 
                            "B) Discrete data" = "incorrect", 
                            "C) Ordinal data" = "correct", 
                            "D) Continuous data" = "incorrect")),
             
             # Question 3
             h4("Question 3: What type of data can take on any value within a given range and is not restricted to specific intervals?"),
             radioButtons("question3", "Select an answer:",
                          c("A) Interval data" = "incorrect", 
                            "B) Nominal data" = "incorrect", 
                            "C) Ordinal data" = "incorrect", 
                            "D) Ratio data" = "correct")),
             
             # Question 4
             h4("Question 4: What does a frequency distribution display?"),
             radioButtons("question4", "Select an answer:",
                          c("A) A scatterplot of data points" = "incorrect", 
                            "B) The range of data values" = "incorrect", 
                            "C) The number of times each distinct value or category appears in a dataset" = "correct", 
                            "D) The standard deviation of data" = "incorrect")),
             
             # Question 5
             h4("Question 5: When is it appropriate to use grouped frequency distributions?"),
             radioButtons("question5", "Select an answer:",
                          c("A) When you have a small dataset" = "incorrect", 
                            "B) When you want to list each individual value" = "incorrect", 
                            "C) When the dataset contains a wide range of values" = "correct", 
                            "D) When you want to calculate the mean of the data" = "incorrect")),
             
             # Question 6
             h4("Question 6: What is the purpose of rounding up the width of each interval in a grouped frequency distribution?"),
             radioButtons("question6", "Select an answer:",
                          c("A) To make the data look more organized" = "incorrect", 
                            "B) To ensure all data points are included in the intervals" = "correct", 
                            "C) To reduce the number of intervals" = "incorrect", 
                            "D) To make the intervals visually appealing" = "incorrect")),
             
             # Question 7
             h4("Question 7: Which data visualization method is suitable for displaying the distribution of discrete data?"),
             radioButtons("question7", "Select an answer:",
                          c("A) Bar plot" = "correct", 
                            "B) Frequency polygon" = "incorrect", 
                            "C) Histogram" = "incorrect", 
                            "D) Scatterplot" = "incorrect")),
             
             # Question 8
             h4("Question 8: In a frequency polygon, what is represented by the line segments connected to class midpoint values?"),
             radioButtons("question8", "Select an answer:",
                          c("A) Frequency of each class" = "correct", 
                            "B) The range of the data" = "incorrect", 
                            "C) The standard deviation of the data" = "incorrect", 
                            "D) The mode of the data" = "incorrect")),
             
             # Question 9
             h4("Question 9: What is the term for a distribution that is not symmetric and has a longer tail on one side?"),
             radioButtons("question9", "Select an answer:",
                          c("A) Skewness" = "correct", 
                            "B) Kurtosis" = "incorrect", 
                            "C) Symmetry" = "incorrect", 
                            "D) Modality" = "incorrect")),
             
             actionButton('submit', 'Submit'),
             mainPanel(
               verbatimTextOutput('out')
             )
             # Additional UI components for Tab 1 go here
    ),
    tabPanel("Graph", 
             tags$h2("Custom Graph"),
             sidebarLayout(
               sidebarPanel(
                 tags$h2('Make Histogram'),
                 shiny::textInput('code', label = 'Enter your data comma-separated', value = '', width = '100%'),
                 actionButton('sub', 'Submit')
               ),
               mainPanel(
                 verbatimTextOutput('outa')
               )
             ),
             plotOutput('plot'),
             plotOutput('plot2')
             # Additional UI components for Tab 2 go here
    )
  )
)

server <- function(input, output, session) {
  observeEvent(paste0('question',1:9),{
    answers <- c(input$question1, input$question2,input$question3,input$question4,input$question5,input$question6,input$question7,input$question8,input$question9)
    output$out <- renderPrint({
      answers
      tryCatch(paste0(sum(ifelse(answers == 'correct', 1, 0)), '/9'),error = function(e) 'error')
    })
  })
  
  hide('out')
  
  onclick('submit', {
    show('out')
  })
  
  observeEvent(input$sub, {
    tryCatch({
      data <- parse(text = paste0('c(', str_flatten(input$code, collapse = ','), ')')) %>% eval
      conv_dat <- tibble(x = data)
      
      output$outa <- renderPrint({
        conv_dat %>% tabyl(x) %>% adorn_pct_formatting()
      })
      
      output$plot <- renderPlot({
        conv_dat %>% ggplot(aes(x)) +
          geom_bar(col = 'black', fill = 'khaki', bins = 10) +
          papaja::theme_apa()
      })
      
      output$plot2 <- renderPlot({
        createOgivePlot(conv_dat,'x') +
          papaja::theme_apa()
      })
    }, error = function(e) {
      output$outa <- renderPrint({
        e
      })
    })
  })
  
}

shinyApp(ui, server)

