library(shiny)

# Define UI for the application
shinyUI(fluidPage(
  
  # Set vertical layout for each item put on screen
  verticalLayout(
    
    # Application title
    titlePanel("Predict Word Application"),
    
    # Add control widgets
    textInput("input", label = "Type a word or phrase"),
    p("Note: Prediction occurs after a whitespace is typed.", br(), "Only last sentence is taken, if there are a few.", br(), "Getting results may take some seconds.",
      style = "color:blue"),
    
    # Display the result of prediction
    strong("Top 5 suggestions:"),
    verbatimTextOutput("result"),
    
    hr(),
    p(strong("Application details:")),
    tags$ol(
      tags$li("Check out", a(href="http://", "5-slide presentation"), "of the application"),
      tags$li("Explore", a(href="http://", "the code at GitHub"))
      ),
    p(strong("Contact:")),
    p("Gennady Khvorykh,", a(href="http://followbigdata.com", "followbigdata.com"))
    
  ) # End of verticalLayout()
))
