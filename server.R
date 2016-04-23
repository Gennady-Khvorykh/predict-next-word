library(shiny)
source("functions.R")

# Load the model
ngrams <- readRDS("ngrams.rds")

shinyServer(function(input, output) {
  
  # Check the input text and predict next words
  output$result <- renderText({
    
    if (grepl("\\W$", input$input) & grepl("\\w+", input$input)) {
      
      # Add progress bar
      withProgress(message = "Predicting next word...", {
        
        # Predict next word
        match <- predict(input$input, ngrams)
        
        # Choose top 5 words with highest probabilities.
        # If the probabilities are equal, select the rest words randomly
        narrowOutput(match)
      })
    }
    
  })
})
