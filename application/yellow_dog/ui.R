#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Yellow Dog Word Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        verbatimTextOutput(outputId = "dbInfo"),
        
        textInput(inputId = "userText", label = "Enter text"),
        tags$br(),
        actionButton(inputId = "submitButton", "Submit text"),
        verbatimTextOutput(outputId = "suggestion")
    )
  )
))