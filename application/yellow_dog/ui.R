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
  
  
    fluidRow(
        column(10, offset = 2,
             titlePanel("Yellow Dog Word Predictor"))  
        ),
    
    fluidRow(
        column(10, offset = 2,
             verbatimTextOutput(outputId = "dbInfo"),
             textAreaInput(inputId = "userText", label = "Enter text", width = "300%", height = "500"),
             actionButton(inputId = "submitButton", "Submit text"),
             verbatimTextOutput(outputId = "suggestion"))
        )
    )
)
