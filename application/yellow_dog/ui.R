#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
    fluidRow(
        column(10, offset = 1,
             titlePanel("Yellow Dog Word Predictor"))  
        ),
    
    hr(),
    
    fluidRow(
        column(10, offset = 1,
             textAreaInput(inputId = "userText", label = "Enter text", width = "400%", height = "150")
        )
    ),

    fluidRow(
        column(2, offset = 1, actionButton(inputId = "submitButton", "Submit text"))
    ),

    hr(),

    fluidRow(
        column(10, offset = 1, tableOutput(outputId = "dbInfo"))  
    ),
    
    hr(),
    
    fluidRow(
        column(10, offset = 1,
             tableOutput(outputId = "suggestion"))
        )
    )
)
