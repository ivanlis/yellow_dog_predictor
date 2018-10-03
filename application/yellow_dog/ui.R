#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

appCss <- "
#loading-content {
    position: absolute;
    background: #FFCA33;
    opacity: 0.9;
    z-index: 100;
    left: 0;
    right: 0;
    height: 100%;
    text-align: center;
    color: #FFFFFF;
}
"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    useShinyjs(),
    inlineCSS(appCss),
    
    # Loading message
    div(
        id = "loading-content",
        h2("Loading n-grams...")
    ),
  
    hidden(
        div(id = "app-content",
            fluidRow(
                column(10, offset = 1,
                     titlePanel("Yellow Dog Word Predictor"))  
                ),
            
            hr(),
            
        #    fluidRow(
        #        column(10, offset = 1,
        #               verbatimTextOutput(outputId = "dbInfo"))
        #        ),
            
            fluidRow(
                column(10, offset = 1,
                     textAreaInput(inputId = "userText", label = "Enter text", width = "300%", height = "150")
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
    )
)
