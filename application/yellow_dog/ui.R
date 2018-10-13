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

jscode <- '
$(function() {
    $(document).on("keypress", function(e) {
        if (e.which == 32 || e.which == 13)
            //Shiny.onInputChange("queryKey", Math.random());
        {
            setTimeout(function(){ Shiny.onInputChange("queryKey", Math.random()); }, 500)
        }
    });
});
'

appCss <- '
#loading-content {
  position: absolute;
  background: #ffcc00;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #ffffff;
}
'

shinyUI(fluidPage(
    
    useShinyjs(),
    inlineCSS(appCss),
    
    tags$head(tags$script(HTML(jscode))),
    
    div(id = "loading-content", h2("Please, wait a few seconds. Loading the database...")),
     
    hidden(div(id = "app-content",
        
        fluidRow(
            column(10, offset = 1,
                 titlePanel("Yellow Dog Word Predictor"))  
            ),
        
        hr(),
        
        fluidRow(
            column(10, offset = 1,
                   p("Source code: ", 
                     a("https://github.com/ivanlis/yellow_dog_predictor", 
                       href = "https://github.com/ivanlis/yellow_dog_predictor"), br(),
                     "Presentation: ",
                     a("https://ivanlis.github.io/yellow_dog_predictor/final_slides.html",
                       href = "https://ivanlis.github.io/yellow_dog_predictor/final_slides.html")))  
        ),    
        
        fluidRow(
            column(10, offset = 1, 
                   p("Please, enter a text in English. As you press Space or Enter or push the 'Submit text' button, suggestions will appear."))
        ),
        
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
            column(10, offset = 1,
                   tableOutput(outputId = "suggestion"))
        ),
        
        hr(),
        
        fluidRow(
            column(10, offset = 1, tableOutput(outputId = "dbInfo"))  
        )
    
    ))
    
))
