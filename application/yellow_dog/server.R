#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    source("predictWord.R")
    ngrams <- loadDatabase(".")
    
    output$dbInfo <- renderPrint(
        sprintf("Database loaded: %d unigrams, %d bigrams, %d trigrams, %d fourgrams, %d fivegrams",
                nrow(ngrams$unigram), nrow(ngrams$bigram), nrow(ngrams$trigram), nrow(ngrams$fourgram),
                nrow(ngrams$fivegram)))
    

    prediction <- eventReactive(input$submitButton, {
        res <- predictWordKatz(ngrams, input$userText)
        if (is.na(res) || is.na(res$generalResult) || nrow(res$generalResult) == 0)
            "No suggestions"
        else
            res$generalResult[1:10]
    })
    
    output$suggestion <- renderPrint(prediction())
})
