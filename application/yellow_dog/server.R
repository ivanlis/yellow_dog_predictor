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
    
#    output$dbInfo <- renderPrint(
#        sprintf("Database loaded: %d unigrams, %d bigrams, %d trigrams, %d fourgrams, %d fivegrams",
#                nrow(ngrams$unigram), nrow(ngrams$bigram), nrow(ngrams$trigram), nrow(ngrams$fourgram),
#                nrow(ngrams$fivegram)))
    ngramInfo <- data.frame(c(nrow(ngrams$unigram)), c(nrow(ngrams$bigram)), c(nrow(ngrams$trigram)),
                            c(nrow(ngrams$fourgram)), c(nrow(ngrams$fivegram)))
    
    hideElement(id = "loading-content", anim = TRUE, animType = "fade")
    showElement("app-content")
    
    names(ngramInfo) <- c("1-grams", "2-grams", "3-grams", "4-grams", "5-grams")
    output$dbInfo <- renderTable(ngramInfo)
    

    prediction <- eventReactive(input$submitButton, {
        tokenizedInput <- tokenizeInput(input$userText)
        res <- NA
        if (length(tokenizedInput[[1]]) > 0)
            res <- predictWordKatz(ngrams, as.character(tokenizedInput))
        if (is.na(res) || is.na(res$generalResult) || nrow(res$generalResult) == 0)
            data.frame(words = c("No suggestions"))
        else
            res$generalResult[1:min(nrow(res$generalResult), 10), 
                              .(word, nGramOrder = origin, nGramCount = count, KatzProb = katz)]
    })
    
    output$suggestion <- renderTable(prediction())
})
