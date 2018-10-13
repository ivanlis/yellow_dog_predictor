#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
    # Load the DB from files
    source("predictWord.R")
    ngrams <- loadDatabase(".")
    
    hideElement(id = "loading-content", anim = TRUE, animType = "fade")
    showElement(id = "app-content")

    # Display the numbers of n-grams
    ngramInfo <- data.frame(c(nrow(ngrams$unigram)), c(nrow(ngrams$bigram)), c(nrow(ngrams$trigram)),
                            c(nrow(ngrams$fourgram)), c(nrow(ngrams$fivegram)))
    names(ngramInfo) <- c("1-grams", "2-grams", "3-grams", "4-grams", "5-grams")
    output$dbInfo <- renderTable(ngramInfo)
    

    # Build the table of suggested words
    prediction <- eventReactive(c(input$submitButton,
                                 input$queryKey
                                 ),
        
    {
        Sys.sleep(0.2)
        #message(" >>> queryKey = ", input$queryKey)
        lines <- sapply(input$userText, function(s) { 
            gsub("\U0092", "'", 
                 gsub("’", "'", 
                      gsub("-", " ", s, fixed = TRUE),  
                                     fixed = TRUE), 
                 fixed = TRUE) },
            USE.NAMES = FALSE)
        
        message(" >>> string: ", lines)
        tokenizedInput <- tokenizeInput(lines)
        res <- NA
        if (length(tokenizedInput[[1]]) > 0)
            res <- predictWordKatz(ngrams, as.character(tokenizedInput), 
                                   ignoreLastUnknownWords = TRUE)
        if (is.na(res) || is.na(res$generalResult) || nrow(res$generalResult) == 0)
            data.frame(words = c("No suggestions"))
        else
            res$generalResult[1:min(nrow(res$generalResult), 10), 
                              .(word, nGramOrder = origin, nGramCount = count, KatzProb = katz)]
    })
    
    output$suggestion <- renderTable(prediction())
})
