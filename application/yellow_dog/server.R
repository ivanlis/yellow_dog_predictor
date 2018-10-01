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
    output$unigramInfo <- renderPrint(nrow(ngrams$unigram))
    output$bigramInfo <- renderPrint(nrow(ngrams$bigram))
    output$trigramInfo <- renderPrint(nrow(ngrams$trigram))
    output$fourgramInfo <- renderPrint(nrow(ngrams$fourgram))
    output$fivegramInfo <- renderPrint(nrow(ngrams$fivegram))
})
