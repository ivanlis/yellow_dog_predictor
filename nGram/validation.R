library(quanteda)
library(readtext)
source("predictWord.R")

computePerplexityForSentence <- function(database, sentence, logBase = 2,
                                         topForAccuracy = -1)
{
    maxOrder <- database$info$maxOrder
    L <- length(sentence)
    
    #message(" >>> SENTENCE: ", sentence)
    cat(" >>> SENTENCE: ", sentence)
    
    # According to Katz, we start at word number maxOrder, first n-gram
    # of the order we defined.
    # But if the sentence is shorter, we use shorter n-grams directly.
    if (L < maxOrder)
    {
        guessed <- 0
        
        if (topForAccuracy > 0)
        {
            res <- predictWordKatz(database, sentence[min(1, L - 1):(L - 1)])
            if (!is.na(res) && !is.na(res$generalResult) && 
                nrow(res$generalResult) > 0)
            {
                guessed <- as.integer(sentence[L] %in% 
                        res$generalResult[1:min(topForAccuracy, nrow(res$generalResult)),
                                          word])
            }
        }
        
        return(
            list(logP = log(computeKatzProbability(database, sentence)$condprob, logBase),
                 cnt = 1, guessed = guessed))
    }
    
    logP = 0
    guessed = 0
    #cnt = 0
    for (i in maxOrder:L)
    {
        currentProb <-
            computeKatzProbability(database, sentence[(i - maxOrder + 1):i])$condprob
        #message(" >>> PROBABILITY:  ", currentProb)
        if (currentProb <= 0)
            error("N-gram ", sentence[(i - maxOrder + 1):i], ": prob = ", currentProb)
        logP <- logP + log(currentProb, logBase)
        
        if (topForAccuracy > 0)
        {
            res <- predictWordKatz(database, sentence[(i - maxOrder + 1):(i - 1)])
            if (!is.na(res) && !is.na(res$generalResult) && 
                nrow(res$generalResult) > 0)
            {
                guessed <- guessed + as.integer(sentence[i] %in% 
                        res$generalResult[1:min(topForAccuracy, nrow(res$generalResult)),
                                                           word])
            }
        }
    }
    
    return(
        list(
            logP = logP,
            guessed = guessed,
            cnt = L - maxOrder + 1
        )
    )
}

computePerplexityForTestDirectory <- function(database, dirPath, logBase = 2,
                                              topForAccuracy = 3)
{
    files <- list.files(path = dirPath, 
                        pattern = sprintf("*.txt"), 
                        full.names = TRUE, 
                        recursive = FALSE)
    
    cnt <- 0
    logP <- 0
    guessed <- 0
    
    for (f in files)
    {
        message(" >> Creating corpus from file ", f, "...")
        partCorpus <- corpus(readtext(f, cache = FALSE, verbosity = 3))
        message(" >> Corpus created.")
        partCorpus <- corpus_reshape(partCorpus, to = "sentences", use_docvars = FALSE)
        message(" >> Corpus reshaped.")
        partTokens <- tokens_tolower(tokens(partCorpus, 
                                        what = "word", 
                                        remove_punct = TRUE, 
                                        include_docvars = FALSE))
        for (i in 1:length(partTokens))
        {
            sentenceRes <- computePerplexityForSentence(ngrams,
                                                        as.character(partTokens[i]),
                                                        logBase, topForAccuracy)
            cnt <- cnt + sentenceRes$cnt
            logP <- logP + sentenceRes$logP
            guessed <- guessed + sentenceRes$guessed
        }
    }
    return(list(logP = logP, guessed = guessed, cnt = cnt))
}

