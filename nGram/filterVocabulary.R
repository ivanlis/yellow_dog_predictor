#library(data.table)
library(quanteda)
##TODO: path to script???
source("../nGram/loadFrequencies.R")


filterVocabulary <- function(matrixDirectory = "../results/dfm",
                 resultsDirectory = "../results",
                 ngramsToFilter = c(2, 3, 4),
                 vocabLimit = 0.95,
                 update = FALSE)
{
    freqs1 <- NA
    vocabPathname <-sprintf("%s/vocab1.dat", matrixDirectory)
    if (!file.exists(vocabPathname) || update)
    {
        # load/compute word frequencies
        message("Loading frequencies for 1-grams...")
        freqs1 <- loadFrequencies(sprintf("%s/generalDfm1.dat", matrixDirectory), 
                                  sprintf("%s/freq1.dat", matrixDirectory))
        message("Frequencies loaded: ", length(freqs1), " features.")
        # filter them
        threshold <- as.integer(vocabLimit * sum(freqs1))
        sufficientWords <- sum(cumsum(freqs1) < threshold) + 1
        message("threshold = ", threshold, " sufficientWords = ", sufficientWords, ".")
        freqs1 <- freqs1[1:sufficientWords]
        message("Left ", sufficientWords, " words. Saving to ", vocabPathname, "...")
        freqFile <- file(vocabPathname, "w")
        serialize(freqs1, freqFile)
        close(freqFile)
        message("Saved.")
    }
    else
    {
        message("Loading filtered frequencies for 1-grams from ", vocabPathname, "...")
        freqFile <- file(vocabPathname, "r")
        freqs1 <- unserialize(freqFile)
        close(freqFile)
        message("Loaded. Number of features: ", length(freqs1), ".")
    }
    
    ## Store the selected vocabulary in a data table.
    #freq1 <- data.table(word = names(freq1[1:K1]), probability = freq1[1:K1])
    #setkey(freq1, word)
    
    message(length(ngramsToFilter))
    
    for (ngramType in ngramsToFilter)
    {
        files <- list.files(path = resultsDirectory, 
                            pattern = sprintf("dfm%d_*", ngramType), 
                            full.names = TRUE, 
                            recursive = FALSE)
        for (f in files)
        {
            dfmFile <- file(f, "r")
            message("Processing file ", f, "...")
            dfMatrix <- unserialize(dfmFile)
            close(dfmFile)
            
            dfMatrix <- dfm_group(dfMatrix, groups = rep(1, nrow(dfMatrix)))
            # sort the features by frequency
            dfMatrix <- dfm_sort(dfMatrix);
            
            message("Before: ", nfeat(dfMatrix), ".")
            #TODO: split n-grams
            dfMatrix <- dfMatrix[, featnames(dfMatrix) %in% names(freqs1)]
            message("After: ", nfeat(dfMatrix))
            
            #freqs <- colSums(partMatr)            
            
            outVocabPathname <- sprintf("%s/voc_%s", resultsDirectory, f)
            message("Saving filtered DFM to " , outVocabPathname, "...")
            dfmFile <- file(outVocabPathname, "w")
            serialize(dfMatrix, dfmFile)
            close(dfmFile)
            message("Stored.")
        }
    }
}


