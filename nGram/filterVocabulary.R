library(data.table)
library(quanteda)
##TODO: path to script???
source("../nGram/loadFrequencies.R")


belongsToVocab <- function(ngram, ngramType, vocab)
{
    splitRes <- strsplit(ngram, "_")[[1]]
    if (length(splitRes) != ngramType)
        return(FALSE)
    res = TRUE
    for (term in splitRes)
    {
        #if (nchar(term) == 0 || !(term %in% vocab))
        if (nchar(term) == 0 || nrow(vocab[word == term, nomatch = 0]) == 0)
        {
            res = FALSE
            break
        }
    }
    return(res)
}


filterVocabulary <- function(matrixDirectory = "../results/dfm",
                 resultsDirectory = "../results",
                 ngramsToFilter = c(2, 3, 4),
                 vocabLimit = 0.9,
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
    
    # Store the selected vocabulary in a data table.
    freqs1 <- data.table(word = names(freqs1), probability = freqs1)
    setkey(freqs1, word)
    
    message(length(ngramsToFilter))
    
    
    for (ngramType in ngramsToFilter)
    {
        files <- list.files(path = resultsDirectory, 
                            pattern = sprintf("dfm%d_*", ngramType), 
                            full.names = TRUE, 
                            recursive = FALSE)
        cnt <- 0
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
            #belong <- sapply(featnames(dfMatrix), function(el) { belongsToVocab(el, ngramType, names(freqs1)) },
            #                 USE.NAMES = FALSE)
            belong <- sapply(featnames(dfMatrix), function(el) { belongsToVocab(el, ngramType, freqs1) },
                             USE.NAMES = FALSE)            
            
            dfMatrix <- dfMatrix[, belong]
            message("After: ", nfeat(dfMatrix))
            
            #freqs <- colSums(partMatr)            
            
            outVocabPathname <- sprintf("%s/voc%d_%02d.dat", resultsDirectory, ngramType, cnt)
            message("Saving filtered DFM to " , outVocabPathname, "...")
            dfmFile <- file(outVocabPathname, "w")
            serialize(dfMatrix, dfmFile)
            close(dfmFile)
            message("Stored.")
            
            cnt <- cnt + 1
        }
    }
}


