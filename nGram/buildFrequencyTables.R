library(data.table)
library(quanteda)

source("../nGram/loadFrequencies.R")


# split el by '_', supposing it is an n-gram; select token i
parseTerm <- function(el, n, i)
{
    splitRes <- strsplit(el, "_")[[1]]
    if (length(splitRes) != n)
        ""
    else
        splitRes[i]
}

freqVectorToTable <- function(freqs, n)
{
    res <- NA
        
    term1 <- sapply(names(freqs), function(el) { parseTerm(el, n, 1) }, USE.NAMES = FALSE)
    if (n == 1)
    {
        res <- data.table(term1 = term1, probability = freqs)
        #setkey(res, term1)
        return(res)
    }
    term2 <- sapply(names(freqs), function(el) { parseTerm(el, n, 2) }, USE.NAMES = FALSE)
    if (n == 2)
    {
        res <- data.table(term1 = term1, term2 = term2, probability = freqs)
        #setkey(res, term1, term2)
        return(res)
    }
    term3 <- sapply(names(freqs), function(el) { parseTerm(el, n, 3) }, USE.NAMES = FALSE)
    if (n == 3)
    {
        res <- data.table(term1 = term1, term2 = term2, term3 = term3, probability = freqs)
        #setkey(res, term1, term2, term3)
        return(res)
    }
    term4 <- sapply(names(freqs), function(el) { parseTerm(el, n, 4) }, USE.NAMES = FALSE)
    if (n == 4)
    {
        res <- data.table(term1 = term1, term2 = term2, term3 = term3, term4 = term4, probability = freqs)
        #setkey(res, term1, term2, term3, term4)
        return(res)
    }
    term5 <- sapply(names(freqs), function(el) { parseTerm(el, n, 5) }, USE.NAMES = FALSE)
    if (n == 5)
    {
        res <- data.table(term1 = term1, term2 = term2, term3 = term3, term4 = term4, term5 = term5, probability = freqs)
        #setkey(res, term1, term2, term3, term4)
        return(res)
    }
    
    
    return(res)
}

mergeWithVocab <- function(freqs1, currFreqs, n)
{
    freqs <- currFreqs
    
    if (n >= 2)
    {
        setkey(freqs, term1)
        freqs <- merge(freqs, freqs1[, .(id, word)], by.x = "term1", by.y = "word")
        setnames(freqs, old = c("id"), new = c("id1"))
        freqs <- freqs[,-c("term1")]
        setkey(freqs, term2)
        freqs <- merge(freqs, freqs1[, .(id, word)], by.x = "term2", by.y = "word")
        setnames(freqs, old = c("id"), new = c("id2"))
        freqs <- freqs[, -c("term2")]
    }
    
    if (n >= 3)
    {
        setkey(freqs, term3)
        freqs <- merge(freqs, freqs1[, .(id, word)], by.x = "term3", by.y = "word")
        setnames(freqs, old = c("id"), new = c("id3"))
        freqs <- freqs[, -c("term3")]
    }
    
    if (n >= 4)
    {
        setkey(freqs, term4)
        freqs <- merge(freqs, freqs1[, .(id, word)], by.x = "term4", by.y = "word")
        setnames(freqs, old = c("id"), new = c("id4"))
        freqs <- freqs[, -c("term4")]        
    }
    
    if (n >= 5)
    {
        setkey(freqs, term5)
        freqs <- merge(freqs, freqs1[, .(id, word)], by.x = "term5", by.y = "word")
        setnames(freqs, old = c("id"), new = c("id5"))
        freqs <- freqs[, -c("term5")]        
    }
    
    if (n == 2)
        setkey(freqs, id1, id2)
    else if (n == 3)
        setkey(freqs, id1, id2, id3)
    else if (n == 4)
        setkey(freqs, id1, id2, id3, id4)
    else if (n == 5)
        setkey(freqs, id1, id2, id3, id4, id5)    
    
    return(freqs)
}

groupBoundTable <- function(vocTable, ngramType)
{
    if (ngramType < 0)
        return(NA)
    
    if (ngramType == 2)
        return(vocTable[, .(probability = sum(probability)), by = .(id1, id2)])
    else if (ngramType == 3)
        return(vocTable[, .(probability = sum(probability)), by = .(id1, id2, id3)])
    else if (ngramType == 4)
        return(vocTable[, .(probability = sum(probability)), by = .(id1, id2, id3, id4)])
    else if (ngramType == 5)
        return(vocTable[, .(probability = sum(probability)), by = .(id1, id2, id3, id4, id5)])
}

buildFrequencyTables <- function(matrixDirectory = "../results/dfm",
                             resultsDirectory = "../results",
                             tablesDirectory = "../results/tables",
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
                                  sprintf("%s/freq1.dat", matrixDirectory), update)
        freqs1 <- sort(freqs1)
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
    freqs1 <- data.table(id = 1:length(freqs1), word = names(freqs1), probability = freqs1)
    setkey(freqs1, word)
    
    vocabTablePathname <- sprintf("%s/tabvocab1.csv", tablesDirectory)
    write.csv(freqs1, vocabTablePathname, row.names = FALSE)
    
    
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
            
            currentFreqs <- colSums(dfMatrix)
            message("Before: ", length(currentFreqs), ".")
            
            currentFreqs <- freqVectorToTable(currentFreqs, ngramType)
            currentFreqs <- mergeWithVocab(freqs1 = freqs1, currFreqs = currentFreqs, n = ngramType)
            
            message("After: ", nrow(currentFreqs), ".")
            
            vocabTablePathname <- sprintf("%s/tabvoc%d_%02d.csv", tablesDirectory, ngramType, cnt)
            message("Saving filtered DFM to " , vocabTablePathname, "...")
            write.csv(currentFreqs, vocabTablePathname, row.names = FALSE)
            message("Stored.")
            
            cnt <- cnt + 1
        }
    }
}


gatherVocabTables <- function(tablesDirectory = "../results/tables",
                              ngramsToFilter = c(2, 3, 4))
{
    for (ngramType in ngramsToFilter)
    {
        accumTable <- NA
        
        files <- list.files(path = tablesDirectory, 
                            pattern = sprintf("tabvoc%d_*", ngramType), 
                            full.names = TRUE, 
                            recursive = FALSE)
        
        for (f in files)
        {
            message("Reading table from ", f, "...")
            currentTable <- fread(f)
            message("Read. Columns: ", ncol(currentTable), " Lines: ", nrow(currentTable))
            
            if (!is.data.table(accumTable))
                accumTable <- currentTable
            else
            {
                message("Binding ", nrow(accumTable), " + ", nrow(currentTable), "...")
                accumTable <- rbindlist(list(accumTable, currentTable), use.names = TRUE)
                message("Bound: ", nrow(accumTable), " x ", ncol(accumTable), ".")
                accumTable <- groupBoundTable(accumTable, ngramType)
            }
            message("After gathering: ", nrow(accumTable), " x ", ncol(accumTable))
            message(names(accumTable))
        }
        
        write.csv(accumTable, sprintf("%s/table%dgramVoc.csv", tablesDirectory, ngramType), row.names = FALSE)
    }
}

filterVocabTables <- function(tableDirectory = "../resutls/tables", ngramsToFilter = 1:5, threshold = 1,
                              countsToDiscount = 1:5)
{
    #source("predictWord.R")
    
    for (ngramType in ngramsToFilter)
    {
        pathName <-
            if (ngramType == 1)
                sprintf("%s/tabvocab1.csv", tableDirectory)
            else
                sprintf("%s/table%dgramVoc.csv", tableDirectory, ngramType)
        nextPathName <- sprintf("%s/table%dgramVoc.csv", tableDirectory, ngramType + 1)
        discountPathname <- sprintf("%s/discount%d.csv", tableDirectory, ngramType)
        
        message("Reading table from ", pathName, "...")
        currentTable <- fread(pathName)
        message("Read. Columns: ", ncol(currentTable), " Rows: ", nrow(currentTable))
        
        discountTable <- computeDiscountFunc(ngramType, currentTable, countsToDiscount)
        write.csv(discountTable, discountPathname, row.names = FALSE)
        message("Discount table computed and stored.")
        
        currentTable <- currentTable[probability > threshold]
        message("After filtering. Columns: ", ncol(currentTable), " Rows: ", nrow(currentTable))
        
        if (file.exists(nextPathName))
        {
            nextTable <- fread(nextPathName)
            currentTable <-
                if (ngramType == 1)
                    merge(currentTable, nextTable[,.(totalHigher = sum(probability)), by = .(id1)], 
                          by.x = c("id"), by.y = c("id1"))                    
                else if (ngramType == 2)
                    merge(currentTable, nextTable[,.(totalHigher = sum(probability)), by = .(id1, id2)], 
                          by.x = c("id1", "id2"), by.y = c("id1", "id2"))
                else if (ngramType == 3)
                    merge(currentTable, nextTable[,.(totalHigher = sum(probability)), by = .(id1, id2, id3)], 
                          by.x = c("id1", "id2", "id3"), by.y = c("id1", "id2", "id3"))
                else
                    merge(currentTable, nextTable[,.(totalHigher = sum(probability)), by = .(id1, id2, id3, id4)], 
                          by.x = c("id1", "id2", "id3", "id4"), by.y = c("id1", "id2", "id3", "id4"))

        }
        
        write.csv(currentTable, pathName, row.names = FALSE)
    }
}
