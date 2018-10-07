library(data.table)
library(quanteda)

bigramsToSuggest = 10
trigramsToSuggest = 50
fourgramsToSuggest = 50

maxOrder = 5
unknownProb = 0.015

# to fit in memory, limit the number of candidates extracted
# (alpha may be approximate)
maxCandidates1 = 100
maxCandidates2 = 100
maxCandidates3 = 100
maxCandidates4 = 100
maxCandidates5 = 100
maxCandidates6 = 100


computeDiscountFunc <- function(ngramType, tableNgram, countsToDiscount, 
                                ngramPath = "", update = FALSE)
{
    if (!update)
    {
        discountPathname <- sprintf("%s/discount%d.csv", ngramPath, ngramType)
        if (file.exists(discountPathname))
        {
            discount <- fread(discountPathname)
            setindex(discount, r)
            return(discount)
        }
    }
    
    counts <- (tableNgram[,.(featWithCount = .N), by = .(probability)])[order(probability), 
                                                                        .(r = probability, featWithCount, discount = 1)]
    
   # message(names(counts))
    
    k <- max(countsToDiscount)
    #message("k = ", k)
    
    if (k < counts[, min(r)])
    {
        #message("Returning constant 1 discount function...")
        return(counts[, .(r, discount)])
    }
        
    n1 <- (counts[r == 1, featWithCount])[1]
    nkplus1 <- (counts[r == k + 1, featWithCount])[1]
    message("n1 = ", n1, " nkplus1 = ", nkplus1)
    
    for (i in countsToDiscount)
    {
        nr <- (counts[r == i, featWithCount])[1]
        if (is.na(nr))
            nr = 0
        nrplus1 <- (counts[r == i + 1, featWithCount])[1]
        if (is.na(nr))
            nr = 0
        
        discount1 <- ((i + 1) * nrplus1) / (i * nr)
        discountk <- (discount1 - (k + 1) * nkplus1 / n1) / (1 - (k + 1) * nkplus1 / n1) 
        
        message("For i = ", i, ": nr = ", nr, ", nrplus1 = ", nrplus1, ", discount1 = ", discount1, " discountk = ", discountk)
        
        counts[r == i, discount := discountk]
    }
    
    #counts <- merge(counts, data.table(countsToDiscount = countsToDiscount), 
    #                by.x = "count", by.y = "countsToDiscount")[, .(count, discount)]
    
    res <- counts[, .(r, discount)]
    setindex(res, r)
    
    return(res)
}

assignUnknownWordProbability <- function(database, prob = 0.0)
{
    database$info$probUnknownWord <- prob
    
    # modify discount for unigrams
    beta <- prob / (1 - prob)
    #discountVec <- rep(as.double(1 / (1 + beta)), nrow(database$unigramDiscount))
    database$unigramDiscount[, discount := rep(as.double(1 / (1 + beta)), nrow(database$unigramDiscount))]
    database$unigramDiscount <- rbind(database$unigramDiscount,
                                      data.table(r = database$info$endOfSentenceCount, discount = 1 / (1 + beta)))
    setindex(database$unigramDiscount, r)
    
    database$info$unigramDiscountValue <- 1 / (1 + beta)
    
    return(database)
}

computeCounts <- function(database)
{
    res <- vector("integer", database$info$maxOrder)
    res[1] <- database$unigram[, sum(probability)]
    res[2] <- database$bigram[, sum(probability)]
    res[3] <- database$trigram[, sum(probability)]
    res[4] <- database$fourgram[, sum(probability)]
    res[5] <- database$fivegram[, sum(probability)]
    
    return(res)
}

computeEosCounts <- function(database)
{
    if (database$info$maxOrder < 2)
        return(0)
    res <- vector("integer", database$info$maxOrder)
    for (i in 2:database$info$maxOrder)
        res[i] <- database$info$totalCounts[i - 1] - database$info$totalCounts[i]
    res[1] <- res[2]
    
    return(res)
}

setIdKeys <- function(database)
{
    setindex(database$unigram, word);
    setindex(database$unigram, id);
    setindex(database$unigram, probability);
    
    setindex(database$bigram, id1)
    setindex(database$bigram, id2)
    setindex(database$bigram, id1, id2)
    setindex(database$bigram, probability)
    
    setindex(database$trigram, id1)
    setindex(database$trigram, id2)
    setindex(database$trigram, id3)
    setindex(database$trigram, id1, id2, id3)
    setindex(database$trigram, probability)
    
    setindex(database$fourgram, id1)
    setindex(database$fourgram, id2)
    setindex(database$fourgram, id3)
    setindex(database$fourgram, id4)
    setindex(database$fourgram, id1, id2, id3, id4)
    setindex(database$fourgram, probability)
    
    setindex(database$fivegram, id1)
    setindex(database$fivegram, id2)
    setindex(database$fivegram, id3)
    setindex(database$fivegram, id4)
    setindex(database$fivegram, id5)
    setindex(database$fivegram, id1, id2, id3, id4, id5)
    setindex(database$fivegram, probability)
    
    #setkey(database$unigram, id)
    #setindex(database$unigram, probability)
    #setkey(database$bigram, id1, id2)
    #setindex(database$bigram, probability)
    #setkey(database$trigram, id1, id2, id3)
    #setindex(database$trigram, probability)
    #setkey(database$fourgram, id1, id2, id3, id4)
    #setindex(database$fourgram, probability)
    
    return(database)
}

loadDatabase <- function(ngramPath = "../results/tables")
{
    # Load tables from CSV files
    #tableWord <- fread(sprintf("%s/tableWord.csv", ngramPath))
    tableWord <- fread(sprintf("%s/tabvocab1.csv", ngramPath))
    #setkey(tableWord, word)
    #tableBigram <- fread(sprintf("%s/tableBigram.csv", ngramPath))
    tableBigram <- fread(sprintf("%s/table2gramVoc.csv", ngramPath))
    #setkey(tableBigram, id1)
    #tableTrigram <- fread(sprintf("%s/tableTrigram.csv", ngramPath))
    tableTrigram <- fread(sprintf("%s/table3gramVoc.csv", ngramPath))
    #setkey(tableTrigram, id1, id2)
    #tableFourgram <- fread(sprintf("%s/tableFourgram.csv", ngramPath))
    tableFourgram <- fread(sprintf("%s/table4gramVoc.csv", ngramPath))
    #setkey(tableFourgram, id1, id2, id3)
    tableFivegram <- fread(sprintf("%s/table5gramVoc.csv", ngramPath))
    
    
    # Compute some useful values
    #endOfSentenceCount <- tableWord[, sum(probability)] - tableBigram[, sum(probability)]
    endOfSentenceCount <- tableWord[, sum(probability - totalHigher)]
    totalUnigramCount <- tableWord[, sum(probability)]
    
    
    res <- list(unigram = tableWord, bigram = tableBigram, 
         trigram = tableTrigram, fourgram = tableFourgram, fivegram = tableFivegram,
         
         unigramDiscount = computeDiscountFunc(1, tableWord, 1:5, ngramPath = ngramPath),
         bigramDiscount = computeDiscountFunc(2, tableBigram, 1:5, ngramPath = ngramPath),
         trigramDiscount = computeDiscountFunc(3, tableTrigram, 1:5, ngramPath = ngramPath),
         fourgramDiscount = computeDiscountFunc(4, tableFourgram, 1:5, ngramPath = ngramPath),
         fivegramDiscount = computeDiscountFunc(5, tableFivegram, 1:5, ngramPath = ngramPath),
         
         info = list(maxOrder = maxOrder, 
                     endOfSentenceCount = endOfSentenceCount,
                     totalUnigramCount = totalUnigramCount,
                     probUnknownWord = 0.0))
    
    #res$info$totalCounts <- computeCounts(res)
    #res$info$eosCounts <- computeEosCounts(res)
    
    res <- assignUnknownWordProbability(res, prob = unknownProb)
    res <- setIdKeys(res)
    
    return(res)
}


getNgramTable <- function(database, n)
{
    if (n == 1)
        return(database$unigram)
    if (n == 2)
        return(database$bigram)
    if (n == 3)
        return(database$trigram)
    if (n == 4)
        return(database$fourgram)
    if (n == 5)
        return(database$fivegram)
    
    return(NA)
}

getDiscountTable <- function(database, n)
{
    if (n == 1)
        return(database$unigramDiscount)
    if (n == 2)
        return(database$bigramDiscount)
    if (n == 3)
        return(database$trigramDiscount)
    if (n == 4)
        return(database$fourgramDiscount)
    if (n == 5)
        return(database$fivegramDiscount)
    
    return(NA)    
}

getIds <- function(dbTable, n, i)
{
    if (n == 1)
        return(c(dbTable[i, id]))
    else if (n == 2)
        return(c(dbTable[i, id1], dbTable[i, id2]))
    else if (n == 3)
        return(c(dbTable[i, id1], dbTable[i, id2], dbTable[i, id3]))
    else if (n == 4)
        return(c(dbTable[i, id1], dbTable[i, id2], dbTable[i, id3], dbTable[i, id4]))
    else if (n == 5)
        return(c(dbTable[i, id1], dbTable[i, id2], dbTable[i, id3], dbTable[i, id4], dbTable[i, id5]))
    else
        return(integer(0))
}



predictWord <- function(database, string)
{
    words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 3)
    
    
    bigramResult <- character(0)
    trigramResult <- character(0)
    fourgramResult <- character(0)
    
    if (length(words) >= 1)
    {
        searchWord1 <- words[length(words)]
        
        bigrams <- merge(
            ((merge(database$unigram[word == searchWord1, .(id, word)], 
                         database$bigram, by.x = "id", by.y = "id1")
                    [,.(id2, probability)])[order(-probability)])[1:min(bigramsToSuggest, .N)],
            database$unigram[, .(id, word)], by.x = "id2", by.y = "id"
            )[, .(word, probability)]
        #bigramResult <- database$unigram[id %in% bigrams$id2, word]
        bigramResult <- bigrams[, word]
        
        if (length(words) >= 2)
        {
            searchWord2 <- words[length(words) - 1]
            
            trigrams <- merge(
                        ((merge(merge(database$unigram[word == searchWord2, .(id, word)], 
                                      database$trigram, by.x = "id", by.y = "id1")
                                [, .(id2, id3, probability)],
                                
                                database$unigram[word == searchWord1, .(id, word)],
                                by.x = "id2", by.y = "id")
                          [,.(id3, probability)])[order(-probability)])[1:min(trigramsToSuggest, .N)],
                        
                        database$unigram[, .(id, word)], by.x = "id3", by.y = "id"
                        )[, .(word, probability)]
            #trigramResult <- database$unigram[id %in% trigrams$id3, word]
            trigramResult <- trigrams[, word]
            
            
            if (length(words) >= 3)
            {
                searchWord3 <- words[length(words) - 2]
                #cat("searchWord3", searchWord3, "\n")
                
                fourgrams <- merge(
                              ((merge(merge(merge(database$unigram[word == searchWord3, .(id, word)],
                                   database$fourgram, by.x = "id", by.y = "id1")
                              [, .(id2, id3, id4, probability)],
                              
                              database$unigram[word == searchWord2, .(id, word)],
                              by.x = "id2", by.y = "id")[, .(id3, id4, probability)],
                              database$unigram[word == searchWord1, .(id, word)],
                              by.x = "id3", by.y = "id")[, .(id4, probability)])
                            [order(-probability)])[1:min(fourgramsToSuggest, .N)],
                            
                            database$unigram[, .(id, word)], by.x = "id4", by.y = "id"
                            )[, .(word, probability)]
                              
                #fourgramResult <- database$unigram[id %in% fourgrams$id4, word]
                fourgramResult <- fourgrams[, word]
            }
        }
    }
    
    list(bigramWords = bigramResult, trigramWords = trigramResult, 
         fourgramWords = fourgramResult)
}

findProb <- function(database, string)
{
    words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 4)
    #message(words)
    
    unigramProb <- 0
    bigramProb <- 0
    trigramProb <- 0
    fourgramProb <- 0
    
    if (length(words) >= 1)
    {
        searchWord1 <- words[length(words)]
        unigramResult <- database$unigram[word == searchWord1, .(probability)]
        #message(nrow(unigramResult))
        unigramProb <- if (nrow(unigramResult) > 0)
            unigramResult[1, probability]
        else
            0
            
        
        
        if (length(words) >= 2)
        {
            searchWord2 <- words[length(words) - 1]
            
            
            bigramResult <- merge(
                ((merge(database$unigram[word == searchWord2, .(id, word)], 
                        database$bigram, by.x = "id", by.y = "id1")
                  [,.(id2, probability)])),
                database$unigram[word == searchWord1, .(id, word)], by.x = "id2", by.y = "id"
            )[, .(probability)]
            
            bigramProb <-
                if (nrow(bigramResult) > 0)
                    bigramResult[1, probability]
                else
                    0
                        
            
            
            if (length(words) >= 3)
            {
                searchWord3 <- words[length(words) - 2]
                
                
                trigramResult <- merge(
                    merge(
                        merge(database$unigram[word == searchWord3, .(id)], 
                            database$trigram, by.x = "id", by.y = "id1")[,.(id2, id3, probability)],
                        database$unigram[word == searchWord2, .(id)], by.x = "id2", by.y = "id")[, .(id3, probability)], 
                    database$unigram[word == searchWord1, .(id)], by.x = "id3", by.y = "id")[, .(probability)]                
                
                trigramProb <- 
                    if (nrow(trigramResult) > 0)
                        trigramResult[1, probability]
                    else
                        0
                
                
                if (length(words) >= 4)
                {

                    searchWord4 <- words[length(words) - 3]
                    
                    fourgramResult <- merge(merge(
                        merge(
                            merge(database$unigram[word == searchWord4, .(id)], 
                                  database$fourgram, by.x = "id", by.y = "id1")[,.(id2, id3, id4, probability)],
                            database$unigram[word == searchWord3, .(id)], by.x = "id2", by.y = "id")[, .(id3, id4, probability)], 
                        database$unigram[word == searchWord2, .(id)], by.x = "id3", by.y = "id")[, .(id4, probability)],
                        database$unigram[word == searchWord1, .(id)], by.x = "id4", by.y = "id")[, .(probability)]                
                    
                        fourgramProb <- 
                            if (nrow(fourgramResult) > 0)
                                fourgramResult[1, probability]
                            else
                            0                
                }
            }
        }
    }
    
    return(list(unigramProb = unigramProb, bigramProb = bigramProb, trigramProb = trigramProb, fourgramProb = fourgramProb))
}


## Helper functions
selectNgramCount <- function(database, terms)
{
    res <- 0
    
    # n-gram order
    n <- length(terms)
    
    #TODO: n == 5
    if (n > 0)
    {
        if (n == 4)
        {
            res <- merge(merge(merge(
                merge(database$fourgram, database$unigram[word == terms[1], .(id, word)],
                      by.x = "id1", by.y = "id")[, .(id2, id3, id4, probability, totalHigher)],
                database$unigram[word == terms[2], .(id, word)], 
                by.x = "id2", by.y = "id")[, .(id3, id4, probability, totalHigher)], 
                database$unigram[word == terms[3], .(id, word)], by.x = "id3", by.y = "id")[, .(id4, probability, totalHigher)],
                database$unigram[word == terms[4], .(id, word)], by.x = "id4", by.y = "id")[, .(probability, totalHigher)]
        } else if (n == 3)
        {
            res <- merge(merge(
                merge(database$trigram, database$unigram[word == terms[1], .(id, word)],
                      by.x = "id1", by.y = "id")[, .(id2, id3, probability, totalHigher)],
                database$unigram[word == terms[2], .(id, word)], 
                by.x = "id2", by.y = "id")[, .(id3, probability, totalHigher)], 
                database$unigram[word == terms[3], .(id, word)], by.x = "id3", by.y = "id")[, .(probability, totalHigher)]
        } else if (n == 2)
        {
            res <- merge(
                       merge(database$bigram, database$unigram[word == terms[1], .(id, word)], 
                                           by.x = "id1", by.y = "id")[, .(id2, probability, totalHigher)], 
                       database$unigram[word == terms[2], .(id, word)], 
                       by.x = "id2", by.y = "id")[, .(probability, totalHigher)]
            
        } else if (n == 1)
        {
            res <- database$unigram[word == terms[1], .(probability, totalHigher)]
        }
        
        if (nrow(res) > 0)
            res <- c(res[1, probability], res[1, totalHigher])
        else
            res <- c(0, 0)
    } else if (n == 0)
    {
        #uniCount <- database$unigram[, sum(probability)]
        # take into account "end of sentence"
        uniCount <- database$info$totalUnigramCount
        #res <- c(database$info$totalUnigramCount + (database$info$totalUnigramCount - database$bigram[, sum(probability)]))
        res <- c(database$info$totalUnigramCount + database$info$endOfSentenceCount, database$info$totalUnigramCount)
    }
    
    
#    if (length(res) == 0)
#        res <- 0
    
    return(res)
}

## The same as selectNgram(), but use ids immediately
selectNgramCountLight <- function(database, ids)
{
    res <- 0
    
    # n-gram order
    n <- length(ids)
    
    if (n > 0)
    {
        if (n == 5)
        {
            res <- database$fivegram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3] & id4 == ids[4] & id5 == ids[5], 
                                     .(probability)]        
        } else if (n == 4)
        {
            res <- database$fourgram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3] & id4 == ids[4], 
                                     .(probability, totalHigher)]
        } else if (n == 3)
        {
            res <- database$trigram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3], .(probability, totalHigher)]
        } else if (n == 2)
        {
            res <- database$bigram[id1 == ids[1] & id2 == ids[2], .(probability, totalHigher)]
            
        } else if (n == 1)
        {
            res <- database$unigram[id == ids[1], .(probability, totalHigher)]
        }

        if (nrow(res) > 0)
        {   
            if (ncol(res) > 1)
                res <- c(res[1, probability], res[1, totalHigher])
            else
                res <- c(res[1, probability], 0)
        }
        else
            res <- c(0, 0)                
        
    } else if (n == 0)
    {
        #uniCount <- database$unigram[, sum(probability)]
        # take into account "end of sentence"
        #res <- uniCount + (uniCount - database$bigram[, sum(probability)])
        res <- c(database$info$totalUnigramCount + database$info$endOfSentenceCount, database$info$totalUnigramCount)
    }
    
    
#    if (length(res) == 0)
#        res <- 0
    
    return(res)
}


selectCandidates <- function(database, terms, preNgramCount = -1)
{
    res <- NA
    n <- length(terms) + 1

    if (preNgramCount < 0)
    {
        if (n > 2)
            preNgramCount <- selectNgramCount(database, terms[1:(length(terms) - 1)])
        else
            preNgramCount <- selectNgramCount(database, character(0))
    }
    #message("preNgramCount = ", preNgramCount)
    
    if (n == 5)
    {
        discountTable <- database$fivegramDiscount
        
        # by count to get the discount
        res <- merge(
            # by id to get the word string
            merge(
                # by fourth word id
                merge(
                    # by third word id
                    merge(
                        # by second word id
                        merge(
                            # by first word id
                            merge(database$unigram[word == terms[1], .(id, word)],
                                  database$fivegram, by.x = "id", by.y = "id1")[, .(id2, id3, id4, id5, probability)],
                            
                            database$unigram[word == terms[2], .(id, word)],
                            by.x = "id2", by.y = "id")[, .(id3, id4, id5, probability)],
                        
                        database$unigram[word == terms[3], .(id, word)],
                        by.x = "id3", by.y = "id")[, .(id4, id5, probability)],
                    database$unigram[word == terms[4], .(id, word)],
                    by.x = "id4", by.y = "id")[, .(id = id5, count = probability, 
                                                   condprob1 = probability / preNgramCount)],
                
                database$unigram[, .(wid = id, word)], 
                by.x = "id", by.y = "wid")[, .(id, word, count, condprob1)], discountTable, 
            by.x = "count", by.y = "r")[order(-count), 
                                        .(id, word, count, condprob = condprob1 * discount)][min(.N, 1):min(.N, maxCandidates5)]
    } else if (n == 4)
    {
        discountTable <- database$fourgramDiscount
        
        # by count to get the discount
        res <- merge(
                   # by id to get the word string
                   merge(
                       # by third word id
                        merge(
                            # by second word id
                            merge(
                                # by first word id
                                merge(database$unigram[word == terms[1], .(id, word)],
                              database$fourgram, by.x = "id", by.y = "id1")[, .(id2, id3, id4, probability)],
                        
                                database$unigram[word == terms[2], .(id, word)],
                                by.x = "id2", by.y = "id")[, .(id3, id4, probability)],
                        
                        database$unigram[word == terms[3], .(id, word)],
                        by.x = "id3", by.y = "id")[, .(id = id4, count = probability, 
                                                 condprob1 = probability / preNgramCount)],
            
                    database$unigram[, .(wid = id, word)], 
                    by.x = "id", by.y = "wid")[, .(id, word, count, condprob1)], discountTable, 
                by.x = "count", by.y = "r")[order(-count), 
                                        .(id, word, count, condprob = condprob1 * discount)][min(.N, 1):min(.N, maxCandidates4)]
    } else if (n == 3)
    {
        discountTable <- database$trigramDiscount
        # by count to get the discount
        res <- merge(
            # by id to get the word string
            merge(
                # by second word id
                merge(
                    # by first word id
                    merge(
                        database$trigram, database$unigram[word == terms[1], .(id, word)],
                        by.x = "id1", by.y = "id")[, .(id2, id3, probability)],
                    
                    database$unigram[word == terms[2], .(id, word)],
                    by.x = "id2", by.y = "id")[, .(id = id3, count = probability, 
                                                   condprob1 = probability / preNgramCount)],
                
                database$unigram[, .(wid = id, word)], 
                by.x = "id", by.y = "wid")[, .(id, word, count, condprob1)], discountTable, 
            by.x = "count", by.y = "r")[order(-count), .(id, word, count, 
                                                    condprob = condprob1 * discount)][min(.N, 1):min(.N, maxCandidates3)]        
        
    } else if (n == 2)
    {
        discountTable <- database$bigramDiscount
        # by count to get the discount
        res <- merge(
            # by id to get the word string
            merge(
                # by first word id
                merge(
                    database$bigram,
                    database$unigram[word == terms[1], .(id, word)],
                    by.x = "id1", by.y = "id")[, .(id = id2, count = probability, 
                                                   condprob1 = probability / preNgramCount)],
                
                database$unigram[, .(wid = id, word)], 
                by.x = "id", by.y = "wid")[, .(id, word, count, condprob1)], discountTable, 
            by.x = "count", by.y = "r")[order(-count), .(id, word, count, 
                                                    condprob = condprob1 * discount)][min(.N, 1):min(.N, maxCandidates2)]        
    }
    
    return(res)
}

## The same as selectCandidates(), but without extracting strings
selectCandidatesLight <- function(database, ids, preNgramCount = -1)
{
    #TODO: use ids instead of terms!!!
    res <- NA
    n <- length(ids) + 1
    
    if (preNgramCount < 0)
    {
        if (n > 2)
            preNgramCount <- selectNgramCountLight(database, ids[1:(length(ids) - 1)])
        else
            preNgramCount <- selectNgramCountLight(database, character(0))
    }
    #message("preNgramCount = ", preNgramCount)
    
    if (n == 5)
    {
        discountTable <- database$fivegramDiscount
        
        # by count to get the discount
        res <- merge(database$fivegram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3] & id4 == ids[4], 
                                       .(id = id5, count = probability, 
                                         condprob1 = probability / preNgramCount)], 
                     discountTable, 
                     by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]        
    } else if (n == 4)
    {
        discountTable <- database$fourgramDiscount
        
        # by count to get the discount
        res <- merge(database$fourgram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3], 
                                       .(id = id4, count = probability, 
                                         condprob1 = probability / preNgramCount)], 
                     discountTable, 
            by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]
    } else if (n == 3)
    {
        discountTable <- database$trigramDiscount
        # by count to get the discount
        res <- merge(database$trigram[id1 == ids[1] & id2 == ids[2],
                     .(id = id3, count = probability, condprob1 = probability / preNgramCount)],
                    discountTable, 
            by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]        
        
    } else if (n == 2)
    {
        discountTable <- database$bigramDiscount
        # by count to get the discount
        res <- merge(database$bigram[id1 == ids[1], 
                            .(id = id2, count = probability, condprob1 = probability / preNgramCount)], 
                     discountTable, 
            by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]        
    }
    
    return(res)
}


computeEosCondProb <- function(database, n, preNgramCount, preTotalHigher)
{
    if (preNgramCount == 0)
        return(0)
    
    discountTable <- getDiscountTable(database, n)
    
    eosNgrams <- preNgramCount - preTotalHigher
    eosDiscount <- discountTable[r == eosNgrams, discount]
    if (length(eosDiscount) == 0)
        eosDiscount <- 1
    #message("eosNgrams: ", eosNgrams, " eosDiscount: ", eosDiscount)
    return(eosNgrams * eosDiscount / preNgramCount)    
}

computeEosCondProb1 <- function(database, n, candidates, preNgramCount)
{
    if (preNgramCount == 0)
        return(0)
    
    discountTable <-
        if (n == 1)
            database$unigramDiscount
        else if (n == 2)
            database$bigramDiscount
        else if (n == 3)
            database$trigramDiscount
        else if (n == 4)
            database$fourgramDiscount
        else if (n == 5)
            database$fivegramDiscount
        
    #TODO: change
    eosNgrams <- preNgramCount - candidates[, sum(count)]
    eosDiscount <- discountTable[r == eosNgrams, discount]
    if (length(eosDiscount) == 0)
        eosDiscount <- 1
    #message("eosNgrams: ", eosNgrams, " eosDiscount: ", eosDiscount)
    return(eosNgrams * eosDiscount / preNgramCount)    
}

computeAlpha <- function(database, candidatesN, candidatesNminus1, eosCondProbN, eosCondProbNminus1)
{
    sumPredictedNN <- candidatesN[, sum(condprob)] + eosCondProbN
    #sumPredictedNNminus1 <- merge(candidatesN[, .(id)], candidatesNminus1[, .(id, condprob)],
    #                        by.x = "id", by.y = "id")[, sum(condprob)] + eosCondProbNminus1
    sumPredictedNNminus1 <- eosCondProbNminus1
    if (nrow(candidatesN) > 0)
        sumPredictedNNminus1 <- sumPredictedNNminus1 + candidatesNminus1[id %in% candidatesN$id, sum(condprob)] 
    return((1 - sumPredictedNN) / (1 - sumPredictedNNminus1))
}


predictWordKatz <- function(database, words)
{
    #words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 4)
    
    #maxOrder <- 4
    
    ids <- rep(-1, length(words))
    for (i in 1:length(ids))
    {
        currentId <- database$unigram[word == words[i], id]
        if (length(currentId) > 0)
            ids[i] <- currentId
    }
    
    results <- vector(mode = "list", length = maxOrder)
    
    eosUnigrams <- database$info$endOfSentenceCount
    results[[1]] <- 
        list(candidates = database$unigram[, .(id, condprob = database$info$unigramDiscountValue * 
                                                   probability / (sum(probability) + eosUnigrams))],
             eosNgramCondprob = database$info$unigramDiscountValue * 
                 eosUnigrams / (database$unigram[, sum(probability)] + eosUnigrams),
             alpha = 1)
    
    #TODO: unnecessary?
    #database$unigram[, .(id, condprob = probability / (sum(probability) + eosUnigrams))]
    
    stopProcessing <- FALSE
    
    for (n in 2:min(maxOrder, length(words) + 1))
    {
        message("Processing n = ", n, "...")
        #terms <- words[(length(words) - n + 2):length(words)]
        termIds <- ids[(length(words) - n + 2):length(words)]
        message("Terms: ", length(terms), ".")
        
        results[[n]] <- list(candidates = NA, eosNgramCondprob = 0, alpha = 1.0)
        if (stopProcessing || termIds[length(termIds) - n + 2] < 0)
        {
            stopProcessing <- TRUE
            next
        }
        
        #preNgramCount <- selectNgramCount(database, terms)
        preNgramCount <- selectNgramCountLight(database, termIds)
        message("preNgramCount = ", preNgramCount[1])
        #if (preNgramCount == 0)
        #    break
        #candidatesN <- selectCandidates(database, terms, preNgramCount[1])
        candidatesN <- selectCandidatesLight(database, termIds, preNgramCount[1])
        message("candidatesN: ", nrow(candidatesN))
        #if (nrow(candidatesN) == 0)
        #    break
        eosNgramCondprob <- computeEosCondProb(database, n, preNgramCount[1], preNgramCount[2])
        message("eosNgramCondprob = ", eosNgramCondprob)
        alpha <- 1.0
        if (nrow(candidatesN) > 0)
        {
            alpha <- computeAlpha(database, candidatesN, results[[n - 1]]$candidates, 
                         eosNgramCondprob, results[[n - 1]]$eosNgramCondprob)            
            results[[n]] <- list(candidates = candidatesN, eosNgramCondprob = eosNgramCondprob, alpha = 1.0)
        }
        else
        {
            results[[n]] <- list(candidates = NA, eosNgramCondprob = eosNgramCondprob, alpha = 1.0)
            stopProcessing = TRUE;
        }

        for (i in 1:(n - 1))
            results[[i]]$alpha <- results[[i]]$alpha * alpha
    }
    
    generalResult <- NA
    for (n in 2:maxOrder)
    {
        if (is.data.table(results[[n]]$candidates))
        {
#            reformatted <- results[[n]]$candidates[, 
#                                                   .(id, word, count, 
#                                                     katz = condprob * results[[n]]$alpha, 
#                                                     origin = n)]
            
            reformatted <- merge(results[[n]]$candidates, database$unigram, 
                                 by.x = "id", by.y = "id")[, .(id, word, count, katz = condprob * results[[n]]$alpha, origin = n)]
            
            if (is.data.table(generalResult))
                generalResult <- rbind(generalResult, reformatted)
            else
                generalResult <- reformatted
        }
        else
            break
    }

    # Group the words that come from different order n-grams.
    # Keep the highest order only (the definition of the Katz backoff).
    if (is.data.table(generalResult))
        generalResult <- generalResult[order(-origin), .SD[1], by = .(id)][order(-katz)]
    
    list(#results = results,
         generalResult = (if (is.data.table(generalResult)) { generalResult } else {NA}))    
}

## Compute alpha and store it as and additional column
precomputeAlpha <- function(database, ngramPath = "../results/tables")
{
    database <- setIdKeys(database)
    
    #maxOrder <- 4
    
    for (n in 1:maxOrder)
    {
        message("Processing n = ", n, "...")
        currentTable <- getNgramTable(database, n)
        currentTable[, alpha := 1]
        currentTable[, condprob := 0]
        discountTable <- getDiscountTable(database, n)
        if (n == maxOrder)
            break;
        
        for (i in 1:nrow(currentTable))
        {
            if (i %% 100 == 0)
                message("Processing row ", i, "...")
            
            ids <- getIds(currentTable, n, i)
            
            preNminus1count <- 0
            
            candidatesN <-
                if (n == 1)
                {
                    database$unigram[, .(id, condprob = database$info$unigramDiscountValue * 
                                             probability / (sum(probability) + database$info$endOfSentenceCount))]
                } else
                {
                    # last n - 1 ids
                    idsLower <- tail(ids, n - 1)
                    preNminus1count <- selectNgramCountLight(database, idsLower)
                    selectCandidatesLight(database, idsLower, preNminus1count[1])
                }
            
            #setindex(candidatesN, id)
            
            preNgramCount <- currentTable[i, probability]
            candidatesNplus1 <- selectCandidatesLight(database, ids, preNgramCount[1])
            
            
            eosNgramCondprob <- 
                if (n == 1)
                    database$info$unigramDiscountValue * database$info$endOfSentenceCount / 
                        (database$info$totalUnigramCount + database$info$endOfSentenceCount)
                else
                    computeEosCondProb(database, n, preNminus1count[1], preNminus1count[2])
            
            eosNplus1Condprob <- computeEosCondProb(database, n + 1, preNgramCount[1], preNgramCount[2])
            
            al <- computeAlpha(database, candidatesNplus1, candidatesN, 
                                  eosNplus1Condprob, eosNgramCondprob)
            
            discount <- discountTable[r == currentTable[i, probability], discount]
            #discount <- 1
            if (length(discount) == 0)
                discount <- 1
            
            lowerNgramCount <- 
                if (n == 1)
                    database$unigram[, sum(probability)] + database$info$endOfSentenceCount
                else
                    selectNgramCountLight(database, ids[1:(n - 1)])
            
            #currentTable[i, c("alpha", "condprob") := c(al, probability * discount / lowerNgramCount)]
            currentTable[i, alpha := al]
            currentTable[i, condprob := probability * discount / lowerNgramCount]
        } # end loop by i
        
        tablePathname <- sprintf("%s/tabVocAlpha%d.csv", ngramPath, n)
        write.csv(currentTable, tablePathname, row.names = FALSE)
    }
}

# maxPossibleOrder: to limit the search. For example, if in the 4 previous words only
# the bigram formed by the last two words existed (neither the 3- nor 4-gram),
# this time the maximum order is 3
computeKatzProbability <- function(database, words, maxPossibleOrder = maxOrder)
{
    results <- vector(mode = "list", length = maxOrder)
    
    maxOrderByNumWords <- length(words)
    maxOrderHere = min(c(maxPossibleOrder, maxOrder, maxOrderByNumWords))
    
    if (maxOrderHere == 0)
        return(0.0)
    
    # the words we are interested in
    words <- tail(words, maxOrderHere)
    # find their ids
    ids = vector("integer", maxOrderHere)
    lastNotFound <- 0
    for (i in maxOrderHere:1)
    {
        id <- -1
        if (i < maxOrderHere || words[i] != ";;;EOS;;;")
            id <- database$unigram[word == words[i], id]
        if (length(id) == 0)
        {
            ids[i] <- NA
            if (i != maxOrderHere)
                lastNotFound <- i
        }
        else
            ids[i] <- id
    }

    if (lastNotFound > 0)
    {
        maxOrderHere <- maxOrderHere - lastNotFound
        ids <- tail(ids, maxOrderHere)
    }
       
    alpha = 1.0
    eosNplus1Condprob <- 0
    
    
    message("maxOrderHere = ", maxOrderHere)
    
    for (ord in maxOrderHere:1)
    {
        message("Processing ord = ", ord, "...")
        currentIds <- tail(ids, ord)
        count <-
            if (!is.na(ids[length(ids)]) && ids[length(ids)] >= 0)
                selectNgramCountLight(database, currentIds)[1]
            else
                -1
        
        totalCount <- 
            if (ord > 1)
                selectNgramCountLight(database, head(currentIds, ord - 1))[1]
            else
                (database$info$totalUnigramCount + database$info$endOfSentenceCount)
        
        
        discount <-
            if (count > 0)
                getDiscountTable(database, ord)[r == count, discount]
            else
                1.0
        
        
        # compute alpha, multiply it by the accumulated alpha value
        #      if it is the highes possible order, do not do anything
        if (ord < maxOrderHere)
        {
            preNminus1count <- 0
             
            # candidates guessed at order ord given the (ord - 1) last terms 
            candidatesN <-
                if (ord == 1)
                {
                    database$unigram[, .(id, condprob = database$info$unigramDiscountValue * 
                                             probability / (sum(probability) + database$info$endOfSentenceCount))]
                } else
                {
                    # last n - 1 ids
                    #idsLower <- tail(currentIds, ord - 1)
                    idsLower <- ids[(length(ids) - ord + 1):(length(ids) - 1)]
                    preNminus1count <- selectNgramCountLight(database, idsLower)
                    selectCandidatesLight(database, idsLower, preNminus1count[1])
                }
            
             #setindex(candidatesN, id)
             
            idsLowerN <- ids[(length(ids) - ord):(length(ids) - 1)]
            preNgramCount <- selectNgramCountLight(database, idsLowerN)
            
            candidatesNplus1 <- selectCandidatesLight(database, idsLowerN, preNgramCount[1])
             
            eosNgramCondprob <- 
                if (ord == 1)
                    database$info$endOfSentenceCount / 
                        (database$info$totalUnigramCount + database$info$endOfSentenceCount)
                else #TODO: if possible, use precomputed values for eos counts!!!
                    computeEosCondProb(database, ord, preNminus1count[1], preNminus1count[2])
            
            eosNplus1Condprob <- computeEosCondProb(database, ord + 1, preNgramCount[1], preNgramCount[2])
             
            al <- computeAlpha(database, candidatesNplus1, candidatesN, 
                                eosNplus1Condprob, eosNgramCondprob)
            
            message("for ord = ", ord, ":  al = ", al)
            if (al > 0)
                alpha <- alpha * al
        }
        
        
        if (count > 0)
            return(list(
                condprob = alpha * count * discount / totalCount,
                order = ord,
                count = count, 
                discount = discount,
                alpha = alpha))
        else if (count < 0)
        {
            if (is.na(ids[length(ids)]))
            {
                if (ord == 1) # unknown word
                    return(list(
                        condprob = database$info$probUnknownWord,
                        order = ord,
                        count = count, 
                        discount = discount,
                        alpha = alpha))
            } else if (ids[length(ids)] < 0) # end of sentence
            {
                #TODO: this case when max. ord is 1
                if (eosNplus1Condprob > 0)
                    return(list(
                        condprob = eosNplus1Condprob,
                        order = ord + 1,
                        count = count, 
                        discount = discount,
                        alpha = alpha))                
            }
        }
    }
}

tokenizeInput <- function(text, sentenceBoundary = TRUE)
{
    #TODO: transform apostrophe and hyphen
    if (sentenceBoundary)
        text <- tokens(text, what = "sentence", include_docvars = FALSE, remove_punct = TRUE)
    if (ntoken(text) == 0)
        return(text)
    return(tokens_tolower(tokens(text[[1]], what = "word", remove_punct = TRUE, include_docvars = FALSE)))
}
