library(data.table)

bigramsToSuggest = 10
trigramsToSuggest = 50
fourgramsToSuggest = 50


computeDiscountFunc <- function(ngramType, tableNgram, countsToDiscount)
{
    ##DEBUG!!!!!
    #return(function(r) { return(data.table(r = r, discount = rep(1.0, length(r)))) })
    
    
    
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
        nrplus1 <- (counts[r == i + 1, featWithCount])[1]
        
        
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


loadDatabase <- function(ngramPath = "../results/tables")
{
    # Load tables from CSV files
    #tableWord <- fread(sprintf("%s/tableWord.csv", ngramPath))
    tableWord <- fread(sprintf("%s/tabvocab1.csv", ngramPath))
    setkey(tableWord, word)
    #tableBigram <- fread(sprintf("%s/tableBigram.csv", ngramPath))
    tableBigram <- fread(sprintf("%s/table2gramVoc.csv", ngramPath))
    setkey(tableBigram, id1)
    #tableTrigram <- fread(sprintf("%s/tableTrigram.csv", ngramPath))
    tableTrigram <- fread(sprintf("%s/table3gramVoc.csv", ngramPath))
    setkey(tableTrigram, id1, id2)
    #tableFourgram <- fread(sprintf("%s/tableFourgram.csv", ngramPath))
    tableFourgram <- fread(sprintf("%s/table4gramVoc.csv", ngramPath))
    setkey(tableFourgram, id1, id2, id3)
    
    
    # Compute some useful values
    endOfSentenceCount <- tableWord[, sum(probability)] - tableBigram[, sum(probability)]
    
    
    
    list(unigram = tableWord, bigram = tableBigram, 
         trigram = tableTrigram, fourgram = tableFourgram,
         
         unigramDiscount = computeDiscountFunc(1, tableWord, 1:5),
         bigramDiscount = computeDiscountFunc(2, tableBigram, 1:5),
         trigramDiscount = computeDiscountFunc(3, tableTrigram, 1:5),
         fourgramDiscount = computeDiscountFunc(4, tableFourgram, 1:5),
         
         info = list(endOfSentenceCount = endOfSentenceCount))
    
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




predictWordKatz1 <- function(database, string)
{
    words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 3)
    
    
    bigramResult <- character(0)
    trigramResult <- character(0)
    fourgramResult <- character(0)
    
    bigramCandidates  <- NA
    trigramCandidates <- NA
    fourgramCandidates <- NA
    
    preUnigramCount <- 0
    preBigramCount <- 0
    preTrigramCount <- 0
    
    
    # counts of n-grams with end of sentence
    eosUnigrams <- database$info$endOfSentenceCount
    eosBigrams <- 0
    eosBigramCondprob <- 0
    sumPredicted22 <- 0
    sumPredicted21 <- 0
    alpha1 <- 1
    eosTrigrams <- 0
    eosTrigramCondprob <- 0
    sumPredicted33 <- 0
    sumPredicted32 <- 0
    alpha2 <- 1
    eosFourgrams <- 0
    eosFourgramCondprob <- 0
    sumPredicted44 <- 0
    sumPredicted43 <- 0
    alpha3 <- 1
    
    stop <- FALSE
    
    if (length(words) >= 1)
    {
        searchWord1 <- words[length(words)]
        
        preUnigramCount <- (database$unigram[word == searchWord1, probability])[1]
        message("preUnigramCount = ", preUnigramCount)
        
        
        bigramCandidates <- merge(
            
                    merge(
                    merge(database$unigram[word == searchWord1, .(id, word)], 
                    database$bigram, by.x = "id", by.y = "id1")[,.(id = id2, count = probability, 
                                                                   condprob1 = probability
                                                                   / preUnigramCount)],
                    
                    
                    database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                    )[, .(id, word, count, condprob1)], database$bigramDiscount, by.x = "count", by.y = "r"
                    )[order(-count), .(id, word, count, condprob = condprob1 * discount)]
        
        if (nrow(bigramCandidates) > 0)
        {
            eosBigrams <- preUnigramCount - bigramCandidates[, sum(count)]
            eosDiscount <- database$bigramDiscount[r == eosBigrams, discount]
            message(eosDiscount)
            if (length(eosDiscount) == 0)
                eosDiscount <- 1
            eosBigramCondprob <- eosBigrams * eosDiscount / preUnigramCount
            sumPredicted22 <- bigramCandidates[, sum(condprob)] + eosBigramCondprob
            #TODO: discount for unigrams
            sumPredicted21 <- (merge(bigramCandidates[, .(id)], 
                                     database$unigram[, .(id, condprob = probability / (sum(probability) + eosUnigrams))],
                                     by.x = "id", by.y = "id"))[, sum(condprob)]
            alpha1 <- (1 - sumPredicted22) / (1 - sumPredicted21)
            
            message("Candidates from bigrams: ", nrow(bigramCandidates))
            message("eosBigrams = ", eosBigrams, ", eosBigramCondprob = ", eosBigramCondprob)
            message("sumPredicted22 = ", sumPredicted22, ", sumPredicted21 = ", sumPredicted21, ", alpha1 = ", alpha1)
        } else
            stop <- TRUE
        
        if (length(words) >= 2 && !stop)
        {
            searchWord2 <- words[length(words) - 1]
            
            preBigramCount <- (merge(merge(database$bigram, database$unigram[word == searchWord2, .(id, word)], 
                                           by.x = "id1", by.y = "id")[, .(id2, probability)], 
                                     database$unigram[word == searchWord1, .(id, word)], by.x = "id2", by.y = "id")[, probability])[1]
            message("preBigramCount = ", preBigramCount)
            
            trigramCandidates <- merge( 
                
                        merge(
                        merge(merge(database$unigram[word == searchWord2, .(id, word)], 
                              database$trigram, by.x = "id", by.y = "id1")
                        [, .(id2, id3, probability)],
                        
                        database$unigram[word == searchWord1, .(id, word)],
                        by.x = "id2", by.y = "id")[,.(id = id3, count = probability, condprob1 = probability  
                                                      / preBigramCount)],
                        
                        
                        database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                        )[, .(id, word, count, condprob1)],
                        
                        database$trigramDiscount, by.x = "count", by.y = "r"
                        )[order(-count), .(id, word, count, condprob = condprob1 * discount)]
            
            if (nrow(trigramCandidates) > 0)
            {
                eosTrigrams <- preBigramCount - trigramCandidates[, sum(count)]
                eosDiscount <- database$trigramDiscount[r == eosTrigrams, discount]
                if (length(eosDiscount) == 0)
                    eosDiscount <- 1
                eosTrigramCondprob <- eosTrigrams * eosDiscount / preBigramCount
                sumPredicted33 <- trigramCandidates[, sum(condprob)] + eosTrigramCondprob
                sumPredicted32 <- merge(trigramCandidates[, .(id)], bigramCandidates[, .(id, condprob)])[, sum(condprob)] +
                                                                                                        eosBigramCondprob
                
                alpha2 <- (1 - sumPredicted33) / (1 - sumPredicted32)
                message("Candidates from trigrams: ", nrow(trigramCandidates))
                message("eosTrigrams = ", eosTrigrams, ", eosTrigramCondprob = ", eosTrigramCondprob)
                message("sumPredicted33 = ", sumPredicted33, ", sumPredicted32 = ", sumPredicted32, ", alpha2 = ", alpha2)
            } else
                stop <- TRUE
            
            if (length(words) >= 3 && !stop)
            {
                searchWord3 <- words[length(words) - 2]
                
                preTrigramCount <- merge(merge(
                    merge(database$trigram, database$unigram[word == searchWord3, .(id, word)],
                          by.x = "id1", by.y = "id")[, .(id2, id3, probability)],
                    database$unigram[word == searchWord2, .(id, word)], 
                    by.x = "id2", by.y = "id")[, .(id3, probability)], 
                    database$unigram[word == searchWord1, .(id, word)], by.x = "id3", by.y = "id")[, .(probability)]
                
                if (nrow(preTrigramCount) > 0)
                {
                    preTrigramCount <- preTrigramCount[1, probability]
                    
                    message("preTrigramCount = ", preTrigramCount)
                    
                    
                    fourgramCandidates <- merge( 
                                merge(                    
                                merge(merge(merge(database$unigram[word == searchWord3, .(id, word)],
                                            database$fourgram, by.x = "id", by.y = "id1")
                                      [, .(id2, id3, id4, probability)],
                                      
                                      database$unigram[word == searchWord2, .(id, word)],
                                      by.x = "id2", by.y = "id")[, .(id3, id4, probability)],
                                database$unigram[word == searchWord1, .(id, word)],
                                by.x = "id3", by.y = "id")[, .(id = id4, count = probability, 
                                                               condprob1 = probability / preTrigramCount)],
                                
                                
                                database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                                )[, .(id, word, count, condprob1)], database$fourgramDiscount, 
                                by.x = "count", by.y = "r")[order(-count), .(id, word, count, condprob = condprob1 * discount)]
                    
                    
                    if (nrow(fourgramCandidates) > 0)
                    {
                        eosFourgrams <- preTrigramCount - fourgramCandidates[, sum(count)]
                        eosDiscount <- database$fourgramDiscount[r == eosFourgrams,discount]
                        if (length(eosDiscount) == 0)
                            eosDiscount <- 1
                        eosFourgramCondprob <- eosFourgrams * eosDiscount / preTrigramCount
                        sumPredicted44 <- fourgramCandidates[, sum(condprob)] + eosFourgramCondprob
                        sumPredicted43 <- merge(fourgramCandidates[, .(id)], trigramCandidates[, .(id, condprob)],
                                                by.x = "id", by.y = "id")[, sum(condprob)] + eosTrigramCondprob
                        alpha3 <- (1 - sumPredicted44) / (1 - sumPredicted43)
                        message("Candidates from fourgrams: ", nrow(fourgramCandidates))
                        message("eosFourgrams = ", eosFourgrams, ", eosFourgramCondprob = ", eosFourgramCondprob)
                        message("sumPredicted44 = ", sumPredicted44, ", sumPredicted43 = ", sumPredicted43, ", alpha3 = ", alpha3)
                    } else
                        stop <- TRUE
                }
                else
                    stop <- TRUE
            }
        }
    }
    
    generalResult <- NA
    if (is.data.table(bigramCandidates))
        generalResult <- bigramCandidates[, .(id, word, count, katz = condprob * alpha3 * alpha2, origin = 2)]
    if (is.data.table(trigramCandidates))
        generalResult <- rbind(generalResult, trigramCandidates[, .(id, word, count, katz = condprob * alpha3, origin = 3)])
    if (is.data.table(fourgramCandidates))
        generalResult <- rbind(generalResult, fourgramCandidates[, .(id, word, count, katz = condprob, origin = 4)])
    
    list(bigramCandidates = bigramCandidates, trigramCandidates = trigramCandidates, fourgramCandidates = fourgramCandidates,
         generalResult = generalResult[order(-katz)])
}



## Helper functions
selectNgramCount <- function(database, terms)
{
    res <- 0
    
    # n-gram order
    n <- length(terms)
    
    if (n == 3)
    {
        res <- merge(merge(
            merge(database$trigram, database$unigram[word == terms[1], .(id, word)],
                  by.x = "id1", by.y = "id")[, .(id2, id3, probability)],
            database$unigram[word == terms[2], .(id, word)], 
            by.x = "id2", by.y = "id")[, .(id3, probability)], 
            database$unigram[word == terms[3], .(id, word)], by.x = "id3", by.y = "id")[, probability]
    } else if (n == 2)
    {
        res <- merge(
                   merge(database$bigram, database$unigram[word == terms[1], .(id, word)], 
                                       by.x = "id1", by.y = "id")[, .(id2, probability)], 
                   database$unigram[word == terms[2], .(id, word)], 
                   by.x = "id2", by.y = "id")[, probability]
        
    } else if (n == 1)
    {
        res <- database$unigram[word == terms[1], probability]
    } else if (n == 0)
    {
        uniCount <- database$unigram[, sum(probability)]
        # take into account "end of sentence"
        res <- uniCount + (uniCount - database$bigram[, sum(probability)])
    }
    
    
    if (length(res) == 0)
        res <- 0
    
    return(res)
}

## The same as selectNgram(), but use ids immediately
selectNgramCountLight <- function(database, ids)
{
    res <- 0
    
    # n-gram order
    n <- length(ids)
    
    if (n == 3)
    {
        res <- database$trigram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3], probability]
    } else if (n == 2)
    {
        res <- database$bigram[id1 == ids[1] & id2 == ids[2], probability]
        
    } else if (n == 1)
    {
        res <- database$unigram[id == ids[1], probability]
    } else if (n == 0)
    {
        uniCount <- database$unigram[, sum(probability)]
        # take into account "end of sentence"
        res <- uniCount + (uniCount - database$bigram[, sum(probability)])
    }
    
    
    if (length(res) == 0)
        res <- 0
    
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
    
    if (n == 4)
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
                by.x = "count", by.y = "r")[order(-count), .(id, word, count, condprob = condprob1 * discount)]
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
            by.x = "count", by.y = "r")[order(-count), .(id, word, count, condprob = condprob1 * discount)]        
        
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
            by.x = "count", by.y = "r")[order(-count), .(id, word, count, condprob = condprob1 * discount)]        
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
    
    if (n == 4)
    {
        discountTable <- database$fourgramDiscount
        
        # by count to get the discount
        res <- merge(database$fourgram[id1 == ids[1] & id2 == ids[2] & id3 == ids[3], 
                                       .(id = id4, count = probability, 
                                         condprob1 = probability / preNgramCount), on = c("id1", "id2", "id3")], 
                     discountTable, 
            by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]
    } else if (n == 3)
    {
        discountTable <- database$trigramDiscount
        # by count to get the discount
        res <- merge(database$trigram[id1 == ids[1] & id2 == ids[2],
                     .(id = id3, count = probability, condprob1 = probability / preNgramCount), on = c("id1", "id2")],
                    discountTable, 
            by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]        
        
    } else if (n == 2)
    {
        discountTable <- database$bigramDiscount
        # by count to get the discount
        res <- merge(database$bigram[id1 == ids[1], 
                            .(id = id2, count = probability, condprob1 = probability / preNgramCount), on = c("id1")], 
                     discountTable, 
            by.x = "count", by.y = "r")[, .(id, count, condprob = condprob1 * discount)]        
    }
    
    return(res)
}


computeEosCondProb <- function(database, n, candidates, preNgramCount)
{
    discountTable <-
        if (n == 1)
            database$unigramDiscount
        else if (n == 2)
            database$bigramDiscount
        else if (n == 3)
            database$trigramDiscount
        else if (n == 4)
            database$fourgramDiscount
        
    
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
    sumPredictedNNminus1 <- candidatesNminus1[id %in% candidatesN$id, sum(condprob)] + eosCondProbNminus1
    return((1 - sumPredictedNN) / (1 - sumPredictedNNminus1))
}


predictWordKatz <- function(database, string)
{
    words <- tail(strsplit(tolower(string), "\\s+", fixed = FALSE, perl = TRUE)[[1]], 3)
    
    maxOrder <- 4
    
    results <- vector(mode = "list", length = maxOrder)
    
    eosUnigrams <- database$info$endOfSentenceCount
    results[[1]] <- 
        list(candidates = database$unigram[, .(id, condprob = probability / (sum(probability) + 
                                                                                 eosUnigrams))],
             eosNgramCondprob = eosUnigrams / (database$unigram[, sum(probability)] + eosUnigrams),
             alpha = 1)
    
    database$unigram[, .(id, condprob = probability / (sum(probability) + eosUnigrams))]
    
    
    for (n in 2:min(maxOrder, length(words) + 1))
    {
        message("Processing n = ", n, "...")
        terms <- words[(length(words) - n + 2):length(words)]
        message("Terms: ", length(terms), ".")
        
        results[[n]] <- list(candidates = NA, eosNgramCondprob = 0, alpha = 1.0)
        
        preNgramCount <- selectNgramCount(database, terms)
        message("preNgramCount = ", preNgramCount)
        if (preNgramCount == 0)
            break
        candidatesN <- selectCandidates(database, terms, preNgramCount)
        message("candidatesN: ", nrow(candidatesN))
        if (nrow(candidatesN) == 0)
            break
        eosNgramCondprob <- computeEosCondProb(database, n, candidatesN, preNgramCount)
        message("eosNgramCondprob = ", eosNgramCondprob)
        results[[n]] <- list(candidates = candidatesN, eosNgramCondprob = eosNgramCondprob, alpha = 1.0)
        alpha <- computeAlpha(database, candidatesN, results[[n - 1]]$candidates, 
                              eosNgramCondprob, results[[n - 1]]$eosNgramCondprob)

        for (i in 1:(n - 1))
            results[[i]]$alpha <- results[[i]]$alpha * alpha
    }
    
    generalResult <- NA
    for (n in 2:maxOrder)
    {
        if (is.data.table(results[[n]]$candidates))
        {
            reformatted <- results[[n]]$candidates[, 
                                                   .(id, word, count, 
                                                     katz = condprob * results[[n - 1]]$alpha, 
                                                     origin = n)]
            
            if (is.data.table(generalResult))
                generalResult <- rbind(generalResult, reformatted)
            else
                generalResult <- reformatted
        }
        else
            break
    }

    list(results = results,
         generalResult = (if (is.data.table(generalResult)) {generalResult[order(-katz)]} else {NA}))    
}

setIdKeys <- function(database)
{
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

## Compute alpha and store it as and additional column
precomputeAlpha <- function(database, ngramPath = "../results/tables")
{
    database <- setIdKeys(database)
    
    maxOrder <- 4
    
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
        #resVec <- foreach(i = 1:nrow(currentTable), .combine = c) %dopar%
        {
            if (i %% 100 == 0)
                message("Processing row ", i, "...")
            
            ids <- getIds(currentTable, n, i)
            
            preNminus1count <- 0
            
            candidatesN <-
                if (n == 1)
                {
                    database$unigram[, .(id, condprob = probability / (sum(probability) + 
                                                                           database$info$endOfSentenceCount))]
                } else
                {
                    # last n - 1 ids
                    idsLower <- tail(ids, n - 1)
                    preNminus1count <- selectNgramCountLight(database, idsLower)
                    selectCandidatesLight(database, idsLower, preNminus1count)
                }
            
            #setindex(candidatesN, id)
            
            preNgramCount <- currentTable[i, probability]
            candidatesNplus1 <- selectCandidatesLight(database, ids, preNgramCount)
            
            
            eosNgramCondprob <- 
                if (n == 1)
                    database$info$endOfSentenceCount / 
                        (database$unigram[, sum(probability)] + database$info$endOfSentenceCount)
                else
                    computeEosCondProb(database, n, candidatesN, preNminus1count)
            
            eosNplus1Condprob <- computeEosCondProb(database, n + 1, candidatesNplus1, preNgramCount)
            
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
        
        stopImplicitCluster()
        
        tablePathname <- sprintf("%s/tabVocAlpha%d.csv", ngramPath, n)
        write.csv(currentTable, tablePathname, row.names = FALSE)
    }
}

