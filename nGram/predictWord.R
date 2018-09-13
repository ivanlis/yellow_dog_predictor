library(data.table)

bigramsToSuggest = 10
trigramsToSuggest = 50
fourgramsToSuggest = 50


computeDiscountFunc <- function(ngramType, tableNgram, countsToDiscount)
{
    ##DEBUG!!!!!
    #return(function(r) { return(data.table(r = r, discount = rep(1.0, length(r)))) })
    
    
    
    counts <- (tableNgram[,.(featWithCount = .N), by = .(probability)])[order(probability), 
                                                                        .(count = probability, featWithCount, discount = 1)]
    
   # message(names(counts))
    
    k <- max(countsToDiscount)
    #message("k = ", k)
    
    if (k < counts[, min(count)])
    {
        #message("Returning constant 1 discount function...")
        return(function(r) { return(data.table(r = r, discount = rep(1.0, length(r)))) })
    }
        
    n1 <- (counts[count == 1, featWithCount])[1]
    nkplus1 <- (counts[count == k + 1, featWithCount])[1]
    #message("n1 = ", n1)
    
    for (r in countsToDiscount)
    {
        nr <- (counts[count == r, featWithCount])[1]
        nrplus1 <- (counts[count == r + 1, featWithCount])[1]
        
        
        discount1 <- ((r + 1) * nrplus1) / (r * nr)
        discountk <- (discount1 - (k + 1) * nkplus1 / n1) / (1 - (k + 1) * nkplus1 / n1) 
        
        #message("For r = ", r, ": discount1 = ", discount1, " discountk = ", discountk)
        
        counts[count == r, discount := discountk]
    }
    
    counts <- merge(counts, data.table(countsToDiscount = countsToDiscount), 
                    by.x = "count", by.y = "countsToDiscount")[, .(count, discount)]
    
    return(
        function(r1) {
            mergeRes <- (merge(data.table(r = r1), counts, 
                               by.x = "r", by.y = "count", all.x = TRUE)
                         )[, .(r, discount = sapply(discount, function(el) { if (is.na(el)) {return(1)} else {return(el)} } ))]
#            )[r1]
            return(mergeRes)
        }
    )
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




predictWordKatz <- function(database, string)
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
            
            
                    merge(database$unigram[word == searchWord1, .(id, word)], 
                    database$bigram, by.x = "id", by.y = "id1")[,.(id = id2, count = probability, 
                                                                   condprob = probability * 
                                                                       database$bigramDiscount(probability)[, discount] 
                                                                   / preUnigramCount)],
                    
                    
                    database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                    )[order(-count), .(id, word, count, condprob)]
        
        if (nrow(bigramCandidates) > 0)
        {
            eosBigrams <- preUnigramCount - bigramCandidates[, sum(count)]
            eosBigramCondprob <- eosBigrams * database$bigramDiscount(eosBigrams)[,discount] / preUnigramCount
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
                
                
                        merge(merge(database$unigram[word == searchWord2, .(id, word)], 
                              database$trigram, by.x = "id", by.y = "id1")
                        [, .(id2, id3, probability)],
                        
                        database$unigram[word == searchWord1, .(id, word)],
                        by.x = "id2", by.y = "id")[,.(id = id3, count = probability, condprob = probability * 
                                                          database$trigramDiscount(probability)[, discount] 
                                                      / preBigramCount)],
                        
                        
                        database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                        )[order(-count), .(id, word, count, condprob)]
            
            if (nrow(trigramCandidates) > 0)
            {
                eosTrigrams <- preBigramCount - trigramCandidates[, sum(count)]
                eosTrigramCondprob <- eosTrigrams * database$trigramDiscount(eosTrigrams)[,discount] / preBigramCount
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
                        
                                merge(merge(merge(database$unigram[word == searchWord3, .(id, word)],
                                            database$fourgram, by.x = "id", by.y = "id1")
                                      [, .(id2, id3, id4, probability)],
                                      
                                      database$unigram[word == searchWord2, .(id, word)],
                                      by.x = "id2", by.y = "id")[, .(id3, id4, probability)],
                                database$unigram[word == searchWord1, .(id, word)],
                                by.x = "id3", by.y = "id")[, .(id = id4, count = probability, condprob = probability * 
                                                                   database$fourgramDiscount(probability)[, discount] 
                                                               / preTrigramCount)],
                                
                                
                                database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                                )[order(-count), .(id, word, count, condprob)]
                    
                    if (nrow(fourgramCandidates) > 0)
                    {
                        eosFourgrams <- preTrigramCount - fourgramCandidates[, sum(count)]
                        eosFourgramCondprob <- eosFourgrams * database$fourgramDiscount(eosFourgrams)[,discount] / preTrigramCount
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
