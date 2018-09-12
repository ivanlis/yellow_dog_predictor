library(data.table)

bigramsToSuggest = 10
trigramsToSuggest = 50
fourgramsToSuggest = 50


computeDiscountFunc <- function(ngramType, tableNgram, countsToDiscount)
{
    counts <- (tableNgram[,.(featWithCount = .N), by = .(probability)])[order(probability), 
                                                                        .(count = probability, featWithCount, discount = 1)]
    
   # message(names(counts))
    
    for (r in countsToDiscount)
    {
        nr <- (counts[count == r, featWithCount])[1]
        nrplus1 <- (counts[count == r + 1, featWithCount])[1]
        counts[count == r, discount := ((r + 1) * nrplus1) / (r * nr)]
    }
    
    counts <- merge(counts, data.table(countsToDiscount = countsToDiscount), 
                    by.x = "count", by.y = "countsToDiscount")[, .(count, discount)]
    
    return(
        function(r) {
            mergeRes <- (merge(data.table(r = r), counts, 
                               by.x = "r", by.y = "count", all.x = TRUE)
                         )[, .(r, discount = sapply(discount, function(el) { if (is.na(el)) {return(1)} else {return(el)} } ))]
            return(mergeRes)
        }
    )
}


loadDatabase <- function(ngramPath = "../results/tables")
{
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
    
    
    list(unigram = tableWord, bigram = tableBigram, 
         trigram = tableTrigram, fourgram = tableFourgram,
         
         unigramDiscount = computeDiscountFunc(1, tableWord, 1:5),
         bigramDiscount = computeDiscountFunc(2, tableBigram, 1:5),
         trigramDiscount = computeDiscountFunc(3, tableTrigram, 1:5),
         fourgramDiscount = computeDiscountFunc(4, tableFourgram, 1:5))
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
    
    preUnigramCount <- 0
    preBigramCount <- 0
    preTrigramCount <- 0
    
    if (length(words) >= 1)
    {
        searchWord1 <- words[length(words)]
        
        preUnigramCount <- (database$unigram[word == searchWord1, probability])[1]
        
        bigramCandidates <- merge(
            
            
                    merge(database$unigram[word == searchWord1, .(id, word)], 
                    database$bigram, by.x = "id", by.y = "id1")[,.(id = id2, count = probability, 
                                                                   condprob = probability / preUnigramCount)],
                    
                    
                    database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                    )[order(-count), .(id, word, count, condprob)]
        
        message("preUnigramCount = ", preUnigramCount)
                                 
        
        
        if (length(words) >= 2)
        {
            searchWord2 <- words[length(words) - 1]
            
            preBigramCount <- (merge(merge(database$bigram, database$unigram[word == searchWord2, .(id, word)], 
                                           by.x = "id1", by.y = "id")[, .(id2, probability)], 
                                     database$unigram[word == searchWord1, .(id, word)], by.x = "id2", by.y = "id")[, probability])[1]
            
            trigramCandidates <- merge( 
                
                
                        merge(merge(database$unigram[word == searchWord2, .(id, word)], 
                              database$trigram, by.x = "id", by.y = "id1")
                        [, .(id2, id3, probability)],
                        
                        database$unigram[word == searchWord1, .(id, word)],
                        by.x = "id2", by.y = "id")[,.(id = id3, count = probability, condprob = probability / preBigramCount)],
                        
                        
                        database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                        )[order(-count), .(id, word, count, condprob)]
            
            
            message("preBigramCount = ", preBigramCount)
            
            
            
            if (length(words) >= 3)
            {
                searchWord3 <- words[length(words) - 2]
                
                preTrigramCount <- (merge(merge(
                    merge(database$trigram, database$unigram[word == searchWord3, .(id, word)],
                          by.x = "id1", by.y = "id")[, .(id2, id3, probability)],
                    database$unigram[word == searchWord2, .(id, word)], 
                    by.x = "id2", by.y = "id")[, .(id3, probability)], 
                    database$unigram[word == searchWord1, .(id, word)], by.x = "id3", by.y = "id")[1, probability])[1]
                
                fourgramCandidates <- merge( 
                    
                            merge(merge(merge(database$unigram[word == searchWord3, .(id, word)],
                                        database$fourgram, by.x = "id", by.y = "id1")
                                  [, .(id2, id3, id4, probability)],
                                  
                                  database$unigram[word == searchWord2, .(id, word)],
                                  by.x = "id2", by.y = "id")[, .(id3, id4, probability)],
                            database$unigram[word == searchWord1, .(id, word)],
                            by.x = "id3", by.y = "id")[, .(id = id4, count = probability, condprob = probability / preTrigramCount)],
                            
                            
                            database$unigram[, .(wid = id, word)], by.x = "id", by.y = "wid"
                            )[order(-count), .(id, word, count, condprob)]
                

                message("preTrigramCount = ", preTrigramCount)
            }
        }
    }
    
    list(bigramCandidates = bigramCandidates, trigramCandidates = trigramCandidates, fourgramCandidates = fourgramCandidates)
}
