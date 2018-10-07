source("validation.R")
source("../nGram/predictWord.R")

# Function to validate a series of precomputed models with different
# percentage of original vocabulary kept
validateModelsVocab <- function(testingDir, 
                                modelPathBase = "../results_validation/course", 
                                vocabPerc = c(90, 92, 94, 96, 98),
                                topForAccuracy = 3)
{
    genResult <- data.frame(vocabPerc = vocabPerc, 
                            logP = rep(0.0, length(vocabPerc)),
                            guessed = rep(0, length(vocabPerc)),
                            cnt = rep(0, length(vocabPerc)))
    
    for (perc in vocabPerc)
    {
        modelTableDir <- sprintf("%s%d/tables", modelPathBase, perc)
        message(" >> Reading database for percentage ", perc, " from ", 
                modelTableDir, "...")
        database <- loadDatabase(modelTableDir)
        message(" >> Unigrams: ", nrow(database$unigram), 
                ", bigrams: ", nrow(database$bigram),
                ", trigrams: ", nrow(database$trigram),
                ", fourgrams: ", nrow(database$fourgram),
                ", fivegrams: ", nrow(database$fivegram))
        res <- computePerplexityForTestDirectory(database, testingDir)
        
        message(" >> logP = ", res$logP, ", guessed = ", res$guessed,
                ", cnt = ", res$cnt)
        
        genResult[vocabPerc == perc, "logP"] <- res$logP
        genResult[vocabPerc == perc, "guessed"] <- res$guessed
        genResult[vocabPerc == perc, "cnt"] <- res$cnt
    }
    
    return(genResult)
}