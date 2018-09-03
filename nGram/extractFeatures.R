# defaults
defaultDatasetsDirectory <- "../materials/datasets"
defaultTextDirectory <- "../materials/datasets/english_split"
defaultTokensDirectory <- "./results"
defaultMatrixDirectory <- "./results"

defaultBadWordsFileName <- "../misc/bad-words-edited.txt"

library(quanteda)
library(readtext)


buildMatrix <- function(toks, cnt = 0, ngramType = 1, matrixDirectory = ".")
{
    cat("Computing DFM for id=", cnt, "...\n")
    dfMatr <- dfm(toks)
    cat("DFM for id=", cnt, "computed.\n")
    matrFileName <- sprintf("%s/dfm%d_%02d.dat", matrixDirectory, ngramType, cnt)
    matrFile <- file(matrFileName, "w")
    cat("Storing DFM for id=", cnt, "in", matrFileName, "...\n")
    serialize(dfMatr, matrFile)
    close(matrFile)
    cat("DFM for id=", cnt, "stored.\n")
    
    dfMatr
}

buildTokens <- function(tokSource, 
            badWords = c(),
            cnt = 0, 
            tokensDirectory = defaultTokensDirectory, 
            ngramType = 1, 
            rawTok = FALSE)
{
    toks <- NA
    tokFileName <- ""
    
    if (rawTok)
    {
        cat("Computing raw tokens for cnt", cnt, "ngramType =", ngramType, "...\n")
        toks <- tokens_tolower(tokens(tokSource, what = "word", 
                                      remove_numbers = FALSE, 
                                      remove_punct = TRUE,
                                      remove_twitter = FALSE,
                                      remove_url = FALSE,
                                      include_docvars = FALSE,
                                      ngrams = 1
        ))
        cat("Raw tokens for cnt", cnt, "ngramType =", ngramType, "built.\n")
        
        cat("Removing profane words...\n")
        toks <- tokens_replace(toks, badWords, 
                               replacement = rep(";;;censored;;;", length(badWords)), 
                               verbose=3)
        
        tokFileName <- sprintf("%s/rawtokens_%02d.dat", tokensDirectory, cnt)
    }
    else
    {
        cat("Computing filtered tokens for cnt", cnt, "ngramType =", ngramType, "...\n")
        toks <- tokens(tokSource, what = "word", 
                       remove_numbers = FALSE, 
                       remove_punct = FALSE,
                       remove_twitter = TRUE,
                       remove_url = TRUE,
                       include_docvars = FALSE,
                       ngrams = ngramType
        )
        
        cat("Removing profane words...\n")
        toks <- tokens_remove(toks, ";;;censored;;;", valuetype = "regex", verbose=3)        
        
        cat("Removing number and symbol containing words from tokens...\n")
        #toks <- tokens_remove(toks, "[0-9]", valuetype = "regex", verbose=3)
        regex <- "[^a-z-'_]"
        if (ngramType == 1)
            regex <- "[^a-z-']"
        toks <- tokens_remove(toks, regex, valuetype = "regex", verbose=3)
        
        tokFileName <- sprintf("%s/tokens%d_%02d.dat", tokensDirectory, ngramType, cnt)
    }
    
    cat("Saving tokens to file ", tokFileName, "\n")
    tokFile <- file(tokFileName, "w")
    serialize(toks, tokFile)
    close(tokFile)
    cat("Tokens for cnt =", cnt, "saved.\n")
    
    toks
}

filterMatrix <- function(matr, cnt = 0, keepTokens = 0.9, matrixDirectory = ".", 
                         ngramType = 1)
{
    matr <- dfm_sort(matr)
    counts <- colSums(matr)
    threshold <- sum(counts) * keepTokens
    lastIndex <- sum(cumsum(counts) < threshold) + 1
    
    
    cat("Filtering matrix,", keepTokens, "<->", threshold, "last index =", lastIndex, "\n")
    matr <- matr[, 1:lastIndex]
    
    fmatrCon <- file(sprintf("%s/filtered_dfm%d_%02d.dat", matrixDirectory, ngramType, cnt), "w")
    serialize(matr, fmatrCon)
    close(fmatrCon)
}

extractFeatures <- function(textDirectory = defaultTextDirectory,
                        tokensDirectory = defaultTokensDirectory,
                        matrixDirectory = defaultMatrixDirectory,
                        datasetsDirectory = defaultDatasetsDirectory,
                        buildTokens = FALSE,
                        buildMatrices = TRUE,
                        mergeMatrices = FALSE,
                        mergeFiltered = FALSE,
                        filterSteps = c(3, 8, 14),
                        ngramTypes = 1:3)
{
    resultMatrix <- NA
    
    if (buildTokens)
    {
        # names of files containing (split) texts
        files <- list.files(path = textDirectory, 
                            pattern = "*.txt", 
                            full.names = TRUE, 
                            recursive = FALSE)
        # list of profane words to filter them out
        #badWords <- readLines(sprintf("%s/bad-words-edited.txt", datasetsDirectory))
        badWords <- readLines("../misc/bad-words-edited.txt")
        #badWords <- sapply(badWords, 
        #                   FUN = function(str) { 
        #                           sprintf("^%s$|_%s$|^%s_|_%s_", str, str, str, str) 
        #                       }, 
        #                   USE.NAMES = FALSE)
        
        cnt <- 0
        
        for (f in files)
        {
            cat("Creating corpus", cnt, "from", f, "...\n")
            partCorpus <- corpus(readtext(f, cache = FALSE, verbosity = 3))
            cat("Corpus", cnt, "created.\n")    
            cat("Reshaping corpus", cnt, "...\n")
            partCorpus <- corpus_reshape(partCorpus, to = "sentences", use_docvars = FALSE)
            cat("Corpus", cnt, "reshaped.\n")
            
            # Now, tokenize this corpus
            cat("Tokenizing corpus", cnt, "...\n")
            cat("Building raw tokens...\n")
            rawToks <- buildTokens(partCorpus, badWords, cnt, tokensDirectory, 
                                   ngramType = 1, rawTok = TRUE)
            cat("Raw tokens built.\n")
            for (ngramType in ngramTypes)
            {
                cat("Building filtered tokens for ngramType =", ngramType, "\n")
                toks <- buildTokens(rawToks, badWords, cnt, tokensDirectory,
                                    ngramType = ngramType, rawTok = FALSE)
                # build DFMs if needed
                if (buildMatrices)
                {
                    # store matrix only, do not accumulate them
                    buildMatrix(toks, cnt, ngramType, matrixDirectory)
                }                
                rm(toks)
            }
            
            cat("Corpus", cnt, "tokenized.\n")
            cnt <- cnt + 1
        }
    }
    
    if (buildMatrices && !buildTokens)
    {
        cat("Building DFMs from saved tokens, directory", tokensDirectory, "...\n")
        
        for (ngramType in ngramTypes)
        {
            files <- list.files(tokensDirectory, 
                                pattern = sprintf("tokens%d_.*.dat", ngramType), 
                                full.names = TRUE,
                                recursive = FALSE)
            
            cat("Processing", length(files), "token files...\n")
            
            cnt <- 0
            for (f in files)
            {
                tokFile <- file(f, "r")
                cat("Reading tokens for id=", cnt, "from", f, "...\n")
                toks <- unserialize(tokFile)
                close(tokFile)
                cat("Tokens for id=", cnt, "read.\n")
                
                # store matrix only, do not accumulate them
                buildMatrix(toks, cnt, ngramType, matrixDirectory)
                rm(toks)
                
                cnt <- cnt + 1
            }
        }
    }    
    
    
    if (mergeMatrices)
    {
        cat("Building overall DFM from saved DFMs, directory", matrixDirectory, "...\n")
        for (ngramType in ngramTypes)
        #for (ngramType in 3:3)
        {
            resultMatrix <- NA
            
            files <- list.files(matrixDirectory, 
                                pattern = sprintf("dfm%d_.*.dat", ngramType), 
                                full.names = TRUE,
                                recursive = FALSE)
            
            cnt <- 0
            
            for (f in files)
            {
                matrFile <- file(f, "r")
                cat("Reading DFM for id=", cnt, "from", f, "...\n")
                newMatrix <- unserialize(matrFile)
                close(matrFile)
                cat("DFM for id=", cnt, "read.\n")
                
                cat("Grouping all rows...\n")
                newMatrix <- dfm_group(newMatrix, groups = rep(1, nrow(newMatrix)))
                cat("All rows grouped.\n")
                
                if (is.dfm(resultMatrix))
                {
                    resultMatrix <- rbind(resultMatrix, newMatrix)
                    resultMatrix <- dfm_group(resultMatrix, groups = rep(1, nrow(resultMatrix)))
                }
                else
                    resultMatrix <- newMatrix
                
                rm(newMatrix)
                
                cat("Iteration ", cnt, ", ngramType =", ngramType, 
                    ":  dim(resultMatrix) =", dim(resultMatrix), "\n")
                
                
                if (ngramType >= 3 && cnt %in% c(filterSteps, length(files) - 1))
                {
                    filterMatrix(resultMatrix, cnt, keepTokens = 0.6, 
                                 matrixDirectory = matrixDirectory, 
                                 ngramType = ngramType)
                    resultMatrix <- NA
                }
                
                
                cnt <- cnt + 1
            }
            
            if (is.dfm(resultMatrix))
            {
                genMatrFileName <- sprintf("%s/generalDfm%d.dat", 
                                           matrixDirectory, ngramType)
                genMatrFile <- file(genMatrFileName, "w")
                cat("Saving general DFM for ngramType = ", ngramType, "...\n")
                serialize(resultMatrix, genMatrFile)
                close(genMatrFile)
                cat("General DFM for ngramType = ", ngramType, "saved.\n")
                rm(resultMatrix)
            }
        }
    }
    
    if (mergeFiltered)
    {
        #for (ngramType in 4:4)
        for (ngramType in 3:4)
        #ngramType = 3
        {
            resultMatrix <- NA
            
            files <- list.files(matrixDirectory, 
                                pattern = sprintf("filtered_dfm%d_.*.dat", ngramType), 
                                full.names = TRUE,
                                recursive = FALSE)   
            cnt <- 0
            for (f in files)
            {
                matrFile <- file(f, "r")
                cat("Reading DFM for id=", cnt, "from", f, "...\n")
                newMatrix <- unserialize(matrFile)
                close(matrFile)
                cat("DFM for id=", cnt, "read.\n")
                
                cat("Grouping all rows...\n")
                newMatrix <- dfm_group(newMatrix, groups = rep(1, nrow(newMatrix)))
                cat("All rows grouped.\n")
                
                if (is.dfm(resultMatrix))
                {
                    resultMatrix <- rbind(resultMatrix, newMatrix)
                    resultMatrix <- dfm_group(resultMatrix, groups = rep(1, nrow(resultMatrix)))
                }
                else
                    resultMatrix <- newMatrix
                
                rm(newMatrix)
                
                cat("Iteration ", cnt, ", ngramType =", ngramType, 
                    ":  dim(resultMatrix) =", dim(resultMatrix), "\n")
                cnt <- cnt + 1
            }
            
            
            if (is.dfm(resultMatrix))
            {
                genMatrFileName <- sprintf("%s/generalDfm%d.dat", 
                                           matrixDirectory, ngramType)
                genMatrFile <- file(genMatrFileName, "w")
                cat("Saving general DFM for ngramType = ", ngramType, "...\n")
                serialize(resultMatrix, genMatrFile)
                close(genMatrFile)
                cat("General DFM for ngramType = ", ngramType, "saved.\n")
                rm(resultMatrix)
            }        
        }
    }    
    
}

