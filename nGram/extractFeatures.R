# defaults
defaultDatasetsDirectory <- "../materials/datasets"
defaultTextDirectory <- "../materials/datasets/english_split"
defaultTokensDirectory <- "../results"
defaultMatrixDirectory <- "../results"

defaultBadWordsFileName <- "../misc/bad-words-edited.txt"

library(quanteda)
library(readtext)
library(stopwords)


buildMatrix <- function(toks, cnt = 0, ngramType = 1, matrixDirectory = ".")
{
    message("Computing DFM for id=", cnt, "...")
    dfMatr <- dfm(toks)
    message("DFM for id=", cnt, "computed.")
    matrFileName <- sprintf("%s/dfm%d_%02d.dat", matrixDirectory, ngramType, cnt)
    matrFile <- file(matrFileName, "w")
    message("Storing DFM for id=", cnt, "in", matrFileName, "...")
    serialize(dfMatr, matrFile)
    close(matrFile)
    message("DFM for id=", cnt, "stored.")
    
    dfMatr
}

buildTokens <- function(tokSource, 
            badWords = c(),
            cnt = 0, 
            tokensDirectory = defaultTokensDirectory, 
            ngramType = 1, 
            rawTok = FALSE,
            removeStop = FALSE)
{
    toks <- NA
    tokFileName <- ""
    
    if (rawTok)
    {
        message("Computing raw tokens for cnt", cnt, "ngramType =", ngramType, "...")
        toks <- tokens_tolower(tokens(tokSource, what = "word", 
                                      remove_numbers = FALSE, 
                                      remove_punct = TRUE,
                                      remove_twitter = FALSE,
                                      remove_url = FALSE,
                                      include_docvars = FALSE,
                                      ngrams = 1
        ))
        message("Raw tokens for cnt", cnt, "ngramType =", ngramType, "built.")
        
        if (removeStop)
        {
            message("Removing stop words...")
            toks <- tokens_remove(toks, stopwords::stopwords("en", "snowball"), verbose = 3)
        }
        
        message("Removing profane words...")
        toks <- tokens_replace(toks, badWords, 
                               replacement = rep(";;;censored;;;", length(badWords)), 
                               verbose=3)
        
        tokFileName <- sprintf("%s/rawtokens_%02d.dat", tokensDirectory, cnt)
    }
    else
    {
        message("Computing filtered tokens for cnt", cnt, "ngramType =", ngramType, "...")
        toks <- tokens(tokSource, what = "word", 
                       remove_numbers = FALSE, 
                       remove_punct = FALSE,
                       remove_twitter = TRUE,
                       remove_url = TRUE,
                       include_docvars = FALSE,
                       ngrams = ngramType
        )
        
        message("Removing profane words...")
        toks <- tokens_remove(toks, ";;;censored;;;", valuetype = "regex", verbose=3)        
        
        message("Removing number and symbol containing words from tokens...")
        #toks <- tokens_remove(toks, "[0-9]", valuetype = "regex", verbose=3)
        regex <- "[^a-z-'_]"
        if (ngramType == 1)
            regex <- "[^a-z-']"
        toks <- tokens_remove(toks, regex, valuetype = "regex", verbose=3)
        
        tokFileName <- sprintf("%s/tokens%d_%02d.dat", tokensDirectory, ngramType, cnt)
    }
    
    message("Saving tokens to file ", tokFileName)
    tokFile <- file(tokFileName, "w")
    serialize(toks, tokFile)
    close(tokFile)
    message("Tokens for cnt =", cnt, "saved.")
    
    toks
}

filterMatrix <- function(matr, cnt = 0, keepTokens = 0.9, matrixDirectory = ".", 
                         ngramType = 1)
{
    matr <- dfm_sort(matr)
    counts <- colSums(matr)
    threshold <- sum(counts) * keepTokens
    lastIndex <- sum(cumsum(counts) < threshold) + 1
    
    
    message("Filtering matrix,", keepTokens, "<->", threshold, "last index =", lastIndex)
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
                        removeStop = FALSE,
                        filterSteps = c(3, 8, 14),
                        ngramTypes = 1:4)
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
            message("Creating corpus", cnt, "from", f, "...")
            partCorpus <- corpus(readtext(f, cache = FALSE, verbosity = 3))
            message("Corpus", cnt, "created.")    
            message("Reshaping corpus", cnt, "...")
            partCorpus <- corpus_reshape(partCorpus, to = "sentences", use_docvars = FALSE)
            message("Corpus", cnt, "reshaped.")
            
            # Now, tokenize this corpus
            message("Tokenizing corpus", cnt, "...")
            message("Building raw tokens...")
            rawToks <- buildTokens(partCorpus, badWords, cnt, tokensDirectory, 
                                   ngramType = 1, rawTok = TRUE, removeStop = removeStop)
            message("Raw tokens built.")
            for (ngramType in ngramTypes)
            {
                message("Building filtered tokens for ngramType =", ngramType)
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
            
            message("Corpus", cnt, "tokenized.")
            cnt <- cnt + 1
        }
    }
    
    if (buildMatrices && !buildTokens)
    {
        message("Building DFMs from saved tokens, directory", tokensDirectory, "...")
        
        for (ngramType in ngramTypes)
        {
            files <- list.files(tokensDirectory, 
                                pattern = sprintf("tokens%d_.*.dat", ngramType), 
                                full.names = TRUE,
                                recursive = FALSE)
            
            message("Processing", length(files), "token files...")
            
            cnt <- 0
            for (f in files)
            {
                tokFile <- file(f, "r")
                message("Reading tokens for id=", cnt, "from", f, "...")
                toks <- unserialize(tokFile)
                close(tokFile)
                message("Tokens for id=", cnt, "read.")
                
                # store matrix only, do not accumulate them
                buildMatrix(toks, cnt, ngramType, matrixDirectory)
                rm(toks)
                
                cnt <- cnt + 1
            }
        }
    }    
    
    
    if (mergeMatrices)
    {
        message("Building overall DFM from saved DFMs, directory", matrixDirectory, "...")
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
                message("Reading DFM for id=", cnt, "from", f, "...")
                newMatrix <- unserialize(matrFile)
                close(matrFile)
                message("DFM for id=", cnt, "read.")
                
                message("Grouping all rows...")
                newMatrix <- dfm_group(newMatrix, groups = rep(1, nrow(newMatrix)))
                message("All rows grouped.")
                
                if (is.dfm(resultMatrix))
                {
                    resultMatrix <- rbind(resultMatrix, newMatrix)
                    resultMatrix <- dfm_group(resultMatrix, groups = rep(1, nrow(resultMatrix)))
                }
                else
                    resultMatrix <- newMatrix
                
                rm(newMatrix)
                
                message("Iteration ", cnt, ", ngramType =", ngramType, 
                    ":  dim(resultMatrix) =", dim(resultMatrix))
                
                
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
                message("Saving general DFM for ngramType = ", ngramType, "...")
                serialize(resultMatrix, genMatrFile)
                close(genMatrFile)
                message("General DFM for ngramType = ", ngramType, "saved.")
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
                message("Reading DFM for id=", cnt, "from", f, "...")
                newMatrix <- unserialize(matrFile)
                close(matrFile)
                message("DFM for id=", cnt, "read.")
                
                message("Grouping all rows...")
                newMatrix <- dfm_group(newMatrix, groups = rep(1, nrow(newMatrix)))
                message("All rows grouped.")
                
                if (is.dfm(resultMatrix))
                {
                    resultMatrix <- rbind(resultMatrix, newMatrix)
                    resultMatrix <- dfm_group(resultMatrix, groups = rep(1, nrow(resultMatrix)))
                }
                else
                    resultMatrix <- newMatrix
                
                rm(newMatrix)
                
                message("Iteration ", cnt, ", ngramType =", ngramType, 
                    ":  dim(resultMatrix) =", dim(resultMatrix))
                cnt <- cnt + 1
            }
            
            
            if (is.dfm(resultMatrix))
            {
                genMatrFileName <- sprintf("%s/generalDfm%d.dat", 
                                           matrixDirectory, ngramType)
                genMatrFile <- file(genMatrFileName, "w")
                message("Saving general DFM for ngramType = ", ngramType, "...")
                serialize(resultMatrix, genMatrFile)
                close(genMatrFile)
                message("General DFM for ngramType = ", ngramType, "saved.")
                rm(resultMatrix)
            }        
        }
    }    
    
}

