library(quanteda)

loadFrequencies <- function(pathName, freqPathname, update = FALSE)
{
    if (!file.exists(freqPathname) || update)
    {
        matrCon <- file(pathName, "r")
        partMatr <- unserialize(matrCon)
        close(matrCon)
        # group all the rows, just in case
        partMatr <- dfm_group(partMatr, groups = rep(1, nrow(partMatr)))
        # sort the features by frequency
        partMatr <- dfm_sort(partMatr);
        
        freqs <- colSums(partMatr)
        freqCon <- file(freqPathname, "w")
        serialize(freqs, freqCon)
        close(freqCon)
        
        freqs
    }
    else
    {
        freqCon <- file(freqPathname, "r")
        freqs <- unserialize(freqCon)
        close(freqCon)
        
        freqs
    }
}


dfmsToFreq <- function(matrixDirectory, freqPathname, ngramType = 1, ids = c(1), update = FALSE)
{
    if (!update && file.exists(freqPathname))
    {
        message("Reading frequencies from ", freqPathname, "...")
        freqCon <- file(freqPathname, "r")
        freqs <- unserialize(freqCon)
        close(freqCon)
        message("Frequencies read.")
        
        return(freqs)        
    }
    
    resultMatrix = NA
    
    for (i in ids)
    {
        pathName <- sprintf("%s/dfm%d_%02d.dat", matrixDirectory, ngramType, i)
        
        matrFile <- file(pathName, "r")
        message("Reading DFM for id=", i, " from ", pathName)
        newMatrix <- unserialize(matrFile)
        close(matrFile)
        message("DFM for id=", i, " read.")
        
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
    }
    
    resultMatrix <- dfm_sort(resultMatrix)
    freqs <- colSums(resultMatrix)
    freqCon <- file(freqPathname, "w")
    serialize(freqs, freqCon)
    close(freqCon)
    
    freqs
}
