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
