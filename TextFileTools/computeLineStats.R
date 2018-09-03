# A simple summary of a set of text files

computeLineStats <- function(fileNames, fileDescr = character(0))
{
    #fileNames <- c("../materials/datasets/final/en_US/en_US.blogs.txt",
    #               "../materials/datasets/final/en_US/en_US.news.txt",
    #               "../materials/datasets/final/en_US/en_US.twitter.txt")
    
    maxLength <- vector("numeric", length(fileNames))
    minLength <- vector("numeric", length(fileNames))
    avgLength <- vector("numeric", length(fileNames))
    lineNum <- vector("numeric", length(fileNames))
    
    maxLines <- 250000
    
    fileCnt <- 1
    for (f in fileNames)
    {
        message("Processing ", f)
        
        maxLength[fileCnt] <- -Inf
        minLength[fileCnt] <- Inf
        avgLength[fileCnt] <- 0
        lineNum[fileCnt] <- 0
        
        con <- file(f, "r")
        fileProcessed <- FALSE        
        linesProcessed <- 0
        
        while (!fileProcessed)
        {
            lines <- readLines(con, n = maxLines)
            currentLines <- length(lines)
            
            if (currentLines > 0)
            {
                linesLength <- sapply(lines, FUN = function(str) { nchar(str) }, USE.NAMES = FALSE)
                currMin <- min(linesLength)
                currMax <- max(linesLength)
                currAvg <- mean(linesLength)
                message("Current range: ", currMin, " .. ", currMax)
                message("Current average: ", currAvg)
                #cat("Comparing", maxLength[fileCnt], "<", currMax, "...\n")
                if (maxLength[fileCnt] < currMax)
                    maxLength[fileCnt] <- currMax
                if (minLength[fileCnt] > currMin)
                    minLength[fileCnt] <- currMin
                
                
                avgLength[fileCnt] <- avgLength[fileCnt] * linesProcessed / (linesProcessed + currentLines) +
                    currAvg * currentLines / (linesProcessed + currentLines)
                
                
                linesProcessed <- linesProcessed + currentLines
            }
            else
                fileProcessed = TRUE
            
            lineNum[fileCnt] <- linesProcessed
        }
        close(con)
        
        fileCnt <- fileCnt + 1
    }
    
    #list(minLength, maxLength, avgLength)
    res <- data.frame(minLength = minLength, maxLength = maxLength, avgLength = avgLength, lineNum = lineNum)
    if (length(fileDescr) > 0)
        row.names(res) <- fileDescr
    else
        row.names(res) <- fileNames
    res
}
