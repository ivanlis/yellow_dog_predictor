splitFiles <- function()
{
    files <- list.files(path = "../materials/datasets/final/en_US", 
                        pattern = "*.txt", 
                        full.names = TRUE, 
                        recursive = FALSE)
    
    maxLines <- 250000
    outputDir <- "../materials/datasets/english_split"
    
    cnt <- 0
    
    for (f in files)
    {
        inputCon <- file(f, "r")
        
        fileProcessed <- FALSE
        splitCnt <- 0
        
        while (!fileProcessed)
        {
            message("Reading ", maxLines, " from ", f, "...")
            lines <- readLines(inputCon, maxLines)
            message("Lines read.")
        
            if (length(lines) > 0)
            {
                outputPathname <- sprintf("%s/%d_%d.txt", outputDir, cnt, splitCnt)
                message("Writing ", length(lines), " to ", outputPathname)
                writeLines(lines, outputPathname)
                message(length(lines), " written to ", outputPathname)
                splitCnt <- splitCnt + 1    
            }
            else
                fileProcessed <- TRUE
        }
        
        close(inputCon)
        
        cnt <- cnt + 1
    }
}