splitFiles <- function(sourcePath = "../materials/datasets/final/en_US",
                       outputDir = "../materials/datasets/english_split", maxLines = 250000,
                       unifyApostrophe = TRUE,
                       hyphenToSpace = TRUE)
{
    files <- list.files(path = sourcePath, 
                        pattern = "*.txt", 
                        full.names = TRUE, 
                        recursive = FALSE)
    
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
                if (unifyApostrophe)
                    lines <- sapply(lines, function(s) { gsub("\U0092", "'", gsub("’", "'", s,  fixed = TRUE), fixed = TRUE) },
                           USE.NAMES = FALSE)
                
                if (hyphenToSpace)
                    lines <- sapply(lines, function(s) { gsub("-", " ", s, fixed = TRUE) }, USE.NAMES = FALSE)
                
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


splitFilesTrainTest <- function(sourcePath = "../materials/datasets/final/en_US",
                       outputDir = "../materials/datasets/validation",
                       samplePart = 0.25, trainPart = 0.8,
                       maxLines = 250000,
                       numRandom = 1000000,
                       unifyApostrophe = TRUE,
                       hyphenToSpace = TRUE)
{
    files <- list.files(path = sourcePath, 
                        pattern = "*.txt", 
                        full.names = TRUE, 
                        recursive = FALSE)
    
    if (!dir.exists(outputDir))
    {
        message("Creating output directory ", outputDir, "...")
        dir.create(outputDir)
    }
    
    trainingDir <- sprintf("%s/training", outputDir)
    testingDir <- sprintf("%s/testing", outputDir)
    
    if (!dir.exists(trainingDir))
        dir.create(trainingDir)
    
    if (!dir.exists(testingDir))
        dir.create(testingDir)
    
    samplingFlag <- rbinom(numRandom, 1, samplePart)
    trainingFlag <- rbinom(numRandom, 1, trainPart)
    
    cnt <- 0
    lineCnt <- 0
    trainCnt <- 0
    testCnt <- 0
    
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
                #outputPathname <- sprintf("%s/%d_%d.txt", outputDir, cnt, splitCnt)
                #message("Writing ", length(lines), " to ", outputPathname)
                #writeLines(lines, outputPathname)
                #message(length(lines), " written to ", outputPathname)
                
                trainingPathname <- sprintf("%s/%d_%d.txt", trainingDir, cnt, splitCnt)
                testingPathname <- sprintf("%s/%d_%d.txt", testingDir, cnt, splitCnt)
                
                
                trainingLines <- character(0)
                testingLines <- character(0)
                
                for (line in lines)
                {
                    if (nchar(line) == 0)
                        next
                    # Do we include this line in our sample?
                    isSample <- samplingFlag[lineCnt %% length(samplingFlag) + 1]
                    if (isSample > 0)
                    {
                        if (unifyApostrophe)
                            line <- gsub("\U0092", "'", gsub("’", "'", 
                                                             line,  fixed = TRUE), 
                                         fixed = TRUE)
                        
                        if (hyphenToSpace)
                            line <- gsub("-", " ", line, fixed = TRUE)
                        
                        # Does it belong to the training or the testing set?
                        isTraining <- trainingFlag[lineCnt %% length(trainingFlag)]
                        if (isTraining)
                        {
                            trainingLines <- c(trainingLines, line)
                            trainCnt <- trainCnt + 1
                        }
                        else
                        {
                            testingLines <- c(testingLines, line)
                            testCnt <- testCnt + 1
                        }
                    }
                    lineCnt <- lineCnt + 1    
                }
                
                trainingFile <- file(trainingPathname, "w")
                testingFile <- file(testingPathname, "w")
                
                writeLines(trainingLines, trainingFile)
                writeLines(testingLines, testingFile)
                
                close(trainingFile)
                close(testingFile)
                
                splitCnt <- splitCnt + 1    
            }
            else
                fileProcessed <- TRUE
        }
        
        close(inputCon)
        
        cnt <- cnt + 1
    }
    
    return(list(lineCnt = lineCnt, trainCnt = trainCnt, testCnt = testCnt))
}
    