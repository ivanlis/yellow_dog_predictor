source("../TextFileTools/splitFiles.R")
source("../nGram/extractFeatures.R")
source("../nGram/buildFrequencyTables.R")


# Split the dataset provided by the course into training and testing datasets
splitDataset <- function()
{
    set.seed(2018)
    splitFilesTrainTest("../materials/datasets/final/en_US",
                        "../materials/datasets/validation_course")
}

# Prepare full text from course materials
splitFilesCourse <- function()
{
    splitFiles(sourcePath = "../materials/datasets/final/en_US",
               outputDir = "../materials/datasets/english_split")
}
# Prepare full text with additional corpora
splitFilesEnhanced <- function()
{
    splitFiles(sourcePath = "../materials/datasets/enhanced",
               outputDir = "../materials/datasets/enhanced_split")
}

commonResDir <- "../results_validation/results"
fullCourseResDir <- "../results_course"
fullEnhancedResDir <- "../results_enhanced"

extractCommonFeatures <- function(fromDirectory = "../materials/datasets/validation_course/training",
                                  toDirectory = commonResDir)
{
    if (!dir.exists(toDirectory))
        dir.create(toDirectory)
    #commonMatrixDir <- "../results_validation/dfm"
    #if (!dir.exists(commonMatrixDir))
    #    dir.create(commonMatrixDir)
    
    extractFeatures(textDirectory = fromDirectory,
                    tokensDirectory = toDirectory,
                    matrixDirectory = toDirectory,
                    buildTokens = TRUE, buildMatrices = TRUE, ngramTypes = 1:5)
    extractFeatures(textDirectory = fromDirectory,
                    tokensDirectory = toDirectory,
                    matrixDirectory = toDirectory,
                    buildTokens = FALSE, buildMatrices = FALSE, mergeMatrices = TRUE,
                    ngramTypes = c(1))
}

# Extract common features from course materials (for the full model)
extractCommonFeaturesCourse <- function()
{
    extractCommonFeatures("../materials/datasets/english_split", fullCourseResDir)
}
# Extract common features with additional corpora (for the full enhanced model)
extractCommonFeaturesEnhanced <- function()
{
    extractCommonFeatures("../materials/datasets/enhanced_split", fullEnhancedResDir)
}

buildTrainedModelTables <- function(fromDirectory = commonResDir)
{
    for (perc in c(90, 92, 94, 96, 98))
    {
        resDir <- sprintf("../results_validation/course%d", perc)
        matrixDir <- sprintf("%s/dfm", resDir)
        tablesDir <- sprintf("%s/tables", resDir)
        if (!dir.exists(resDir))
            dir.create(resDir)
        if (!dir.exists(matrixDir))
            dir.create(matrixDir)
        if (!dir.exists(tablesDir))
            dir.create(tablesDir)
    
    
        file.copy(sprintf("%s/generalDfm1.dat", fromDirectory), matrixDir)
        buildFrequencyTables(matrixDirectory = matrixDir, resultsDirectory = fromDirectory,
                             tablesDirectory = tablesDir, ngramsToFilter = 2:5,
                             vocabLimit = 0.01 * perc, update = TRUE)
        gatherVocabTables(tablesDirectory = tablesDir, ngramsToFilter = 2:5)
        filterVocabTables(tableDirectory = tablesDir, ngramsToFilter = 1:5,
                          countsToDiscount = 1:5)
    }
}

# Build full model
buildFullModel <- function(fromDir, perc = 94)
{
    resDir <- fromDir
    matrixDir <- sprintf("%s/dfm", resDir)
    tablesDir <- sprintf("%s/tables", resDir)
    if (!dir.exists(resDir))
        dir.create(resDir)
    if (!dir.exists(matrixDir))
        dir.create(matrixDir)
    if (!dir.exists(tablesDir))
        dir.create(tablesDir)
    
    file.copy(sprintf("%s/generalDfm1.dat", resDir), matrixDir)
    buildFrequencyTables(matrixDirectory = matrixDir, resultsDirectory = resDir,
                         tablesDirectory = tablesDir, ngramsToFilter = 2:5,
                         vocabLimit = 0.01 * perc, update = TRUE)
    gatherVocabTables(tablesDirectory = tablesDir, ngramsToFilter = 2:5)
    filterVocabTables(tableDirectory = tablesDir, ngramsToFilter = 1:5,
                      countsToDiscount = 1:5)    
}

buildFullCourseModel <- function(perc = 94)
{
    buildFullModel(fullCourseResDir, perc)
}

# Build enhanced model
buildFullEnhancedModes <- function(perc = 94)
{
    buildFullModel(fullEnhancedResDir, perc)
}
