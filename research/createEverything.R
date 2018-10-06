source("../TextFileTools/splitFiles.R")

# Split the dataset provided by the course into training and testing datasets
#set.seed(2018)
#splitFilesTrainTest("../materials/datasets/final/en_US",
#                    "../materials/datasets/validation_course")

source("../nGram/extractFeatures.R")
source("../nGram/buildFrequencyTables.R")


commonResDir <- "../results_validation/results"
if (!dir.exists(commonResDir))
    dir.create(commonResDir)
#commonMatrixDir <- "../results_validation/dfm"
#if (!dir.exists(commonMatrixDir))
#    dir.create(commonMatrixDir)

extractFeatures(textDirectory = "../materials/datasets/validation_course/training",
                tokensDirectory = commonResDir,
                matrixDirectory = commonResDir,
                buildTokens = TRUE, buildMatrices = TRUE, ngramTypes = 1:5)
extractFeatures(textDirectory = "../materials/datasets/validation_course/training",
                tokensDirectory = commonResDir,
                matrixDirectory = commonResDir,
                buildTokens = FALSE, buildMatrices = FALSE, mergeMatrices = TRUE,
                ngramTypes = c(1))

for (perc in c(90, 92, 94, 96, 98))
{
    resDir <- sprintf("../results_validation/course%d", perc)
    #matrixDir <- sprintf("%s/dfm", resDir)
    tablesDir <- sprintf("%s/tables", resDir)
    if (!dir.exists(resDir))
        dir.create(resDir)
    if (!dir.exists(matrixDir))
        dir.create(matrixDir)
    if (!dir.exists(tablesDir))
        dir.create(tablesDir)


    file.copy(sprintf("%s/generalDfm1.dat", commonResDir), matrixDir)
    buildFrequencyTables(matrixDirectory = matrixDir, resultsDirectory = commonResDir,
                         tablesDirectory = tablesDir, ngramsToFilter = 2:5,
                         vocabLimit = 0.01 * perc, update = TRUE)
    gatherVocabTables(tablesDirectory = tablesDir, ngramsToFilter = 2:5)
    filterVocabTables(tableDirectory = tablesDir, ngramsToFilter = 1:5,
                      countsToDiscount = 1:5)
}