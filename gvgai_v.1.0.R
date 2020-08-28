##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO GVGAI games (gvgai_vN.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis, based on the metric of generality
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#   
#   Data provided by Fernando Martinez-Plumed, collected from some papers
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    25 June 2019. First operative version
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


setwd("/.../ **** PUT YOUR FOLDER HERE ****")
source("generality.R")


DATADIR <- "gvgai.data"
OUTPUTDIR <- "gvgai.results"




##############################################################################
####################### READING THE DATA #####################################
##############################################################################


df <- readRDS(paste0(DATADIR,"/GVGP.results.orig.rds"))


nrow(df)  # 
ncol(df)  # 
#row.names(df) <- df[, "name"]  # Use the column with the names for the row names
#df <- df[,-1] # Remove the column with the names
row.names(df)  # 23 systems
colnames(df)  # 245 games (49 each with 5 versions)
sum(is.na(df)) # No missing values

GROUP <- "NOGROUP"
#GROUP <- "GROUP5"   # if GROUP5 we merge the 5 version into one game (item) and have only 49 items. Otherwise 245.
if (GROUP == "GROUP5") {
  studyName <- "gvgaiGROUP5"
  colIdx <- seq(1,ncol(df),5)
  dfGroup <- df[,colIdx]
  for (i in colIdx) {
    dfGroup[,(i-1)/5+1] <- rowMeans(df[,i:(i+4)])
  }
  df <- dfGroup
} else {
  studyName <- "gvgai"
}

nrow(df)
ncol(df)


##############################################################################
################## FA/PCA ANALYSIS ON THE ORIGINAL DATA ######################
##############################################################################



# FA BEFORE WE BINARISE

if (GROUP == "GROUP5") {
  fit <- PerformFAandPCA(df)
  FAloadings <- fit$loadings
  FAVaccounted <-  mean(fit$communalities)
  cat(paste0("The mean of the FA loadings is ", mean(FAloadings), " and the accounted variance is: ", FAVaccounted, "\n"))
}





##############################################################################
############## METHOD 4: STEPWISE POPULATIONAL ##############################
##############################################################################

# METHOD 4A: scale
#METHOD="SCALE"
# METHOD 4B: rank
METHOD <- "RANK"
#nDifficulties <- NULL
nDifficulties <- 100   # A lot of resolution
#, BREAK_TIES= FALSE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5) {
  

# We create an empty matrix with the appropriate size
if (is.null(nDifficulties)) {
  nDifficulties <- nrow(df)
}
responseMatrix <- data.frame(matrix(data = NA, nrow = nrow(df), ncol = nDifficulties*ncol(df)))

names <- rownames(df)

for (j in 1:nrow(df)) { # agents
  difficulties <- NULL
  responses <- NULL
  abilities <- NULL
  for (i in 1:ncol(df)) {  # items
    r <- GenerateStepwiseResponsesAndDifficultiesFromOrder(df[j,i], df[,i], nDifficulties=nDifficulties, METHOD=METHOD, BREAK_TIES= FALSE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5)

    difficulties <- c(difficulties,r$difficulties)
    responses <- c(responses, r$responses)
    abilities <- c(abilities, r$ability)
  } 
  
  # Since they are stepwise curves, the aggregated ability is the mean, and the spread is the sd of the abilities
  print(paste0(names[j],": derived as mean of positions of stepwise curves and spread as sd of the positions"))
  print(mean(abilities))
  print(popsd(abilities))
  
  responseMatrix[j,] <- responses

  KMIN <- min(difficulties)
  KMAX <- max(difficulties)
  r <- PlotACCwithIndicators(difficulties,responses,KMIN,KMAX, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)
  unlist(r)

#  readline(prompt="Press [enter] to continue")
}

nrow(responseMatrix)
ncol(responseMatrix)
length(difficulties)

# This would not be needed, as we have derived the abilities and spread from the stepwise curves (printed above)
r <- calculateIndicatorsForAll(responseMatrix, difficulties, KMIN=KMIN, KMAX=KMAX)

if (is.null(nDifficulties)) {
  textDiff <- ""
} else {
  textDiff <- nDifficulties
}
studyName3 <- paste0(studyName, ".stepwise", METHOD, textDiff)

r2 <- PerformFullStudy(studyName3, responseMatrix, difficulties, DO_FA = FALSE, DO_IRT = FALSE, ESTIMATESIGMOID= FALSE,   CHOSEN_SUBJECTS = c(1,4,10))


# Nicer plot
filename <- paste0(studyName3,".capability-vs-spread-BETTER")
OpenPDFEPS(filename, 4, 6)
sizeLegend <- 0.45
PlotCapabilityVsSpread(r$capabilities,r$spreads,  MINCAP=KMIN, MAXCAP=KMAX, legendtext=names, legendpos="topright", pch=1:nAgents, col=1:nAgents, sizeLegend = sizeLegend, splitLegend=TRUE)
ClosePDFEPS()

