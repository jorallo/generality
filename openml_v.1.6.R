##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO CLASSIFICATION PROBLEMS (openml_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis, based on the metric of generality first introduced in 
#   J. Hernandez-Orallo "I.G.", March 15th, 2018
#   https://riunet.upv.es/bitstream/handle/10251/100267/secondbest.pdf
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    09/07/2019  First operative version
#  - v.1.1    12/07/2019  We can consider the data with different values of k, and the human data
#  - v.1.2    11/08/2019  New results included and the possibility of averaging a few difficulties (using the ranks)
#  - v.1.3    25/06/2020  Now it generates a .csv with the classifiers with their names, capability and generality to help identify them in the plots. 
#  - v.1.4    30/06/2020  We can now show the results for only 75 of the 150 instances, using SELECT75 = TRUE. 
#  - v.1.5    01/07/2020  Working with new data from OpenML that corrects some problems with many classifiers. 
#  - v.1.6    03/07/2020  Now it outputs the most capable and the most general classifiers
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################



setwd("/OneDrive - UPV/__SUBMISSIONS/2018/General intelligence/__NewCode__")
source("generality.R")

DATADIR <- "openml.data"
OUTPUTDIR <- "openml.results"






################################################################################
################################################################################
# INDIVIDUAL RESULTS FOR IRIS #############
################################################################################
################################################################################

###############
# Loading data
###############


#OPENML_DATA <- "4427x150"                 # It had problems with a a lot of bad models. Used original but never use again
#datasetnamebase <- "iris"

#OPENML_DATA <- "Study59_4432X150"          # This is the study "59". It works ok.
#datasetnamebase <- "irisN"

#OPENML_DATA <- "Study59_Weka_2037x150"    # This is the study "59" but only with weka classifiers. It works ok.
#datasetnamebase <- "irisNW"

#OPENML_DATA <- "Study59_Sklearn_1986x150" # This is the study "59" but only with sklearn classifiers. It works ok.
#datasetnamebase <- "irisNP"

#OPENML_DATA <- "Study59_MLR_60x150"       # This is the study "59" but only with MLR classifiers. It works ok.
#datasetnamebase <- "irisNM"              # This one breaks the FA analysis

OPENML_DATA <- "Study7306_473x150"        # This is the study 7306. A much smaller study.
datasetnamebase <- "irisS"


Df <-  read.csv(paste0(DATADIR, "/results_Iris_", OPENML_DATA, ".csv"))

KVALUE <- "_k10"   # (from "_k10" to _k20)
#KVALUE <- "_Wk20"   # "_Wk20" : weights between k10 and k20

SELECT75 <- FALSE

TEST <- "A"  # "A" or "B" (which subset of the 150 examples are used)

BIN_DIFFICULTIES <- FALSE

#ACCURACY_CUT <- 0 # Use this if no cut
#ACCURACY_CUT <- 1/2  # We cut by 1/2, and if so, normalise.
#ACCURACY_CUT <- 1/3  # We cut by the number of classes, and if so, shouldn't we normalise? No, to keep the same scale on the plots
ACCURACY_CUT <- 0.35


if (KVALUE == "_Wk20") {
  difficultyMetrics10 <- read.csv(paste0(DATADIR, "/diff_Iris_9x150", "_k10", ".csv"))
  difficultyMetrics20 <- read.csv(paste0(DATADIR, "/diff_Iris_9x150", "_k20", ".csv"))
  difficultyMetrics <- difficultyMetrics10 
  for (i in 2:ncol(difficultyMetrics)) {
    difficultyMetrics[1,i] <- (difficultyMetrics10[1,i] + difficultyMetrics20[1,i]) / 2  # We average the first row (KDN)
  }  
} else {
  difficultyMetrics <- read.csv(paste0(DATADIR, "/diff_Iris_9x150", KVALUE, ".csv"))
}
dim(difficultyMetrics)  # 9 151 (first column is names)

summary(Df)
Df[1,]
nrow(Df)  # 4427
ncol(Df)  # 151 - 1 for names (first column) = 150
nItems <- ncol(Df) - 1

summary(difficultyMetrics)


###############
# Choosing one difficulty metric from the nine
################
difficultyMetricNames <- difficultyMetrics[,1]
#difficultyMetricNames <- c("kDN", "IDS", "IDCP", "TD_P", "TD_U", "ICL", "ICLD", "MV", "ICP")
difficultyMetrics <- difficultyMetrics[,2:ncol(difficultyMetrics)]
dim(difficultyMetrics)  # 9 150 (first column with names gone)

# Use values between 1 to 7, but not 8 or 9, as all difficulties are 0 for MV and ICP
# 2 and 3 are very similar
# Also 7 is not worth the effort, as it is almost the same as 6.
# And 5 is a refinement of 4 (4 only has two difficulties)

difficultyMetric <- 1     #1  #4  #5  (1, and 5 are used in the paper)


## Hardness measures adopted in Smith et al. (also in our AIJ paper)
# http://axon.cs.byu.edu/papers/smith.ml2013.pdf

##  kDN_measure(dataset,k) --- k-Disagreeing Neighbors (kDN)

##  TD_measure(dataset) --- Tree Depth (TD)
##   *** TD --- pruned tree (TD P)
##   *** TU --- unpruned tree (TD U)
# Decision trees also provide a way to estimate the description length, or Kolmogorov complexity, of an instance. The depth of the leaf node that classifies an instance can give an intuition of the description length required for an instance. For example, an instance that requires 15 attribute splits before arriving at a leaf node is more complex than an instance that only requires 1 attribute split. Therefore, tree depth measures the depth of the leaf node for an instance in an induced C4.5 decision tree (both                                                                                                                                                                            pruned (TD P) and unpruned (TD U)) as an estimate of the minimum description length for an instance.

##  CL_measure(dataset)
##  *** CL --- Class Likelihood (CL)
##  *** CLD --- Class Likelihood Difference (CLD)

##  MV_measure(dataset)
##  *** MV --- Minority Value (MV)
##  *** CB --- Class Balance (CB)

##  DS_measure(dataset) --- Disjunct Size (DS)

##  DCP_measure(dataset) --- Disjunct Class Percentage (DCP)

## HINTS
## TD: high values, high hardness
## kDN: high values, high hardness
## CL, CLD: low values, high hardness   (THEY ONLY CHANGE SCALE)
## MV: high values, high hardness
## CB: low values, high hardness
## DS: low values, high hardness
## DSP: low values, high hardness

#difficultyMetricNames <- c("kDN", "IDS", "IDCP", "TD_P", "TD_U", "ICL", "ICLD", "MV", "ICP")
# So we have to change sign for "IDS", "IDCP", "ICL", "ICLD", "ICP")
DIFF_INVERTED <- c(2,3,6,7,9)
difficultyMetrics[DIFF_INVERTED, ] <- -difficultyMetrics[DIFF_INVERTED, ] 

AVERAGE_DIFFICULTIES <- FALSE
if (AVERAGE_DIFFICULTIES) {
  diffName <- "COMP"
  DIFF_SELECTION <- c(1,2,5,6)  # The most representative ones
  
  diff <- rep(0,nItems)
  for (i in DIFF_SELECTION) {
    #scale
#    diff <- diff + scale(unlist(difficultyMetrics[i, ]))[,1]
    #rank
    diff <- diff + (rank(unlist(difficultyMetrics[i, ]))-1)/nItems  # By subtracting -1 and dividing by nItems we get ranks between 0 and 1 
  }  
  # average
  difficulties <- diff / length(DIFF_SELECTION)
} else {
  diffName <- difficultyMetricNames[difficultyMetric]
  difficulties <- unlist(difficultyMetrics[difficultyMetric,])
}



if (diffName == "KDN") {
  diffName <- paste0(diffName, KVALUE)
}


if (BIN_DIFFICULTIES) {
  diffName <- paste0(diffName, "binned")
}


  

if (SELECT75) {
  datasetname <- paste0(datasetnamebase, "75", TEST)   # A or B
} else {
  datasetname <- paste0(datasetnamebase, "150")
}


if (ACCURACY_CUT > 0) {
  accCut <- sprintf("MinAcc%1.2f-", ACCURACY_CUT)
  studyName <- paste0(datasetname, ".", accCut, "diff", diffName)
} else {
  studyName <- paste0(datasetname, ".diff", diffName)
}




agentNames <- Df[,1]
responseMatrix <- Df[,2:ncol(Df)]

difficulties


hist(difficulties, breaks=20)
hist(difficulties[1:75], breaks=20)
hist(difficulties[76:150], breaks=20)


# selDifficulties <- 76:150

if (TEST == "A") {
  selDifficulties <- read.csv(paste0(DATADIR, "/Iris_humans_75testA-SELECTED-ITEMS.csv"), header=FALSE)
} else {
  selDifficulties <- read.csv(paste0(DATADIR, "/Iris_humans_75testB-SELECTED-ITEMS.csv"), header=FALSE)
}

selDifficulties <- unlist(selDifficulties)
length(selDifficulties)
hist(difficulties[selDifficulties], breaks=20)


if (SELECT75) {
  responseMatrix <- responseMatrix[,selDifficulties]
  difficulties <- difficulties[selDifficulties]
  nItems <- length(difficulties)
}

if (BIN_DIFFICULTIES) {
  difficulties <- BinDifficulties(difficulties, NBins=9) 
}  


filename <- paste0(studyName, ".histdiff")
OpenPDFEPS(filename, 4, 6)
hist(difficulties, breaks=20)
ClosePDFEPS()


rMeans <- rowMeans(responseMatrix)
hist(rMeans, breaks=50)

filename <- paste0(studyName, ".histrowmean")
OpenPDFEPS(filename, 4, 6)
hist(rMeans, breaks=50)
ClosePDFEPS()


agentNames <- agentNames[rMeans >= ACCURACY_CUT]
responseMatrix <- responseMatrix[rMeans >= ACCURACY_CUT,] 

nAgents <- nrow(responseMatrix)
nAgents
#[1] 1440  (from 4427)
# 414 for the small study
 






###############
# Generality Analysis
################

DO_FA <- (ACCURACY_CUT == 0)  # Only do FA when we have the bad classifiers so we have all columns with some variance.


results <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA= DO_FA, ESTIMATESIGMOID= FALSE) #, DO_IRT = FALSE)


cor(results$irt_difficulties, results$difficulties)
cor(results$irt_difficulties, results$difficulties, method="spearman")

cor(results$irt_difficulties, results$cMeans)
cor(results$irt_difficulties, results$cMeans, method="spearman")


#results <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA= FALSE, ESTIMATESIGMOID= FALSE, DO_IRT = FALSE)

#maxsel <- min(nAgents,1000)
#selection <- 1:maxsel
selection <- 1:length(results$capabilities)
maxsel <- max(selection)

rank <- sort(results$capabilities, index.return=TRUE, decreasing=TRUE)  # best first
#rank <- sort(results$capabilities, index.return=TRUE, decreasing=FALSE) # worst first
indexes_topten_incapability <- rank$ix[selection]  # Top ten in capability
topN <- cbind(agentNames[indexes_topten_incapability],results$capabilities[indexes_topten_incapability], results$spread[indexes_topten_incapability])
topN
#unique(topN)
topNnodup <- topN[!duplicated(topN[,2:3]),]
topNnodup

# numCaps <- length(topNnodup)

if (!require('xtable')) {  
  install.packages('xtable')
  require(xtable)
}  

print(xtable(topNnodup[1:5,], type = "latex"), file = paste0(OUTPUTDIR, "/", studyName, "-BestCap.tex"))

minD <- min(difficulties)

mostcapable <- responseMatrix[indexes_topten_incapability[1],]
filename <- paste0(studyName, ".ACCmostcap")
OpenPDFEPS(filename, 4, 6)
res <- PlotACCwithIndicators(difficulties,unlist(mostcapable),legendposx=minD-0.02,legendpos=0.26)
ClosePDFEPS()

leastcapable <- responseMatrix[indexes_topten_incapability[maxsel],]
filename <- paste0(studyName, ".ACCleastcap")
OpenPDFEPS(filename, 4, 6)
res <- PlotACCwithIndicators(difficulties,unlist(leastcapable),legendposx=minD-0.02,legendpos=0.26)
ClosePDFEPS()


#rank <- sort(results$spreads, index.return=TRUE, decreasing=TRUE)  # best first
rank <- sort(results$spreads, index.return=TRUE, decreasing=FALSE) # worst first
indexes_topten_inspread <- rank$ix[selection]  # Top ten in generality
topN <- cbind(agentNames[indexes_topten_inspread],results$capabilities[indexes_topten_inspread], results$spread[indexes_topten_inspread])  
topN
#unique(topN)
topNnodup <- topN[!duplicated(topN[,2:3]),]
topNnodup

print(xtable(topNnodup[1:5,], type = "latex"), file = paste0(OUTPUTDIR, "/", studyName, "-BestGen.tex"))

#numGens <- length(topNnodup)

results$spread[indexes_topten_inspread[1]]
agentNames[indexes_topten_inspread[1]]
mostgeneral <- responseMatrix[indexes_topten_inspread[1],]
filename <- paste0(studyName, ".ACCmostgen")
OpenPDFEPS(filename, 4, 6)
res <- PlotACCwithIndicators(difficulties,unlist(mostgeneral),legendposx=minD-0.02,legendpos=0.26)
ClosePDFEPS()

results$spread[indexes_topten_inspread[maxsel]]
agentNames[indexes_topten_inspread[maxsel]]
leastgeneral <- responseMatrix[indexes_topten_inspread[maxsel],]
filename <- paste0(studyName, ".ACCleastgen")
OpenPDFEPS(filename, 4, 6)
res <- PlotACCwithIndicators(difficulties,unlist(leastgeneral),legendposx=minD-0.02,legendpos=0.26)
ClosePDFEPS()



all <- cbind(agentNames,results$capabilities, results$spread)  
colnames(all)[2:3] <- c("capabilities", "spread")
write.csv(all, paste0(OUTPUTDIR, "/", studyName, ".csv"))

















###############
# Humans
###############

# USE DATA WITH 2 HUMANS (BETTER CONTROLLED EXPERIMENT) OR 5 HUMANS (HOME-MADE)
# DfHumans <- read.csv(paste0(DATADIR, "/results_Iris_2x150-HUMANS.csv"))
DfHumans <- read.csv(paste0(DATADIR, "/results_Iris_5x150-HUMANS.csv"))


summary(DfHumans)
DfHumans[1,]
nHumans <- nrow(DfHumans)  # 2 OR 5
ncol(DfHumans)  # 151 - 1 for names (first column) = 150


agentNames <- c(agentNames, DfHumans[,1])
length(agentNames)


sum(DfHumans[1,2:ncol(Df)])
sum(DfHumans[2,2:ncol(Df)])
sum(DfHumans[3,2:ncol(Df)])
sum(DfHumans[4,2:ncol(Df)])
sum(DfHumans[5,2:ncol(Df)])

rmHumans <- DfHumans[,2:ncol(Df)]
if (SELECT75) {
  rmHumans <- rmHumans[,selDifficulties]
} 

responseMatrix <- rbind(responseMatrix, rmHumans)


#difficulties <- runif(length(difficulties))

nAgents <- nrow(responseMatrix)
ncol(responseMatrix)


studyNameWithHumans <- paste0(studyName, "-whumans")

results <- PerformFullStudy(studyNameWithHumans, responseMatrix, difficulties, DO_FA= FALSE, ESTIMATESIGMOID= FALSE)

results$capabilities[(nAgents-nHumans+1):nAgents]

###############
# Better capability-vs-spread plot for all
###############

ANONYMOUS_HUMANS <- TRUE
# Names from the file
humanNames <- as.character(DfHumans[,1])

if (ANONYMOUS_HUMANS) {
  # Or anonymous names
  humanNames <- NULL
  for (i in 1:nHumans) {
    humanNames <- c(humanNames, paste0("human", i)) #c("human1", "human2", "human3", "human4", "human5")
  }
}

legendtext=c(rep("machine classifiers", nAgents-nHumans), humanNames[1:nHumans])
pch = c(rep(1, nAgents-nHumans), 2:(2+nAgents-1))
col = c(rep("black", nAgents-nHumans), 2:(2+nAgents-1))

if (AVERAGE_DIFFICULTIES) {
  print("Averaged difficulties with ranks go from 0 to 1, so we take this range")
  minDiff <- 0
  maxDiff <- 1
} else {
  minDiff <- min(difficulties)
  maxDiff <- max(difficulties)
}

filename <- paste0(studyNameWithHumans, ".capability-vs-spread-BETTER")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(results$capabilities,results$spreads, minDiff, maxDiff, legendtext=legendtext, legendpos="topright", pch = pch, col = col)
#PlotCapabilityVsSpread(results$capabilities,results$spreads, minDiff, maxDiff, legendtext=legendtext, legendpos="topright", pch = pch, col = col, maxylim=0.82)
ClosePDFEPS()

hist(difficulties)


