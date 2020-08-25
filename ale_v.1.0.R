##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO ALE DATA (Atari Games) (ale_vN.N.R)
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
#   Data provided by Fernando Martinez-Plumed, collected from some papers
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    9 Apr 2019. First operative version
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


setwd("/OneDrive - UPV/__SUBMISSIONS/2018/General intelligence/__NewCode__")
source("generality.R")


DATADIR <- "ale.data"
OUTPUTDIR <- "ale.results"

studyName0 <- "ale.noop"




##############################################################################
####################### READING THE DATA #####################################
##############################################################################



# Data from our paper: 
# Fernando Martinez-Plumed ; Jose Hernandez-Orallo "Dual Indicators to Analyse AI Benchmarks: Difficulty, Discrimination, Ability and Generality"
# 10.1109/TG.2018.2883773
# and the original papers
# and Table 5 from
# https://openreview.net/pdf?id=rkHVZWZAZ. 


#SYSTEMS <- "withoutRANDOM18"
SYSTEMS <- "withRANDOM24"  # 
#SYSTEMS <- "withoutRANDOM23"  # 

if (SYSTEMS == "withoutRANDOM18") {
  df <- read.csv(paste0(DATADIR,"/ALE_clean_18x45.csv"))
  studyName <- paste0(studyName0, "18")
  sizeLegend <- 0.45
} else if (SYSTEMS == "withRANDOM24") {
  df <- read.csv(paste0(DATADIR,"/ALE_clean_24x45.csv"))
  studyName <- paste0(studyName0, "24")
  sizeLegend <- 0.45 # as we are using splitLegend=TRUE, otherwise it should be smaller
} else if  (SYSTEMS == "withoutRANDOM23") {
  df <- read.csv(paste0(DATADIR,"/ALE_clean_24x45.csv"))
  studyName <- paste0(studyName0, "23")
  sizeLegend <- 0.45 # as we are using splitLegend=TRUE, otherwise it should be smaller
} else {
  ERROR()
}

nrow(df)  # Should be 18 or 24
ncol(df)  # 19 or 46 originally
row.names(df) <- df[, "name"]  # Use the column with the names for the row names
df <- df[,-1] # Remove the column with the names
row.names(df)  # 18 or 24 systems
colnames(df)  # 45 games


if  (SYSTEMS == "withoutRANDOM23") {
  rowsToKeep <- (rownames(df) != "random noop")
  df <- df[rowsToKeep,]
  row.names(df)  # 23 systems
}

# DATA READY!



##############################################################################
################## FA/PCA ANALYSIS ON THE ORIGINAL DATA ######################
##############################################################################



# FA BEFORE WE BINARISE

# WE SHOULDN'T INCLUDE THE RANDOM SYSTEM HERE AS IT WILL AFFECT THE LOADINGS! With random it gets better as it is a consistently bad system

fit <- PerformFAandPCA(df)
FAloadings <- fit$loadings
FAVaccounted <-  mean(fit$communalities)
cat(paste0("The mean of the FA loadings is ", mean(FAloadings), " and the accounted variance is: ", FAVaccounted, "\n"))




##############################################################################
######## METHOD 3A: REFERENCE POPULATIONAL (USING HUMANS OR MEAN) ############
##############################################################################

# NOW WE BINARISE USING A THRESHOLD

REFERENCE <- "HUMAN"
#REFERENCE <- "MEAN"
#REFERENCE <- "MEDIAN"
if (REFERENCE == "HUMAN") {
  REF_METHOD <- "COLUMN"
  REF_ROW <- "human.noop"  # row 10
} else  {
  REF_METHOD <- REFERENCE
  REF_ROW <- NULL
} 


#COMPARISON <- "GREATER_OR_EQUAL"
#COMPARISON <- "GREATER"
COMPARISON <- "GREATER_BUTEQUALHALF"

if (COMPARISON == "GREATER_BUTEQUALHALF") {
  nnn <- "-EQUALHALF" 
} else {
  nnn <- ""
}

studyName2 <- paste0(studyName, nnn, ".ref", REFERENCE)

  


r <- GenerateBinarisedResponsesAndDifficultiesFromCrispReference(df, REF_METHOD=REF_METHOD, REF_ROW= REF_ROW, COMPARISON = COMPARISON)
dfBinary <- r$dfBinary
difficulties <- r$difficulties

dfBinary[,"Video.Pinball"]  # An easy one
dfBinary[,"Bowling"]  # A difficult one (no one reaches human performans)

names <- NULL
capabilities <- NULL
spreads <- NULL
meansBinary <- NULL
variancesBinary <- NULL
means <- NULL
variances <- NULL
nAgents <- nrow(dfBinary)  
for (i in 1:nAgents) {
  name <- row.names(dfBinary[i,])
  responses <- as.numeric(unname(dfBinary[i,]))
#  responses
  r <- PlotACCwithIndicators(difficulties,responses,kmin=0,kmax=2, SHOW_UNMERGED_POINTS= FALSE, SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, startplot= (i==1), mycol=i, LEGEND= FALSE, LEGEND2 = TRUE, legendpos=1-0.05*(i-1), mypch= ".", name= name)
  unlist(r)
  names[i] <- name
  capabilities[i] <- r$capability
  spreads[i] <- r$spread
  meansBinary[i] <- r$mean
  variancesBinary[i] <- r$variance
  means[i] <- mean(as.numeric(unname(df[i,])))
  variances[i] <- popvar(as.numeric(unname(df[i,])))
}

# For comparison. Meaningless, as is dominated by a Bernouilli
plot(meansBinary, variancesBinary, xlim=c(0,1), pch=1:nAgents, col=1:nAgents)

# For comparison, what we would see if we didn't do generality analysis
filename <- paste0(studyName,".scoremean-vs-scorevar")
OpenPDFEPS(filename, 4, 6)
plot(means, variances, pch=1:nAgents, col=1:nAgents, xlab= "mean(score)", ylab= "var(score)")
legend("topleft", legend=names, pch=1:nAgents, col=1:nAgents, cex=0.5)
ClosePDFEPS()

# The right thing :-)
filename <- paste0(studyName2,".capability-vs-spread-BETTER")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(capabilities,spreads, 0, 1, legendtext=names, legendpos="topright", pch=1:nAgents, col=1:nAgents, sizeLegend = sizeLegend, splitLegend=TRUE)
ClosePDFEPS()




r <- PerformFullStudy(studyName2, dfBinary, difficulties, DO_FA = FALSE, DO_IRT = FALSE, ESTIMATESIGMOID= FALSE,   CHOSEN_SUBJECTS = c(1,4,10))

# We remove Krull as it is above humans for all systems and doesn't allow IRT
#columnsToKeep <- setdiff(colnames(dfBinary),"Krull")
columnsToKeep <- (colnames(dfBinary) != "Krull")
dfBinary2 <- dfBinary[, columnsToKeep]
difficulties2 <- difficulties[columnsToKeep]
shiftDiff <- 0  # We do this to avoid negative difficulties  # Now it works ok if we put shift 0 as the calculations now work for negative difficulties

centreDiffScale <- mean(difficulties2)+shiftDiff
devDiffScale = sd(difficulties2)

rirt <- PerformFullStudy(studyName2, dfBinary2, difficulties=NULL, centreDiffScale = centreDiffScale, devDiffScale = devDiffScale, DO_FA = TRUE, DO_IRT = TRUE, ESTIMATESIGMOID= FALSE,   CHOSEN_SUBJECTS = c(1,4,10))  # Same but with IRT difficulties  # Same but with IRT difficulties

# Limits for capabilities and spreads for IRT difficulties?
summary(rirt$irt_difficulties)


# Nicer plot
filename <- paste0(studyName2,".capability-vs-spread-IRTdiff-BETTER")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(rirt$capabilities,rirt$spreads, min(0, min(rirt$capabilities)), max(1.5, max(rirt$capabilities)), legendtext=names, legendpos="topright", pch=1:nAgents, col=1:nAgents, sizeLegend = sizeLegend,  SHOW_ASYMPTOTES=FALSE, splitLegend=TRUE)
ClosePDFEPS()






##############################################################################
############## METHOD 4: STEPWISE POPULATIONAL ##############################
##############################################################################

# NOW WE ADDRESS METHOD 4 (STEPWISE POPULATIONAL CASE

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
PlotCapabilityVsSpread(r$capabilities,r$spreads,  MINCAP=KMIN, MAXCAP=KMAX, legendtext=names, legendpos="topright", pch=1:nAgents, col=1:nAgents, sizeLegend = sizeLegend, splitLegend=TRUE)
ClosePDFEPS()

