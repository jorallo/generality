##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO THE C-TEST DATA (ctest_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis, based on the metric of generality
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    29 Mar 2019. First operative version: only for the mean result.
#  - V.1.1    23 June 2019. Now we include the detailed data for the 48 students and 35 items
#  - V.1.2    30 June 2019. Plots all individual ACCs
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################



setwd("/.../ **** PUT YOUR FOLDER HERE ****")
source("generality.R")

DATADIR <- "ctest.data"
OUTPUTDIR <- "ctest.results"





################################################################################
################################################################################
# AGGREGATED RESULTS ###########################################################
################################################################################
################################################################################



HitRatio <- c(0.890625,0.590277778,0.652777778,0.40625,0.28125,0.3125,0.177083333,0.052083333)
h <- 1:length(HitRatio)
START_DIFF <- 6
h <- h + START_DIFF # Starts in 7
plot(h,HitRatio, pch=15, col="blue", ylim=c(0,1), yaxt="n", mgp=c(2,0.5,0))
mylaby <- seq(0,1,0.2)
axis(2, at=mylaby,labels=mylaby, las=2, mgp=c(2,0.6,0))
lines(h,HitRatio, pch=15, col="blue")


filename <- "ctest-humans-aggregatedACC"
OpenPDFEPS(filename, 4, 6)

difficulties <- h
responses <- HitRatio
KMIN <- 0
KMAX <- max(difficulties) + 10

#r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE) 

r <- OptimisticallyExtrapolateACC(difficulties, responses, KMIN, KMAX)
r <- PlotACCwithIndicators(r$difficulties,r$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
unlist(r)

ClosePDFEPS()





################################################################################
################################################################################
# INDIVIDUAL RESULTS FOR 48 STUDENTS : BOTH INDUCTIVE AND ABDUCTIVE #############
################################################################################
################################################################################

###############
# Loading data
###############

Df <-  read.csv(paste0(DATADIR, "/ctest-data.csv"))

nHumans <- nrow(Df)-1
items <- 6:40   # Induction and abduction
nItems <- length(items)
summary(Df)
Df[1,]

difficulties <- unlist(Df[1,items])
responses <- Df[2:(nHumans+1),items]
IQs <- Df[2:(nHumans+1),5]


#BINARISE_RESPONSES <- "binary"
BINARISE_RESPONSES <- "nonbinary"

if (BINARISE_RESPONSES == "binary") {
  # First Option (but some columns would always be 0 and no variance)
  responseMatrix <- round(responses)  # We convert -0.25 to 0. So it's the same to leave it unanswered than to answer wrong
} else {
  # Second Option
  responseMatrix <- (responses + 0.25) / 1.25  # We put it between 0 and 1
}

responseMatrix



magic <-  read.csv(paste0(DATADIR, "/magichaskeller.csv"))
#length(magic[,3])

if (BINARISE_RESPONSES == "binary") {
  responseMatrix[nHumans+1,] <- round(magic[,3])
} else {
  responseMatrix[nHumans+1,] <- magic[,3] # It's already between 0 and 1... No need to normalise: (magic[,3] + 0.25) / 1.25  # We put it between 0 and 1
}


other <-  read.csv(paste0(DATADIR, "/othermethods.csv"))
otherIdxs <- 2:12
nOther <- length(otherIdxs)
for (i in otherIdxs) {
  if (BINARISE_RESPONSES == "binary") {
    r <- round(other[,i])
  } else {
    r <-  other[,i]  # It's already between 0 and 1... No need to normalise: (other[,i]+ 0.25) / 1.25
  }
  responseMatrix[nHumans+i,] <- r
}

responseMatrix

nAll <- nHumans + 1 + length(otherIdxs)


###############
# We print the ACC for all agents
###############

myNames <- names(other[otherIdxs])
myNamesComplete <- c("magic",myNames)

for (i in 1:nrow(responseMatrix)) {
  if (i <= nAll) {
    filename <- paste0("ctestACC-human", i, "-", BINARISE_RESPONSES)
  }  else {
    filename <- paste0("ctestACC-", myNamesComplete[i-nAgents], "-", BINARISE_RESPONSES)
  }
  OpenPDFEPS(filename, 4, 6)
  r <- PlotACCwithIndicators(difficulties,unlist(responseMatrix[i,]),xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, EXTEND_CURVE= TRUE) #, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
  ClosePDFEPS()
}


###############
# We now make the analysis
###############



studyName <- paste0("ctest.humans.", BINARISE_RESPONSES)

if (BINARISE_RESPONSES == "binary") {
  DO_FA <- FALSE
} else {
  DO_FA <- TRUE
}

results <- PerformFullStudy(studyName, responseMatrix[1:nHumans,], difficulties, DO_FA= DO_FA) #, DO_IRT = FALSE)
rMeans <- rowMeans(responseMatrix[1:nHumans,])
cor(rMeans, IQs)
cor(results$capabilities, IQs)
cor(results$spreads, IQs)

cor(results$irt_difficulties, results$difficulties)
cor(results$irt_difficulties, results$difficulties, method="spearman")

cor(results$irt_difficulties, results$cMeans)
cor(results$irt_difficulties, results$cMeans, method="spearman")

studyName <- paste0("ctest.machines.", BINARISE_RESPONSES)

results <- PerformFullStudy(studyName, responseMatrix[(nHumans+1):nAll,], difficulties, DO_FA= DO_FA) #, DO_IRT = FALSE)


studyName <- paste0("ctest.all.", BINARISE_RESPONSES)

results <- PerformFullStudy(studyName, responseMatrix, difficulties) #, DO_IRT = FALSE)


###############
# Better capability-vs-spread plot for all
###############

legendtext <- c(rep("Humans",nHumans), "magic")
pch=c(rep(1,nHumans),2)
col=c(rep("black",nHumans), "orange")

legendtext <- c(legendtext, myNames)
pch <- c(pch, 3:(3+nOther-1))
colBase <- c(7:8, "brown", 10:14, "orange")
col <- c(col, rep(colBase,10))

filename <- paste0(studyName, ".capability-vs-spread-WITH-LABELS")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(results$capabilities,results$spreads, min(difficulties), max(difficulties), legendtext=legendtext, legendpos="topright", pch = pch, col = col)
ClosePDFEPS()



















################################################################################
################################################################################
# INDIVIDUAL RESULTS FOR 48 STUDENTS :ONLY INDUCTIVE ITEMS        #############
################################################################################
################################################################################

#items <- 6:25   # Only induction
items <- 1:20
nItems <- length(items)

difficulties <- difficulties[items] # unlist(Df[1,items])
responseMatrix <- responseMatrix[1:nAll, items]


studyName <- paste0("ctest-inductive.humans.", BINARISE_RESPONSES)

results <- PerformFullStudy(studyName, responseMatrix[1:nHumans,], difficulties, DO_FA= DO_FA) #, DO_IRT = FALSE)
#cor(results$capabilities, IQs)
#cor(results$spreads, IQs)


studyName <- paste0("ctest-inductive.machines.", BINARISE_RESPONSES)

results <- PerformFullStudy(studyName, responseMatrix[(nHumans+1):nAll,], difficulties, DO_FA= DO_FA) #, DO_IRT = FALSE)



studyName <- paste0("ctest-inductive.all.", BINARISE_RESPONSES)

results <- PerformFullStudy(studyName, responseMatrix, difficulties) #, DO_IRT = FALSE)



###############
# Better capability-vs-spread plot for all
###############

filename <- paste0(studyName, ".capability-vs-spread-WITH-LABELS")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(results$capabilities,results$spreads, min(difficulties), max(difficulties), legendtext=legendtext, legendpos="topright", pch = pch, col = col)
ClosePDFEPS()


  


