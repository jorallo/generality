##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO DATA ABOUT RAT PERFORMANCE ON  SPAN DATA (odorspan_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis, based on the metric of generality
#
# Data from experiment 2:
# L. Brooke April, Katherine Bruce, Mark Galizio "The magic number 70 (plus or minus 20): Variables determining performance in the Rodent Odor Span Task", Learning and Motivation 44 (2013) 143- 158
#
# FINAL TABLE 10-TRIAL BLOCKS WITH TRIALS 2-61 AND AVERAGING FOR 36-48-72 VERSIONS OF THE EXPERIMENT WHEN AVAILABLE
#
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    4 June 2020. First operative version
#  - v.1.1   27 June 2020. We add experiments with the 71 trials
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


setwd("/.../ **** PUT YOUR FOLDER HERE ****")
source("generality.R")

DATADIR <- "odor.span.task.data"
OUTPUTDIR <- "odor.span.task.results"









################################################################################
################################################################################
# INDIVIDUAL RESULTS FOR 10 RATS   #############
################################################################################
################################################################################

###############
# Loading data
###############

NO_TRIALS <- 70

if (NO_TRIALS == 60) {
  Df <-  read.csv(paste0(DATADIR, "/odor.span.task.60trials.csv"), header= TRUE)
} else {
  Df <-  read.csv(paste0(DATADIR, "/odor.span.task.70trials.csv"), header= TRUE)

}

nRats <- nrow(Df)
#items <- 2:7 
items <- 2:(NO_TRIALS/10 + 1) 
nItems <- length(items)
summary(Df)
Df[1,]

ADD_ZERODIFF <- TRUE  # Adds 0 difficulty

#difficulties <- seq(10, 60, 10)
#difficulties <- seq(5.5, 55.5, 10)  # The block 1 to 10 should have mean number of seen scents equal to 5.5
difficulties <- seq(5.5, NO_TRIALS - 4.5, 10)  # The block 2 to 11 should have mean number of *seen* scents equal to 5.5: (11+2)/2-1

responseMatrix  <- Df[1:nRats,items]

if (ADD_ZERODIFF) {
  difficulties <- c(0,difficulties)
  nItems <- nItems + 1
  r <- responseMatrix
  cn <- colnames(r)
  r$X0 <- rep(1,10)      # 1 for 100% accuracy
  r <- r[, c("X0", cn)]
  responseMatrix <- r
}

DIVIDE_ACCURACIES <- TRUE # curves should go down to 0.5, which is the random expectancy
if (DIVIDE_ACCURACIES) {
  responseMatrix <- (responseMatrix - 0.5) * 2
}
  



KMIN <- 0
KMAX <- 120


###############
# We print the ACC for all agents
###############

#FINAL TABLE 10-TRIAL BLOCKS WITH TRIALS 2-61 AND AVERAGING FOR 36-48-72 VERSIONS OF THE EXPERIMENT WHEN AVAILABLE

for (i in 1:nrow(responseMatrix)) {
  filename <- paste0("odorspan-36-48-72odours-", NO_TRIALS, "trials-", Df[i,1])
  OpenPDFEPS(filename, 4, 6)
  r <- PlotACCwithIndicators(difficulties,unlist(responseMatrix[i,]),xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, EXTEND_CURVE= TRUE) #, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
  ClosePDFEPS()
}


###############
# We now make the analysis
###############



studyName <- paste0("odorspan-36-48-72odours-", NO_TRIALS, "trials")

results <- PerformFullStudy(studyName, responseMatrix[1:nRats,], difficulties, DO_FA= FALSE, DO_IRT = FALSE)
rMeans <- rowMeans(responseMatrix[1:nRats,])



###############
# Better capability-vs-spread plot for all
###############

legendtext <- Df[,1]
pch=1:nRats
col=1:nRats

filename <- paste0(studyName, ".capability-vs-spread-WITH-LABELS")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(results$capabilities,results$spreads, min(difficulties), max(difficulties), legendtext=legendtext, legendpos="topright", pch = pch, col = col)
ClosePDFEPS()



###############
# Adding the means (adds a 11th rat as the mean)
###############

nRats <- nRats + 1
responseMatrix[nRats,] <- colMeans(responseMatrix)

studyName <- paste0("odorspan-36-48-72odours-", NO_TRIALS, "trials-withmeans")
results <- PerformFullStudy(studyName, responseMatrix[1:nRats,], difficulties, DO_FA= FALSE, DO_IRT = FALSE)
rMeans <- rowMeans(responseMatrix[1:nRats,])

legendtext <- c(sapply(legendtext, toString), "mean")
pch=1:nRats
col=1:nRats

filename <- paste0(studyName, ".capability-vs-spread-WITH-LABELS")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(results$capabilities,results$spreads, min(difficulties), max(difficulties), legendtext=legendtext, legendpos="topright", pch = pch, col = col)
ClosePDFEPS()







###############
# A calculation of 72 trials, when old scent is taken from the previous ones
###############

#In experiment 2, assuming that one picks a seen odour randomly to compare with the new odour, we have the following distribution.

# Case 1: Always baited
# Case 2: c(1,0,...)
# Case 3: 1/2 probably for the first and the second odours c(1.5, 0.5, ...)

N <- 72
v <- rep(0,N)
for (i in 2:N) {
  for (j in 1:i) {
    v[j-1] <- v[j-1] + 1/(i-1)
  }    
}
v
mean(v)
plot(v)

"First, the use of only two comparison stimuli may have
enhanced performance; recall that in Experiment 1, accuracy was consistently higher and the impact of the memory load
was lower with two comparisons. A second factor that appears to be critical is the extended training of the rats in Experiment
2. The number of OST training sessions for rats in Experiment 2 ranged from 88 to 250, and this extensive training is markedly
different from most published OST studies and from Experiment 1 in which much less training was administered. This point
begs the question of just what types of stimulus control might develop with extended training in the OST that would permit
such accurate performance with so many stimuli to remember and over such an extended temporal interval."
"One possibility
is that rats can recognize the relative familiarity of odors in some fashion that is largely independent of the number of stimuli
to remember. In other words, within a given session, choices may involve an assessment of the "newness" or "oldness" of
a stimulus in relation to the other stimuli present on a given trial, with the ultimate choice being made towards the least
familiar option."

"However, it is clear from the high levels of accuracy observed in Experiment 2 that the capacity for remembering odors in
well-trained rats is not limited to 72 stimuli. There may well be an upper limit to the number of odors rats can remember in
the OST, but if so, it was not reached in the present study."

"Indeed, the very high capacities displayed in the present study suggest that the OST is measuring a type of remembering
that is quite different from that of the classic working memory tasks used with human participants"


# SEPARATE D2, E1, E5, F11, F16; They have seen the odours before








