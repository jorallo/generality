##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO Orangutan Data (Damerious et al.) (damerius-burkart-etal_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis, based on the metric of generality
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#   
#   Data provided by Judith Burkart, from:
#   Laura A. Damerius, Judith M. Burkart, Maria A. van Noordwijk, Daniel B.M. Haun, Zaida K. Kosonen, Biruté M.F. Galdikasd Yenny Saraswati, Denny Kurniawan, Carel P. van Schaik
#   "General cognitive abilities in orangutans (Pongo abelii and Pongo pygmaeus)"
#
#
# LICENCE:
#   GPL (except the data)
#
# VERSION HISTORY:
#  - V.1.0    15 Apr 2019. First operative version
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


setwd("/.../ **** PUT YOUR FOLDER HERE ****")
source("generality.R")


DATADIR <- "damerius-burkart-etal.data"
OUTPUTDIR <- "damerius-burkart-etal.results"

studyName <- "damerius-burkart"




##############################################################################
####################### READING THE DATA #####################################
##############################################################################

dat = read.csv(paste0(DATADIR,"/damerius-burkart-etal_with_S1.csv"), header = TRUE)

names(dat) <- c("Nr.", "Name", "Flexibility", "Detour", "Tool", "Reversal", "Causal", "Sex", "Age", "Species", "Background", "Dataset")
names <- dat[,"Name"]
names <- unlist(lapply(names, toString))  # As a vector of strings
head(dat)
nAgents <- nrow(dat)

itemColumns <- 3:7 # "Flexibility", "Detour", "Tool", "Reversal", "Causal"



##############################################################################
############## METHOD 4: STEPWISE POPULATIONAL ##############################
##############################################################################

# NOW WE ADDRESS METHOD 4 (STEPWISE POPULATIONAL CASE)

# Home-made: We do it without using the generality library

dat[1]  # Number
dat[2]  # Name

dat[1,]  # "Amin"
dat[,3]  # Flexiblity

# We calculate ranks
rdat0 <- dat
for (i in itemColumns) { # "Flexibility", "Detour", "Tool", "Reversal", "Causal"
  rdat0[i] <- rank(t(dat[i]))
}

head(rdat0)
summary(rdat0)

NORMALISE <- TRUE

if (NORMALISE) {
  rdat <- rdat0
  rdat[itemColumns] <- (rdat[itemColumns]-1)/(nAgents-1)   # We convert ranks to 0 .. N-1 and then divide by N-1
#  rdat[itemColumns] <- (rdat[itemColumns])/(nAgents)   # We convert ranks to 1 .. N and then divide by N
  N <- 1
} else {
  rdat <- rdat0
  N <- nAgents
} 

rdat[,3]  # Flexiblity : Amin: 0.85576923
rdat[,4]
rdat[,5]
rdat[,6]
rdat[,7]
rdat[1,itemColumns] # "Amin"
#  Flexibility    Detour      Tool  Reversal    Causal
#   0.8557692 0.6153846 0.9134615 0.2307692 0.6442308

means <- apply(rdat[,itemColumns],1,mean)  # 1: rowwise
sds <- apply(rdat[,itemColumns],1,popsd)
sds
gen <- 1/sds

means[1] #Amin  # 0.6519231
sds[1]          # 0.2403


x <- seq(0,N, length.out = 1000)
x_norm <- x / N
max_sd <- sqrt(x_norm*(1-x_norm))*N


pchs <- c(2,3,3,4,4,5,5,6,6,6,6)
cols <- c("orange", "blue", "pink", "green", "brown", "yellow", "olivedrab", "khaki2", "violet", "cyan", "grey")
groupNames <- c("all", "males", "females", "age<10", "age>=10", "abelli", "pigmaeus", "bkg-human", "bkg-station", "bkg-wild", "bkg-unknown")
groupIdxs <- NULL

groupIdxs[[1]]<- rep(TRUE, length(means))
males <- (rdat[,"Sex"]=="male")
groupIdxs[[2]] <- males
females <- (rdat[,"Sex"]=="female")
groupIdxs[[3]] <- females

young <- (rdat[,"Age"]<10)
groupIdxs[[4]] <- young
old <- (rdat[,"Age"]>=10)
groupIdxs[[5]] <- old

abelii <- (rdat[,"Species"]=="Pongo abelii")
groupIdxs[[6]] <- abelii
pygmaeus <- (rdat[,"Species"]=="Pongo pygmaeus")
groupIdxs[[7]] <- pygmaeus

human <- (rdat[,"Background"]=="human")
groupIdxs[[8]] <- human
station <- (rdat[,"Background"]=="station")
groupIdxs[[9]] <- station
wild <- (rdat[,"Background"]=="wild")
groupIdxs[[10]] <- wild
unknown <- (rdat[,"Background"]=="unknown")
groupIdxs[[11]] <- unknown


plot(means, sds, xlim = c(0,N), ylim = c(0,max(max_sd)), pch=1, cex=0.5, xlab = "performance", ylab = "1/generality", col="white")

text(means, sds, rdat[,"Name"], cex=0.55, col="gray90") #, adj=0.5)
#points(means, sds, pch=1, cex=0.5)

lines(x, max_sd)
for (i in 1:length(pchs)) {
  points(mean(means[groupIdxs[[i]]]), mean(sds[groupIdxs[[i]]]), col=cols[i], pch=pchs[i], cex=0.75)
}

legend("topright", pch = pchs, legend = groupNames, col=cols, cex=0.75)




##############################################################################
############## METHOD 4: STEPWISE POPULATIONAL ##############################
##############################################################################

# NOW WE USE THE generality LIBRARY


df <- dat[,itemColumns]  # We just keep the item results columns to have a proper item-respondent matrix

nrow(df)
ncol(df)
summary(df)

# Factor analysis
fit <- PerformFAandPCA(df)
FAloadings <- fit$loadings
FAVaccounted <-  mean(fit$communalities)
cat(paste0("The mean of the FA loadings is ", mean(FAloadings), " and the accounted variance is: ", FAVaccounted, "\n"))



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
#    r <- GenerateStepwiseResponsesAndDifficultiesFromOrder(df[j,i], df[,i], nDifficulties=nDifficulties, METHOD=METHOD, BREAK_TIES= FALSE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5, AVERAGE_RANK=FALSE)
    # 1 for i=1, j=1
#    r <- GenerateStepwiseResponsesAndDifficultiesFromOrder(df[j,i], df[,i], nDifficulties=nDifficulties, METHOD=METHOD, BREAK_TIES= FALSE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5, AVERAGE_RANK=TRUE)
    # 0.8490566 for i=1, j=1
    r <- GenerateStepwiseResponsesAndDifficultiesFromOrder(df[j,i], df[-j,i], nDifficulties=nDifficulties, METHOD=METHOD, BREAK_TIES= FALSE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5, AVERAGE_RANK=TRUE)
    # 0.8557692 for i=1, j=1
    # 0.8557692 with the rank method above!
#    me <- 0
#    for (k in 1:10000) {
#      r <- GenerateStepwiseResponsesAndDifficultiesFromOrder(df[j,i], df[,i], nDifficulties=nDifficulties, METHOD=METHOD, BREAK_TIES= TRUE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5, AVERAGE_RANK=FALSE)
#      me <- me + r$ability
#    }
#     me / 10000  # 0.849 for i=1, j=1, 0.8555 if we remove -j.
    
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

ret <- PlotCapabilityVsSpread(r$capabilities,r$spreads,  MINCAP=KMIN, MAXCAP=KMAX, legendtext=names, legendpos="topright", pch=1:nAgents, col=1:nAgents, plotNames=TRUE)
legendposx= NULL
legendposy <- 0.79
sizeLegend <- 0.4
for (i in 1:length(pchs)) {
  idxs <- groupIdxs[[i]]
  legendposx <- 0.905
  ret <- PlotCapabilityVsSpread(mean(r$capabilities[idxs]),mean(r$spreads[idxs]),  MINCAP=KMIN, MAXCAP=KMAX, legendpos="topright", legendtext=groupNames[i], legendposx=legendposx, legendposy=legendposy, pch=pchs[i], col=cols[i], sizeLegend = sizeLegend, startplot=FALSE, SHOW_ASYMPTOTES=FALSE, SHOW_METRICS=FALSE)
  legendposx= ret$legendposx
  legendposy= ret$legendposy
}

ClosePDFEPS()



studyName <- paste0(studyName3, ".", "gender")
filename <- sprintf("%s.histspreads", studyName)
OpenPDFEPS(filename, 4, 6)
PlotSplitHistograms(r$spreads[groupIdxs[[1]]], r$spreads[groupIdxs[[2]]], r$spreads[groupIdxs[[3]]])
#, mean(rnew$FAloadings), rnew$FAVaccounted, mean(rnewa$FAloadings), rnewa$FAVaccounted, mean(rnewb$FAloadings), rnewb$FAVaccounted) 
ClosePDFEPS()




# SLODR ANALYSIS

chosen <- (r$capabilities > quantile(r$capabilities, 0.5))  # selecting by capability
sum(chosen)  # 26 out of 53

responseMatrixHigh <- responseMatrix[chosen,]
studyName <- paste0(studyName3, ".", "highcapability")

rhigh <- PerformFullStudy(studyName, responseMatrixHigh, difficulties, DO_FA=FALSE, DO_IRT=FALSE, ESTIMATESIGMOID= FALSE,   CHOSEN_SUBJECTS = c(1,2,3))

responseMatrixLow <- responseMatrix[!chosen,]
studyName <- paste0(studyName3, ".", "lowcapability")

rlow <- PerformFullStudy(studyName, responseMatrixLow, difficulties, DO_FA=FALSE, DO_IRT=FALSE, ESTIMATESIGMOID= FALSE,   CHOSEN_SUBJECTS = c(1,2,3))


mean(r$capabilities)
mean(rhigh$capabilities) 
mean(r$capabilities[chosen])  # The same as above
mean(rlow$capabilities) 
mean(r$capabilities[!chosen])  # The same as above

mean(r$spreads)
mean(rhigh$spreads) 
mean(r$spreads[chosen])  # The same as above
mean(rlow$spreads)  
mean(r$spreads[!chosen]) # The same as above


studyName <- paste0(studyName3, ".", "capabilities") # SUBGROUP,
filename <- sprintf("%s.histspreads", studyName)
OpenPDFEPS(filename, 4, 6)
PlotSplitHistograms(r$spread, rlow$spreads, rhigh$spreads) #,  mean(r$FAloadings), r$FAVaccounted, mean(rlow$FAloadings), rlow$FAVaccounted, mean(rhigh$FAloadings), rhigh$FAVaccounted) 
ClosePDFEPS()






