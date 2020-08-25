##############################################################################
#
#                       MAZES: GENERALITY ANALYSIS (mazes_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis of the mazes data, based on the metric of generality first introduced in 
#   J. Hernandez-Orallo "I.G.", March 15th, 2018
#   https://riunet.upv.es/bitstream/handle/10251/100267/secondbest.pdf
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
# The data from mazes (including difficulty) and part of the code for loading and processin gthe data has been adapted from Bao Sheng Loe, University of Cambridge.
#
# LICENCE:
#   GPL (except for the mazes data, which is property of Bao Sheng Loe)
#
# VERSION HISTORY:
#  - v.0.1 - 0.9 Early versions developed during 2018
#  - V.1.0    27 Mar 2019. First operative version in a standalone way
#  - V.1.1    17 June 2019. Includes new partitions and binning
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


# This would be needed for np, mazeDiff, etc. which are not used
#if (!require('mazeGen')) { 
#  install.packages('mazeGen')
#  require(mazeGen)
#}

# nodePosition <- np(rank=16,satPercent=0.3,seed=1)
# calculate difficulty
# mazeDiff(nodePosition, model="m1") # This takes a long while!!!


setwd("/OneDrive - UPV/__SUBMISSIONS/2018/General intelligence/__NewCode__")
source("generality.R")

DATADIR <- "mazes.humans.data"
OUTPUTDIR <- "mazes.humans.results"

##############################################################################################################
##############################################################################################################
### INFO ABOUT THE DATASET ###################################################################################
##############################################################################################################
##############################################################################################################

# THERE ARE TWO DATASETS: old and new
# The old dataset ("old dataset.csv") has 1032 respondents and 18 items
# "X210"    "X211"    "X213"    "X215"    "X216"    "X217"    "X222"    "X223"    "X224"    "X225"    "X226"    "X227"    "X228"    "X229"    "X230"    "X231"    "X232"    "X233"
# The new dataset ("cleanedJose.csv") has 530 respondents and 23 items
# "X247"    "X248"    "X249"    "X250"    "X251"    "X252"    "X253"    "X254"    "X255"    "X256"    "X257"    "X258"    "X259"    "X260"    "X261"    "X262"    "X263"    "X264"    "X266"    "X267"    "X268"    "X270"    "X271" 
# Only 5 items in common between the two datasets so there's not much to do with both together.
# Actually the last 4 in the new dataset correspond to five in the old data, namely:
# oldX266 = newX216, oldX267 = newX217, oldX268 = newX225, oldX270 = newX227, oldX271 = newX233
# Also, only the new dataset has demographic data, which can be found in the file "demographics.csv"
# But only for 496 of the 530 respondents




################################################################################
################################################################################
### Old (big) dataset ##########################################################
################################################################################
################################################################################

oldDf <- read.csv(paste0(DATADIR, "/old dataset.csv"))
nrow(oldDf) # 1032
ncol(oldDf) # 20
names(oldDf)
#[1] "X"       "session" "X210"    "X211"    "X213"    "X215"    "X216"    "X217"    "X222"    "X223"    "X224"    "X225"   
#[13] "X226"    "X227"    "X228"    "X229"    "X230"    "X231"    "X232"    "X233"   

# We remove useless columns: "X" and "session"
oldDfResponses <- oldDf[,-c(1,2)]
nrow(oldDfResponses) # 1032
nItemsOld <- ncol(oldDfResponses) # 20



################################################################################
################################################################################
### New (big) dataset (but more columns and demographic data ) #################
################################################################################
################################################################################

newDf <-  read.csv(paste0(DATADIR, "/cleanedJose.csv"))
nrow(newDf) # 530
ncol(newDf) # 26
names(newDf)
# [1] "X"       "session" "ssid"    "X247"    "X248"    "X249"    "X250"    "X251"    "X252"    "X253"    "X254"    "X255"   
# [13] "X256"    "X257"    "X258"    "X259"    "X260"    "X261"    "X262"    "X263"    "X264"    "X266"    "X267"    "X268"   
# [25] "X270"    "X271" 

# load demographic data
demoDf <- read.csv(paste0(DATADIR, "/demographics.csv"))
nrow(demoDf) # 496
ncol(demoDf) # 14
names(demoDf)
# [1] "id"             "gender"         "session"        "nationality"    "ethnicity"      "education"      "employment"    
# [8] "maritialStatus" "lengthOfTime"   "dobDay"         "dobMonth"       "dobYear"        "birthday"       "ssid"#

# join with demographic data
newDfWithDemographics <- merge(newDf, demoDf, by="session") 
nrow(newDfWithDemographics) # 496
ncol(newDfWithDemographics) # 39


# We now eliminate useless columns: "session", "X" and "ssid.X"
# We also eliminate the columns of the demographic data for now.
ONLY_THOSE_WITH_DEMO <- TRUE # If this is true we eliminate those without demographic data (as we won't be able to compare the results when we do subgroups)
if (ONLY_THOSE_WITH_DEMO) {
  newDfResponses<- newDfWithDemographics[,c(4:26)] 
} else {
  newDfResponses<- newDf[,-c(1:3)]
}
ncol(newDf)  # 26
nItemsNew <- ncol(newDfResponses) #23






################################################################################
################################################################################
### United dataset #############################################################
################################################################################
################################################################################

# Renaming those columns (items) that are the same before joining
newDfResponsesAndRenamed <- plyr::rename(newDfResponses,c(X266 = 'X216', X267 ="X217", X268 = "X225", X270 = 'X227' , X271 ="X233" ))

# rbind new and old dataset (PUT ROWS TOGETHER)
allDfResponses <- rbind.fill(newDfResponsesAndRenamed, oldDfResponses)
nrow(allDfResponses) # 1528
nItemsAll <- ncol(allDfResponses) # 36
names(allDfResponses)
# X247 to X264    (18: ONLY NEW DATASET)
# x216, x217, x225, x227, X233 (5: COMMON FOR BOTH DATASETS): The equivalence of item codes: # oldX266 = newX216, oldX267 = newX217, oldX268 = newX225, oldX270 = newX227, oldX271 = newX233
# x210, x211, x213, x215, x222, X223, X224, X226, X228 to x232    (13: ONLY OLD DATASET)

# visually check linking is correct
image(t(is.na(allDfResponses))[,nrow(allDfResponses):1],
      main = "missingness patterns (white = NA)") # 5 linking items
# Error: Error in plot.new() : figure margins too large  (this is because the Plots window in RStudio is too small)




################################################################################
################################################################################
### INTRINSIC DIFFICULTIES #####################################################
################################################################################
################################################################################

# m3 difficulty value
allDifficulties <- c(9.525685, 8.877153, 11.01401, 10.21133, 12.16474, 11.66572,  # X247 to X252    (6: ONLY NEW DATASET)
          13.26292, 13.53889, 14.90944, 14.90944, 15.77249, 15.77249,  # X253 to X258    (6: ONLY NEW DATASET)
          16.31832, 16.80505, 18.05222, 19.12021, 19.41059, 18.25986,  # x259 to x264    (6: ONLY NEW DATASET)
          11.41418, 11.73435, 14.10147, 14.93203, 10.1093,            # x216, x217, x225, x227, X233 (5: COMMON FOR BOTH DATASETS): The equivalence of item codes: # oldX266 = newX216, oldX267 = newX217, oldX268 = newX225, oldX270 = newX227, oldX271 = newX233
          7.019727, 7.673181, 9.112719, 9.792316, 12.950742,          # x210, x211, x213, x215, x222   (5: ONLY OLD DATASET)
          13.068525, 13.99611, 14.93203, 16.59572, 16.18616, 17.57939, # X223, X224, X226, X228, X229, X230  (6: ONLY OLD DATASET)
          17.33689, 9.112719)                                          # x231, x232    (2: ONLY OLD DATASET)
# For these items, respectively (the items in the new dataset come first)
# "X247" "X248" "X249" "X250" "X251" "X252" "X253" "X254" "X255" "X256" "X257" "X258" "X259" "X260" "X261" "X262" "X263" "X264" 
# "X216" "X217" "X225" "X227"  
# "X233" "X210" "X211" "X213" "X215" "X222"
# "X223" "X224" "X226" "X228" "X229" "X230"
# "X231" "X232"
length(allDifficulties)

diffIndexesNewData <- 1:nItemsNew  # 1:23
diffIndexesOldData <- (nItemsAll - nItemsOld + 1): nItemsAll  # 19:36 (total 18)





################################################################################
################################################################################
### NOW, WE HAVE EVERYTHING, WE CAN DO THE SELECTIONS WE WANT ##################
################################################################################
################################################################################





################################################################################
################################################################################
# EXPLORING THE OLD DATASET ####################################################
################################################################################
################################################################################

studyName <- "mazes.humans.oldData"
responseMatrix <- oldDfResponses
difficulties <- allDifficulties[diffIndexesOldData]  # External difficulties

# FULL STUDY WITH THE THREE APPROACHES: FA, IRT and GA
# Use external difficulties
rold <- PerformFullStudy(studyName, responseMatrix, difficulties)

# Use IRT difficulties
rirtold <- PerformFullStudy(studyName, responseMatrix, difficulties=NULL, centreDiffScale = mean(difficulties), devDiffScale = sd(difficulties))  # Same but with IRT difficulties

# Do not compare r1 and r2 as they don't have the same difficulties.



################################################################################
################################################################################
### EXPLORING THE NEW DATASET ##################################################
### THESE ARE THE GOOD RESULTS AND PLOTS #######################################
################################################################################
################################################################################

studyName <- "mazes.humans.newData"
responseMatrix <- newDfResponsesAndRenamed

rm <- rowMeans(newDfResponsesAndRenamed)
print("Checking that the minimum row mean is 0, so that we have one agent being right for all, so that the space is calibrated")
print(min(rm))  # ex: row 67
print("Checking that the maximum row mean is 1, so that we have one agent being right for all, so that the space is calibrated")
print(max(rm))  # ex: row 16

difficulties <- allDifficulties[diffIndexesNewData]  # External difficulties
print("Min and max difficulties:")
print(min(difficulties))
print(max(difficulties))

# FULL STUDY WITH THE THREE APPROACHES: FA, IRT and GA
# Use external difficulties
rnew <- PerformFullStudy(studyName, responseMatrix, difficulties)

# FULL STUDY WITH THE THREE APPROACHES: FA, IRT and GA
# Use IRT difficulties
rirtnew <- PerformFullStudy(studyName, responseMatrix, difficulties=NULL, centreDiffScale = mean(difficulties), devDiffScale = sd(difficulties))  # Same but with IRT difficulties

# Do not compare r1 and r2 as they don't have the same difficulties.


# What about binning the difficulties? It doesn't make sense as we originally have 23 difficulties, so it actually adds noise, as it groups them by equal width bins
numBins <- 10  # There are 23 difficulties originally. Binning them must be drastic or it is not noticed
binnedDifficulties <- BinDifficulties(difficulties, numBins) 
studyNameBINNEDdiff <- "mazes.humans.newData.BINNEDdiff"
rnewbinned <- PerformFullStudy(studyNameBINNEDdiff, responseMatrix, binnedDifficulties)







################################################################################
################################################################################
# EXPLORING THE NEW DATASET WITH GROUPS ########################################
################################################################################
################################################################################


# We have many demographic attributes to play with in newDfWithDemographics
# They have the same order as newDfResponsesAndRenamed, so we can use it for selecting rows

difficulties <- allDifficulties[diffIndexesNewData]  # External difficulties
studyName0 <- "mazes.humans.newData"

SUBGROUP <- "age"
#SUBGROUP <- "ethnicity"
#SUBGROUP <- "education"
#SUBGROUP <- "gender"
if (SUBGROUP == "age") {
  SELATTR <- newDfWithDemographics[,'dobYear']
  SELATTR <- 2017 - SELATTR  # Age: assuming the experiments were done in 2017.
  SELATTR[SELATTR < 18] <- NA
  SELATTR[SELATTR > 100] <- NA
  BELOWCUT <- (SELATTR < median(SELATTR, na.rm=TRUE))
  count(BELOWCUT)
  SUBGROUPVALUES <- c("young", "old")
} else if (SUBGROUP == "ethnicity") {  
  SELATTR <- newDfWithDemographics[,'ethnicity']
  BELOWCUT <- (SELATTR == 7)  # Majority ethnicity
  count(BELOWCUT)
  SUBGROUPVALUES <- c("majority", "minority")  
} else if (SUBGROUP == "education") {  
  SELATTR <- newDfWithDemographics[,'education']
  BELOWCUT <- (SELATTR < 5)
  SUBGROUPVALUES <- c("low", "high")  
  count(BELOWCUT)
} else if (SUBGROUP == "gender") {  
  SELATTR <- newDfWithDemographics[,'gender']
  BELOWCUT <- (SELATTR < mean(SELATTR, na.rm=TRUE))  # For gender, this selects those < 1.393, more common, i.e., 1, I think they're men
  SUBGROUPVALUES <- c("male", "female")  
  count(BELOWCUT)
}


SELROWSa <- (BELOWCUT==TRUE) & !is.na(BELOWCUT)
count(SELROWSa)
responseMatrix <- newDfResponsesAndRenamed[SELROWSa,]
studyName <- paste0(studyName0, ".", SUBGROUP, ".", SUBGROUPVALUES[1])

rnewa <- PerformFullStudy(studyName, responseMatrix, difficulties)


SELROWSb <- (BELOWCUT==FALSE) & !is.na(BELOWCUT)
count(SELROWSb)
responseMatrix <- newDfResponsesAndRenamed[SELROWSb,]
studyName <- paste0(studyName0, ".", SUBGROUP, ".", SUBGROUPVALUES[2])

rnewb <- PerformFullStudy(studyName, responseMatrix, difficulties)

# We can compare the capabilities and the spreads, and the correlations with the attribute values used to create the groups
mean(rnew$capabilities)
mean(rnewa$capabilities)
mean(rnew$capabilities[SELROWSa]) # The same as above
mean(rnewb$capabilities)
mean(rnew$capabilities[SELROWSb]) # The same as above
cor(rnew$capabilities,SELATTR, use="complete.obs")

mean(rnew$spread)
mean(rnewa$spread)
mean(rnew$spread[SELROWSa]) # The same as above
mean(rnewb$spread)
mean(rnew$spread[SELROWSb]) # The same as above
cor(rnew$spreads,SELATTR, use="complete.obs")


studyName <- paste0(studyName0, ".", SUBGROUP)
filename <- sprintf("%s.histspreads", studyName)
OpenPDFEPS(filename, 4, 6)
PlotSplitHistograms(rnew$spread, rnewa$spreads, rnewb$spreads, mean(rnew$FAloadings), rnew$FAVaccounted, mean(rnewa$FAloadings), rnewa$FAVaccounted, mean(rnewb$FAloadings), rnewb$FAVaccounted) 

ClosePDFEPS()


# Check the loadings (already in the plot above)
rnew$FAloadings      # Prop var 0.228
mean(rnew$FAloadings)   # 0.46

rnewa$FAloadings  # Prop var 0.246 gender
mean(rnewa$FAloadings)  # 0.48

rnewb$FAloadings   # Prop var 0.177 gender
mean(rnewb$FAloadings)  # 0.40




################################################################################
################################################################################
# EXPLORING THE NEW DATASET BY CAPABILITY (SLODR) ##############################
################################################################################
################################################################################



# SLODR ANALYSIS





# TWO GROUPS: SEPARATED BY MEDIAN

#chosen <- (rnew$capabilities > quantile(rnew$capabilities, 0.75))  # selecting by capability
chosen <- (rnew$capabilities > quantile(rnew$capabilities, 0.5))  # selecting by capability
sum(chosen)


responseMatrix <- newDfResponsesAndRenamed[chosen,]
studyName <- paste0(studyName0, ".", "highcapability")

rnewhigh <- PerformFullStudy(studyName, responseMatrix, difficulties)

responseMatrix <- newDfResponsesAndRenamed[!chosen,]
studyName <- paste0(studyName0, ".", "lowcapability")

rnewlow <- PerformFullStudy(studyName, responseMatrix, difficulties)


mean(rnew$capabilities)
mean(rnewhigh$capabilities) 
mean(rnew$capabilities[chosen])  # The same as above
mean(rnewlow$capabilities) 
mean(rnew$capabilities[!chosen])  # The same as above

mean(rnew$spreads)
mean(rnewhigh$spreads) 
mean(rnew$spreads[chosen])  # The same as above
mean(rnewlow$spreads)  
mean(rnew$spreads[!chosen]) # The same as above


studyName <- paste0(studyName0, ".", "capabilities") # SUBGROUP,
filename <- sprintf("%s.histspreads", studyName)
OpenPDFEPS(filename, 4, 6)
PlotSplitHistograms(rnew$spread, rnewlow$spreads, rnewhigh$spreads,  mean(rnew$FAloadings), rnew$FAVaccounted, mean(rnewlow$FAloadings), rnewlow$FAVaccounted, mean(rnewhigh$FAloadings), rnewhigh$FAVaccounted) 
ClosePDFEPS()


# Check the loadings (already in the plot above)
rnew$FAloadings      # Prop var 0.228
mean(rnew$FAloadings)   # 0.46

rnewlow$FAloadings   # Prop var 0.108
mean(rnewlow$FAloadings)  # 0.27

rnewhigh$FAloadings  # Prop var 0.073
mean(rnewhigh$FAloadings)  # 0.09





# FOUR GROUPS: SEPARATED BY ABIITIES (EQUAL WIDTH, DIFFERENT SIZE)

mincap <- min(rnew$capabilities)
maxcap <- max(rnew$capabilities)

chosen1 <- (rnew$capabilities > mincap + 3*(maxcap-mincap)/4)  # selecting by capability
sum(chosen1)

responseMatrix <- newDfResponsesAndRenamed[chosen1,]
studyName <- paste0(studyName0, ".", "quarter4of4capability")

rnewquarter4of4 <- PerformFullStudy(studyName, responseMatrix, difficulties, INDIVIDUAL_SUBJECTS = FALSE)


chosen2 <- !chosen1 & (rnew$capabilities > mincap + 2*(maxcap-mincap)/4)  # selecting by capability
sum(chosen2)

responseMatrix <- newDfResponsesAndRenamed[chosen2,]
studyName <- paste0(studyName0, ".", "quarter3of4capability")

rnewquarter3of4 <- PerformFullStudy(studyName, responseMatrix, difficulties, INDIVIDUAL_SUBJECTS = FALSE)


chosen3 <- !chosen1 & !chosen2 & (rnew$capabilities > mincap + 1*(maxcap-mincap)/4)  # selecting by capability
sum(chosen3)

responseMatrix <- newDfResponsesAndRenamed[chosen3,]
studyName <- paste0(studyName0, ".", "quarter2of4capability")

rnewquarter2of4 <- PerformFullStudy(studyName, responseMatrix, difficulties, INDIVIDUAL_SUBJECTS = FALSE)



chosen4 <- !chosen1 & !chosen2 & !chosen3 & (rnew$capabilities > mincap + 0*(maxcap-mincap)/4)  # selecting by capability
sum(chosen4)

responseMatrix <- newDfResponsesAndRenamed[chosen4,]
studyName <- paste0(studyName0, ".", "quarter1of4capability")

rnewquarter1of4 <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA = FALSE, DO_IRT=FALSE, INDIVIDUAL_SUBJECTS = FALSE)  # Fails because some columns are constant

