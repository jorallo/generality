##############################################################################
#
#   PCTB (Herrmann2007 results analysis) : GENERALITY ANALYSIS (herrmann_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis of Herrmann et al data 2007, used in this paper:
#   Cognition: The Cultural Intelligence Hypothesis
#   Humans Have Evolved Specialized Skills of Social
#   10.1126/science.1146282
#   Science 317, 1360 (2007);
#   Esther Herrmann, et al.
#
# and the the metric of generality first introduced in 
#   J. Hernandez-Orallo "I.G.", March 15th, 2018
#   https://riunet.upv.es/bitstream/handle/10251/100267/secondbest.pdf
#
# It mostly applies the method: "DIFFICULTY FROM DISTRIBUTIONAL REFERENCE"
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    08 Apr 2019. First operative version
#
# FUTURE FEATURES
#
##############################################################################


##############################################################################################################
##############################################################################################################
################## FUNCTIONS ##################################################################################
##############################################################################################################
##############################################################################################################






##############################################################################################################
##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
#############################################################################
##############################################################################################################

setwd("/OneDrive - UPV/__SUBMISSIONS/2018/General intelligence/__NewCode__")
source("generality.R")

OUTPUTDIR <- "herrmann.results"
studyName <- "herrmann-etal"


##############################################################################
################## LOADING THE DATA. TAKEN FROM TABLE 2 #######################
##############################################################################

# Data taken from the paper
n_chimpanzees <- 106
n_orangutans <- 32
n_humans <- 105

Space_human <- c(0.91,0.79,0.55,0.57)
Space_chimpanzee <- c(0.95,0.64,0.56,0.70)
Space_orangutan <- c(0.85,0.60,0.46,0.47)

Quantity_human <- c(0.71,0.64)
Quantity_chimpanzee <- c(0.66,0.69)
Quantity_orangutan <- c(0.64,0.61)

Causality_human <- c(0.85,0.83,0.23,0.71)
Causality_chimpanzee <- c(0.61,0.68,0.74,0.61)
Causality_orangutan <- c(0.56,0.64,0.38,0.63)

Soclearn_human <- c(0.86)
Soclearn_chimpanzee <- c(0.10)
Soclearn_orangutan <- c(0.07)

Communication_human <- c(0.84,0.72,0.59)
Communication_chimpanzee <- c(0.63,0.74,0.34)
Communication_orangutan <- c(0.65,0.73,0.26)

Mind_human <- c(0.45,0.85)
Mind_chimpanzee <- c(0.22,0.59)
Mind_orangutan <- c(0.17,0.56)

# We consider the 16 tasks, each with the same weight in the means and sd
humanAll <- c(Space_human, Quantity_human, Causality_human, Soclearn_human, Communication_human, Mind_human)
chimpanzeeAll <- c(Space_chimpanzee, Quantity_chimpanzee, Causality_chimpanzee, Soclearn_chimpanzee, Communication_chimpanzee, Mind_chimpanzee)
orangutanAll <- c(Space_orangutan, Quantity_orangutan, Causality_orangutan, Soclearn_orangutan, Communication_orangutan, Mind_orangutan)

human_mean <- mean(humanAll)
human_sd <- popsd(humanAll)
chimpanzee_mean <- mean(chimpanzeeAll)
chimpanzee_sd <- popsd(chimpanzeeAll)
orangutan_mean <- mean(orangutanAll)
orangutan_sd <- popsd(orangutanAll)

# We consider the 6 categories, each with the same weight in the means and sd
humanAllPerCategory <- c(mean(Space_human), mean(Quantity_human), mean(Causality_human), mean(Soclearn_human), mean(Communication_human), mean(Mind_human))
chimpanzeeAllPerCategory <- c(mean(Space_chimpanzee), mean(Quantity_chimpanzee), mean(Causality_chimpanzee), mean(Soclearn_chimpanzee), mean(Communication_chimpanzee), mean(Mind_chimpanzee))
orangutanAllPerCategory <- c(mean(Space_orangutan), mean(Quantity_orangutan), mean(Causality_orangutan), mean(Soclearn_orangutan), mean(Communication_orangutan), mean(Mind_orangutan))
# The values are also in Table 2 in the paper. The results are almost the same, except for the third decimal digit. We take their values instead of ours.
humanAllPerCategory <- c(0.71, 0.67, 0.65, 0.86, 0.72, 0.65)
chimpanzeeAllPerCategory <- c(0.71, 0.68, 0.66, 0.10, 0.57, 0.40)
orangutanAllPerCategory <- c(0.60, 0.63, 0.55, 0.07, 0.55, 0.36)

humanMeanPerCategory <- mean(humanAllPerCategory)
humanSdPerCategory <- popsd(humanAllPerCategory)
chimpanzeeMeanPerCategory <- mean(chimpanzeeAllPerCategory)
chimpanzeeSdPerCategory <- popsd(chimpanzeeAllPerCategory)
orangutanMeanPerCategory <- mean(orangutanAllPerCategory)
orangutanSdPerCategory <- popsd(orangutanAllPerCategory)


# Taken from Table S5. We're interested in the coefficients of variations.
# Now format is different. Each of the three values represent human, chimp and orang respectively.
# (note that we're recovering the means from the paper again, they're exactly as above)
Space_variation <- c(16.6, 15.6, 22.5)
Space_mean <- c(0.71, 0.71, 0.60)
Quantities_variation <- c(20.9, 13.2, 17.4)
Quantities_mean <- c(0.67, 0.68, 0.63)
Causality_variation <- c(20.0, 19.7, 23.6)
Causality_mean <- c(0.65, 0.66, 0.55  )
Soclearn_variation <- c(34.8, NA, NA)    # Note the NAs
Soclearn_mean <- c(0.86, 0.10, 0.07 )
Communication_variation <- c(30.5, 24.5, 30.6)
Communication_mean <- c(0.72, 0.57, 0.55)
Mind_variation <- c(29.0, 32.5, 30.5)
Mind_mean <- c(0.65, 0.40, 0.36)

# We calculate (recover) the SD from the coefficients of variation and the mean (https://en.wikipedia.org/wiki/Coefficient_of_variation)
Mind_sd <- Mind_mean * Mind_variation / 100
Communication_sd <- Communication_mean * Communication_variation / 100
Soclearn_sd <- Soclearn_mean * Soclearn_variation / 100
Causality_sd <- Causality_mean * Causality_variation / 100
Quantities_sd <- Quantities_mean * Quantities_variation / 100
Space_sd <- Space_mean * Space_variation / 100

humanAllSdPerCategory <- c(Space_sd[1], Quantities_sd [1], Causality_sd[1], Soclearn_sd[1], Communication_sd[1],  Mind_sd[1])


#familyColours <- c("blue", "forestgreen", "orange")
familyColours <- c("purple", "grey", "orange")
familyNames <- c("Humans", "Chimpanzees", "Orangutans")
categoryNames <- c("Space", "Quantities", "Causality", "Social Learning", "Communication", "Theory of Mind")
categoryShortNames <- c("Space", "Quantity", "Causality", "Soclearn", "Communication", "Mind")


nCategories <- length(categoryNames)
nFamilies <- length(familyNames)


##############################################################################
################## WE FIRST COMPARE MEAN AND VARIABILITY #######################
####### NOTE THAT THESE VARIABILITY ARE ABOUT ROBUSTNESS, TAKEN FROM THE COEFF. OF VARIATION INSIDE FAMILY OR TASK ##########
##############################################################################

x <- (0:1000)/1000
max_var <- x*(1-x)
max_sd <- sqrt(max_var)
max_variation <- 100 * (max_sd / x)


# We can show var, sd or coefficient of variation (what is used in Herrmann's paper)
PLOT <- "VAR"
#PLOT <- "SD"
#PLOT <- "COEFFVAR"


filename <- paste0(studyName, "-VariabilityAnalysis-Intratasks-", PLOT)
OpenPDFEPS(filename, 4, 6)

# We create an empty plot
if (PLOT == "VAR") { 
  plot(x,max_var, type= "l", xlab="mean", ylab="variance")
} else if (PLOT == "SD") {
  plot(x,max_sd, type= "l", xlab="mean", ylab="sd")
} else {
  plot(x,max_variation, type= "l", xlab="mean", ylab="variation", ylim=c(0,200))
}


# Now we show mean and variability for each category

if (PLOT == "VAR") { 
  points(Space_mean, Space_sd^2, col= familyColours, pch=0)
} else if (PLOT == "SD") {
  points(Space_mean, Space_sd, col= familyColours, pch=0)
} else {
  points(Space_mean, Space_variation, col= familyColours, pch=0)
}

if (PLOT == "VAR") { 
  points(Quantities_mean, Quantities_sd^2, col= familyColours, pch=1)
} else if (PLOT == "SD") {
  points(Quantities_mean, Quantities_sd, col= familyColours, pch=1)
} else {
  points(Quantities_mean, Quantities_variation, col= familyColours, pch=1)
}
 
if (PLOT == "VAR") { 
  points(Causality_mean, Causality_sd^2, col= familyColours, pch=2)
} else if (PLOT == "SD") {
  points(Causality_mean, Causality_sd, col= familyColours, pch=2)
} else {
  points(Causality_mean, Causality_variation, col= familyColours, pch=2)

}

# We cannot plot this because we NAs for chimps and orangutans
if (FALSE) {
if (PLOT == "VAR") { 
  points(Soclearn_mean, Soclearn_sd^2, col= familyColours, pch=3)
} else if (PLOT == "SD") {
  points(Soclearn_mean, Soclearn_sd, col= familyColours, pch=3)
} else {
  points(Soclearn_mean, Soclearn_variation, col= familyColours, pch=3)
}
}

if (PLOT == "VAR") { 
  points(Communication_mean, Communication_sd^2, col= familyColours, pch=4)
} else if (PLOT == "SD") {
  points(Communication_mean, Communication_sd, col= familyColours, pch=4)
} else {
  points(Communication_mean, Communication_variation, col= familyColours, pch=4)

}

if (PLOT == "VAR") { 
  points(Mind_mean, Mind_sd^2, col= familyColours, pch=5)
} else if (PLOT == "SD") {
  points(Mind_mean, Mind_sd, col= familyColours, pch=5)
} else {
  points(Mind_mean, Mind_variation, col= familyColours, pch=5)
}

if (FALSE) {
# Lines fitted manually. Ignore this...
human_fac <- 2.75
#lines((x-0.5)/human_fac+0.5,max_sd/human_fac, type= "l", lty=2, col=familyColours[1])
if (PLOT == "VAR") { 
   lines(x,(max_sd/human_fac)^2, type= "l", lty=2, col=familyColours[1])
} else if (PLOT == "SD") {
  lines(x,max_sd/human_fac, type= "l", lty=2, col=familyColours[1])
} else {
  lines(x,max_variation/human_fac, type= "l", lty=2, col=familyColours[1])
}

chimp_fac <- 4.00
#lines((x-0.5)/chimp_fac+0.5,max_sd/chimp_fac, type= "l", lty=3, col=familyColours[2]) 
if (PLOT == "VAR") { 
  lines(x, (max_sd/chimp_fac)^2, type= "l", lty=3, col=familyColours[2])
} else if (PLOT == "SD") {
  lines(x,max_sd/chimp_fac, type= "l", lty=3, col=familyColours[2])
} else {
  lines(x,max_variation/chimp_fac, type= "l", lty=3, col=familyColours[2])
}

orang_fac <- 3.5
#lines((x-0.5)/orang_fac+0.5,max_sd/orang_fac, type= "l", lty=4, col=familyColours[3])
if (PLOT == "VAR") { 
  lines(x, (max_sd/orang_fac)^2, type= "l", lty=4, col=familyColours[3])
} else if (PLOT == "SD") {
  lines(x,max_sd/orang_fac, type= "l", lty=4, col=familyColours[3])
} else {
  lines(x,max_variation/orang_fac, type= "l", lty=4, col=familyColours[3])
}
}

if (PLOT == "VARIATION") {
  location <- "top"
} else {
  location <- "topleft"
}

legend(location, legend=familyNames,  col=familyColours, lty=2:4, cex=0.6)

legend("topright", legend=categoryNames, pch=0:5, cex=0.6)

ClosePDFEPS()





##############################################################################
################## WE NOW COMPARE MEAN AND VARIABILITY #######################
####### BUT NOW IT'S GLOBAL VARIABILITY FOR ALL TASKS  ##########
##############################################################################

x <- (0:1000)/1000
max_var <- x*(1-x)
max_sd <- sqrt(max_var)
max_variation <- 100 * (max_sd / x)


# We can show var, sd or coefficient of variation (what is used in Herrmann's paper)
PLOT <- "VAR"
#PLOT <- "SD"
#PLOT <- "COEFFVAR"


filename2 <- paste0(studyName, ".VariabilityAnalysis-Intertasks-", PLOT)
OpenPDFEPS(filename2, 4, 6)

# We create an empty plot
if (PLOT == "VAR") { 
  plot(x,max_var, type= "l", xlab="mean", ylab="variance")
} else if (PLOT == "SD") {
  plot(x,max_sd, type= "l", xlab="mean", ylab="sd")
} else {
  plot(x,max_variation, type= "l", xlab="mean", ylab="variation", ylim=c(0,200))
}

# NOW WE PLOT THE MEANS, BY TASKS AND BY CATEGORIES ("scales" in the paper)

if (PLOT == "VAR") { 
  points(human_mean, human_sd^2, col= familyColours[1], pch=6)
  points(chimpanzee_mean, chimpanzee_sd^2, col= familyColours[2], pch=6)
  points(orangutan_mean, orangutan_sd^2, col= familyColours[3], pch=6)
  points(humanMeanPerCategory, humanSdPerCategory^2, col= familyColours[1], pch=7)
  points(chimpanzeeMeanPerCategory, chimpanzeeSdPerCategory^2, col= familyColours[2], pch=7)
  points(orangutanMeanPerCategory, orangutanSdPerCategory^2, col= familyColours[3], pch=7)
} else if (PLOT == "SD") {
  points(human_mean, human_sd, col= familyColours[1], pch=6)
  points(chimpanzee_mean, chimpanzee_sd, col= familyColours[2], pch=6)
  points(orangutan_mean, orangutan_sd, col= familyColours[3], pch=6)
  points(humanMeanPerCategory, humanSdPerCategory, col= familyColours[1], pch=7)
  points(chimpanzeeMeanPerCategory, chimpanzeeSdPerCategory, col= familyColours[2], pch=7)
  points(orangutanMeanPerCategory, orangutanSdPerCategory, col= familyColours[3], pch=7)
} else {
  ERROR
}

legend(location, legend=familyNames,  col=familyColours, lty=2:4, cex=0.6)


legend("topright", legend=c("(by task)", "(by scale)"), pch=6:7, cex=0.6)


ClosePDFEPS()






##############################################################################
########################### GENERALITY ANALYSIS ##############################
##############################################################################


# METHOD 5


# Quantiles on humans are difficulties for the rest.
#qbeta(1-0.95, a, b)  # What's the threshold value (magnitude) so that 95% humans are right
#qbeta(1-0.50, a, b)  # What's the threshold value so that 50% humans are right
#qbeta(1-0.25, a, b)  # What's the threshold value so that 25% humans are right

#qnorm(1-0.975, 0, 1)  # 1.96 standard deviations
#qnorm((1-0.9545)/2, 0, 1)  # 2 standard deviations

#1-pbeta(0.9,a,b)  # How many would be right at threshold 0.9
#1-pbeta(0.5,a,b)  # How many would be right at threshold 0.5
#1-pbeta(0.1,a,b)  # How many would be right at threshold 0.1

#1-pnorm(1.96,0,1)  # 2.5%
#1-pnorm(-1.96,0,1)  # 97.5%






humanAllSdPerCategory
humanAllPerCategory
chimpanzeeAllPerCategory
orangutanAllPerCategory

# Choosing humans as a reference
reference <- humanAllPerCategory
reference_sd <- humanAllSdPerCategory
# Other possibilities for references
# Changing a very bad human as a reference (keeping the sd)
# reference <- c(0.3, 0.2, 0.4, 0.45, 0.33, 0.22)
# Changing to 0.5 (keeping the sd)
# reference <- rep(0.5, 6)
# reference_sd <- rep(0.5,6)  # gives generality infinity for all
# reference_sd <- rep(0.1,6)  # very low
# reference_sd <- rep(sqrt(1/12),6)  # gives a=b=1
# reference_sd <- rep(sqrt(1/20),6)  # gives a=b=2



nseries <- nCategories*nFamilies
legendtext <- 1:nseries
legendlty <- 1:nseries
#options(digits = 4)


nDifficulties <- 100
difficulties <- seq(0,1,length.out=100)

difficulties6 <- rep(difficulties, nCategories)

r <- GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference_MULTIPLEVALUES(humanAllPerCategory, reference, reference_sd^2, nDifficulties, distName= "BETA")
difficulties6 <- r$difficulties
responsesHumans6 <- r$responses
abilitiesHumans6 <- r$abilities
mean(r$abilities)  # This should be the ability when aggregated (see below)
popsd(r$abilities)  # This should be the spread when aggregated (see below)

r <- GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference_MULTIPLEVALUES(chimpanzeeAllPerCategory, reference, reference_sd^2, nDifficulties, distName= "BETA")
#difficulties6 <- r$difficulties  # Not needed as it is the same as above
responsesChimps6 <- r$responses
abilitiesChimps6 <- r$abilities
mean(r$abilities)  # This should be the ability when aggregated (see below)
popsd(r$abilities)  # This should be the spread when aggregated (see below)

r <- GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference_MULTIPLEVALUES(orangutanAllPerCategory, reference, reference_sd^2, nDifficulties, distName= "BETA")
#difficulties6 <- r$difficulties # Not needed as it is the same as above
responsesOrangs6 <- r$responses
abilitiesOrangs6 <- r$abilities
r$abilities
mean(r$abilities)  # This should be the ability when aggregated (see below)
popsd(r$abilities)  # This should be the spread when aggregated (see below)


# Plotting the individual stepwise ACCs

plot(difficulties, xlim=c(0,1), ylim=c(0,1), col="white" )
for (i in 1:nCategories) {
  xvalues <- seq(0,1,length.out=nDifficulties)
  pos <- (i-1)*nDifficulties
  
#  lines(xvalues, responsesOrangs6[(pos+1):(pos+nDifficulties)], lty=3, col=i) #, type="b") # , add=(i > 1))
  lines(xvalues, responsesOrangs6[(pos+1):(pos+nDifficulties)], lty=i, col=familyColours[3]) #, type="b") # , add=(i > 1))
  legendtext[i+0*nCategories] <- sprintf("Orang.%s", categoryShortNames[i])
#  legendlty[i+0*nCategories] <- 3
  legendlty[i+0*nCategories] <- i
  
#  lines(xvalues, responsesChimps6[(pos+1):(pos+nDifficulties)], lty=2, col=i) #, type="b") # , add=(i > 1))
  lines(xvalues, responsesChimps6[(pos+1):(pos+nDifficulties)], lty=i, col=familyColours[2]) #, type="b") # , add=(i > 1))
  legendtext[i+1*nCategories] <- sprintf("Chimp.%s", categoryShortNames[i])
#  legendlty[i+1*nCategories] <- 2
  legendlty[i+1*nCategories] <- i
  
#  lines(xvalues, responsesHumans6[(pos+1):(pos+nDifficulties)], lty=4, col=i) #, type="b") # , add=(i > 1))
  lines(xvalues, responsesHumans6[(pos+1):(pos+nDifficulties)], lty=i, col=familyColours[1]) #, type="b") # , add=(i > 1))
  legendtext[i+2*nCategories] <- sprintf("Human%s", categoryShortNames[i])
#  legendlty[i+2*nCategories] <- 4
  legendlty[i+2*nCategories] <- i
  
}
#legend("topright", legend=legendtext, lty= legendlty, col=rep(1:nCategories, length.out=nseries), cex=0.5)
allColours <- c(rep(familyColours[3], nCategories), rep(familyColours[2], nCategories), rep(familyColours[1], nCategories))
legend("topright", legend=legendtext, lty= legendlty, col=allColours, cex=0.5)



# Aggregated plot


filename3 <- paste0("herrmann-etal.GeneralityAnalysis")
OpenPDFEPS(filename3, 4, 6)

capabilities <- NULL
spreads <- NULL
r <- PlotACCwithIndicators(difficulties6,responsesOrangs6,xmin=0,xmax=1, SHOW_UNMERGED_POINTS= FALSE, SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, startplot= TRUE, mycol=familyColours[3], LEGEND= FALSE, LEGEND2 = TRUE, legendpos=0.7, mypch= ".", name= familyNames[3])
unlist(r)
capabilities[3] <- r$capability
spreads[3] <- r$spread

r <- PlotACCwithIndicators(difficulties6,responsesChimps6,xmin=0,xmax=1, SHOW_UNMERGED_POINTS= FALSE, SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, startplot= FALSE, mycol=familyColours[2], LEGEND= FALSE, LEGEND2 = TRUE, legendpos=0.8, mypch= ".", name= familyNames[2])
unlist(r)
capabilities[2] <- r$capability
spreads[2] <- r$spread

r <- PlotACCwithIndicators(difficulties6,responsesHumans6,xmin=0,xmax=1, SHOW_UNMERGED_POINTS= FALSE, SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, startplot= FALSE, mycol=familyColours[1], LEGEND= FALSE, LEGEND2 = TRUE, legendpos=0.9, mypch=".", name= familyNames[1])  #, mylty= 3) 
unlist(r)
capabilities[1] <- r$capability
spreads[1] <- r$spread


ClosePDFEPS()



filename4 <- paste0(studyName,".capability-vs-spread")
OpenPDFEPS(filename4, 4, 6)
PlotCapabilityVsSpread(capabilities,spreads, 0, 1, legendtext=familyNames, legendpos="topright", pch=1:nFamilies, col=familyColours)
ClosePDFEPS()



# METHOD (DIFFICULTY FROM POPULATION)
# we will only have two agents. It doesn't make sense to calculate a populational difficulty!

