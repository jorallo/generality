##############################################################################
#
#   THIS APPLIES GENERALITY ANALYSIS TO THE LAMBDA-1 TEST DATA (lambda_vN.N.R)
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
#  - V.1.0    Adapting from old code from Lambda One.
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################



setwd("/.../ **** PUT YOUR FOLDER HERE ****")
source("generality.R")

DATADIR <- "lambda.data"
OUTPUTDIR <- "lambda.results"








################################################################################
################################################################################
# RESULTS #############
################################################################################
################################################################################

###############
# Loading data
###############


# Column indexes
NCELLS <- 1
NACTIONS <- 2
SPACEDESC <- 3
REWARDPATTERN <- 7
REWARDPATTERNLENGTH <- 8
AGENT <- 10
BEFORE_AFTER <- 12
REWARD <- 16

# This is to prevent read.table to take the pattern as a number
colClasses <- rep(NA, 16)
colClasses[REWARDPATTERN] <- 'character'


RED <- 'coral'
BLUE <- 'blue3'
PCH <- 4  


dd <-  read.table(paste0(DATADIR, "/Experimentos.csv"), header = FALSE,sep=";",dec=",", colClasses= colClasses)



###########
# Choosing some difficulty functions
###########



comp1 <- dd[[REWARDPATTERNLENGTH]]   # Just the length of the pattern for Good and Evil

comp2 <- log(dd[[NCELLS]] * dd[[NACTIONS]] * dd[[REWARDPATTERNLENGTH]])   # A combination of no. of cells, no. of actions and length of the pattern

# ANOTHER OPTION: Environment description + pattern description --> WINZIP.

# This uses the space
cadenes <- paste(dd[[SPACEDESC]], dd[[REWARDPATTERN]])
# cadenes <- paste(dd[[REWARDPATTERN]], dd[[SPACEDESC]])
# cadenes <- paste(dd[[SPACEDESC]], dd[[REWARDPATTERN]], dd[[REWARDPATTERN]])

# This doesn't use the space
cadenes <- paste(dd[[REWARDPATTERN]], "") # to convert to string

comp3 <- 1:length(cadenes)
for (i in 1:length(cadenes)) {
  comp3[i] <- length(memCompress(cadenes[i], type="gzip"))
}
# comp3 <- comp3 * dd[[REWARDPATTERNLENGTH]]
# comp3 <- log2(comp3)



 cadena1 <- dd[[SPACEDESC]]
 cadena2 <- dd[[REWARDPATTERN]]

cadena1 <- paste(cadena1,"")  # Convert to string
comp4 <- 1:length(cadenes)
for (i in 1:length(cadenes)) {
  comp4[i] <- length(memCompress(cadena1[i], type="gzip")) + length(memCompress(cadena2[i], type="gzip"))
}
# comp4 <- comp4 * dd[[REWARDPATTERNLENGTH]]

comp4 <- log2(comp4)


DIFFICULTY <- 2
if (DIFFICULTY == 1) {
  complexity <- comp1
} else if (DIFFICULTY == 2) {
  complexity <- comp2
} else if (DIFFICULTY == 3) {
  complexity <- comp3
} else if (DIFFICULTY == 4) {
  complexity <- comp4
} 



dd <- cbind(dd, complexity)

COMPLEXITY <- length(dd) # index for the complexity column











###########
# PREPARING THE DATA
###########



# These three are not found in the file...
#random <- (dd[,AGENT]=='Random')
#trivialfollower <- (dd[,AGENT]=='TrivialFollower')
#oracle <- (dd[,AGENT]=='Oracle')
qlearning <- (dd[,AGENT]=='QLearning')
human <- (dd[,AGENT]=='Human')




###########
# ONE SINGLE PLOT. NO GENERALITY ANALYSIS YET.
###########

studyName <- paste0("lambda-diff", DIFFICULTY)
  
OpenPDFEPS(paste0(studyName, "-vs-reward"), 5, 5)
#pdf(paste("complexity", outputfile, ".pdf"), height= PDFSIZE, width= PDFSIZE)

data_all <- dd[,]
names(data_all) <- c('Cells', 'at2', 'at3', 'at4', 'at5', 'at6', 'at7', 'PatternLength', 'at9', 'Agent','at11', 'at12', 'at13', 'at14', 'at15', 'Reward')
names(data_all)[COMPLEXITY] <- 'Complexity'

# data_all <- dd[,]
# names(data_all) <- c('Cells', 'at2', 'at3', 'at4', 'at5', 'at6', 'at7', 'at8', 'at9', 'Agent','at11', 'at12', 'at13', 'at14', 'at15', 'Reward')

for(i in c(1:20)) {
if (i == 1)
ntest <- rep(i,14)
else
ntest <- c(ntest, rep(i,14))
}


data_all <- cbind(data_all, ntest)

NTEST <- length(data_all) # index for the complexity column
names(data_all)[NTEST] <- 'ntest'


aov.meu <- aov(Reward~Agent*Complexity, data= data_all)
summary(aov.meu)

#data_qlearning <- dd[(qlearning),]
data_qlearning <- data_all[(qlearning),]
# nrow(data_qlearning)
# names(data_qlearning) <- c('Cells', 'at2', 'at3', 'at4', 'at5', 'at6', 'at7', 'at8', 'at9', 'Agent','at11', 'at12', 'at13', 'at14', 'at15', 'Reward')
# names(data_qlearning)[COMPLEXITY] <- 'Complexity'

#data_human <- dd[(human),]
data_human <- data_all[(human),]
#nrow(data_human)
#names(data_human) <- c('Cells', 'at2', 'at3', 'at4', 'at5', 'at6', 'at7', 'at8', 'at9', 'Agent','at11', 'at12', 'at13', 'at14', 'at15', 'Reward')
#names(data_human)[COMPLEXITY] <- 'Complexity'



plot(c(min(data_all[[COMPLEXITY]]),max(data_all[[COMPLEXITY]])), c(-1,1), col='white', xlab='Difficulty', ylab='Average Reward')

points(data_qlearning[[COMPLEXITY]], data_qlearning[[REWARD]], col = RED, pch=21)
points(data_human[[COMPLEXITY]], data_human[[REWARD]], col=BLUE, pch=PCH)

length(data_qlearning[[COMPLEXITY]]) # 140 instances
length(data_human[[COMPLEXITY]]) # 140 instances

cor(data_qlearning[[COMPLEXITY]], data_qlearning[[REWARD]], method = "spearman")
cor(data_human[[COMPLEXITY]], data_human[[REWARD]], method = "spearman")

cor(data_qlearning[[COMPLEXITY]], data_qlearning[[REWARD]], method = "pearson")
cor(data_human[[COMPLEXITY]], data_human[[REWARD]], method = "pearson")

SHOW_REGRESSION <- FALSE

if (SHOW_REGRESSION) {
# Reg<-lm(Y~X,Data)
Reg<-lm(Reward~Complexity, data_qlearning)
# abline(Reg)
lines(Reg$fitted.value~data_qlearning$Complexity, col='darkred', lty=1)
#, type="o", pch=21, lty=1)

# Reg<-lm(Y~X,Data)
Reg<-lm(Reward~Complexity, data_human)
# abline(Reg)
lines(Reg$fitted.value~data_human$Complexity, col='darkblue', lty=3)
#, type="o", pch=PCH, lty=2)
}

legtext <- c("QLearning", "Human")
col <- c(RED, BLUE)
pch <- c(21,PCH)
# legend("bottomright", inset=0.05, legend=legtext, col=col, lty=lty, lwd=lwd, text.col=col, bty="n")
legend("bottomright", inset=0.05, legend=legtext, col=col, pch=pch, text.col=col, bty="o")

dev.off()

















###########
# GENERALITY ANALYSIS
###########

#myNames <- C("Random", "TrivialFollower", "Oracle", "QLearning", "Human")

myNames <- c("QLearning", "Human")

difficulties_qlearning <- data_qlearning[[COMPLEXITY]]
responses_qlearning <- data_qlearning[[REWARD]]
hist(responses_qlearning)
responses_qlearning[responses_qlearning < 0] <- 0  # All negative responses set to 0
hist(responses_qlearning)


difficulties_human <- data_human[[COMPLEXITY]]
responses_human <- data_human[[REWARD]]
hist(responses_human)
responses_human[responses_human < 0] <- 0  # All negative responses set to 0
hist(responses_human)

difficulties_human - difficulties_qlearning  # THEY ARE THE SAME DIFFICULTIES, SAME ITEMS!!!

difficulties <- difficulties_qlearning

difficulties  # 140 difficulties


###############
# We print the ACC for all agents
###############

KMAXfact <- 1.3

KMIN <- 0 # min(difficulties)
KMAXstrict <- max(difficulties)
KMAX <- KMAXstrict * KMAXfact  # Make space
OpenPDFEPS(paste0(studyName, "-ACC-humans"), 4, 6)
r <- PlotACCwithIndicators(difficulties,responses_human,xmin=KMIN,xmax=KMAX,legendpos=0.92, legendposx = KMAXstrict*0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, EXTEND_CURVE= TRUE) #, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
ClosePDFEPS()

OpenPDFEPS(paste0(studyName, "-ACC-qlearning"), 4, 6)
r <- PlotACCwithIndicators(difficulties,responses_qlearning,xmin=KMIN,xmax=KMAX,legendpos=0.92, legendposx = KMAXstrict*0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, EXTEND_CURVE= TRUE) #, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
ClosePDFEPS()





###############
# We print the ACC for all agents. NOW BINNED DIFFICULTY
###############

studyName <- paste0("lambda-bindiff", DIFFICULTY)

# Equal width binning
binDifficulties <- BinDifficulties(difficulties, 10) 

OpenPDFEPS(paste0(studyName, "-ACC-humans"), 4, 6)
r <- PlotACCwithIndicators(binDifficulties,responses_human,xmin=KMIN,xmax=KMAX,legendpos=0.92, legendposx = KMAXstrict*0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, EXTEND_CURVE= TRUE) #, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
ClosePDFEPS()

OpenPDFEPS(paste0(studyName, "-ACC-qlearning"), 4, 6)
r <- PlotACCwithIndicators(binDifficulties,responses_qlearning,xmin=KMIN,xmax=KMAX,legendpos=0.92, legendposx = KMAXstrict*0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, EXTEND_CURVE= TRUE) #, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 
ClosePDFEPS()




