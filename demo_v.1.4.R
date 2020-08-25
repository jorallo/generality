##############################################################################
#
#   THIS IS A DEMO FOR GENERALITY ANALYSIS (demo_vN.N.R)
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
#  - v.0.1 - 0.9 Early versions developed during 2018
#  - V.1.0    27 Mar 2019. First operative version. It shows differents kinds of plots, assuming a logistic curve, a uniform curve, with different slopes and areas.
#  - V.1.1    28 Mar 2019. Some simple examples included at the beginning. Some ResultMatrices added at the end
#  - v.1.2    04 Apr 2019. Includes more examples with more functions being illustrated.
#  - v.1.3    09 Jul 2019. Includes a random responseMatrix at the end.
#  - v.1.4    07 Aug 2019. New examples for the paper
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


setwd("/OneDrive - UPV/__SUBMISSIONS/2018/General intelligence/__NewCode__")
source("generality.R")

OUTPUTDIR <- "demo.results"



##############################################
#
# Very simple curves
#
##############################################

nitems <- 10000
KMIN <- 0
KMAX <- 10

# FIRST SETTING: No examples with the same difficulty
difficulties <- seq(KMIN, KMAX,length.out=nitems)
difficulties

# Step curve
responses <- c(rep(1,nitems/2), rep(0,nitems/2)) 


CalculateIndicators(difficulties, responses, kmin=KMIN,kmax=KMAX)
  
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Complementary step curve
responses <- c(rep(0,nitems/2), rep(1,nitems/2)) 
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Horizontal curve with binary responses
responses <- c(rep(c(1,0),nitems/2)) 
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Horizontal curve with 0.5 responses
responses <- rep(0.5,nitems)
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Horizontal curve with random responses
responses <- runif(nitems)
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Linearly Decreasing curve
responses <- seq(1,0,length.out=nitems)
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)







# SECOND SETTING: Examples with the same difficulty
HOWMANY_PER_DIFFICULTY <- 10
difficulties <- seq(KMIN, KMAX,length.out=nitems/HOWMANY_PER_DIFFICULTY)
difficulties <- sort(rep(difficulties, HOWMANY_PER_DIFFICULTY))
difficulties


# Step curve
responses <- c(rep(1,nitems/2), rep(0,nitems/2)) 
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Complementary step curve
responses <- c(rep(0,nitems/2), rep(1,nitems/2)) 
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Horizontal curve with binary responses
responses <- c(rep(c(1,0),nitems/2)) 
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Horizontal curve with 0.5 responses
responses <- rep(0.5,nitems)
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Horizontal curve with random responses
responses <- runif(nitems)
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

# Linearly Decreasing curve
responses <- seq(1,0,length.out=nitems)
r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)




# THIRD SETTING: We don't generate the same number of examples for each difficulty

# We use a normal distribution to generate the difficulties
difficulties <- rnorm(nitems, mean=(KMAX-KMIN)/2, sd=1)
responses <- (KMAX-difficulties)/(KMAX-KMIN)+rnorm(nitems,0, 0.05)  # We make a linear relation between difficulty and response, with some random noise
plot(difficulties, responses, ylim=c(0,1))

r <- PlotACCwithIndicators(difficulties,responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r)

rext <- OptimisticallyExtrapolateACC(difficulties, responses, KMIN, KMAX)
r0 <- PlotACCwithIndicators(rext$difficulties,rext$responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
unlist(r0)

# Now we bin the difficulties

numBins <- 30
binnedDifficulties <- BinDifficulties(difficulties, numBins) 

count(binnedDifficulties)
minBin <- 10
instanceSelection <- FilterDifficultiesByFrequency(binnedDifficulties, minBin)
# Just choose 0 for no filtering
#instanceSelection <- FilterDifficultiesByFrequency(binnedDifficulties, 0)
count(binnedDifficulties[instanceSelection])
r2 <- OptimisticallyExtrapolateACC(binnedDifficulties[instanceSelection], responses[instanceSelection], KMIN, KMAX)
r3 <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE)  #, mylty= 3) 



##############################################
#
# More complex generator and curves (used for the reports and papers)
# We can generate different kinds of curves with different parameters
#
##############################################


# MAIN OPTION: kind of function
#function_type <- "-logistic"  # logistic
#function_type <- "-strunif"
#function_type <- "-strunif2"  # Uniform ending in the last bin
#function_type <- "-decunif"
function_type <- "-normal"

MAX_GENERALITY <- TRUE # If true, prints generalities > 10000 as INF.

# MAIN OPTION: "a" (SLOPE)
a=1.0 # 4.0 # 2500 # 4.0  # slope  : a = reliability or generality  (for ICC: discrimination)
#b=4.95  # position : ability (usually adds 0.05)                    (for ICC: difficulty

# MAIN OPTION: "b" (POSITION)
b=10 # 13  # position : ability (usually adds 0.05)  (for ICC: difficulty

c=0

threshold <- 0.8   # 1 - epsilon  // if threshold = NULL, no threshold
                             # Only applicable for strunif, strunif2 and decunif, basically setting the "height" of curve
offset0 <- 0

resolution <- 10  # x-resolution: It used to be 
# MAIN OPTION: "nh" (NUMBINS)
numbins <- 25 * resolution
binsize <- 1 / resolution

# MAIN OPTION: "ppd" (POINTSPERDIFF)
pointsperdiff <- 100 # 0 # 1000


# MAIN OPTION: print tests. This is just for some particular plots at the end of the article
print_testA <- FALSE
testband_locationA <- 0.5

if (print_testA) {
  test_stringA <- paste0("-testbandA", testband_locationA)
} else {
  test_stringA <- ""
}

print_testB <- FALSE
testband_locationB <- 0.1

if (print_testB) {
  test_stringB <- paste0("-testbandB", testband_locationB)
} else {
  test_stringB <- ""
}

print_testC <- FALSE
testband_locationC <- 0.05

if (print_testC) {
  test_stringC <- paste0("-testbandC", testband_locationC)
} else {
  test_stringC <- ""
}

print_testD <- FALSE
testband_locationD <- 0.5

if (print_testD) {
  test_stringD <- paste0("-testbandD", testband_locationD)
} else {
  test_stringD <- ""
}




set.seed(0)

show <- NULL
showmean <- NULL
showvar <- NULL

xp <- rep(0,numbins*pointsperdiff)
yp <- rep(0,numbins*pointsperdiff)
cont <- 1
for (i in 1:numbins) {
  vec <- NULL
  for (j in 1:pointsperdiff) {
    
    if (function_type == "-logistic") {
      theta <- (i-1)*binsize+offset0
      
      val <- c + (1-c)/(1+exp(-a*(theta+rnorm(1,0)-b)))  
      
      val <- 1 - val  
      if (!is.null(threshold)) {
        val <- c + (1-c)/(1+exp(a*(theta-b))) 
        val <- 1*(val >= runif(1,0,1))
      }
    } else if (function_type == "-normal") {
      # theta <- i+offset0 + rnorm(1,0,1/a)  # the ability has a normal variability (reliability = a)
      #  val <- 1*(theta >= b)   # The value is 1 if theta >= b. Otherwise 0
      #  val <- 1 - val
      
      my_theta <- b + rnorm(1,0,1/a)  # the ability has a normal variability (reliability = a)
      my_diff <- (i-1)*binsize+offset0   # the difficulty of the item
      val <- 1*(my_theta >= my_diff)
    } else if (function_type == "-strunif") {
      val <- runif(1,0,1)
      if (!is.null(threshold)) {
        val <- 1*(val >= threshold)    
      }
    } else if (function_type == "-strunif2") {
      if (i != numbins) {
        val <- runif(1,0,1)
        if (!is.null(threshold)) {
          val <- 1*(val >= threshold)    
        }
      } else {
        val <- 0
      }  
    } else if (function_type == "-decunif") {
      val <- runif(1,0,(numbins-i)/numbins)
      if (!is.null(threshold)) {
        val <- 1*(val >= threshold)    
      }
    } else {
      ERROR_function_type_unknown()
    }
    xp[cont] <- (i-1)*binsize + offset0
    yp[cont] <- val
    vec[j] <- val
    cont <- cont + 1
  }
  show <- c(show,list(vec))
  showmean[i] <- mean(vec)
  showvar[i] <- popvar(vec)
}

#show <- list(c(0.3,0.2,0.7), c(0.4,0.2), c(0.8,0.2), c(0.8,0.2))
kmin <- offset0 # +1 # + 1
kmax <- numbins / resolution + kmin - 1 # length(show)+ kmin-1




KMIN <- kmin
KMAX <- kmax

filename <- sprintf("ACC-a%1.1f-b%1.1f-nh%d-ppd%d-th%1.1f%s%s%s%s%s",a,b,numbins,pointsperdiff,threshold,function_type, test_stringA, test_stringB, test_stringC, test_stringD)
OpenPDFEPS(filename, 5, 5)


# This is using the aggregation. This is the old way (we aggregate outside the function)
# difficultiesForPlot <- unique(xp) # xrange
# responseAreasForPlot <- showmean
#plot(difficultiesForPlot,responseAreasForPlot)
# r <- PlotACCwithIndicators(difficultiesForPlot,responseAreasForPlot,xmin=KMIN,xmax=KMAX,legendpos=0.9,  mypch=".")  #, mylty= 3) 


difficultiesI <- xp  #-1
responsesI <- yp
# plot(difficultiesI,responsesI)  # slow
#r <- PlotACCwithIndicators(difficultiesI,responsesI,xmin=KMIN,xmax=KMAX,legendpos=0.9,  mypch=".", SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
r <- PlotACCwithIndicators(difficultiesI,responsesI,  mypch=".", SHOW_GLOBAL_MEAN=TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 


# Used for some plots in the paper

WIDTH_BAND <- 0.05

if (print_testA) {
  # lines(c(kmin,kmax),c(testband_location,testband_location),lty=1, col="black", lwd=20)
  rect(kmin, testband_locationA-WIDTH_BAND, kmax+1, testband_locationA+WIDTH_BAND, density = 5, border = "red", col="red")
} 

if (print_testB) {
  # lines(c(kmin,kmax),c(testband_location,testband_location),lty=1, col="black", lwd=20)
  rect(kmin, testband_locationB-WIDTH_BAND, kmax+1, testband_locationB+WIDTH_BAND, density = 5, border = "green", col="green")
} 

SAME_AREA <- FALSE # If same area make this 1. If same effort it gets tricky
MY_WIDTH <- kmax-kmin

if (print_testC) {
  if (SAME_AREA) {
    rect(b, testband_locationC-WIDTH_BAND, b+((kmax-kmin)/(MY_WIDTH/10*SAME_AREA)), testband_locationC+WIDTH_BAND, density = 50, border = "orange", col=add.alpha("orange",0.5))
  } else {  # more tricky to calculate
    rect(b, testband_locationC-WIDTH_BAND, b+7.61, testband_locationC+WIDTH_BAND, density = 50, border = "orange", col=add.alpha("orange",0.5))
  }    
} 

if (print_testD) {
  rect(b, testband_locationD-0.5, b+((kmax-kmin)/MY_WIDTH), testband_locationD+0.5, density = 50, border = "violet", col=add.alpha("violet",0.5))
} 


ClosePDFEPS()




##############################################
#
# Now we explore ResponseMatrices
#
##############################################

nitems <- 30
KMIN <- 10
KMAX <- 20

# FIRST SETTING: No examples with the same difficulty
difficulties <- seq(KMIN, KMAX,length.out=nitems)
difficulties

nsubjects <- 100
m <- matrix(nrow=nsubjects, ncol=nitems)
responseMatrix <- data.frame(m)

for (i in 1:nsubjects) {
  # All step-function subjects with different abilities (from low to high)
  step <- round(((i-1) / (nsubjects-1)) * (nitems))  # With this we ensure no column is constant
  print(step)
    responses <- c(rep(1,step), rep(0,nitems-step))
  responseMatrix[i,] <- responses
}

studyName <- "demo.steps"
r <- PerformFullStudy(studyName, responseMatrix, difficulties) #, DO_FA=FALSE)


for (i in 1:nsubjects) {
  # All uniform subjects with different abilities (from low to high)
  value <- i/(nsubjects+1)
  print(value)
  responses <- rep(value,nitems)
  responseMatrix[i,] <- responses
}

studyName <- "demo.uniform"
r <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA=FALSE, DO_IRT=FALSE)



for (i in 1:nsubjects) {
  # All subjects with random results
  responses <- round(runif(nitems))
  responseMatrix[i,] <- responses
}

studyName <- "demo.random"
r <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA=FALSE, DO_IRT=FALSE)


for (i in 1:nsubjects) {
  # All subjects with random results
  responses <- 1*(runif(nitems) <= i/(nsubjects+1))  # The probability of 1 increases as we move subject 
  responseMatrix[i,] <- responses
}

studyName <- "demo.randomUnifCap"
r <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA=FALSE, DO_IRT=FALSE)





##############################################
#
# The one at the beginning of the paper
#
##############################################

nsubjects <- 2
nitems <- 24
KMIN <- 0
KMAX <- 2
m <- matrix(nrow=nsubjects, ncol=nitems)
responseMatrix <- data.frame(m)

#responseMatrix[1,1] <- c(0.75, 0.65, 0.70, 0.75, 0.65, 0.70, 0.75, 0.65, 0.70, 0.70)
#responseMatrix[2,1] <- c(0.00, 1.00, 0.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 0)
#responseMatrix[3,1] <- c(1.00, 1.00, 1.00, 1.00, 1.00, 0, 0, 0, 0, 0)

nDomains <- 4
r1 <- c(1,1,1,0, 1,1,1,0, 1,1,0,0, 1,1,1,0, 1,1,0,0, 1,1,0,0)
r2 <- c(1,1,1,1, 1,1,1,1, 1,0,1,1, 1,1,1,1, 0,0,0,0, 0,0,0,0)
responseMatrix[1,] <- r1
responseMatrix[2,] <- r2

#difficulties <- seq(KMIN, KMAX,length.out=nitems)
#difficulties <- c(rep(0.5,8), rep(1.5,8), rep(2.5,8))
difficulties <- c(rep(0,8), rep(1,8), rep(2,8))
difficulties


#PlotACCwithIndicators(difficulties, r1, kmax = 2.2, startplot=TRUE, mycol="blue", mylty=1, LEGEND=FALSE, LEGEND2=TRUE, name="Ag1", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE= TRUE, legendpos=0.95)
#PlotACCwithIndicators(difficulties, r2, kmax = 2.2, startplot=FALSE, mycol="darkgreen", mylty=1, LEGEND=FALSE, LEGEND2=TRUE, name="Ag2", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE= TRUE, legendpos=0.9)



OpenPDFEPS("demo.agent1", 5, 5)

PlotACCwithIndicators(difficulties, r1, kmin= -0.2, kmax = 2.38, startplot=TRUE, mycol="blue", mylty=1, LEGEND=TRUE, LEGEND2=FALSE, name="Ag1", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE= TRUE, legendpos=0.95, legendposx=1.5, EDIFFICULTY = FALSE, SPREAD = FALSE, xaxt='n')
#axis(side = 1, at = c(0.5,1.5),labels = T, col="white", col.ticks= "white", col.axis="white")
axis(side = 1, at = c(0,1,2),labels = c("low", "medium", "high"))


#textPos <- 2.15
textPos <- -0.15
WIDTH_BAND <- 1/nDomains

myColours <- c("red", "darkgreen", "orange", "purple")

score1 <- mean(r1[seq(1,nitems,nDomains)])
rect(0, 0, 2*score1, WIDTH_BAND, density = 5, border = myColours[1], col= myColours[1]) 
text(textPos,0.5*WIDTH_BAND, "A", col=myColours[1])
score2 <- mean(r1[seq(2,nitems,nDomains)])
rect(0, WIDTH_BAND, 2*score2, 2*WIDTH_BAND, density = 5, border = myColours[2], col=myColours[2]) 
text(textPos,1.5*WIDTH_BAND, "B", col=myColours[2])
score3 <- mean(r1[seq(3,nitems,nDomains)])
rect(0, 2*WIDTH_BAND, 2*score3, 3*WIDTH_BAND, density = 5, border = myColours[3], col=myColours[3]) 
text(textPos,2.5*WIDTH_BAND, "C", col=myColours[3])
score4 <- mean(r1[seq(4,nitems,nDomains)])
rect(0, 3*WIDTH_BAND, 2*score4, 4*WIDTH_BAND, density = 5, border = myColours[4], col=myColours[4]) 
text(textPos,3.5*WIDTH_BAND, "D", col=myColours[4])

ClosePDFEPS()


OpenPDFEPS("demo.agent2", 5, 5)


PlotACCwithIndicators(difficulties, r2, kmin= -0.2, kmax = 2.38, startplot=TRUE, mycol="blue", mylty=1, LEGEND=TRUE, LEGEND2=FALSE, name="Ag1", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE= TRUE, legendpos=0.95, legendposx=1.5, EDIFFICULTY = FALSE, SPREAD = FALSE, xaxt='n')
axis(side = 1, at = c(0,1,2),labels = c("low", "medium", "high"))


score1 <- mean(r2[seq(1,nitems,nDomains)])
rect(0, 0, 2*score1, WIDTH_BAND, density = 5, border = myColours[1], col= myColours[1]) 
text(textPos,0.5*WIDTH_BAND, "A", col=myColours[1])
score2 <- mean(r2[seq(2,nitems,nDomains)])
rect(0, WIDTH_BAND, 2*score2, 2*WIDTH_BAND, density = 5, border = myColours[2], col=myColours[2]) 
text(textPos,1.5*WIDTH_BAND, "B", col=myColours[2])
score3 <- mean(r2[seq(3,nitems,nDomains)])
rect(0, 2*WIDTH_BAND, 2*score3, 3*WIDTH_BAND, density = 5, border = myColours[3], col=myColours[3]) 
text(textPos,2.5*WIDTH_BAND, "C", col=myColours[3])
score4 <- mean(r2[seq(4,nitems,nDomains)])
rect(0, 3*WIDTH_BAND, 2*score4, 4*WIDTH_BAND, density = 5, border = myColours[4], col=myColours[4]) 
text(textPos,3.5*WIDTH_BAND, "D", col=myColours[4])


ClosePDFEPS()


studyName <- "demo.agents"

results <- PerformFullStudy(studyName, responseMatrix, difficulties, DO_FA= FALSE, DO_IRT= FALSE, ESTIMATESIGMOID= FALSE)
filename <- paste0(studyName, ".capability-vs-spread-BETTER")
OpenPDFEPS(filename, 4, 6)
legtext <- c("agent a", "agent b")
PlotCapabilityVsSpread(results$capabilities,results$spreads, min(difficulties), max(difficulties), legendtext=legtext, legendpos="topright", pch = c(2,3), col = c("cyan","grey"), SHOW_METRICS = FALSE, GenYAXIS=TRUE)
ClosePDFEPS()


