##############################################################################
#
#                       GENERALITY ANALYSIS (generality_v.N.N.R)
#
##############################################################################
#
# This is R code for doing Generality Analysis, based on the metric of generality first introduced in 
#   J. Hernandez-Orallo "I.G.", March 15th, 2018
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
# 
# Some code (IRT and FA analysis) has been adapted from Bao Sheng Loe, University of Cambridge.
#
# LICENCE:
#   GPL
#
# VERSION HISTORY IN FILE generality.R, WHICH LOADS THIS ONE
#
##############################################################################




##############################################################################
######################## LIBRARIES AND I/O OPTIONS ###########################
##############################################################################


if (!require('psych')) {  
  install.packages('psych')
  require(psych)
}  

if (!require('mirt')) { 
  install.packages('mirt')
  require(mirt)
}

if (!require('plyr')) { 
  install.packages('plyr')
  require(plyr)
}


OUTPUT <- "."

OpenPDFEPS <- function(file, PDFheight=PDFheight, PDFwidth=PDFwidth) {
  if (PDFEPS == 1) {
    pdf(paste(OUTPUTDIR, "/", file, ".pdf", sep=""), height= PDFheight, width= PDFwidth)
  } else if (PDFEPS == 2) {
    postscript(paste(OUTPUTDIR, "/", file, ".eps", sep=""), height= PDFheight, width= PDFwidth, horizontal=FALSE)
  }
}

ClosePDFEPS <- function() {
  if (PDFEPS != 0)
    dev.off()
}


PDFEPS <- 1 # 0 None, 1 PDF, 2 EPS
PDFheight= 8 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one plot
PDFwidth= 8 # 7 by default




##############################################################################
################################# FUNCTIONS ##################################
##############################################################################

# Some basic functions to calculate vars of a matrix, rowwise and columnwise.

rowVars <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

rowPopVars <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2])
}

colVars <- function(x, ...) {
  apply(x, 2, var)
}

colMedians <- function(x, ...) {
  apply(x, 2, median)
}

popvar <- function(x) {
  n <- length(x)
  var(x)*(n-1)/n
}

popsd <- function(v) {
  sqrt(popvar(v))
}

colPopVars <- function(x, ...) {
  apply(x, 2, popvar)
}





# Given a mean and a variance, returns the alpha and beta parameters of a beta distribution
EstBetaParameters <- function(mu, var) {
  # This function basically solves this set of equations: (see e.g.: https://en.wikipedia.org/wiki/Beta_distribution)
  # varbeta <- a*b / ((a+b)^2 * (a+b+1))
  # meanbeta <- a / (a+b)
  
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# Maps a value to its quantile, given a parametric distribution described by a mean and a variance
Map2Quantile <- function(value, m, v, distName="NORMAL") {
  # mean and variance
  
  if (distName=="NORMAL") {
    cut <- pnorm(value, m, v)
  } else if (distName=="BETA") {
    r <- EstBetaParameters(m, v)
    a <- r$alpha
    b <- r$beta
    
    cut <- pbeta(value,a,b)  
  } else {
    print("Unknown distribution")
    ERROR()
  }
  
  return(cut)  
}

# For METHOD 5: STEPWISE DISTRIBUTIONAL REFERENCE
GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference <- function(value, m, v, nDifficulties, distName="NORMAL", TIED_VALUE = 0.5) {
  difficulties <- seq(0,1,length.out=nDifficulties)

  cut <- Map2Quantile(value, m, v, distName)
  
  responses <- (cut > difficulties)*1.0  # Step function at cut
  responses <- responses + (cut == difficulties)*TIED_VALUE  # We add 0, 0.5 or 1 for ties.
  
  ret <- NULL
  ret$responses <- responses
  ret$difficulties <- difficulties
  ret$ability <- cut   # The ability turns out to be the cut!
                      #  .. the position of the curve, and as it is a step, this is the ability   
  return(ret)
} 

# For METHOD 5: STEPWISE DISTRIBUTIONAL REFERENCE
# Generates n stepwise ACCs per each value in values, where n=nDifficulties, which will go from 0 to 1
#Old name Quantile2Reference
GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference_MULTIPLEVALUES <- function(values, reference_values, reference_vars, nDifficulties, distName="NORMAL", TIED_VALUE = 0.5) {
 # difficulties <- seq(0,1,length.out=nDifficulties)
  nValues <- length(values)
  
#  difficultiesRep <- rep(difficulties, nValues)
  difficultiesRep <- NULL
  responsesRep <- NULL
  abilitiesRep <- NULL

  for (i in 1:nValues) {
    # Generates a stepwise curve
    m <- reference_values[i]
    v <- reference_vars[i]  
    value <- values[i]
  #  cut <- Map2Quantile(values[i], m, v, distName)
  #  responses <- (cut >= difficulties)*1.0  # Step function at cut
    r <- GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference(value, m, v, nDifficulties, distName, TIED_VALUE) 
    # Adds the values for the result
    responsesRep <- c(responsesRep, r$responses)
    difficultiesRep <- c(difficultiesRep, r$difficulties)
    abilitiesRep <- c(abilitiesRep, r$ability)
  }
  
  ret <- NULL
  ret$difficulties <- difficultiesRep
  ret$responses <- responsesRep
  ret$abilities <- abilitiesRep
  return(ret)
  
}


# For METHOD3A: REFERENCE POPULATIONAL (USING A COLUMN OR MEAN)
# REF_METHOD="COLUMN", "MEAN" or "MEDIAN" (deprecated) 
GenerateBinarisedResponsesAndDifficultiesFromCrispReference <- function(df, REF_METHOD="MEAN", REF_ROW= NULL, COMPARISON = "GREATER_OR_EQUAL") {
  
  if (REF_METHOD == "COLUMN") {
    reference <- df[REF_ROW,] 
  } else if  (REFERENCE == "MEAN") {
    reference <- colMeans(df)
  } else if  (REFERENCE == "MEDIAN") {
    reference <- colMedians(df)
  } 
  
  dfBinary <- df
  for (i in 1:nrow(df)) {
    if (COMPARISON == "GREATER_OR_EQUAL") {  # Note that here we don't accept 0.5 responses for equal cases
      dfBinary[i,] <- (df[i,] >= reference)
    } else if (COMPARISON == "GREATER") {
      dfBinary[i,] <- (df[i,] > reference)
    } else if (COMPARISON == "GREATER_BUTEQUALHALF") {
      dfBinary[i,] <- ((df[i,] > reference) + (df[i,] >= reference)) / 2  # 0.5 for equal cases
    }  
  }
  
  
  # summary(dfBinary)
  # head(dfBinary)
  # nrow(dfBinary)
  
  # We remove this, but only for calculating the difficulties
  dfBinaryNoRef <- dfBinary[setdiff(rownames(dfBinary),REF_ROW),]  # Remove REF results (reference)
  nrow(dfBinaryNoRef)
  
  if (REF_METHOD == "COLUMN") {
    difficulties <- 1 - as.numeric(unname(colMeans(dfBinaryNoRef)))
  } else if (REF_METHOD == "MEAN") {
    difficulties <- 1 - as.numeric(unname(colMeans(dfBinary)))
  } else if (REF_METHOD == "MEDIAN") {
    ERROR()  # It doesn't make sense, as the difficulty would be constant for all
  }
  
  ret <- NULL
  ret$dfBinary <- dfBinary
  ret$difficulties <- difficulties
  return(ret)
}



# For METHOD 4a and 4b: STEPWISE POPULATIONAL
# Generates n stepwise ACCs from score using the values in item_scores as population, where n=number of values in item_scores. Difficulties will go from 0 to 1
GenerateStepwiseResponsesAndDifficultiesFromOrder <- function(score, item_scores, nDifficulties=NULL, METHOD="RANK", BREAK_TIES_AT_RANDOM= FALSE, DIFFICULTIES= "UNIFORM_DIFFICULTIES", TIED_VALUE = 0.5, AVERAGE_RANK=TRUE) {

  # item_scores <- c(1, 3, 23, 4, 4, 8, 7)
  # score <- 15
  
  m <- length(item_scores) # num of agents

  if (is.null(nDifficulties)) {
    nDifficulties <- m
  }
  
  if (BREAK_TIES_AT_RANDOM) {  # This is not recommended as it may depend on the seed a lot
    epsilon <- 0.000001
    item_scores <- item_scores + runif(m,-epsilon,epsilon)
  }
  
  s <- sort(item_scores, decreasing = FALSE) #, index.return=TRUE)
  
  if (METHOD == "RANK") {
    if (AVERAGE_RANK) {  # We are fair with ties
      mappedscore <- (sum(score > s))/m
      mappedscore <- mappedscore + 0.5* (sum(score == s))/m
    }
    else {
      mappedscore <- (sum(score >= s))/m
    }  
    if (DIFFICULTIES == "UNIFORM_WITH_TIES") {  # THIS DOESN'T WORK if nDifficulties is not equal to m
      difficulties <- (sort(rank(item_scores))-1)/(m-1)   # non-uniform (non-regular), as there might be cases with rank 4.5 because of ties
    }  else if (DIFFICULTIES == "UNIFORM_DIFFICULTIES") {
      difficulties <- seq(0,1,length.out=nDifficulties)  # uniform difficulties
    } else {
      print("OTHER REGULAR SCALES TO DERIVE DIFFICULTIES NOT IMPLEMENTED")
      ERROR()
    }
  } else { # SCALE
    sc <- scale(item_scores)
    mappedscore <- (score - attr(sc,"scaled:center")) / attr(sc, "scaled:scale")
    if (DIFFICULTIES == "IRREGULAR") {
#    item_scores <- scale(item_scores)
      difficulties <- sort(as.numeric(sc))  # non-regular
    } else if (DIFFICULTIES == "UNIFORM_DIFFICULTIES") {
      w <- 3 # With 3sd we cover 99.7 of the mass of a normal distribution
      difficulties <- seq(-w,w,length.out=nDifficulties)  # uniform difficulties
    } else {
      print("OTHER REGULAR SCALES TO DERIVE DIFFICULTIES NOT IMPLEMENTED")
      ERROR()
    }
  }
 
  responses <- NULL
  
  # OLD METHOD. DOESN'T WORK IF THE NUMBER OF DIFFICULTIES IS GREATER THAN m
  # for (i in 1:m) {
  #   if (score < s[i]) {
  #     responses[i] <- 0
  #   } else if (score > s[i]) {
  #     responses[i] <- 1
  #   } else {
  #     responses[i] <- TIED_VALUE
  #   }
  # }
  

  for (d in 1:nDifficulties) {
    if (mappedscore < difficulties[d]) {
      responses[d] <- 0
    } else if (mappedscore > difficulties[d]) {
      responses[d] <- 1
    } else {
      responses[d] <- TIED_VALUE
    }
  }
  
  difficulties
  responses
  if (length(responses) != length(difficulties)) {
    STOPPING()
  }
  
  ret <- NULL
  ret$difficulties <- difficulties
  ret$responses <- responses
  ret$ability <- mappedscore  # This is the position of the curve, and as it is a step, this is the ability
  return(ret)
  
}

CheckDifficultiesAndResponses <- function(difficulties, responses, mergeBins=TRUE) {
# Function
#   Checks that difficulties and responses have the same size, and that difficulties are ordered
#   Also, if mergebins is TRUE then averages all responses of exactly the same difficulty
# Args:
#   difficulties: the difficulties of each point/bin, on the x-axis
#   responses: the (average) responses for each difficulty, on the y-axis
# Returns: 
#   A named list with the size ($N), and ordered vectors of $difficulties and $responses
# 
  
  difficulties <- unlist(difficulties)  # Just in case it is not a vector
  responses <- unlist(responses)          # Just in case it is not a vector
  
    
  # Check sizes
  N <- length(responses)
  M <- length(difficulties)
  if (N != M) {
    print("ERROR: Difficulties and responses have different sizes")
    print(N)
    print(M)
    print("STOPPING!")
    STOP()    
  }
  
  if (min(responses) < 0) {
    print("ERROR: Some responses are below 0")
    print(N)
    print(M)
    print("STOPPING!")
    STOP()    
  }

  if (max(responses) > 1) {
    print("ERROR: Some responses are above 0")
    print(N)
    print(M)
    print("STOPPING!")
    STOP()    
  }
  
  if (mergeBins) {
    difficultiesUnique <- sort(unique(difficulties))
    NUnique <- length(difficultiesUnique)
    responsesUnique <- rep(0,NUnique)
    for (i in 1:NUnique) {
      responsesUnique[i] <- mean(responses[difficulties==difficultiesUnique[i]])
    }
    difficulties <- difficultiesUnique
    responses <- responsesUnique
    N <- NUnique
  }
  else {  # with mergBins, the difficulties gets ordered, so this is just done otherwise
    # We don't assume that the vector of difficulties is ordered, so we sort it just in case
    ord <- sort(difficulties, index.return=TRUE)
    difficulties <- ord$x
    responses <- responses[ord$ix]
  }    
  
  ret <- NULL
  ret$N <- unname(N)
  ret$difficulties <- unname(difficulties)
  ret$responses <- unname(responses)
  return(ret)
}


# Equal width binning
BinDifficulties <- function(difficulties, NBins=20) {
  
  N <- length(difficulties)
  
  mind <- min(difficulties) 
  maxd <- max(difficulties) 
  mind
  maxd 
  
  width <- (maxd - mind)/NBins
  epsilon <- 0.00001
  b <- seq(mind - epsilon, maxd + epsilon, length.out=NBins)  # We use the epsilon so the first element in included (otherwise it gives a NA)
  bincodes <- .bincode(difficulties, b)
  
  newDifficulties <- NULL
  for (i in 1:N) {
    newDifficulties[i] <- mind + width*(bincodes[i]-0.5)  # We assign the middle value fo reach been
  }
  
  newDifficulties
  min(newDifficulties)
  max(newDifficulties)
  
  #  plot(difficulties, newDifficulties)
  newDifficulties
}


# Filter those difficulties that have at least frequency >= MinCount
# Returns the index
FilterDifficultiesByFrequency <- function(difficulties, MinCount=3) {
  # r <- hist(difficulties)
  # r$counts
  c <- count(difficulties)
  #  c
  sel <- c[c["freq"]>=MinCount,"x"]
  instanceSelection <- difficulties %in% sel
  #  filteredDifficulties <- difficulties[difficulties %in% sel]
  
  #  return(filteredDifficulties)
  return(instanceSelection)
} 




CalculateIndicators <- function(difficulties, responses, kmin=NULL, kmax=NULL, VERBOSE=FALSE, mergeBins=TRUE) {
# Function
#   Takes an ACC series of (x,y) points (where x is the difficulty and y is the result) and
#   returns a vector with four indicators: capability, expected difficulty, spread and generality
# Args:
#   difficulties: the difficulties of each point/bin, on the x-axis
#   responses: the (average) responses for each difficulty, on the y-axis
#   kmin, kmax: minimum and maximum values of the difficulties (range) to be considered for the calculation. 
#               If this range is smaller then some points are not used, if it is larger, it is padded with a 1 area on the left
#   VERBOSE: whether some internal "prints" are shown. Used for debugging.
# Returns: 
#    A named list with $capability, expected difficulty ($expdiff), $spread and $generality
#  

  #Checks that difficulties and responses have the same size, and that difficulties are ordered
  res <- CheckDifficultiesAndResponses(difficulties, responses, mergeBins)
  N <- res$N
  difficulties <- res$difficulties
  responses <- res$responses
  
  
  # We now arrange things to take left (kmin) and right (kmax) limits into account
  LeftArea <- 0 # Area to be added on the left (0 for the moment)
  imin <- 1 # We start with the first index
  
  kmin0 <- min(difficulties)
  if (is.null(kmin)) {
    kmin <- kmin0
  } else {
    if (kmin < kmin0) {  # If kmin is lower than the minimum given difficulty we assume an area of 1 x the width 
      LeftArea <- 1*(kmin0 - kmin)
    } else if (kmin > kmin0) {  # if kmin is higher than the minimum given difficulty, we have to adapt the indexes of calculation
      for (i in 1:N) {
        if (kmin <= difficulties[i])
          break
      }
      imin <- i
    }
  }
  if (VERBOSE) {
    print(LeftArea)
    print(imin)
  }
  
  imax <- N
  kmax0 <- max(difficulties)
  if (is.null(kmax)) {   # NOT USED
    kmax <- kmax0
  } else {
    if (kmax < kmax0) {  # if kmax is lower than the minimum given difficulty, we have to adapt the indexes of calculation
      for (i in N:1) {
        if (kmax >= difficulties[i])
          break
      }
      imax <- i
    }
  }
  if (VERBOSE)
    print(imax)
  
  
  ##### ABILITY CALCULATION: IT IS THE AREA
  # Integral calculation Trapezoidal area
  #capability2 <- kmin # This would assume the response is one between 0 and kmin. But what if difficulties are negative? 
  # capability2 <- 0 # This is wrong if we have to assume a 1 area or if difficulties can be negative
  capability2 <-  LeftArea  # We use the 1 area between the range that has been used with kmin above.
  
  for (i in imin:(imax-1))  {
    capability2 <- capability2 + (difficulties[i+1]-difficulties[i])*(responses[i] + responses[i+1])/ 2
  }
 
  # Easier (but only works if binsize is ok and uniformly spaced, not general enough.)
  #capability <- sum(responses) - binsize / 2
  #capability
  # Should match sum(HitRatio)+START_DIFF+1
  
  capability <- capability2 + kmin  # We have to add kmin as capability depends on the location and the above area doesn't
  if (VERBOSE)
    print(capability)
  
  
  ##### RESOURCES CALCULATION: ABILITY * DIFFICULTIES: RESOURCES EFFECTIVELY USED (EffectiveEffort)
  # Integral calculation
  # Again, we first calculate the part on the left if necessary
  if (kmin < kmin0) {  # If kmin is lower than the minimum given difficulty we have to add the resources of this initial segment
    resources2 <-  (1/2)*(kmin0 - kmin)*(kmin0 + kmin)
  } else {
    resources2  <- 0    
  }

  for (i in imin:(imax-1))  {
    #  https://www.wolframalpha.com/input/?i=%5Cint_%7Ba%7D%5E%7Bb%7D+x*(c%2B(d-c)*(x-a)%2F(b-a))dx
    a <- difficulties[i]
    b <- difficulties[i+1]
    c <- responses[i] 
    d <- responses[i+1]
    my_area <- (1/6)*(b-a)*(a*(2*c+d)+b*(c+2*d))
    resources2 <- resources2 + my_area
  }

  # This alternative way would lose some precision if we're using areas
  # resources <- sum(responses*difficulties)
  # resources
  
  resources <- resources2
  if (VERBOSE)
    print(resources)  
  
  
  ##### EXPECTED DIFFICULTY CALCULATION: RESOURCES EFFECTIVELY USED PER SUCCESSFUL RESPONSES (EffectiveEffortNorm)
  resourcesNorm <- resources / capability2  # We  must use capability2, as this is the area, ignoring the location #sum(responses)
  # Resources used / ability = EXPECTED DIFFICULTY: we divide by abil to convert it into a density function whose sum f(x) is 1.
  expdiff <- resourcesNorm
  expdiff2 <- expdiff - kmin  # expdiff2 is independent of the location
  
  if (VERBOSE)
    print(expdiff)
  
  
  ##### SPREAD CALCULATION: 
  spreadsqr <- 2*capability2*expdiff2 - (capability2)^2  # Yet another equivalent expression. We use capability2 and expdiff2 as they are independent of the location (only matters when kmin is not 0)
  
  if (spreadsqr < 0) {
    if (spreadsqr > -0.00001) { # a precision issue
      spreadsqr <- 0
    } else {
      print("ERROR: Negative spread^2: 2*capability*expdiff - (capability)^2")
      print(2*capability*expdiff)
      print(capability^2)
      print("STOPPING!")
      STOP()
    }  
  }
  
  spread <- abs(sqrt(spreadsqr))  # Final version of spread
  if (VERBOSE)
    print(spread)
  
  
  #####  GENERALITY
  generality <- 1 / spread  
  if (VERBOSE)
    print(generality)
  
  
  # RETURNS the key indicators
  ret <- NULL
  ret$capability <- unname(capability)
  ret$expdiff <- unname(expdiff)
  ret$spread <- unname(spread)
  ret$generality <- unname(generality)
  return(ret) # returns capability, expected difficulty, spread and generality
}


# This is an optimistic extrapolation
OptimisticallyExtrapolateACC <- function(difficulties, responses, KMIN, KMAX) {
  kmin <- min(difficulties)
  kmax <- max(difficulties)
  halfdiff <- 0.00001  # This is important to avoid having two values at the same difficulty
  # Plot of aggregated results extrapolating on the left and right with steps
  difficulties <- c(KMIN,kmin-halfdiff,difficulties,kmax+halfdiff,KMAX)
  responses <- c(1,1,responses,0,0)
  
  ret <- NULL
  ret$difficulties <- difficulties
  ret$responses <- responses
  return(ret)
}



# In case, we don't want to set the KMAX and KMIN manually, this does automatically
calculateLIMITSforPlots <- function(difficulties, responses, ZEROMIN = "ifminnotnegative") {
  # We determine the limits for the plots of the ACCs and for the estimation of a sigmoid
  # We set the minimum at 0 and the maximum at double the point that is closest to the centre
  
  mindiff <- min(difficulties)
  
  maxdiff <- max(difficulties)
  
  #METHOD <- "LR"
  METHOD <- "INTERQUANTILE"

  
  if (METHOD == "LR") {
    #plot(difficulties, responses)
    r <- lm(responses ~ difficulties)
    int <- r$coefficients[1]
    coe <- r$coefficients[2]
    # Where is 1?
    KMIN <- (1-int)/coe
    coe*1.26+int
    # Where is 0.5? (halfpoint)
    KMID <- (0.5-int)/coe
    coe*21.78+int
    # where is 0
    KMAX <- (0-int)/coe
    coe*42.30+int
    
    KMIN
    KMAX
    

    if (mindiff < KMIN) {
      KMIN <- mindiff
    } else {
      KMIN <- (mindiff+KMIN)/2
    }
    
    KMIN
    # 5.07 (or 0)
    
    if (maxdiff > KMAX) {
      KMAX <- maxdiff
    } else {
      KMAX <- (maxdiff+KMAX)/2
    }
    KMAX
    # 30.86
  } else {
    qhigh <- quantile(difficulties, 0.95)
    qlow <- quantile(difficulties, 0.05)
    q50 <- quantile(difficulties, 0.5)
    
    KMAX <- q50+(qhigh-qlow)*2
    # 33.00
    
    if (maxdiff > KMAX) {
      KMAX <- maxdiff
    }
    
    KMIN <- q50-(qhigh-qlow)*2
    # -4.80
    
    if (mindiff < KMIN) {
      KMIN <- mindiff
    }   
    
  }  

  # Makes KMIN = 0 if the minimum difficulty is not smaller
  if (ZEROMIN == "ifminnotnegative") {
    if (mindiff >= 0)
       KMIN <- 0 
  }
  
  # Old way: very ad hoc for the example
  # KMIN <- 0
  # pos.min <- which.min(abs(responses - 0.5)) # Index of the value that is closest to 0.5 (the center)
  # points(difficulties[pos.min], responses[pos.min], col="red")
  # KMAX <- 2* difficulties[pos.min] # We make KMAX just the double of this centre value  
  # 32.63
  
  ret <- NULL
  ret$KMIN <- unname(KMIN)
  ret$KMAX <- unname(KMAX)
  return(ret)
}


# function needed for visualization purposes
#sigmoid = function(params, x) {
#  params[1] / (1 + exp(-params[2] * (x - params[3])))
#}

EstimateAndPlotSigmoidACC <- function(difficulties, responses, KMIN, KMAX, legendpos= 0.9) {
  ADD_EXTREMES <- TRUE
  if (ADD_EXTREMES) {
    MYREP <- 1000
    difficulties_EST <- c(rep(KMIN,MYREP), difficulties, rep(KMAX,MYREP))
    responses_EST <- c(rep(1,MYREP), responses, rep(0,MYREP))
  } else {
    difficulties_EST <- difficulties
    responses_EST <- responses  
  }  
  #mydf <- data.frame(difficulties_EST, responses_EST)
  
  # fitting code
  #fitmodel <- nls(responses~a/(1 + exp(-b * (difficulties-c))), start=list(a=1,b=-0.1,c=21.5),control= nls.control(maxiter=1000, minFactor=1/32000, warnOnly=TRUE))
  #params=coef(fitmodel)
  
  fitmodel <- nls(responses_EST~1/(1 + exp(-b * (difficulties_EST-c))), start=list(b=-0.1,c=21.5),control= nls.control(maxiter=1000, minFactor=1/32000, warnOnly=TRUE))
  # get the coefficients using the coef function
  params=c(1,coef(fitmodel))  # First parameter is fixed to 1
  
  # a: max (1)
  # b: slope (negative)
  # c: location 
  #params = c(1,-0.4,17.5)  # 
  #mean((sigmoid(params,difficulties)-responses)^2)  # Mean squared error
  #params = c(1,-0.2,18.3)  # 
  #mean((sigmoid(params,difficulties)-responses)^2)  # Mean squared error
  #params = c(1,-0.1,21.5)  # 
  #mean((sigmoid(params,difficulties)-responses)^2)  # Mean squared error  # Best
  #params = c(1,-0.05,20)  # 
  #mean((sigmoid(params,difficulties)-responses)^2)  # Mean squared error
  
  #  xlab="h" # expression(theta)
  #  ylab=expression(psi) # "Acc"
  
  difficultiesSIGMOID <- seq(KMIN,KMAX,0.1)
  #responsesSIGMOID <- sigmoid(params,difficultiesSIGMOID)
  responsesSIGMOID <- params[1] / (1 + exp(-params[2] * (difficultiesSIGMOID - params[3])))
  #  plot(difficultiesSIGMOID,responsesSIGMOID,type="l", ylim=c(0,1),xlim=c(KMIN,KMAX), col="green", xlab=xlab, ylab=ylab)
  #  points(difficulties_EST,responses_EST)
  
  
  r <- PlotACCwithIndicators(difficultiesSIGMOID,responsesSIGMOID, mycol="darkgreen", mylty=1, mypch=-1, LEGEND=FALSE,LEGEND2=TRUE, SHOW_UNMERGED_POINTS=FALSE, legendpos= legendpos, name="Sigmoidal", kmin=KMIN, kmax=KMAX)
  PlotACCwithIndicators(difficulties_EST,responses_EST,startplot=FALSE, mycol="blue", mylty=0, LEGEND=FALSE,LEGEND2=TRUE, SHOW_UNMERGED_POINTS=TRUE, legendpos= legendpos-0.1, name="Pointwise", kmin=KMIN, kmax=KMAX)
  #print(params)
  
  
  #myfit <- glm(responses ~ difficulties, data = mydf, family = quasibinomial("logit"))
  #myfit <- glm(responses ~ difficulties, data = mydf, family = quasi("logit"))
  #plot(difficulties,predict(myfit,mydf), xlim = c(KMIN,KMAX))
  
  ret <- NULL
  ret$difficultiesSIGMOID <- difficultiesSIGMOID
  ret$responsesSIGMOID <- responsesSIGMOID
  ret$capability <- unname(r$capability)
  ret$expdiff <- unname(r$expdiff)
  ret$spread <- unname(r$spread)
  ret$generality <- unname(r$generality)
  return(ret)
}




## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

# xmin and xmax are NOT USED!!!!
PlotACCwithIndicators <- function(difficulties0, responses0, kmin=NULL, kmax=NULL,  mergeBins=TRUE, xmin=NULL, xmax=NULL, startplot=TRUE, mycol="blue", mylty=1, LEGEND=TRUE, LEGEND2=FALSE, name=NULL, lengthname= NULL, legendpos=0.9, mypch=15, VERBOSE=FALSE, SHOW_UNMERGED_POINTS= TRUE, SHOW_GLOBAL_MEAN= FALSE, SHOW_GLOBAL_VARIANCE= FALSE, DISTINGUISH_EXTRAPOLATION= FALSE, EXTEND_CURVE= FALSE, legendposx = NULL, EDIFFICULTY = TRUE, SPREAD = TRUE, xaxt='t') {
  # Function
  #   Takes an ACC series of (x,y) points (where x is the difficulty and y is the result) and
  #   plots it as a curve
  # Args:
  #   difficulties0: the difficulties of each point/bin, on the x-axis
  #   responses0: the (average) responses for each difficulty, on the y-axis
  #   kmin, kmax: minimum and maximum for the calculation of the indicators
  #   xmin, xmax: minimum and maximum for plotting on the x-axis
  #   startplot: whether to start a plot
  #   mycol, mylty: colour and style of the curve (mylty= 0 if we don't want to show the curve)
  #   mypch: style of the points of the curve (-1 if we don't want to show the points)
  #   LEGEND, LEGEND2: LEGEND=TRUE shows the standard values (capability, expdiff, spread, generality), while LEGEND2=TRUE shows a more abridged version
  #   legendpos: position (used as parameter for the function "text")
  # Returns: 
  #    A list with capability, expected difficulty, spread and generality
  #  
  
  if (is.null(lengthname)) {
    lengthname <- nchar(name)
  }
  
  # Checks that difficulties and responses have the same size, and that difficultis are ordered
  res <- CheckDifficultiesAndResponses(difficulties0, responses0, mergeBins)
  # N <- res$N  # not used
  difficulties <- res$difficulties
  responses <- res$responses
  
  # PLOTTING
  xlab="h" # expression(theta)
  ylab=expression(psi) # "Acc"
  
  
  # THESE ARGUMENTS ARE NOT USED!!!!!
  if (is.null(xmin))
    xmin <- min(difficulties)

  if (is.null(xmax))
    xmax <- max(difficulties)
  
  if (is.null(kmin))
    kmin <- xmin
  
  if (is.null(kmax))
    kmax <- xmax
  
  
  plottitle= NULL
  if (startplot) {
#    plot(0, 0, xlim= c(xmin-0.5, xmax+0.5), ylim = c(0, 1), xlab=xlab, ylab=ylab, col="white", main=plottitle, cex.lab=1.4) #, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    plot(0, 0, xlim= c(kmin, kmax), ylim = c(0, 1), xlab=xlab, ylab=ylab, col="white", main=plottitle, cex.lab=1.4, xaxt = xaxt) #, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  }
  
  
  
  if (SHOW_UNMERGED_POINTS) {
    # VERY SLOW
    #points(difficultiesI, yp, col="grey")  # again, to place them above
    #points(kmin:kmax, showmean[1:numbins], col="red")  # again, to place them above
    # MUCH FASTER WHEN PRINTING:
    df <- data.frame(difficulties0,responses0)
    points(unique(df), col=add.alpha("grey",0.5))  # again, to place them above
  }
  
  
  if (mypch >= 0)
    points(difficulties,responses, col=mycol, pch=mypch)

  if (EXTEND_CURVE) {
    lines(c(kmin,difficulties[1]),c(1,1), col=mycol, lwd=2, pch=3, lty=3) 
    lines(c(difficulties[1],difficulties[1]),c(1,responses[1]), col=mycol, lwd=2, pch=3, lty=3) 
    lines(c(difficulties[length(difficulties)], difficulties[length(difficulties)]), c(responses[length(responses)],0), col=mycol, lwd=2, pch=3, lty=3) 
    lines(c(difficulties[length(difficulties)], kmax),c(0,0), col=mycol, lwd=2, pch=3, lty=3) 
  }
 
  
  if (DISTINGUISH_EXTRAPOLATION) {
    len <- length(difficulties)
    
    lines(difficulties,responses, col=mycol, lwd=2, pch=3, lty=3) 
    lines(difficulties[3:(len-2)],responses[3:(len-2)], col=mycol, lwd=2, pch=3, lty=mylty)  
  } else {
   lines(difficulties,responses, col=mycol, lwd=2, pch=3, lty=mylty)          # mean per point
  }
  
  res <- CalculateIndicators(difficulties, responses, kmin, kmax, VERBOSE=VERBOSE, mergeBins=mergeBins)
  capability <- res$capability
  expdiff <- res$expdiff
  spread <- res$spread
  generality <- res$generality
  
  TEXTPOS <- 0.6 # 0.68 # * (kmax-kmin) # 3.5
  TEXTDEN <- 1 # (kmax-kmin)  # 5
  
  if (is.null(legendposx)) {
     legendposx <- TEXTPOS*(kmax-kmin)/TEXTDEN+kmin
  } 
  xpos2 <- legendposx
  
  legsize2 <- 0.65
  letsize <- 0.025
  if (LEGEND2) {
    if (!is.null(name)) {
      text(xpos2, legendpos, sprintf("%s:", name), pos=4, cex=legsize2, col=mycol)
#      xpos2 <- xpos2 + (nchar(name)+2)*legsize2*(0.5/TEXTDEN)
      xpos2 <- xpos2 + (lengthname+2)*legsize2*(kmax-kmin)*letsize
    }
  }
  
  leg<- legendpos # 0.93
  legstep <- 0.065
  legsize <- 0.95
  if (LEGEND2) {
#    text(3.9*(kmax-kmin)/TEXTDEN+kmin, legendpos, expression(psi), pos=4, cex=legsize2, col=mycol)
    text(xpos2, legendpos, expression(psi), pos=4, cex=legsize2, col=mycol)
#    xpos2 <- xpos2 + 2*legsize2*(0.5/TEXTDEN)
    xpos2 <- xpos2 + (2)*legsize2*(kmax-kmin)*letsize
#    text(4.1*(kmax-kmin)/TEXTDEN+kmin, legendpos, sprintf("%.2f", capability), pos=4, cex=legsize2, col=mycol)
    text(xpos2, legendpos, sprintf("%.2f", capability), pos=4, cex=legsize2, col=mycol)
#    xpos2 <- xpos2 + 6*legsize2*(0.5/TEXTDEN)
    xpos2 <- xpos2 + (6)*legsize2*(kmax-kmin)*letsize
  }
 
  globalmeanI <- mean(responses0)
  if (SHOW_GLOBAL_MEAN) {
    if (LEGEND) {
      text(legendposx, leg, sprintf("Mean: %.3f", globalmeanI), pos=4, cex=legsize)
      leg <- leg -legstep
    }  
  }
  
  globalvarianceI <- popvar(responses0)
  if (SHOW_GLOBAL_VARIANCE) {
    if (LEGEND) {
      text(legendposx, leg, sprintf("Variance: %.3f", globalvarianceI), pos=4, cex=legsize)
      leg <- leg -legstep
    }  
  }
  
  
  if (LEGEND) {
    text(legendposx, leg, sprintf("Capability: %.2f", capability), pos=4, cex=legsize)
    leg <- leg -legstep
  }
  
  if (LEGEND && EDIFFICULTY) {
    text(legendposx, leg, sprintf("E[Difficulty]: %.2f", expdiff), pos=4, cex=legsize)
    leg <- leg -legstep
  }
  
  if (LEGEND && SPREAD) {
    text(legendposx, leg, sprintf("Spread: %.2f", spread), pos=4, cex=legsize)
    leg <- leg -legstep
  }
  
  if (LEGEND2) {
#    text(4.65*(kmax-kmin)/5+kmin, legendpos, expression(gamma), pos=4, cex=legsize2, col=mycol)
    text(xpos2, legendpos, expression(gamma), pos=4, cex=legsize2, col=mycol)
#    xpos2 <- xpos2 + 1.75*legsize2*(0.5/TEXTDEN)
    xpos2 <- xpos2 + (1.6)*legsize2*(kmax-kmin)*letsize
    #    text(4.83*(kmax-kmin)/5+kmin, legendpos, sprintf("%.2f", generality), pos=4, cex=legsize2, col=mycol)
    text(xpos2, legendpos, sprintf("%.2f", generality), pos=4, cex=legsize2, col=mycol)    
  }
  MAX_GENERALITY <- FALSE
  if (MAX_GENERALITY && generality > 10000) {
    if (LEGEND)
      text(legendposx, leg, sprintf("Generality: INF"), pos=4, cex=legsize)
  } else {
    if (LEGEND)
      text(legendposx, leg, sprintf("Generality: %.2f", generality), pos=4, cex=legsize)
  }  
  leg <- leg -legstep

  
  # returns the key indicators
  res$mean <- globalmeanI
  res$variance <- globalvarianceI
  
  return(res) # returns capability, expected difficulty, spread and generality
  
}




# the ncols in responseMatrix and difficulties may vary in 4 units. In this case, it means that the function will pad two 1s at the beginning and two 0s at the end.
calculateIndicatorsForAll <- function(responseMatrix, difficulties, KMIN, KMAX) {
  # Also plots, but very messy
  
  listcolors <- c("red","blue","maroon","green","orange","pink","brown","grey", "darkmagenta","darkcyan","gold","darkgreen")
  
  HOWMANY <- nrow(responseMatrix)
  capabilities <- rep(0,HOWMANY)
  expdiffs <- rep(0,HOWMANY)
  spreads <- rep(0,HOWMANY)
  generalities <-  rep(0,HOWMANY)
  for (i in 1:HOWMANY) {
    resp <- t(responseMatrix[i,])
    if (length(difficulties) == (length(resp)+4)) {  # This
      resp <- c(1,1,resp,0,0)
    }  
    #  res <- ploteverything(difficultiesForPlotting,resp,kmin,kmax,startplot=(i==1),listcolors[i],mylty=i,LEGEND=FALSE,LEGEND2=TRUE,legendpos=(0.5*i/HOWMANY)+0.47)
    #  cap <- res[1]
    #  gen <- res[2]
    
    res2 <- PlotACCwithIndicators(difficulties,resp,xmin=KMIN,xmax=KMAX,startplot=(i==1),mycol=listcolors[i],mylty=i,LEGEND=FALSE,LEGEND2=TRUE,legendpos=(0.5*i/HOWMANY)+0.47)
    cap <- res2$capability # unlist(res2[[1]])
    gen <- res2$generality # unlist(res2[[4]])
    
    capabilities[i] <- cap
    expdiffs[i] <- res2$expdiff
    spreads[i] <- res2$spread
    generalities[i] <- gen
    #print(c(cap, gen))
  }
  
  ret <- NULL
  ret$capabilities <- capabilities
  ret$expdiffs <- expdiffs
  ret$spreads <- spreads
  ret$generalities <- generalities
  return(ret)
}





# MAXCAP usually must be the maximum difficulty.
PlotCapabilityVsSpread <- function(capabilities, spreads, MINCAP=NULL, MAXCAP=NULL, legendtext=NULL, legendpos="bottom", pch=1, col=1, sizeLegend=0.6, SHOW_ASYMPTOTES=TRUE, splitLegend=FALSE, plotNames=FALSE, startplot=TRUE, legendposx= NULL, legendposy= NULL, SHOW_METRICS=TRUE, GenYAXIS=FALSE, maxylim= NULL) {
 
  if (!is.null(legendtext)) {
    ixNondup <- !duplicated(legendtext)
    legendtext <- legendtext[ixNondup]
    pch2 <- pch[ixNondup]
    col2 <- col[ixNondup]
  }
  
   #mindiff <- min(difficulties)
  #maxdiff <- max(difficulties)
  if (is.null(MINCAP)) {
    mincap <- min(capabilities)
  } else {
    mincap <- MINCAP
  } 
  if (is.null(MAXCAP)) {
    maxcap <- max(capabilities)
  } else {
    maxcap <- MAXCAP
  }
  constant_x <- NULL
  constant_y <- NULL
  abstruse_x <- NULL
  abstruse_y <- NULL
  epsilon <- 0.000001
  
  npoints <- 100
  xpoints <- seq(mincap,maxcap,length.out=npoints)
  j <- 1
  
  CALCULATE_CURVES_ALGEBRAICALLY <- TRUE
  if (!CALCULATE_CURVES_ALGEBRAICALLY) {
    
    for(i in xpoints) {
      # Constant curve: # Spread is high but not maximum
      capa <- i
      capaheight <- (capa - mincap) / (maxcap - mincap)
      res <- CalculateIndicators(c(min(0,mincap),mincap, mincap+epsilon, maxcap), c(1, 1, capaheight, capaheight)) 
      #  points(capa, res$spread, col="blue")
      #  points(res$capability, res$spread, col="blue", cex=0.1)
      constant_x[j] <- res$capability
      constant_y[j] <- res$spread
  
      
      
      # Increasing step!!! Very abstruse one: Spread is really maximum
      capa <- i - mincap
  
      #   The solution below was very problematic as it generated ties and negative values with mincap was 0.
      #    res <- CalculateIndicators(c(0,mincap - 0.000001, mincap, maxcap-capa-0.00001, maxcap-capa, maxcap), c(1, 1, 0, 0, 1, 1)) 
      #   The solution below was also problematic as it generated negative values at the end
      #    res <- CalculateIndicators(c(0,mincap + epsilon, mincap + 2*epsilon, maxcap-capa- 2*epsilon, maxcap-capa-epsilon, maxcap), c(1, 1, 0, 0, 1, 1)) 
      # The one below works well!
      res <- CalculateIndicators(c(min(0,mincap),mincap + epsilon, mincap + 2*epsilon, maxcap-capa +3*epsilon, maxcap-capa+4*epsilon, maxcap+ 5*epsilon), c(1, 1, 0, 0, 1, 1)) 
      #  points(capa, res$spread, col="red")
      #  points(res$capability, res$spread, col="red", cex=0.1)
      abstruse_x[j] <- res$capability
      abstruse_y[j] <- res$spread
      
      j <- j +1
    }
  } else {
    constant_x <- xpoints
    abstruse_x <- xpoints
    for(i in xpoints) {
      # Algebraic way of deriving spread from capability for a constant curve
      # sqrt((cap-min)*(max-min)-(cap-min)^2)
      # sqrt((cap-min)*((max-min)-(cap-min)))  # This is the standard expression that shows this is sqrt(2) smaller than the maximum spread below (abstruse)
      #algebraicSpread <- sqrt((res$capability - mincap)*(maxcap-mincap)-(res$capability-mincap)^2)
      
      algebraicSpread <- (i-mincap)*((maxcap-mincap) - (i-mincap))
      if ((algebraicSpread < 0) && (algebraicSpread > -0.0001)) # Precision error probably
        algebraicSpread <- 0
      
      if (algebraicSpread < 0) {
        print("Error calculating the spread of an abstruse curve. Get a negative value before sqrt")
        print(algebraicSpread)
        print(maxcap)
        print(mincap)
        print(i)
        STOP()
      }
      
  
      constant_y[j] <- sqrt(algebraicSpread)
      
      # Algebraic way of deriving spread from capability for an abstruse curve
      # sqrt(2*(max-min)*(cap-min)-2*(cap-min)^2)
      # sqrt(2*(cap-min)*((max-min)-(cap-min))# This is the standard expression that shows this is sqrt(2) larger than the maximum spread above (constant)

      abstruse_y[j] <- sqrt(2*algebraicSpread)
      
      j <- j +1
    }
  }
  
  if (is.null(maxylim))
     maxylim <- max(abstruse_y)*1.08  # 7.3 
  if (startplot) {
    if (!GenYAXIS) {
      plot(capabilities,spreads, xlab="capability", ylab="spread", xlim=c(mincap, maxcap), ylim=c(0,maxylim), col="white")
    } else {
      ## add extra space to right margin of plot within frame
      par(mar=c(5, 4, 4, 4.25) + 0.1)  #default is c(5, 4, 4, 2) + 0.1.
      plot(capabilities,spreads, xlab="capability", ylab="spread", xlim=c(mincap, maxcap), ylim=c(0,maxylim), col="white")
#      par(new=TRUE)
#      plot(capabilities,spreads, xlab="capability", ylab="spread", xlim=c(mincap, maxcap), ylim=c(0,maxylim), col="white")
      ylabels <- seq(0,maxylim, length.out=5)  # 5 values on the right y-axis
      genylabels <- 1/ylabels
      genylabels <- format(genylabels, digits=2)
      mtext("generality",side=4,line=2.75) 
      #axis(4, ylim=c(1/min(spread),1/max(spread))) #, col="red",col.axis="red",las=1)  
      axis(4, at=ylabels, labels= genylabels)
      par(mar=c(5, 4, 4, 2) + 0.1)
    }  
  }
  if (!plotNames) {
    #plot(capabilities,spreads, xlab="capability", ylab="spread", xlim=c(mincap, maxcap), ylim=c(0,maxylim), pch=pch, col=col)
    points(capabilities,spreads, pch=pch, col=col)
  } else {
    #plot(capabilities,spreads, xlab="capability", ylab="spread", xlim=c(mincap, maxcap), ylim=c(0,maxylim), col="white")
    text(means, sds, legendtext, cex=0.55, col="gray90") #, adj=0.5)
  }
  
  corCapSpread <- cor(capabilities,spreads)
  
  width <- (maxcap - mincap)
  xpos <- mincap - width*0.045
  xposright <- maxcap - width*0.67 # centre
  
  if (SHOW_ASYMPTOTES) {
    text(x=xposright,y=maxylim/0.86/1.16,pos=4,sprintf("··· Max spread (inc. step)"), cex=0.7, col="red")
    text(x=xposright,y=maxylim/0.91/1.16,pos=4,sprintf("··· High spread (constant)"), cex=0.7, col="blue")
    text(x=xposright,y=maxylim/0.96/1.16,pos=4,sprintf("··· Min spread (dec. step)"), cex=0.7, col="darkgreen")
  }
  
  if (!plotNames) {
    if (!is.null(legendtext)) {
      if (!splitLegend) {
        if (is.null(legendposy)) {
          L <- legend(legendpos, legend=legendtext, pch=pch2, col=col2, cex=sizeLegend)
        } else {
          L <- legend(x= legendposx, y= legendposy, legend=legendtext, pch=pch2, col=col2, cex=sizeLegend)          
        }  
        legendposy <- L$rect$top - L$rect$h
        legendposx <- L$rect$left
      } else {
        num <- length(legendtext)  # how many to show
        firsthalf <- 1:ceiling(num/2)
        secondhalf <- (ceiling(num/2)+1):num
        #print(firsthalf)
        #print(secondhalf)
        L1 <- legend(legendpos, legend=legendtext[firsthalf], pch=pch[firsthalf], text.col= "white", col="white", box.col="white", cex=sizeLegend) # We draw all in white to see the position and size
        L2 <- legend(legendpos, legend=legendtext[secondhalf], pch=pch[secondhalf], text.col= "white", col="white", box.col="white", cex=sizeLegend) # We draw all in white to see the position and size
        left = L2$rect$left
        width = L1$rect$w
        #print(left)
        #print(width)
        legend(x=left - width, y= L1$rect$top, legend=legendtext[firsthalf], pch=pch2[firsthalf], col=col2[firsthalf], cex=sizeLegend)
        if (num%%2 == 0) { # if even
          legend(x=left, y= L1$rect$top, legend=legendtext[secondhalf], pch=pch2[secondhalf], col=col2[secondhalf], cex=sizeLegend)
        } else { # if odd, we add an empty entry so that both rectangles have the same size
          legend(x=left, y= L1$rect$top, legend=c(legendtext[secondhalf],""), pch=c(pch2[secondhalf],0), col=c(col2[secondhalf],"white"), cex=sizeLegend)
        }  
      }  
    }  
  }  

  if (SHOW_ASYMPTOTES) {
    lines(xpoints, rep(0,length(xpoints)), col="darkgreen", lty=3)  
    lines(constant_x, constant_y, col="blue", lty=3)
    lines(abstruse_x, abstruse_y, col="red", lty=3)
  }

  
  if (SHOW_METRICS) {
    text(x=xpos,y=maxylim/0.86/1.16,pos=4,sprintf("Corr(cap,spr): %.2f", corCapSpread), cex=0.7)
    text(x=xpos,y=maxylim/0.91/1.16,pos=4,sprintf("Mean(cap): %.2f", mean(capabilities)), cex=0.7)
    text(x=xpos,y=maxylim/0.96/1.16,pos=4,sprintf("Mean(spr): %.2f", mean(spreads)), cex=0.7)
    
    
    
    # Usually the vectors don't come ordered and we have to order them, otherwise the area below doesn't work well
    res <- sort(capabilities, index.return=TRUE)
    capabilities <- res$x
    spreads <- spreads[res$ix]
    
    normSpreads <- spreads
    c <- 0
    l <- length(spreads)
    for (i in 1:l) {
      w <- which.min(abs(capabilities[i] - constant_x))
      y <- constant_y[w]
      if (spreads[i] > y) {
        c <- c +1
      }
      den <- abstruse_y[w]^2 - constant_y[w]^2
      if (den == 0) {  # all 0
        normSpreads[i] <- 0
      } else {
        normSpreads[i] <- (spreads[i]^2 - constant_y[w]^2) / den
      }  
    }  
    percAbstruse <- 100*c / l
    
    b <- 5 # bins
    binnedSpreads <- 1:b
    width <- (maxcap - mincap) / b
    for (i in 1:b) {
      indexes <- (capabilities >= ((mincap+(i-1)*width)) & (capabilities <= (mincap+(i)*width)))
      binnedSpreads[i] <- mean(spreads[indexes])
    }
    
    x <- c(mincap,capabilities, maxcap) # We ensure that the limits of the area are the same
    y <- c(0,spreads, 0)
    
    
    area <- sum(diff(x) * (head(y,-1)+tail(y,-1)))/2
    
    text(x=xpos,y=maxylim/1.02/1.16,pos=4,sprintf("%% Abstruse: %.2f%% (%d of %d)", percAbstruse, c, l), cex=0.7)
    corCapNormSpread <- cor(capabilities,normSpreads)
#    text(x=xpos,y=maxylim/1.08/1.16,pos=4,sprintf("Normalised spread. Mean: %.2f (corr. with cap.: %.2f)", mean(normSpreads), corCapNormSpread), cex=0.7)
    text(x=xpos,y=maxylim/1.08/1.16,pos=4,sprintf("Normalised generality. Mean: %.2f (corr. with cap.: %.2f)", -mean(normSpreads), -corCapNormSpread), cex=0.7)
#    text(x=xpos,y=maxylim/1.15/1.16,pos=4,sprintf("Mean binned spread: %.2f (%d bins)", mean(binnedSpreads), b), cex=0.7)
#    text(x=xpos,y=maxylim/1.23/1.16,pos=4,sprintf("Area per unit: %.2f (from %.2f to %.2f)", area/(maxcap-mincap), mincap, maxcap), cex=0.7)
  }
  
  
  
  ret <- NULL
  ret$legendposx <- legendposx
  ret$legendposy <- legendposy
  return(ret)
}




# Plots histogram of the whole population, a first group and a second gorup
PlotSplitHistograms <- function(allpop, subpop1, subpop2, 
                                allFAloadingsmean = NULL, allFAVaccounted = NULL,
                                subpop1FAloadingsmean = NULL, subpop1FAVaccounted = NULL,
                                subpop2FAloadingsmean = NULL, subpop2FAVaccounted = NULL,
                                xlab="Spreads") {
  
  minsp <- min(allpop)
  maxsp <- max(allpop)
  breaks <- seq(minsp, maxsp, length.out=40)
  
  histall <- hist(allpop, breaks=breaks, plot=FALSE)
  histhigh <- hist(subpop1, breaks=breaks, plot=FALSE)
  histlow <- hist(subpop2, breaks=breaks, plot=FALSE)
  
  plot(histall, border="grey", xlim=c(minsp,maxsp), main="", xlab=xlab)  # all histogram
  plot(histhigh, col=rgb(0,0,1,1/4), xlim=c(minsp,maxsp), main="", add=T)  # first histogram, high capability group -> low spreads  # BLUE
  plot(histlow, col=rgb(1,0,0,1/4), xlim=c(minsp,maxsp), add=T)  # second histogram, low capability group -> high spreads # RED
  
  s <- ""
  if (!is.null(allFAloadingsmean)) {
    s <- sprintf(" FAldngs Mean: %.2f, VarAccntd: %.2f", allFAloadingsmean, allFAVaccounted)
    #    print(s)
    #    s <- "hello"
  }
  
  text(x=min(histall$breaks),y=max(histall$counts)/1, pos=4,sprintf("All Subjects. %s Mean: %.2f%s    ", xlab, mean(allpop), s), cex=0.8)
  
  s <- ""
  if (!is.null(subpop1FAloadingsmean)) {
    s <- sprintf(" FAldngs Mean: %.2f, VarAccntd: %.2f", subpop1FAloadingsmean, subpop1FAVaccounted)
    print(subpop1FAloadingsmean)
    print(subpop1FAVaccounted)
    #    print(s)
    #    s <- "hello2"
  }  
  text(x=min(histall$breaks),y=max(histall$counts)/1.07,pos=4,sprintf("1st Group. %s Mean: %.2f%s    ", xlab, mean(subpop1), s, col="blue"), cex=0.8)
  
  s <- ""
  if (!is.null(subpop2FAloadingsmean)) {
    s <- sprintf(" FAldngs Mean: %.2f, VarAccntd: %.2f", subpop2FAloadingsmean, subpop2FAVaccounted)
    #    print(t)
    #    s <- "hello3"
  }
  text(x=min(histall$breaks),y=max(histall$counts)/1.07^2,pos=4,sprintf("2nd Group. %s Mean: %.2f%s    ", xlab, mean(subpop2), s, col="red"), cex=0.8)
}


#FACTOR ANALYSIS
PerformFAandPCA <- function(responseMatrix, nfactors=1) {
  # Function
  #   Takes a responsMatrix and performs Factor Analysis - One factor by default --- AND PCA 
  #   Plots a Scree Curve
  # Args:
  #   responseMatrix: rows are agents, cols are items
  #   nfactors: FA made with this number of factors
  # Returns: 
  #   the fit, including the loadings fit$loading

  psych::KMO(responseMatrix) # check factor recovery.
  # MSA > .9 is marvelous, in the .80s, mertitourious, in the .70s, middling, in the .60s, medicore, in the 50s, miserable, and less than .5, unacceptable.
  
  #fit <- psych::fa(responseMatrix, nfactors, cor='tet', fm='minres') # one factor  # cor Tetrachoric
  fit <- psych::fa(responseMatrix, nfactors, cor='cor', fm='minres') # one factor  # cor Pearson
  summary(fit)
  print(fit$Vaccounted)  # Accounted matrix
  #loadings(fit, cutoff=.3) # loadings
  loadings(fit, cutoff=.0) # loadings  # PROPORTION OF VARIANCE EXPLAINED
  
  # psych::scree(responseMatrix) # scree plot 
  cat("\n")
  psych::fa.parallel(responseMatrix, main="", ylabel="Eigenvalues") # parallel analysis  # HOW MANY FACTORS AND HOW MANY COMPONENT
  
  return(fit)
}




# IRT Analysis
PerformIRT <- function(responseMatrix, itemtype="2PL") {
  # Function
  #   Takes a responsMatrix and performs IRT analysis
  # Args:
  #   responseMatrix: rows are agents, cols are items
  #   itemtype: type of curve: 2 parameter as default.
  # Returns: 
  #   the parameters of the curves: discriminations and difficulties
  
  
  NATTR1 <- ncol(responseMatrix)
  valCols <- NULL
  nonValCols <- NULL
  for (i in 1:NATTR1) {
    values <- unique(round(responseMatrix[,i]))   # We use "round" as some methods convert everything to 0 and 1.
   # print(values)
    if (length(values) == 1) {
      cat(sprintf("\nWhen doing IRT: Column %d removed as it has only one value: %d", i, values))
      cat("\nIt will not be included in the IRT calculation and then replaced by disc=0 and diff= median\n\n")
      nonValCols <- c(nonValCols, i)
    } else {
      valCols <- c(valCols, i)
    }
  }
  responseMatrix <- responseMatrix[,valCols]
  NATTR <- ncol(responseMatrix)
  
  #fitmirt <- mirt(responseMatrix,1, itemtype="Rasch") # rasch
  fitmirt <- mirt(responseMatrix,1, itemtype=itemtype) # 2pl
  
  
  plot(fitmirt, which.items=c(1:NATTR), facet=FALSE, type='trace') # plot items
  beta <- coef(fitmirt, simplify=T, IRTpar=T) # beta table
  irt_discriminations <- beta$items[,1] # abilities
  irt_difficulties <- beta$items[,2] # difficulties
  
  disc <- 1:NATTR1 
  disc[nonValCols] <- 0  # For those items we couldn't calculate IRT values because they are constant, discrimination is 0
  disc[valCols] <- irt_discriminations
  diff <- 1:NATTR1 
  diff[nonValCols] <- median(irt_difficulties)  # For those items we couldn't calculate IRT values because they are constant, discrimination is 0
  diff[valCols] <- irt_difficulties

  # RETURNS the parameters
  ret <- NULL
  ret$irt_discriminations <- disc
  ret$irt_difficulties <- diff
  return(ret) # returns irt_discriminations and irt_difficulties
  
}





PerformFullStudy = function(studyName, responseMatrix, difficulties=NULL, centreDiffScale=NULL, devDiffScale = NULL, DO_FA = TRUE, DO_IRT= TRUE, ESTIMATESIGMOID= TRUE,   CHOSEN_SUBJECTS = c(3,7,23), INDIVIDUAL_SUBJECTS = TRUE) {

    cat(paste0("\nPERFORMING FULL STUDY: ", studyName, "\n"))
  
  # WE START WITH A MATRIX OF BINARY RESULTS (responseMatrix)
  # Columns are items, Rows are respondents (individuals)
  
  # We check there are no NAs
  if (sum(is.na(responseMatrix)) > 0) {
    cat("\nThere are NAs in the matrix. STOPPING")
    stop()
  }
  
  nitems <- ncol(responseMatrix)
  nsubjects <- nrow(responseMatrix)
  cat(paste0("\nAnalysing a binary response matrix with ", nitems, " items (columns) and ", nsubjects, " subjects or agents (rows).\n"))
  
  # We calculate row and column means and variances (and sd)
  cMeans <- as.numeric(unname(colMeans(responseMatrix)))
  cVars <- as.numeric(unname(colVars(responseMatrix)))
  cPopVars <- as.numeric(unname(colPopVars(responseMatrix)))
  cPopStdDevs <- sqrt(cPopVars)
  rMeans <- as.numeric(unname(rowMeans(responseMatrix)))
  rVars <- as.numeric(unname(rowVars(responseMatrix)))
  rPopVars <- as.numeric(unname(rowPopVars(responseMatrix)))
  rPopStdDevs <- sqrt(rPopVars)
  
  #Row Means
  # We calculate row means as a proxy for ability or performance (average response)
  # Proxy for ability : row means
  #rMeans
  
  #Row Variance
  # Proxy for generality: row variance
  #rPopVars
  #hist(rPopVars)
  
  ###################
  # 1 RMEAN VS RSTDDEV : We see the Bernoulli distribution
  ###################
  
  # We show the relation between average response and individual std deviation (variation)
  # As results are binary, we have a Bernoulli connection
  filename <- sprintf("%s.mean-vs-stddev", studyName)
  cat(paste0("Plotting Mean vs StdDev for subjects (Bernoulli shape) on file: ", filename, "\n"))
  OpenPDFEPS(filename, 4, 6)
  plot(xlab="Subject's Mean Response", ylab= "Subject's Std. Dev.", rMeans, rPopStdDevs, col=rgb(0,0,0,alpha=0.1),pch=19)
  dev.off()
  # cor(rMeans, rPopStdDevs)
  
  # Same as before but with jitter
  #jitter1 <- runif(length(rMeans), 0, 0.02)
  #jitter2 <- runif(length(rMeans), 0, 0.02)
  #plot(rMeans+jitter1, 1/sqrt(rVars)+jitter2*3,col=rgb(0,0,0,alpha=0.1),pch=19)
  #respondentMeans <- rMeans+jitter1
  #respondentStdDevs <-sqrt(rVars)+jitter2*2
  #plot(xlab="Respondents' mean", ylab= "Respondents' Std. Dev.", respondentMeans, respondentStdDevs,col=rgb(0,0,0,alpha=0.1),pch=19)
  
  
  ###################
  # 2 FAandPCA 
  ###################
  
  if (DO_FA) {
    cat(paste0("We now do FA and PCA\n"))
    filename <- sprintf("%s.FAandPCA-screeplot", studyName)
    OpenPDFEPS(filename, 4, 6)
    fit <- PerformFAandPCA(responseMatrix)
    dev.off()
    FAloadings <- fit$loadings
    cat(paste0("FA loadings are:\n"))
    cat(paste0(FAloadings))
    cat(paste0("\nMaximum FA loading is ", max(FAloadings), "and minimum FA loading is ", min(FAloadings), "\n"))
    FAVaccounted <-  mean(fit$communalities)
    cat(paste0("The mean of the FA loadings is ", mean(FAloadings), " and the accounted variance is: ", FAVaccounted, "\n"))
    #[1] 0.4648792
    #FAVaccounted <- fit$Vaccounted  # Not a scalar. The line below does the trick
    cat(paste0("Correlation between FAloadings and cMeans: ", cor(FAloadings, cMeans), "\n"))
    cat(paste0("Correlation between FAloadings and cPopVars: ", cor(FAloadings, cPopVars), "\n"))
  } else {
    FAloadings <- NA
    FAVaccounted <- NA
  }
  
  ################### 
  # 3 Do IRT and compare with given difficulties trivial difficulties (col means). 
  #   If no given difficulties, use IRT or trivial
  ###################
  
  
  if (DO_IRT) {
    cat(paste0("\nWe now do a 2PL IRT to obtain populational difficulties\n"))
    
    res <- PerformIRT(responseMatrix)
    irt_discriminations <- res$irt_discriminations
    irt_difficulties <- res$irt_difficulties
    
    cat(paste0("Correlation between IRT discriminations and IRT difficulties: ",   cor(irt_discriminations, irt_difficulties), "\n"))
    
    
    cat(paste0("Correlation between IRT difficulties and cMeans: ",   cor(irt_difficulties,cMeans), ". ")) 
    # We see that the irt_difficulties are basically complementary to means
    filename <- sprintf("%s.irtdiffs-and-means", studyName)
    OpenPDFEPS(filename, 4, 6)
    plot(irt_difficulties,cMeans, ylim = c(0,1), xlab= "Difficulty (IRT)", ylab="Item's Mean Response", col="blue")     
    pearCor <- cor(irt_difficulties, cMeans, method="pearson")
    spearCor <- cor(irt_difficulties, cMeans, method="spearman")
    xPos <- min(irt_difficulties)+(max(irt_difficulties)-min(irt_difficulties))*0.78
    text(x=xPos,y=0.96,sprintf("   Pearson correlation: %.2f", pearCor), cex=0.9)
    text(x=xPos,y=0.88,sprintf("Spearman correlation: %.2f", spearCor), cex=0.9)
    dev.off()
    cat(paste0("Full relation plotted on file: ",  filename, "\n")) 
    
    cat(paste0("Correlation between IRT discriminations and cPopVars: ", cor(irt_discriminations, cPopVars), "\n"))
    if (!is.na(FAloadings)) {
      cat(paste0("Correlation between FAloadings and IRT discriminations: ",  cor(FAloadings, irt_discriminations), "\n"))
      cat(paste0("Correlation between FAloadings and IRT difficulties ", cor(FAloadings, irt_difficulties), "\n"))
    }
    
    if (is.null(difficulties)) {
      cat(paste0("\nExternal difficulties not provided. "))
      
      USE_IRT_DIFFICULTIES <- "-IRTdiff"
      if (USE_IRT_DIFFICULTIES == "-IRTdiff") {
        cat(paste0("Using IRT difficulties from now on.\n"))
        difficulties <- irt_difficulties
        if (!is.null(centreDiffScale)) {  # normalise the values to have the given median
          #meddiff <- median(difficulties)
          centrediff <- mean(difficulties)
          devdiff <- sd(difficulties)
          difficulties <- (difficulties - centrediff)/devdiff
          difficulties <- difficulties*devDiffScale + centreDiffScale
        } 
      } else {
        cat(paste0("Using average results per column as proxy for difficulty from now on.\n"))
        difficulties <- -cMax
        if (!is.null(centreDiffScale))  {  # normalise the values to have the given median
          #meddiff <- median(difficulties)
          centrediff <- mean(difficulties)
          devdiff <- sd(difficulties)
          difficulties <- (difficulties - centrediff)/devdiff
          difficulties <- difficulties*devDiffScale + centreDiffScale
        }        
      }
    } else {
      cat(paste0("\nDifficulties provided externally. Comparing with all other difficulties.\n"))
      USE_IRT_DIFFICULTIES <- ""
      
      # We see that the difficulties have a relevance on means
      filename <- sprintf("%s.intdiffs-and-means", studyName, "\n")
      cat(paste0("Scatter plot of used difficulties and column means in file: ", filename, "\n")) 
      OpenPDFEPS(filename, 4, 6)
      plot(difficulties,cMeans, ylim=c(0,1), xlab="h", ylab="Item's Mean Response", col="blue")
      pearCor <- cor(difficulties, cMeans, method="pearson")
      spearCor <- cor(difficulties, cMeans, method="spearman")
      xPos <- min(difficulties)+(max(difficulties)-min(difficulties))*0.78
      text(x=xPos,y=0.96,sprintf("   Pearson correlation: %.2f", pearCor), cex=0.9)
      text(x=xPos,y=0.88,sprintf("Spearman correlation: %.2f", spearCor), cex=0.9)
      dev.off()
      
      # We see that irt difficulties are correlated with the theoretical difficulties
      if (!is.na(FAloadings)) {
        cat(paste0("Correlation between FAloadings and external difficulties: ",  cor(FAloadings, difficulties), "\n"))    
      }
      cat(paste0("Correlation between IRT difficulties and external difficulties: ", cor(irt_difficulties, difficulties), "\n"))
      cat(paste0("Correlation between cMeans and external difficulties: ", cor(cMeans, difficulties), "\n"))
    }
  } else {
    irt_difficulties <- NA
    irt_discriminations <- NA
    USE_IRT_DIFFICULTIES <- ""
  }

  
  ###################   
  # 4 Aggregated ACC : TRUE AND ESTIMATED  estimateAndPlotsSigmoidACC 
  ################### 
  cat(paste0("\nAGGREGATED ACC: Now we use these cmeans as aggregated results (all agents together)\n")) 
  
  res <- calculateLIMITSforPlots(difficulties, cMeans)
  KMIN <- res$KMIN
  KMAX <- res$KMAX
  cat(paste0("We have calculated the limits for all plots as KMIN: ", KMIN, " and KMAX: ", KMAX, "\n")) 
  
  if (ESTIMATESIGMOID) {
    filename <- sprintf("%s.sigmoidACC%s", studyName, USE_IRT_DIFFICULTIES)
    OpenPDFEPS(filename, 4, 6)  
    r <- EstimateAndPlotSigmoidACC(difficulties, cMeans, KMIN, KMAX)
    dev.off()
    cat(paste0("Using a sigmoid doesn't give a very good fit as shown in: ", filename, "\n")) 
  }  
  
  ################### 
  # 5 Aggregated GA
  ###################   
  
  cat(paste0("We plot the aggregated results as they are and calculate the capability in this range\n")) 
  
  # ALL AGGREGATED (cMeans)
  # Plot of aggregated results with no extrapolation on the left and right
  r <- PlotACCwithIndicators(difficulties,cMeans,xmin=KMIN,xmax=KMAX,legendpos=0.9)  # does not extrapolate
  cat(paste0("And we get: \n")) 
  print(unlist(r))
  
  cat(paste0("We plot the aggregated results completing with 1 on the left and 0 on the right\n")) 
  
  # We now extrapolate (with difficultiesForPlotting)
  r <- OptimisticallyExtrapolateACC(difficulties, cMeans, KMIN, KMAX)
  difficultiesForPlotting <- r$difficulties
  responses <- r$responses
  
  filename <- sprintf("%s.aggregated-ACC%s", studyName, USE_IRT_DIFFICULTIES)
  OpenPDFEPS(filename, 4, 6)
  r <- PlotACCwithIndicators(difficultiesForPlotting,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9,DISTINGUISH_EXTRAPOLATION= TRUE) 
  
#  r <- PlotACCwithIndicators(difficultiesForPlotting,responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mylty= 3) 
  cat(paste0("And we get: \n")) 
  print(unlist(r))
  aggregatedCapability <- r$capability
  aggregatedSpread <- r$spread
#  r <- PlotACCwithIndicators(difficulties,cMeans,xmin=KMIN,xmax=KMAX,legendpos=0.9, startplot=FALSE, LEGEND=FALSE) 
  #unlist(r) # This gives the non-extrapolated indicators
  ClosePDFEPS()
  cat(paste0("The plot can be found in the file: ", filename, "\n")) 
  
  
  ################### 
  # 6 Individual GA for all subjects
  ################### 
  
  cat(paste0("\nWe now do the GA (generality analysis) indicators for all agents individually\n")) 
  
  res <- calculateIndicatorsForAll(responseMatrix, difficultiesForPlotting,KMIN,KMAX)
  capabilities <- res$capabilities
  expdiffs <- res$expdiffs
  spreads <- res$spreads
  generalities <- res$generalities  
  
  
  
  ################### 
  # 7 Some Individual GA for a few chosen subjects
  ################### 

  if (INDIVIDUAL_SUBJECTS) {
    if (!is.null(CHOSEN_SUBJECTS)) {
      nr <- nrow(responseMatrix)
      cat(paste0("\nAnd print the ACC of a few chosen subjects in particular\n")) 
    
      chosen <- CHOSEN_SUBJECTS[1]
      if (chosen <= nr) {
        filename <- sprintf("%s.ACC1%s", studyName, USE_IRT_DIFFICULTIES)  
        OpenPDFEPS(filename, 4, 6)
        # Example of one with generality non infinity (great spread, low generality, low capability)
        resp1 <- c(1,1,t(responseMatrix[chosen,]),0,0)
        res2 <- PlotACCwithIndicators(difficultiesForPlotting,resp1,xmin=KMIN,xmax=KMAX)
        print(unlist(res2))
        ClosePDFEPS()
      }
      
      chosen <- CHOSEN_SUBJECTS[2]
      if (chosen <= nr) {
        filename <- sprintf("%s.ACC2%s", studyName, USE_IRT_DIFFICULTIES)
        OpenPDFEPS(filename, 4, 6)
        # Example of one with generality non infinity (lower spread, higher capability)
        resp2 <- c(1,1,t(responseMatrix[chosen,]),0,0)
        res2 <- PlotACCwithIndicators(difficultiesForPlotting,resp2,xmin=KMIN,xmax=KMAX)
        print(unlist(res2))
        ClosePDFEPS()
      }
      
      chosen <- CHOSEN_SUBJECTS[3]
      if (chosen <= nr) {
        filename <- sprintf("%s.ACC5%s", studyName, USE_IRT_DIFFICULTIES)
        OpenPDFEPS(filename, 4, 6)
        # Example of one with generality non infinity (high spread, higher capability)
        resp5 <- c(1,1,t(responseMatrix[chosen,]),0,0)
        res2 <- PlotACCwithIndicators(difficultiesForPlotting,resp5,xmin=KMIN,xmax=KMAX)
        print(unlist(res2))
        ClosePDFEPS()
      }
    }  
    
    # Example of one with maximum generality (possibly infinty)
    genOrd <- sort(generalities, decreasing=TRUE, index.return=TRUE)
    super_general <- genOrd$ix
    #super_general <- which(generalities == Inf)
    #super_general <- which(generalities == MaxGen)
    #capability[super_general] # All with maximal generality get all right or all wrong (no perfect ordering apart from this)
    
    lsg <- length(super_general)
    if (lsg >= 2) {
      super_general1 <- super_general[2] # We choose the second one. For the new data this is All wrong.
      if (super_general1 <= nr) {
        filename <- sprintf("%s.ACC3%s", studyName, USE_IRT_DIFFICULTIES)
        OpenPDFEPS(filename, 4, 6)
        resp3 <- c(1,1,t(responseMatrix[super_general1,]),0,0)
        res2 <- PlotACCwithIndicators(difficultiesForPlotting,resp3,xmin=KMIN,xmax=KMAX)
        print(unlist(res2))
        ClosePDFEPS()
      }  
    }
    
    # Example of another one with generality (possibly infinty)
    if (lsg >= 3) {
      super_general1 <- super_general[3] # We choose the third one. For the new data this is All right
      if (super_general1 <= nr) {
        filename <- sprintf("%s.ACC4%s", studyName, USE_IRT_DIFFICULTIES)
        OpenPDFEPS(filename, 4, 6)
        resp4 <- c(1,1,t(responseMatrix[super_general1,]),0,0)
        res2 <- PlotACCwithIndicators(difficultiesForPlotting,resp4,xmin=KMIN,xmax=KMAX)
        print(unlist(res2))
        ClosePDFEPS()
      }  
    }  
  }
  
  
  ###################   
  # 8 CapabilityVsSpread distribution and compare aggregated vs average
  ################### 
  
  cat(paste0("\nFinally, we show distribution of capability and generality and their relation with the aggregated ones\n")) 
  
  medcap <- median(capabilities)
  meancap <- mean(capabilities)
  #aggregatedCapability  # Should match with the mean!
  cat(paste0("Median capability: ", medcap, ", Mean capability: ", meancap, ", Aggregated capability: ", aggregatedCapability, "\n")) 
  
  filename <- sprintf("%s.histcap%s", studyName, USE_IRT_DIFFICULTIES)
  OpenPDFEPS(filename, 4, 6)
  r <- hist(capabilities,30, main="")
  text(x=min(r$breaks),y=max(r$counts)/1,pos=4,sprintf("Mean: %.2f, Median: %.2f", meancap, medcap), cex=0.9)
  text(x=min(r$breaks),y=max(r$counts)/1.07,pos=4,sprintf("Agg. Capability: %.2f", aggregatedCapability), cex=0.9)
  ClosePDFEPS()
  
  cat(paste0("And generate a histogram to file: ", filename, "\n")) 
  
  
  #generality
  #hist(generalities,breaks=50)
  #median(generalities)
  #mean(generalities)  # Inf, as there are infinite values
  #cor(capabilities,generalities)  # There are infinite values. Returns NaN
  #plot(capabilities,generalities)
  #plot(capabilities,log(generalities))
  
  # SPREAD: has a meaningful unit and no infinite values. MEANINGFUL AVERAGE
  medspread <- median(spreads)  
  meanspread <- mean(spreads)
  #aggregatedSpread
  cat(paste0("Median spread: ", medspread, ", Mean spread: ", meanspread, ", Aggregated spread: ", aggregatedSpread, "\n")) 
  
  filename <- sprintf("%s.histspread%s", studyName, USE_IRT_DIFFICULTIES)
  OpenPDFEPS(filename, 4, 6)
  r <- hist(spreads, breaks=30, main="")
  text(x=min(r$breaks),y=max(r$counts)/1,pos=4,sprintf("Mean: %.2f, Median: %.2f", meanspread, medspread), cex=0.9)
  text(x=min(r$breaks),y=max(r$counts)/1.07,pos=4,sprintf("Agg. Spread: %.2f", aggregatedSpread), cex=0.9)
  ClosePDFEPS()
  
  cat(paste0("And generate a histogram to file: ", filename, "\n")) 
  
  
  
  
  # CAPABILITY VS SPREAD (with the constant ACC and abstruse ACC plots)
  # aggregatedSpread
  filename <- sprintf("%s.capability-vs-spread%s", studyName, USE_IRT_DIFFICULTIES)
  OpenPDFEPS(filename, 4, 6)
  PlotCapabilityVsSpread(capabilities, spreads, min(difficulties), max(difficulties))
  ClosePDFEPS()
  cat(paste0("\nLast but not least, a scatterplot between capability and spread to file: ", filename, "\n")) 
  
  
  
  
  ###################
  # RETURNING
  ###################
  
  cat(paste0("\nEND OF FULL STUDY: ", studyName, "\n\n\n"))
  
  ret <- NULL
  ret$FAloadings <- FAloadings
  ret$FAVaccounted <- FAVaccounted
  ret$irt_difficulties <- irt_difficulties
  ret$irt_discriminations <- irt_discriminations
  ret$cMeans <- cMeans
  ret$difficulties <- difficulties
  ret$aggregatedCapability <- aggregatedCapability
  ret$aggregatedSpread <- aggregatedSpread
  ret$capabilities <- capabilities
  ret$spreads <- spreads  
  return(ret)
  
}




#############################################
## END MAIN FUNCTIONS FOR GENERALITY ANALYSIS
###############################################






