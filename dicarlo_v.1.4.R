##############################################################################
#
#                      DICARLO's LAB DATA: GENERALITY ANALYSIS (dicarlo_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis of DiCarlo's data, used in this paper:
#   Rajalingham R, Issa EB, Bashivan P, Kar K, Schmidt K, DiCarlo JJ. Large-scale, high-resolution comparison of the core visual object recognition behavior of humans, monkeys, and state-of-the-art deep artificial neural networks. Journal of Neuroscience. 2018 Aug 15;38(33):7255-69.
#   http://www.jneurosci.org/content/38/33/7255.abstract
#
# and the the metric of generality first introduced in 
#   J. Hernandez-Orallo "I.G.", March 15th, 2018
#   https://riunet.upv.es/bitstream/handle/10251/100267/secondbest.pdf
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#
# LICENCE:
#   GPL (except for the data, which is property of Dicarlo's lab)
#
# VERSION HISTORY:
#  - V.1.0    27 Mar 2019. First operative version in a standalone way
#  - v.1.1    05 Apr 2019. It can work with several metrics now.
#  - V.1.2    05 Apr 2019. Now all families are processed, using appropriate structures and loops, rather than repeated code
#  - V.1.3    01 Jul 2019. Reorganisation of names and plots
#  - v.1.4    02 Jul 2019. Includes heatmap of accuracies, and we use ObjectDifficulties for the plots
#
# FUTURE FEATURES
#
##############################################################################


##############################################################################################################
##############################################################################################################
################## FUNCTION ##################################################################################
##############################################################################################################
##############################################################################################################

range_scale <- function(x){(x-min(x))/(max(x)-min(x))}


##############################################################################################################
##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
#############################################################################
##############################################################################################################

# We use qplot
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

setwd("/OneDrive - UPV/__SUBMISSIONS/2018/General intelligence/__NewCode__")
source("generality.R")

DATADIR <- "dicarlo.data"
OUTPUTDIR <- "dicarlo.results"




##############################################################################################################
##############################################################################################################
################## DATASET ###################################################################################
##############################################################################################################
##############################################################################################################

df <- read.csv(paste0(DATADIR,"/data_meta_sparse.csv"))

head(df)

# bgname: instance name (240 different values)
# obj: object (24 different values)

# Variations
# bgphi: background angle? (-178..179)   : WHAT'S THIS? 
# ryz: yz rotation (-180..180)
# rxz: xz rotation (-180..180)
# rxy: xy rotation (-180..180)     
# ty: y translation (-1..1)
# tz: z translation (-1..1)         
# s: scale (0.70..1.70)

# Results    : WHAT'S THE SCALE?
# ALEXNET       (-1.9..2.2) Mean= 0.0 !!! sd: 0.74
# ZEILER        (-2.0..2.5) Mean= 0.0 !!! sd: 0.65
# VGG           (-2.2..2.8) Mean= 0.0 !!! sd: 0.82
# GOOGLENET     (-2.2..2.5) Mean= 0.0 !!! sd: 0.82
# RESNET        (-2.3..2.4) Mean= 0.0 !!! sd: 0.90   
# GOOGLENETv3   (-2.7..2.0) Mean= 0.0 !!! sd: 0.82
# monk          (-1.1..0.7) Mean= 0.0 !!! sd: 0.30
# hum           (-1.6..1.1) Mean= 0.0 !!! sd: 0.44


# RENAMING THE NAMES FOR EASE OF USE

names(df)[names(df) == 'X'] <- 'item_no'

# Four psychophysical attributes that will be used for difficulties

names(df)[names(df) == 'Eccentricity'] <- 'eccentricity'
names(df)[names(df) == 'Size'] <- 'size'
names(df)[names(df) == 'Pose.Distance.1'] <- 'pose'
names(df)[names(df) == 'Abs.Segmentation.Index'] <- 'contrast'

# Sensitivities

names(df)[names(df) == 'ALEXNET_fc8_multicls20softmax_I1_dprime'] <- 'ALEXNET_dprime'
names(df)[names(df) == 'ZEILER_fc6_multicls20softmax_I1_dprime'] <- 'ZEILER_dprime'
names(df)[names(df) == 'VGG_fc8_multicls20softmax_I1_dprime'] <- 'VGG_dprime'
names(df)[names(df) == 'GOOGLENET_pool5_multicls20softmax_I1_dprime'] <- 'GOOGLENET_dprime'
names(df)[names(df) == 'RESNET101_conv5_multicls20softmax_I1_dprime'] <- 'RESNET_dprime'
names(df)[names(df) == 'GOOGLENETv3_seed0mdl_flattened_last_mixed_multicls20softmax_I1_dprime'] <- 'GOOGLENETv3_dprime'
names(df)[names(df) == 'monk_SUBPOOL_BentoMantoNanoPicassoZico_I1_dprime'] <- "MONKEYS_dprime"
names(df)[names(df) == 'hum_pool_I1_dprime'] <- "HUMANS_dprime"

# The _C versions are equal but scaled to have mean 0, so it is actually the same data but different magnitudes
names(df)[names(df) == 'ALEXNET_fc8_multicls20softmax_I1_dprime_C'] <- 'ALEXNET_dprime_C'
names(df)[names(df) == 'ZEILER_fc6_multicls20softmax_I1_dprime_C'] <- 'ZEILER_dprime_C'
names(df)[names(df) == 'VGG_fc8_multicls20softmax_I1_dprime_C'] <- 'VGG_dprime_C'
names(df)[names(df) == 'GOOGLENET_pool5_multicls20softmax_I1_dprime_C'] <- 'GOOGLENET_dprime_C'
names(df)[names(df) == 'RESNET101_conv5_multicls20softmax_I1_dprime_C'] <- 'RESNET_dprime_C'
names(df)[names(df) == 'GOOGLENETv3_seed0mdl_flattened_last_mixed_multicls20softmax_I1_dprime_C'] <- 'GOOGLENETv3_dprime_C'
names(df)[names(df) == 'monk_SUBPOOL_BentoMantoNanoPicassoZico_I1_dprime_C'] <- "MONKEYS_dprime_C"
names(df)[names(df) == 'hum_pool_I1_dprime_C'] <- "HUMANS_dprime_C"

# Accuracies

names(df)[names(df) == 'ALEXNET_fc8_multicls20softmax_I1_accuracy'] <- 'ALEXNET_accuracy'
names(df)[names(df) == 'ZEILER_fc6_multicls20softmax_I1_accuracy'] <- 'ZEILER_accuracy'
names(df)[names(df) == 'VGG_fc8_multicls20softmax_I1_accuracy'] <- 'VGG_accuracy'
names(df)[names(df) == 'GOOGLENET_pool5_multicls20softmax_I1_accuracy'] <- 'GOOGLENET_accuracy'
names(df)[names(df) == 'RESNET101_conv5_multicls20softmax_I1_accuracy'] <- 'RESNET_accuracy'
names(df)[names(df) == 'GOOGLENETv3_seed0mdl_flattened_last_mixed_multicls20softmax_I1_accuracy'] <- 'GOOGLENETv3_accuracy'
names(df)[names(df) == 'monk_SUBPOOL_BentoMantoNanoPicassoZico_I1_accuracy'] <- "MONKEYS_accuracy"
names(df)[names(df) == 'hum_pool_I1_accuracy'] <- "HUMANS_accuracy"

# THEY ARE NO LONGER THERE
# names(df)[names(df) == 'ALEXNET_fc8_multicls20softmax_I1_accuracy_C'] <- 'ALEXNET_accuracy_C'
# names(df)[names(df) == 'ZEILER_fc6_multicls20softmax_I1_accuracy_C'] <- 'ZEILER_accuracy_C'
# names(df)[names(df) == 'VGG_fc8_multicls20softmax_I1_accuracy_C'] <- 'VGG_accuracy_C'
# names(df)[names(df) == 'GOOGLENET_pool5_multicls20softmax_I1_accuracy_C'] <- 'GOOGLENET_accuracy_C'
# names(df)[names(df) == 'RESNET101_conv5_multicls20softmax_I1_accuracy_C'] <- 'RESNET_accuracy_C'
# names(df)[names(df) == 'GOOGLENETv3_seed0mdl_flattened_last_mixed_multicls20softmax_I1_accuracy_C'] <- 'GOOGLENETv3_accuracy_C'
# names(df)[names(df) == 'monk_SUBPOOL_BentoMantoNanoPicassoZico_I1_accuracy_C'] <- "MONKEYS_accuracy_C"
# names(df)[names(df) == 'hum_pool_I1_accuracy_C'] <- "HUMANS_accuracy_C"


head(df)


#sd(df[,"ZEILER"])

nr <- nrow(df)  # 24 images * 10 variations
nVariations <- 10

# We finally remove some other columns
df <- df[, !(colnames(df) %in% c("bgpsi", "bgscale", "category", "tx", "tname", "id", "texture", "texture_mode", "internal_canonical"))]

summary(df)

#

# Example of row selection
# df <- df[df[,"obj"] =="22_acoustic_guitar",]

# We select some subject families (humans, monkeys and a couple of NN architectures)

# This would be a selection family by family
#humans_dprime <- df[,"HUMANS_dprime"]   # -0.3595
#humans_dprime_C <- df[,"HUMANS_dprime_C"]  # -0.33877
#humans_acc <- df[,"HUMANS_accuracy"]  # -0.344  

# Instead we do a loop an a dframe for all families
FAMILIES <- c("HUMANS", "MONKEYS", "GOOGLENETv3", "RESNET", "ALEXNET", "ZEILER", "VGG", "GOOGLENET")
# In the order of decreasing performance
FAMILIES <- c("HUMANS", "GOOGLENETv3", "RESNET","MONKEYS", "GOOGLENET", "VGG", "ALEXNET", "ZEILER")






##############################################################################################################
##############################################################################################################
### OBJECT GROUPING AND PERFORMANCE PER OBJECT. PLOTTING HEATMAP    ###############
##############################################################################################################
##############################################################################################################



nObjects <- nr/nVariations
ObjectNames <- NULL
# Must follow the order of FAMILIES <- c("HUMANS", "GOOGLENETv3", "RESNET","MONKEYS", "GOOGLENET", "VGG", "ALEXNET", "ZEILER")
myColnames <- c("HUMANS_accuracy", "GOOGLENETv3_accuracy", "RESNET_accuracy","MONKEYS_accuracy", "GOOGLENET_accuracy", "VGG_accuracy", "ALEXNET_accuracy", "ZEILER_accuracy")
nFamilies <- length(myColnames)

mat <- matrix(nrow=nObjects, ncol=nFamilies) #, dimnames= list(c("Object","HumanAcc"))) 
colnames(mat) <- myColnames 

meanObjectAcc <- rep(0,nObjects)
for (i in 1:nObjects) {
  ObjectNames[i] <- as.character(df[(i-1)*nVariations+1,"obj"])
  
  meanAcc <- rep(0,nFamilies)
  for (j in 1:nVariations) {
    for (k in 1:nFamilies) {
      meanAcc[k] <- meanAcc[k] + df[(i-1)*nVariations+j,myColnames[k]]
    }  
  }
  ObjAcc <- 0
  for (k in 1:nFamilies) {
    m <- meanAcc[k]/nVariations
    mat[i, k] <- m
    ObjAcc <- ObjAcc + m
  }
  meanObjectAcc[i] <- ObjAcc / nFamilies
}


ObjectNames <- c("rhino", "calculator", "shorts", "zebra", "MB27346", "build51", 
                 "weimaraner", "interior_130_2", "chickdee", "knife",
                 "interior_103_4", "bear", "MB30203", "furniture_18",
                 "elephant", "MB29874", "stockings", "hanger", "dromedary", "MB28699",
                "spider", "spanner", "MB30758", "guitar") 

mat
dim(mat)  # Check dimensions

# Create x and y labels
yLabels <- ObjectNames
xLabels <- FAMILIES # colnames(mat)

# Set min and max values of rand
min <- min(mat) #, na.rm=T)
max <- max(mat) #, na.rm=T)

# Red and green range from 0 to 1 while Blue ranges from 1 to 0
ColorRamp <- rgb(seq(0.95,0.99,length=50),  # Red
                 seq(0.95,0.05,length=50),  # Green
                 seq(0.95,0.05,length=50))  # Blue
ColorLevels <- seq(min, max, length=length(ColorRamp))


OpenPDFEPS("dicarlo.acc.OBJECTS", 4, 9)

# Set layout.  We are going to include a colorbar next to plot.
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1),
       heights=c(1,1))
#plotting margins.  bottom, left, top, and right. 
#par(mar = c(5,9,2.5,1), font = 2)
par(mar = c(2,5,1,1), font = 2)

# Plot it up!
image(1:ncol(mat), 1:nrow(mat), t(mat),
      col=ColorRamp, xlab="", ylab="",
      axes=FALSE, zlim=c(min,max),
      main= NA)

# Now annotate the plot
box()
axis(side = 1, at=seq(1,length(xLabels),1), labels=xLabels,
     cex.axis=0.6)
axis(side = 2, at=seq(1,length(yLabels),1), labels=yLabels, las= 1,
     cex.axis=0.6)

# Add colorbar to second plot region
par(mar = c(3,2.5,2.5,2))
image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col=ColorRamp,xlab="",ylab="", las = 1, cex.axis=0.7,xaxt="n")

ClosePDFEPS()

##############################################################################################################
##############################################################################################################
### NOW WE USE AND DERIVE DIFFERENT METRICS OF difficulties AND ITS CORRELATION WITH fourfamilies_responses ###############
##############################################################################################################
##############################################################################################################


nrow <- nrow(df)
dprime <- matrix(nrow=nrow, ncol=length(FAMILIES), dimnames= list(1:nrow, FAMILIES)) # Empty df
dprime_C <- dprime # We copy the same empty structure
acc <- dprime # We copy the same empty structure
responses <- dprime # Used later
for (f in FAMILIES) { # We fill the data
  dprime[,f] <- df[,paste0(f, "_dprime")]
  dprime_C[,f] <- df[,paste0(f, "_dprime_C")]
  acc[,f] <- df[,paste0(f, "_accuracy")]
}




# WE CALCULATE THE AGGREGATION OF ALL

fourfamilies_dprime <- (dprime[,"HUMANS"] + dprime[,"MONKEYS"] + dprime[,"GOOGLENETv3"] + dprime[,"RESNET"]) / 4
fourfamilies_dprime_C <- (dprime_C[,"HUMANS"] + dprime_C[,"MONKEYS"] + dprime_C[,"GOOGLENETv3"] + dprime_C[,"RESNET"]) / 4
fourfamilies_acc <- (acc[,"HUMANS"] + acc[,"MONKEYS"] + acc[,"GOOGLENETv3"] + acc[,"RESNET"]) / 4

cor(fourfamilies_dprime, -fourfamilies_acc) # We calculate the correlation between the two different metrics: -0.905 (basically the same)
plot(-fourfamilies_acc, fourfamilies_dprime)
cor(fourfamilies_dprime_C, -fourfamilies_acc)

METRIC <- "normacc"
if (METRIC == "accuracy") {
  fourfamilies_responses <- fourfamilies_acc
  for (f in FAMILIES) {
    responses[,f] <- acc[,f]
  }  
} else if (METRIC == "normacc") {
  fourfamilies_responses <- (fourfamilies_acc - 0.5)*2 
  for (f in FAMILIES) {
    responses[,f] <- (acc[,f] - 0.5)*2 
    # There is one case in the family "VGG" where it gets worse than 0. We make the minimum equal 0
    responses[responses[,f] < 0,f] <- 0 
  } 
} else if (METRIC == "dprime") {
#  max_dprime <- max(c(fourfamilies_dprime), c(humans_dprime), c(monkeys_dprime), c(googlenetv3_dprime), c(resnet_dprime))
#  min_dprime <- min(c(fourfamilies_dprime), c(humans_dprime), c(monkeys_dprime), c(googlenetv3_dprime), c(resnet_dprime))
  max_dprime <- max(dprime)
  min_dprime <- min(dprime)
  fourfamilies_responses <- (fourfamilies_dprime - min_dprime) / (max_dprime - min_dprime)  
  for (f in FAMILIES) {
    responses[,f] <- (dprime[,f] - min_dprime) / (max_dprime - min_dprime)
  }  
} else if (METRIC == "dprimeC") {  # AS WE NORMALISE, THIS IS THE SAME RESULT AS ABOVE
#  max_dprime_C <- max(c(fourfamilies_dprime_C), c(humans_dprime_C), c(monkeys_dprime_C), c(googlenetv3_dprime_C), c(resnet_dprime_C))
#  min_dprime_C <- min(c(fourfamilies_dprime_C), c(humans_dprime_C), c(monkeys_dprime_C), c(googlenetv3_dprime_C), c(resnet_dprime_C))
  max_dprime_C <- max(dprime_C)
  min_dprime_C <- min(dprime_C)
  fourfamilies_responses <- (fourfamilies_dprime_C - min_dprime_C) / (max_dprime_C - min_dprime_C)  
  for (f in FAMILIES) {
    responses[,f] <- (dprime_C[,f] - min_dprime_C) / (max_dprime_C - min_dprime_C) 
  }  
}



# I calculate correlations for some of the psychophysical attributes
# Note that what we should be really interested in is slopes. Slope can be small and correlation can be high. This will come with the generality analysis below

########################
# Exploring eccentricity
########################

eccentricity <- df[,"eccentricity"] 
cor(fourfamilies_responses, eccentricity)  # -0.07
cor(fourfamilies_responses, eccentricity, method="spearman") # rank # - 0.10
# Trying modifications to see if we can increase correlation or adjust the sign of correlation (it must be negative)
eccentricity2 <- range_scale(abs(eccentricity - 1))
cor(fourfamilies_responses, eccentricity2)  # 0.05 (worse)
cor(fourfamilies_responses, eccentricity2, method="spearman")  # 0.07 (worse)
eccentricity2 <- eccentricity # We can't improve

plot(eccentricity, fourfamilies_responses)
points(eccentricity, responses[,"HUMANS"], col="red")
reg <- lm(fourfamilies_responses~eccentricity)
abline(reg)

qplot(eccentricity, fourfamilies_responses, geom = c("point", "smooth"))
qplot(eccentricity, fourfamilies_responses, geom = "smooth")


########################
# Exploring size
########################

size <- df[,"size"]  
cor(fourfamilies_responses, size)  # 0.23
cor(fourfamilies_responses, size, method="spearman")  # 0.205
# Trying modifications to see if we can increase correlation or adjust the sign of correlation (it must be negative)
size2 <- range_scale(-size) 
cor(fourfamilies_responses, size2)  # -0.23

plot(size, fourfamilies_responses)
reg <- lm(fourfamilies_responses~size)
abline(reg)

qplot(size, fourfamilies_responses, geom = c("point", "smooth"))
qplot(size, fourfamilies_responses, geom = "smooth")


########################
# Exploring pose
########################

pose <- df[,"pose"] 
cor(fourfamilies_responses, pose)  # acc: -0.0077, dprime: -0.01
cor(fourfamilies_responses, pose, method="spearman")  # acc: 0.023, dprime: 0.01
# Trying modifications to see if we can increase correlation or adjust the sign of correlation (it must be negative)
pose2 <- range_scale(-abs(1-pose))
cor(fourfamilies_responses, pose2)  # acc: -0.11, dprime: -0.11
cor(fourfamilies_responses, pose2, method="spearman")  # acc: 0.09, dprime= -0.10

plot(pose, fourfamilies_responses)
reg <- lm(fourfamilies_responses~pose)
abline(reg)

qplot(pose, fourfamilies_responses, geom = c("point", "smooth"))
qplot(pose, fourfamilies_responses, geom = "smooth")


########################
# Exploring contrast
########################

contrast <- df[,"contrast"]
cor(fourfamilies_responses, contrast) # acc: 0.255, dprime: 0.29
cor(fourfamilies_responses, contrast, method="spearman")  # acc: 0.29, dprime: 0.30
# Trying modifications to see if we can increase correlation or adjust the sign of correlation (it must be negative)
contrast2 <- range_scale(abs(0.5-contrast)) # best
cor(fourfamilies_responses, contrast2)  # acc: -0.264, dprime: -0.31
cor(fourfamilies_responses, contrast2, method="spearman")  # acc: -0.29, dprime: -0.30


plot(contrast, fourfamilies_responses)
reg <- lm(fourfamilies_responses~contrast)
abline(reg)

qplot(contrast, fourfamilies_responses, geom = c("point", "smooth"))
qplot(contrast, fourfamilies_responses, geom = "smooth")


# CHOOSING THE DIFFICULTIES
# EXPLORING THE BEST COMBINATION OF TRANSFORMATIONS AND AGGREGATIONS TO GET A GOOD METRIC OF difficulties

# OPTIONS: NOTATION
# E/S/P/C for eccentricity/size/pose/constract
# 00 if not used, 1 if used with coefficient 1 (can be omitted), other numbers are the coefficients
# l for only linear transformation, a for absolute (non-linear) transformation
# s for scaling, r for ranking 

# ATTRIBUTE_AGGREGATION = "E00SlsP00Cls"
#ATTRIBUTE_AGGREGATION = "E1sS2sP1sC2s"  # This is the easiest to explain
ATTRIBUTE_AGGREGATION = "ElsSlsPasCls"   # Good results and easy to explain
#ATTRIBUTE_AGGREGATION = "EasSasPasCas"  # This is actually the one with highest correlation



all <- eccentricity -size + pose -contrast
cor(fourfamilies_responses, all)  # acc: -0.26, dprime: -0.31
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.27, dprime: -0.30

all <- range_scale(eccentricity) + range_scale(-size) + range_scale(pose) + range_scale(-contrast)
cor(fourfamilies_responses, all)  # acc: -0.296, dprime: -0.35
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.32, dprime: -0.35

all <- range_scale(-size) + range_scale(-contrast)  # eccentricity and pose only add noise?
cor(fourfamilies_responses, all)  # acc: -0.331, dprime: -0.36
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.33, dprime: -0.36
if (ATTRIBUTE_AGGREGATION == "E00SlsP00Cls") {
  chosenDifficulties <- all
}

all <- range_scale(eccentricity) + 2*range_scale(-size) + range_scale(pose) + 2*range_scale(-contrast)
cor(fourfamilies_responses, all)  # acc: -0.333, dprime: -0.38
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.35, dprime: -0.38

if (ATTRIBUTE_AGGREGATION == "E1sS2sP1sC2s") {
  chosenDifficulties <- all  
}

all <- rank(eccentricity) + rank(-size) + rank(-pose) + rank(-contrast)
cor(fourfamilies_responses, all)  # acc: -0.304, dprime: -0.36
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.30, dprime: -0.34

# Only pose2 as the one that clearly improves with the abs transformation
all <- range_scale(eccentricity) + range_scale(-size) + range_scale(pose2) + range_scale(-contrast)
cor(fourfamilies_responses, all) # acc: -0.37, dprime: -0.43
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.39, dprime: -0.43
if (ATTRIBUTE_AGGREGATION == "ElsSlsPasCls") {
  chosenDifficulties <- all  # EASY TO EXPLAIN
}

# This is actually the one with highest correlation
all <- range_scale(eccentricity2) + range_scale(size2) + range_scale(pose2) + range_scale(contrast2)
cor(fourfamilies_responses, all) # acc: -0.39, dprime: -0.45
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.40, dprime: -0.45
if (ATTRIBUTE_AGGREGATION == "EasSasPasCas") {
  chosenDifficulties <- all    # THIS IS THE ONE!
}

all <- 1*range_scale(eccentricity2) + 2*range_scale(size2) + 1*range_scale(pose2) + 2*range_scale(contrast2)
cor(fourfamilies_responses, all) # acc: -0.38, dprime: -0.43
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.39, dprime: -0.42

all <- rank(eccentricity2) + rank(size2) + rank(pose2) + rank(contrast2)
cor(fourfamilies_responses, all) # acc: -0.37, dprime: -0.43
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.39, dprime: -0.43

all <- rank(eccentricity2) + 2* rank(size2) + rank(pose2) + 2*rank(contrast2)
cor(fourfamilies_responses, all) # acc: -0.38, dprime: -0.44
cor(fourfamilies_responses, all, method="spearman")  # acc: -0.39, dprime: -0.43


# This is what we chosed. Checking again
cor(fourfamilies_responses, chosenDifficulties) 
cor(fourfamilies_responses, chosenDifficulties, method="spearman") 


# Correlations for all the data (no averages)
all_responses <- c(responses[,"HUMANS"], responses[,"MONKEYS"], responses[,"GOOGLENETv3"], responses[,"RESNET"])
all_diffs <- rep(chosenDifficulties,4)
cor(all_responses, all_diffs)  # -0.306
cor(responses[,"HUMANS"], chosenDifficulties)  # -0.344


# We finally use the chosen difficulties

# By binning difficulties into a number of bins, we get more stable curves
difficulties <- chosenDifficulties
numBins <- 15
binnedDifficulties <- BinDifficulties(difficulties, numBins) 
# 25-3 goes well, 20-3 goes badly, 15-3 badly, 10-3 goes mah
# 25-4 goes mah, 20-4 goes well, 13-4, 14-4, 15-4 goes very well, 12-4 poorly, 10-4 goes mah
# 10-5 goes badly, 11-5 goes mah, 12-5 goes well

count(binnedDifficulties)
minBin <- 4
instanceSelection <- FilterDifficultiesByFrequency(binnedDifficulties, minBin)
# Just choose 0 for no filtering
#instanceSelection <- FilterDifficultiesByFrequency(binnedDifficulties, 0)
count(binnedDifficulties[instanceSelection])
length(binnedDifficulties[instanceSelection])  # With 20, 3 ... From 240 to 235 or 232

sum(instanceSelection)



# Correlations for all the data (no averages) of the binned and selected difficulties
all_responses_binned_selected <- c(responses[instanceSelection,"HUMANS"], responses[instanceSelection,"MONKEYS"], responses[instanceSelection,"GOOGLENETv3"], responses[instanceSelection,"RESNET"])
all_diffs_binned_selected <- rep(binnedDifficulties[instanceSelection],4)
cor(all_responses_binned_selected, all_diffs_binned_selected)  # -0.339

re <- responses[instanceSelection,"HUMANS"]
di <- binnedDifficulties[instanceSelection]
cor(re, di)  # -0.397

# These plots are simply the ACC that we output below
qplot(di, re, geom = c("point", "smooth"))
qplot(di, re, geom = "smooth")

maxDiff <- max(difficulties)
minDiff <- min(difficulties)

objDiff <- range_scale(-meanObjectAcc)
# 240 difficulties
objectDifficulties <- rep(objDiff, each=nVariations)
# As they are in the scale 0,1 we put them in the same scale as the original difficulties
objectDifficulties <- objectDifficulties*(maxDiff-minDiff)+minDiff
cor(fourfamilies_responses, objectDifficulties) # acc: -0.63
cor(fourfamilies_responses, objectDifficulties, method="spearman")  # acc: -0.71
  
mergedDifficulties <- range_scale(3*objectDifficulties + difficulties)   # Best with 3 factor
# As they are in the scale 0,1 we put them in the same scale as the original difficulties
mergedDifficulties <- mergedDifficulties*(maxDiff-minDiff)+minDiff
cor(fourfamilies_responses, mergedDifficulties) # acc: -0.68
cor(fourfamilies_responses, mergedDifficulties, method="spearman")  # acc: -0.76

binnedMergedDifficulties <- BinDifficulties(mergedDifficulties, numBins) 
hist(binnedMergedDifficulties)






##############################################################################################################
##############################################################################################################
### GENERALITY ANALYSIS ###############
##############################################################################################################
##############################################################################################################


filename0nodiff <- sprintf("dicarlo.%s-Diff", METRIC)

filename0nobin <- sprintf("%s%s", filename0nodiff, ATTRIBUTE_AGGREGATION)

filename0 <- sprintf("%s-Bins%d-Min%d",  filename0nobin, numBins, minBin)

# NOW CALCULATE GENERALITIES USING THE fourfamilies_responses OF A FAMILY 

qplot(difficulties, fourfamilies_responses, geom = c("point", "smooth"))
qplot(difficulties, fourfamilies_responses, geom = "smooth")
r <- calculateLIMITSforPlots(difficulties, fourfamilies_responses)
KMIN <- r$KMIN
KMAX <- r$KMAX

filename <- paste0(filename0nobin, ".4families")
OpenPDFEPS(filename, 4, 6)
r <- PlotACCwithIndicators(difficulties,fourfamilies_responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE,  DISTINGUISH_EXTRAPOLATION= TRUE, legendposx=3.5)  #, mylty= 3) 
unlist(r)
# 2.13, 2.16
ClosePDFEPS()


filename <- paste0(filename0, ".4families")
OpenPDFEPS(filename, 4, 6)
r2 <- OptimisticallyExtrapolateACC(binnedDifficulties[instanceSelection], fourfamilies_responses[instanceSelection], KMIN, KMAX)
r3 <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE, legendposx=3.5)  #, mylty= 3) 
# 3.02, 0.94
# Binned and selected: 2.74, 0.61
ClosePDFEPS()

filename2 <-  paste0(filename0, "-SIGMOID.4families")
OpenPDFEPS(filename2, 4, 6)
r <- EstimateAndPlotSigmoidACC(r2$difficulties, r2$responses, KMIN, KMAX)
ClosePDFEPS()



#r <- calculateLIMITSforPlots(objectDifficulties, fourfamilies_responses)
#objKMIN <- KMIN # 0 # r$KMIN
#objKMAX <- KMAX # 1 #r$KMAX

filename <- paste0(filename0nodiff, "-OBJECT", ".4families")
OpenPDFEPS(filename, 4, 6)
r2 <- OptimisticallyExtrapolateACC(objectDifficulties,fourfamilies_responses, KMIN, KMAX)
r <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE,  legendposx=3.5)  #, mylty= 3) 
ClosePDFEPS()

filename <- paste0(filename0nobin, "-MERGED", ".4families")
OpenPDFEPS(filename, 4, 6)
r2 <- OptimisticallyExtrapolateACC(mergedDifficulties,fourfamilies_responses, KMIN, KMAX)
r <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE,  legendposx=3.5)  #, mylty= 3) 
ClosePDFEPS() 

filename <- paste0(filename0, "-MERGED", ".4families")
OpenPDFEPS(filename, 4, 6)
r2 <- OptimisticallyExtrapolateACC(binnedMergedDifficulties, fourfamilies_responses, KMIN, KMAX)
r3 <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE, legendposx=3.5)  #, mylty= 3) 
ClosePDFEPS()


# NOW WE DO THIS FOR ALL FAMILIES

capabilitiesCrude <- NULL
capabilitiesBinned <- NULL
capabilitiesSigmoid <- NULL
capabilitiesObject <- NULL
capabilitiesMerged <- NULL
capabilitiesBinnedMerged <- NULL
spreadsCrude <- NULL
spreadsBinned <- NULL
spreadsSigmoid <- NULL
spreadsObject <- NULL
spreadsMerged <- NULL
spreadsBinnedMerged <- NULL

for (f in FAMILIES) {
  
  qplot(difficulties, responses[,f], geom = c("point", "smooth"))
  qplot(difficulties, responses[,f], geom = "smooth")
  
  
  filename <- paste0(filename0nobin, ".", f)
  OpenPDFEPS(filename, 4, 6)
  r0 <- OptimisticallyExtrapolateACC(difficulties,responses[,f], KMIN, KMAX)
  #r <- PlotACCwithIndicators(difficulties,responses[,f],xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, legendposx=3.5)  #, mylty= 3) 
  r <- PlotACCwithIndicators(r0$difficulties,r0$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, legendposx=3.5)  #, mylty= 3) 
  unlist(r)
  capabilitiesCrude[f] <- r$capability
  spreadsCrude[f] <- r$spread
  ClosePDFEPS()
  
  filename <- paste0(filename0, ".", f)
  OpenPDFEPS(filename, 4, 6)
  r2 <- OptimisticallyExtrapolateACC(binnedDifficulties[instanceSelection], responses[instanceSelection,f], KMIN, KMAX)
  r3 <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE, legendposx=3.5)  #, mylty= 3) 
  unlist(r3)
  capabilitiesBinned[f] <- r3$capability
  spreadsBinned[f] <- r3$spread
  ClosePDFEPS()
  
  filename2 <- paste0(filename0, "-SIGMOID.", f)
  OpenPDFEPS(filename2, 4, 6)
  r4 <- EstimateAndPlotSigmoidACC(r2$difficulties, r2$responses, KMIN, KMAX)
  unlist(r4)
  capabilitiesSigmoid[f] <- r4$capability
  spreadsSigmoid[f] <- r4$spread
  ClosePDFEPS()
  
  
  
  filename <- paste0(filename0nodiff, "-OBJECT.", f)
  OpenPDFEPS(filename, 4, 6)
  r2 <- OptimisticallyExtrapolateACC(objectDifficulties,responses[,f], KMIN, KMAX)
  r <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE,legendposx=3.5)  #, mylty= 3) 
  capabilitiesObject[f] <- r$capability
  spreadsObject[f] <- r$spread
  ClosePDFEPS()
  
  filename <- paste0(filename0nobin, "-MERGED.", f)
  OpenPDFEPS(filename, 4, 6)
  r2 <- OptimisticallyExtrapolateACC(mergedDifficulties,responses[,f], KMIN, KMAX)
  r <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.8, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE,legendposx=3.5)  #, mylty= 3) 
  capabilitiesMerged[f] <- r$capability
  spreadsMerged[f] <- r$spread
  ClosePDFEPS() 
  
  filename <- paste0(filename0, "-MERGED.", f)
  OpenPDFEPS(filename, 4, 6)
  r2 <- OptimisticallyExtrapolateACC(binnedMergedDifficulties, responses[,f], KMIN, KMAX)
  r3 <- PlotACCwithIndicators(r2$difficulties,r2$responses,xmin=KMIN,xmax=KMAX,legendpos=0.9, mypch=".", SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE, DISTINGUISH_EXTRAPOLATION= TRUE, legendposx=3.5)  #, mylty= 3) 
  capabilitiesBinnedMerged[f] <- r$capability
  spreadsBinnedMerged[f] <- r$spread
  ClosePDFEPS()
  
  
}


capabilitiesCrude
capabilitiesBinned
capabilitiesSigmoid
capabilitiesObject 
capabilitiesMerged 
capabilitiesBinnedMerged 
spreadsCrude
spreadsBinned
spreadsSigmoid
spreadsObject 
spreadsMerged
spreadsBinnedMerged 

famInd <- 1:length(FAMILIES)


plot(capabilitiesCrude,spreadsCrude, pch=famInd, col=famInd, xlab="capability", ylab="spread")
legend("bottomleft", legend=FAMILIES, pch=famInd, col=famInd)


filename <- paste0(filename0nobin, "..capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(capabilitiesCrude,spreadsCrude, min(difficulties), max(difficulties), legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
ClosePDFEPS()


plot(capabilitiesBinned,spreadsBinned, pch=famInd, col=famInd, xlab="capability", ylab="spread")
legend("bottomleft", legend=FAMILIES, pch=famInd, col=famInd)

filename <- paste0(filename0, "..capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
#PlotCapabilityVsSpread(capabilitiesBinned,spreadsBinned, 0, max(binnedDifficulties), legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
PlotCapabilityVsSpread(capabilitiesBinned,spreadsBinned, min(binnedDifficulties), max(binnedDifficulties), legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
ClosePDFEPS()

plot(capabilitiesSigmoid,spreadsSigmoid, pch=famInd, col=famInd, xlab="capability", ylab="spread")
legend("bottomleft", legend=FAMILIES, pch=famInd, col=famInd)

filename <- paste0(filename0, "-SIGMOID..capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
#PlotCapabilityVsSpread(capabilitiesSigmoid,spreadsSigmoid, KMIN, KMAX, legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
PlotCapabilityVsSpread(capabilitiesSigmoid,spreadsSigmoid, min(difficulties), max(difficulties), , legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
ClosePDFEPS()



plot(capabilitiesObject,spreadsObject, pch=famInd, col=famInd, xlab="capability", ylab="spread")
legend("bottomleft", legend=FAMILIES, pch=famInd, col=famInd)

filename <- paste0(filename0nodiff, "-OBJECT..capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(capabilitiesObject,spreadsObject,  min(objectDifficulties), max(objectDifficulties), legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
ClosePDFEPS()


plot(capabilitiesMerged,spreadsMerged, pch=famInd, col=famInd, xlab="capability", ylab="spread")
legend("bottomleft", legend=FAMILIES, pch=famInd, col=famInd)

filename <- paste0(filename0nobin, "-MERGED.capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(capabilitiesMerged,spreadsMerged,  min(mergedDifficulties), max(mergedDifficulties), legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
ClosePDFEPS()



plot(capabilitiesBinnedMerged,spreadsBinnedMerged, pch=famInd, col=famInd, xlab="capability", ylab="spread")
legend("bottomleft", legend=FAMILIES, pch=famInd, col=famInd)

filename <- paste0(filename0, "-MERGED..capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(capabilitiesMerged,spreadsBinnedMerged, min(binnedMergedDifficulties), max(binnedMergedDifficulties), legendtext=FAMILIES, legendpos="topright", pch=famInd, col=famInd)
ClosePDFEPS()



