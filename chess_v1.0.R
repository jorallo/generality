##############################################################################
#
#         GENERALITY ANALYSIS FOR COMPUTER CHESS COMPETITIONS (chess_vN.N.R)
#
##############################################################################
# 
# This is R code for doing Generality Analysis of chess data, based on the metric of generality
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica ce Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    05 Apr 2019. First version
#
# FUTURE FEATURES
#
##############################################################################




##############################################################################
####################### LIBRARIES AND INCLUDES ###############################
##############################################################################


setwd("/.../ **** PUT YOUR FOLDER HERE ****")
source("generality.R")

DATADIR <- "chess.data"
OUTPUTDIR <- "chess.results"




##############################################################################
####################### READING THE DATA ###############################
##############################################################################


datasetname <- "Reykjavik2005"
#datasetname <- "Leiden2015"

filenameplot <- paste0("chess.", datasetname)
OpenPDFEPS(filenameplot, 5, 5)


dataset <- read.csv(paste0(DATADIR, "/", datasetname,".csv"))
nplayers <- nrow(dataset)

dataset[is.na(dataset)] <- 0.5 # Replaces NA by 0.5
for (i in 1:nplayers) {  # So now we have to correct the sum
  # dataset[i,nplayers+3] <- dataset[i,nplayers+3] + 0.5
  dataset[i,nplayers+3] <- sum(dataset[i,3:(nplayers+2)])
}

dataset
listcolors <- c("red","blue","maroon","green","orange","pink","brown","grey", "darkmagenta","darkcyan","gold","darkgreen")


difficultiesB <- sort(unique(dataset[,nplayers+3])) # Basically sorting the scores
difficultiesB
ndiffs <- length(difficultiesB)
difficultiesForPlot <- c(0,difficultiesB,nplayers)
kmin <- min(difficultiesForPlot)
kmax <- max(difficultiesForPlot)


names <- as.character(dataset[,"Name"])
lengthname <- max(nchar(names)) # The maximum length of a nmae
lengthname <- lengthname - 1 # Works a little better like this


gen <- 0

capabilities <- NULL
spreads <- NULL

for (j in 1:nplayers) {
  
  ResponseAreas <- rep(0,ndiffs)
  Count <- rep(0,ndiffs)
  #  j <- 6
  for (i in 1:nplayers) {
    v <- dataset[i,nplayers+3]
    ind <- match(v, difficultiesB)
    #which(difficultiesB == v)
    
    ResponseAreas[ind] <- ResponseAreas[ind] + dataset[j,i+2]
    Count[ind] <- Count[ind] + 1
  }
  
  ResponseAreas
  Count
  
  ResponseAreasB <- ResponseAreas / Count
  ResponseAreasB
  difficultiesB
  
  # COMPLETE THE CURVES
  
  ResponseAreasForPlot <- c(1,ResponseAreasB,0)
  
  
  kmaxplot <- kmax *1.6 # + 7 # For the legend
  
  name <- names[j]
  
  r <- PlotACCwithIndicators(difficultiesForPlot, ResponseAreasForPlot,kmin=kmin, kmax=kmaxplot,mypch=".", startplot=(j==1), mycol=listcolors[j], mylty=j, LEGEND=FALSE,LEGEND2=TRUE,name=name, lengthname= lengthname, legendpos=(0.5*j/nplayers)+0.47, SHOW_GLOBAL_MEAN= TRUE, SHOW_GLOBAL_VARIANCE=TRUE)  #, mylty= 3) 
  g <- r$generality
#  g <- ploteverything(difficultiesForPlot,ResponseAreasForPlot,kmin,kmax,startplot=(j==1),listcolors[j],mylty=j,LEGEND=FALSE,LEGEND2=TRUE,name=name, legendpos=(0.5*j/nplayers)+0.47)
  
  gen <- gen +   g
  
  capabilities[j] <- r$capability
  spreads[j] <- r$spread
}

print("Average generality: ")
avggen <- gen / nplayers
avggen

print("Average spread: ")
avgspread <- mean(spreads)
avgspread

legsize2 <- 0.65

text(4.3*(kmaxplot-kmin)/5-1, 0.3, "Avg. ", pos=4, cex=legsize2, col="black")
text(4.65*(kmaxplot-kmin)/5-1, 0.3, expression(gamma), pos=4, cex=legsize2, col="black")
text(4.83*(kmaxplot-kmin)/5-1, 0.3, sprintf("%.2f", avggen), pos=4, cex=legsize2, col="black")

text(4.3*(kmaxplot-kmin)/5.05-1.95, 0.25, "Avg. ", pos=4, cex=legsize2, col="black")
text(4.65*(kmaxplot-kmin)/5.05-1.95, 0.25, "spread", pos=4, cex=legsize2, col="black")
text(5.2*(kmaxplot-kmin)/5.05-1.95, 0.25, sprintf("%.2f", avgspread), pos=4, cex=legsize2, col="black")

ClosePDFEPS()





filename <- paste0("chess.", datasetname, ".capability-vs-spread")
OpenPDFEPS(filename, 4, 6)
PlotCapabilityVsSpread(capabilities,spreads, kmin, kmax, legendtext=dataset[,"Name"], legendpos="topright", pch=1:nplayers, col=listcolors[1:nplayers])
ClosePDFEPS()
