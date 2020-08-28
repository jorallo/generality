##############################################################################
#
#   THIS GENERATES SOME POPULATIONS EVOLVING UNDER DIFFERENT CONDITIONS
#   STUDYING THE EFFECT ON GENERALITY AND CAPABILITY
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
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    15 March 2018. First operative version
#
# FUTURE FEATURES
#
##############################################################################



WORKDIR <- "/.../ **** PUT YOUR FOLDER HERE ****")
setwd(WORKDIR)

PDFEPS = 1
openPDFEPS <- function(file, PDFheight=PDFheight, PDFwidth=PDFwidth) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), height= PDFheight, width= PDFwidth)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), height= PDFheight, width= PDFwidth, horizontal=FALSE)
  }
}




## Add an alpha value to a colour
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}




# A random matrix
NITEMS <- 200  # We assume difficulties match the index
NAGENTS <- 200

SELECTION_PRESSURE <- "NONE"
#SELECTION_PRESSURE <- "ONLY_CAPABILITY"
#SELECTION_PRESSURE <- "ONLY_EFFORT"
#SELECTION_PRESSURE <- "BOTH"

MinEff <- sum((0.5:(NITEMS-0.5))*c(rep(1,NITEMS/2),rep(0,NITEMS/2)))

ACC <- "Flat"   # Flat and triangle are used in the supplementary material
#ACC <- "Triangle"
#ACC <- "Step"

# These values depend on the NITEMS, as the higher the NITEMS the lower the variability.
if (ACC == "Flat") {
  MinEff <- MinEff*2.00  # with 1.25 for EFFORT it never ends
} else if (ACC == "Triangle") {
  MinEff <- MinEff*1.25  # with 1.0 for BOTH it never ends
}

filename <- sprintf("GEN-CAP-nitems%d-nagents%d-%s-SELECTION-%s",NITEMS,NAGENTS,ACC,SELECTION_PRESSURE)
openPDFEPS(filename, 5, 5)


set.seed(0)

Cap <- rep(0,NAGENTS)
Eff <- rep(0,NAGENTS)
Spr <-  rep(0,NAGENTS)
Gen <- rep(0,NAGENTS)
resultmat <- matrix(NA,NAGENTS, NITEMS)
j <- 1
while (j <= NAGENTS) {
  for (i in 1:NITEMS) {
    if (ACC == "Flat") {
    # Result independent of difficulty
      resultmat[j,i] <- (runif(1,0,1) >= 0.5)*1
    } else if (ACC == "Step") { 
    # A step on difficulty
      resultmat[j,i] <- (0.5 >= (i-0.5)/NITEMS)*1 
    } else if (ACC == "Triangle") { 
    # Result lower for higher difficulty i
      resultmat[j,i] <- (runif(1,0,1) >= (i-0.5)/NITEMS)*1 # Triangular ACC curve on expectation
    }
    
    resultmat[j,i] <- resultmat[j,i] + runif(1,0,0.0000001)  # This is to avoid NA correlations
  }
  Cap[j] <- sum(resultmat[j,]) 
#  Eff[j] <- sum((1:NITEMS)*resultmat[j,])
  Eff[j] <- sum((0.5:(NITEMS-0.5))*resultmat[j,])
#  Spr[j] <- abs(2*Eff[j]/Cap[j]-Cap[j])  # abs necessary as precision can get negative values
  Spr[j] <- abs(2*Eff[j]-Cap[j]^2)  # abs necessary as precision can get negative values
  Spr[j] <- sqrt(Spr[j])
  Gen[j] <- 1/(Spr[j])
  
  if (SELECTION_PRESSURE=="ONLY_EFFORT") {
    if (Eff[j] < MinEff) {  # Selection: agent j only survives (the loop goes on) if the effort is lower than this value
      j <- j + 1
    }
  } else if (SELECTION_PRESSURE=="ONLY_CAPABILITY") {
    if (Cap[j]/NITEMS >= 0.5) { # At least successful for half of the tasks
      j <- j + 1
    }
  } else if (SELECTION_PRESSURE=="BOTH") {
    if ((Eff[j] < MinEff)   && (Cap[j]/NITEMS >= 0.5)) { #  # Selection: agent j only survives (the loop goes on) and At least successful for half of the tasks
      j <- j + 1
    }
  } else {
    j <- j + 1    
  }
}

Eff
mean(Eff)
Spr
cor(Cap,Spr)
#cor(Cap,Spr*Cap)

cormat <- cor(resultmat)
mean(cormat)

Capability <- Cap/NITEMS

#Gen <- Gen * 1000

xlab=expression(gamma)
ylab=expression(psi) # "Acc"

plottitle= NULL
#plot(0, 0, xlim= c(kmin-0.5, kmax+0.5), ylim = c(0, 1), xlab=xlab, ylab=ylab, col="white", main=plottitle, cex.lab=1.4) #, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(Gen,jitter(Capability, factor=2.5), xlab=xlab, ylab=ylab, col= add.alpha("darkgreen",0.5))

abline(lm(Capability ~ Gen), col="blue")
mycor <- cor(Gen,Capability)
mycor

#text((max(Gen)-min(Gen))/1.3+min(Gen), (max(Capability)-min(Capability))/1.7+min(Capability), sprintf("Corr: %1.2f",mycor), pos=4, cex=0.8, col="blue")
#text(max(Gen), max(Capability), sprintf("Corr: %1.2f",mycor), pos=2, cex=0.8, col="blue")
legend("topleft", sprintf("Corr: %1.2f",mycor), cex=0.8, col="blue")


dev.off()



