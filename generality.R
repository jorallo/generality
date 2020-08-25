# This file is used as a proxy to load the most recent version of the library, so the other files don't have to be changed
# It also contains the version history

source("generality_functions_v.1.0.91.R")

##############################################################################
#
#                       GENERALITY ANALYSIS (generality.R)
#
##############################################################################
#
# This is R code for doing Generality Analysis, based on the metric of generality first introduced in 
#   J. Hernandez-Orallo "I.G.", March 15th, 2018
#   https://riunet.upv.es/bitstream/handle/10251/100267/secondbest.pdf
#
# This code has been developed by
#   JOSE HERNANDEZ-ORALLO, Universitat Politecnica de Valencia
#   jorallo@upv.es, http://josephorallo.webs.upv.es
# 
# Some code (IRT and FA analysis) has been adapted from Bao Sheng Loe, University of Cambridge.
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - v.0.1 - 0.9 Early versions developed during 2018
#  - V.1.0    27 Mar 2019. First operative version in a standalone way
#  - V.1.0.1  27 Mar 2019. plotACCwithIndicators has two new options: SHOW_UNMERGED_POINTS and SHOW_GLOBAL_VARIANCE
#             28 Mar 2019. Factor analysis correlation changed from tetrachoric to Pearson.
#  - v.1.0.2  04 Apr 2019. Minor (cosmetic) changes in EstimateAndPlotSigmoidACC and PlotACCwithIndicators.
#                          Two new functions included
#  - v.1.0.3  09 Apr 2019. Minor changes and three new functions included for performing the GA with a distributional reference
#                            for METHOD 5
#                                          EstBetaParameters 
#                                          Map2Quantile
#                                          GenerateStepwiseResponsesAndDifficultiesFromDistributionalReference... _MULTIPLEVALUES
#  - v.1.0.4  10 Apr 2019. The CalculateIndicators and related functions now work well with negative non-zero kmin (especially curves with negative difficulties)  
#                          New functions:
#                            for METHOD3A: REFERENCE POPULATIONAL (USING A COLUMN OR MEAN)
#                                          GenerateBinarisedResponsesAndDifficultiesFromCrispReference
#                            for METHOD4B: STEPWISE POPULATIONAL
#                                          GenerateStepwiseResponsesAndDifficultiesFromOrder
#                            for METHOD4A (now covers SCALE and RANK)
#  - v.1.0.5  15 Apr 2019. Several modifications and improvements in the way tied rankings are dealt with and some plots
#  - v.1.0.6  14 Jun 2019. New indicators in PlotCapabilityVsSpread 
#  - v.1.0.7  24 Jun 2019. Further improvements in PlotCapabilityVsSpread 
#                          Also PerformIRT now eliminates columns with same value
#  - v.1.0.8  30 Jun 2019. Small modifications to FA and new options for PlotACCwithIndicators
#  - v.1.0.9  09 Aug 2019. Some improvements for  PlotACCwithIndicators. Now it unlists the vectors (just in case) and takes expressions as names
#  - V.1.0.91 02 Jul 2020. Function PlotCapabilityVsSpread has a new parameter maxylim
#
# FUTURE FEATURES
#  - Explain all functions better
#
##############################################################################
