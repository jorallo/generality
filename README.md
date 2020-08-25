## generality

This is R code for doing Generality Analysis, based on the metric of generality explained in:

"Disentangling General Intelligence: A New Generality Measure to Characterise Human, Animal and Artificial Intelligence"
by J. Hernandez-Orallo, Bao Sheng Loe, Lucy Cheke, Fernando Martinez-Plumed, Sean ́O h ́Eigeartaigh
(submitted, 2020)

This code has been developed by Jose Hernandez-Orallo, Universitat Politecnica de Valencia (jorallo@upv.es, http://josephorallo.webs.upv.es). Some code (IRT and FA analysis) has been adapted from Bao Sheng Loe, University of Cambridge.
   
*LICENCE: GPL*

# guidelines 

The functions are implemented in generality_functions_v.X.X.X.R with a wrapper in "generality.R" that is independent of the version. 

We suggest to start with "demo_v.X.X.R", which includes a simple example on how the functions work. 

Some other more complex studies are:
- ale*: 23 AI systems on 45 games of the Atari video games Arcade Learning Environment.
- chess*: Machine players for two editions (2005 and 2015) of the World Computer Chess Championship.
- ctest: Human and machines for Thurstone letter series using results using the instances in the C-test (Hernandez-Orallo 2000).
- damerius**: 53 orangutans on 5 items of physical cognition tasks.  Damerius et al. "General  cognitive abilities in orangutans (pongo abelii and pongo pygmaeus)" Intelligence, 2018
- dicarlo**: Humans, monkeys (macaques) and six deep convolutional artificial NN for object recognition problems. Data from DiCarlo lab (MIT).
- gvgai*: 23 AI systems on 49 games of the General video game AI.
- herrmann*: The Primate Cognition Test Battery, data from the paper: Herrmann et al.  Humans have evolved specialized skills of social cognition:  The cultural intelligence hypothesis. Science, 317(5843):1360–1366, 2007.
- lambda: Humans and Q-learning agents compared with Lambda-One, a benchmark used to compare humans and simple reinforcement learning algorithms.
- mazes++: Humans (530 participants, 496 with demographics) with Elithorn's Perceptual Mazes. 
- odorspan**: Rats (10) for the Odour Span Task (from April et al. "The magic number 70 (plus or minus 20)" Learning and Motivation, 44(3):143–158, 2013.
- openml*: iris classification problem using 473 different ML classifiers from study number 7306 from OpenML.

In order to execute these other scripts, you will need the data, provided in zip files. You must check that the directory is well specified and your DATADIR and OUTPUTDIR are specified. For instance, at the beginning of the ctest.R script you find:

```
setwd("--- write your folder here ---")
source("generality.R")

# You can keep these names as long as:
# you have a /ctest.data folder with the data hanging from the folder above (unzip the data file there)
# you have a /ctest.result folder (possibly empty or with some of the plots in the zip files above)
DATADIR <- "ctest.data"
OUTPUTDIR <- "ctest.results"
```

Full description of all these scenarios can be found in the supplementary material of the paper. This repository includes data folders for many of the above studies, and the data to run the generality analysis for each scenario. Data is sometimes third party (*). If a data folder is not available, please email me to see if this can be shared in accordance with the original authors. 

*August 2020, Jose Hernandez-Orallo*
