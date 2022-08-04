# Script for plots for the paper and the cheat sheet
# new version - August 2022
####################################################
require(ggplot2)
# (1) An example from data published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# a dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
str(res)

# a dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
# bootstrap results and annotation
annotres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(annotres)

# An example with two molecular levels
#
### Rename metabolomic results
metabextendedres <- annotres
  
# Import the dataframe with transcriptomic results 
contigresfilename <- system.file("extdata", "triclosanSVcontigres.txt", package = "DRomics")
contigres <- read.table(contigresfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigres)
  
# Import the dataframe with functional annotation (or any other descriptor/category 
# you want to use, here KEGG pathway classes) 
contigannotfilename <- system.file("extdata", "triclosanSVcontigannot.txt", package = "DRomics")
contigannot <- read.table(contigannotfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigannot)

# Merging of both previous dataframes   
contigextendedres <- merge(x = contigres, y = contigannot, by.x = "id", by.y = "contig")
# to see the structure of this dataframe
str(contigextendedres)

### Merge metabolomic and transcriptomic results
extendedres <- rbind(metabextendedres, contigextendedres)
extendedres$molecular.level <- factor(c(rep("metabolites", nrow(metabextendedres)),
                                        rep("contigs", nrow(contigextendedres))))
str(extendedres)

### Plot of 25th quantiles of BMD-zSD calculated by pathway
### and colored by molecular level
# optional inverse alphabetic ordering of groups for the plot
extendedres$path_class <- factor(extendedres$path_class, 
                                 levels = sort(levels(extendedres$path_class), 
                                               decreasing = TRUE))

############## New figs for the cheat sheet
chosen_path_class <- c("Energy metabolism", 
                       "Membrane transport", 
                       "Lipid metabolism")
selectedres1 <- extendedres[extendedres$path_class %in% chosen_path_class, ]
bmdplot(selectedres1, BMDtype = "zSD", add.CI = TRUE, 
        facetby = "path_class", facetby2 = "molecular.level")
#        colorby = "trend") + labs(col = "trend")

LMresmetab <- extendedres[extendedres$path_class == "Lipid metabolism" & 
                            extendedres$molecular.level ==  "metabolites", ]
bmdplotwithgradient(LMresmetab, BMDtype = "zSD", scaling = TRUE, xmax = 6.6,
                    add.label = TRUE, facetby = "path_class")

LMres <- extendedres[extendedres$path_class == "Lipid metabolism", ]
curvesplot(LMres, facetby = "path_class", 
           facetby2 = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend",
           xmin = 0, xmax = 6.5) + labs(col = "DR_trend")

metabres <- extendedres[extendedres$molecular.level ==  "metabolites", ]
trendplot(metabres, group = "path_class", facetby = "molecular.level") 

sensitivityplot(metabres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")

# sensitivityplot(metabres, BMDtype = "zSD",
#                 group = "path_class", 
#                 BMDsummary = "median.and.IQR")

############### Figs for the paper ???????????
# selection on the sensitivity
selectedres <- selectgroups(extendedres, 
                            group = "path_class",
                            explev = "molecular.level",
                            BMDmax = 1,
                            BMDtype = "zSD", 
                            BMDsummary = "first.quartile",
                            nitems = 3,
                            keepallexplev = TRUE)

trendplot(extendedres, group = "path_class", facetby = "molecular.level") 

sensitivityplot(selectedres, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level", 
                BMDsummary = "first.quartile")

### Plot of medians and IQRs of BMD-zSD calculated by pathway
### and colored by molecular level
sensitivityplot(extendedres, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level", 
                BMDsummary = "median.and.IQR")

chosen_path_class <- c("Membrane transport", 
                       "Lipid metabolism")
selectedres2 <- extendedres[extendedres$path_class %in% chosen_path_class, ]
bmdplotwithgradient(selectedres2, BMDtype = "zSD", scaling = TRUE, xmax = 6.6,
                    facetby = "path_class", facetby2 = "molecular.level")

curvesplot(selectedres2, facetby = "path_class", 
           facetby2 = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend",
           xmin = 0, xmax = 6.5) + labs(col = "DR_trend")