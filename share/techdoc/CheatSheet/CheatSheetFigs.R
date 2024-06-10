# Script for plots for cheat sheet
# new version - June 2024
####################################################
require(ggplot2)
require(DRomics)
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

selectedmetabres <- selectgroups(metabextendedres, 
                                 group = "path_class",
                                 BMDmax = 1,
                                 BMDtype = "zSD", 
                                 BMDsummary = "first.quartile",
                                 nitems = 3)

## one level trendplot
trendplot(selectedmetabres, group = "path_class")


################### one level sensitivityplot ############
jpeg("sensitivityplot4CS.jpg", quality = 100, width=16, height=8,
     units="cm", pointsize=12, res=300)
sensitivityplot(selectedmetabres, group = "path_class") + 
  ylim(0,1) + theme_bw()
dev.off()

################ one level trendplot ###################

#### in the same order as the sensitivityplot
#### FAIRE sensitivitycalc a moyen terme pour aider ce type de manip
# code pris dans sensitivityplot
# groupby <- as.factor(selectedmetabres[, "path_class"])
# firstquartilefun <- function(x) quantile(x, probs = 0.25, na.rm = TRUE)
# dnb <- as.data.frame(table(groupby))
# colnames(dnb) <- c("groupby", "nb_of_items")
# variable <- selectedmetabres[, "BMD.zSD"]
# dnb$firstquartile <- tapply(variable, groupby, firstquartilefun)
# dnb <- dnb[order(dnb$firstquartile), ]
# #### 
# 
# selectedmetabres$path_class <- factor(selectedmetabres$path_class, 
#                                       levels = dnb$groupby)
jpeg("trendplot4CS.jpg", quality = 100, width=16, height=8,
     units="cm", pointsize=12, res=300)
trendplot(selectedmetabres, group = "path_class")  + 
  # theme_minimal()
  # theme_classic()
  theme_bw()
dev.off()

############### multilevel bmdplot #################

chosen_path_class <- c("Membrane transport", 
                       "Lipid metabolism")
selectedres1 <- extendedres[extendedres$path_class %in% chosen_path_class, ]
bmdplot(selectedres1, BMDtype = "zSD", add.CI = TRUE, 
        facetby2 = "molecular.level", facetby = "path_class")
#        colorby = "trend") + labs(col = "trend")

jpeg("bmdplot4CS.jpg", quality = 100, width=14, height=8,
     units="cm", pointsize=12, res=300)
bmdplot(selectedres1, BMDtype = "zSD", add.CI = TRUE, 
        facetby2 = "path_class", facetby = "molecular.level") +
  theme_bw()
dev.off()

############## multilevel curvesplot ######################

chosen_path_class <- c("Lipid metabolism")
LMres <- extendedres[extendedres$path_class %in% chosen_path_class, ]
curvesplot(LMres,  
           facetby = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend") + labs(col = "DR trend")

jpeg("curvesplot4CS.jpg", quality = 100, width=14, height=8,
     units="cm", pointsize=12, res=300)
curvesplot(LMres, 
           facetby = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend") + labs(col = "DR trend") + theme_bw()
dev.off()


############# bmdplotwithgradient ################

LMresmetab <- extendedres[extendedres$path_class == "Lipid metabolism" & 
                            extendedres$molecular.level ==  "metabolites", ]
jpeg("bmdplotwithgradient4CS.jpg", quality = 100, width=12, height=10,
     units="cm", pointsize=12, res=300)
bmdplotwithgradient(LMresmetab, BMDtype = "zSD", scaling = TRUE, xmax = 6.6,
                    add.label = TRUE, facetby = "path_class")
dev.off()

#################################################

