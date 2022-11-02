# Script for plots for the paper 
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

  
################# Fig 1 (diagram) ######################

selectedmetabres <- selectgroups(metabextendedres, 
                            group = "path_class",
                            BMDmax = 1,
                            BMDtype = "zSD", 
                            BMDsummary = "first.quartile",
                            nitems = 3)

## one level trendplot
trendplot(selectedmetabres, group = "path_class")


################### one level sensitivityplot ############
jpeg("sensitivityplot.jpg", quality = 100, width=14, height=16,
     units="cm", pointsize=12, res=300)
sensitivityplot(selectedmetabres, group = "path_class") + 
  ylim(0,1) + theme_bw()
dev.off()
################ one level trendplot ###################
#### in the same order as the sensitivityplot
#### FAIRE sensitivitycalc a moyen terme pour aider ce type de manip

# code pris dans sensitivityplot
groupby <- as.factor(selectedmetabres[, "path_class"])
firstquartilefun <- function(x) quantile(x, probs = 0.25, na.rm = TRUE)
dnb <- as.data.frame(table(groupby))
colnames(dnb) <- c("groupby", "nb_of_items")
variable <- selectedmetabres[, "BMD.zSD"]
dnb$firstquartile <- tapply(variable, groupby, firstquartilefun)
dnb <- dnb[order(dnb$firstquartile), ]
#### 

selectedmetabres$path_class <- factor(selectedmetabres$path_class, 
                                      levels = dnb$groupby)
jpeg("trendplot.jpg", quality = 100, width=14, height=16,
     units="cm", pointsize=12, res=300)
trendplot(selectedmetabres, group = "path_class")  + 
  # theme_minimal()
  # theme_classic()
  theme_bw()
dev.off()

################## targetplot ##################
## on items 88.1, 3 et 15 (see ex.microarray vignette)
microarrayfilename <- system.file("extdata", "transcripto_sample.txt", package = "DRomics")
(o.microarray <- microarraydata(microarrayfilename, norm.method = "quantile"))
(s_quad <- itemselect(o.microarray, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s_quad, progressbar = FALSE))

jpeg("targetplot.jpg", quality = 100, width=14, height=5,
     units="cm", pointsize=12, res=300)
targetplot(c("88.1", "3", "15"), f)
dev.off()

############### multilevel bmdplot #################

chosen_path_class <- c("Energy metabolism", 
                       "Membrane transport", 
                       "Lipid metabolism")
selectedres1 <- extendedres[extendedres$path_class %in% chosen_path_class, ]
bmdplot(selectedres1, BMDtype = "zSD", add.CI = TRUE, 
        facetby = "molecular.level", facetby2 = "path_class")
#        colorby = "trend") + labs(col = "trend")

jpeg("bmdplot.jpg", quality = 100, width=14, height=8,
     units="cm", pointsize=12, res=300)
bmdplot(selectedres1, BMDtype = "zSD", add.CI = TRUE, 
        facetby = "path_class", facetby2 = "molecular.level") +
  theme_bw()
dev.off()





################# Fig 2  ######################
### Plot of 25th quantiles of BMD-zSD calculated by pathway
### and colored by molecular level
# optional inverse alphabetic ordering of groups for the plot
extendedres$path_class <- factor(extendedres$path_class, 
                                 levels = sort(levels(extendedres$path_class), 
                                               decreasing = TRUE))

sensitivityplot(extendedres, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level", 
                BMDsummary = "first.quartile") + theme_bw()

jpeg("Figure2.jpg", quality = 100, width=18, height=10,
     units="cm", pointsize=12, res=300)
sensitivityplot(extendedres, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level", 
                BMDsummary = "first.quartile") + theme_bw()
dev.off()

tiff(filename = "Figure2.tif",
     width = 18, height = 10, units = "cm", pointsize = 12, res = 200)
sensitivityplot(extendedres, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level", 
                BMDsummary = "first.quartile") + theme_bw()
dev.off()


################# Fig 3a  ######################

chosen_path_class <- c("Membrane transport", 
                       "Lipid metabolism")
selectedres2 <- extendedres[extendedres$path_class %in% chosen_path_class, ]
bmdplotwithgradient(selectedres2, BMDtype = "zSD", scaling = TRUE, xmax = 6.6,
              facetby = "path_class", facetby2 = "molecular.level")

jpeg("Figure3a.jpg", quality = 100, width=12, height=10,
     units="cm", pointsize=12, res=300)
bmdplotwithgradient(selectedres2, BMDtype = "zSD", scaling = TRUE, xmax = 6.6,
                    shapeby = "trend",
                    facetby = "path_class", 
                    facetby2 = "molecular.level") + labs(shape = "DR trend")
dev.off()

tiff(filename = "Figure3a.tif",
     width = 12, height = 10, units = "cm", pointsize = 12, res = 200)
bmdplotwithgradient(selectedres2, BMDtype = "zSD", scaling = TRUE, xmax = 6.6,
                    shapeby = "trend",
                    facetby = "path_class", 
                    facetby2 = "molecular.level") + labs(shape = "DR trend")
dev.off()

################# Fig 3b  ######################

curvesplot(selectedres2, facetby = "path_class", 
           facetby2 = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend",
           xmin = 0, xmax = 6.5) + labs(col = "DR trend") + theme_classic()

jpeg("Figure3b.jpg", quality = 100, width=12, height=10,
     units="cm", pointsize=12, res=300)
curvesplot(selectedres2, facetby = "path_class", 
           facetby2 = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend",
           xmin = 0, xmax = 6.5) + labs(col = "DR trend")+ theme_classic()
dev.off()

tiff(filename = "Figure3b.tif",
     width = 12, height = 10, units = "cm", pointsize = 12, res = 200)
curvesplot(selectedres2, facetby = "path_class", 
           facetby2 = "molecular.level", scaling = TRUE, 
           npoints = 100, line.size = 0.5,
           colorby = "trend",
           xmin = 0, xmax = 6.5) + labs(col = "DR trend")+ theme_classic()
dev.off()

################################################################

