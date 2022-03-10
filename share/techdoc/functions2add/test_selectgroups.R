
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

### an ECDFplot of 25th quantiles of BMD-zSD calculated by pathway
sensitivityplot(annotres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")


subannotres <- selectgroups(annotres, 
                         group = "path_class", 
                         BMDmax = 5, 
                         BMDtype = c("zSD", "xfold"), 
                         BMDsummary = "first.quartile",
                         nitemsmin = 10)

sensitivityplot(subannotres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")

subannotres <- selectgroups(annotres, 
                            group = "path_class", 
                            BMDmax = 1, 
                            BMDtype = c("zSD", "xfold"), 
                            BMDsummary = "first.quartile",
                            nitemsmin = 3)

sensitivityplot(subannotres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")

# using the median
sensitivityplot(annotres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "median")

subannotres <- selectgroups(annotres, 
                            group = "path_class", 
                            BMDmax = 2, 
                            BMDtype = c("zSD", "xfold"), 
                            BMDsummary = "median",
                            nitemsmin = 3)

sensitivityplot(subannotres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "median")

#\donttest{
  
  # same plot in log10 BMD scale (not interesting on this example
  # but could be on another one) 
  sensitivityplot(annotres, BMDtype = "zSD",
                  group = "path_class",  
                  BMDsummary = "first.quartile",
                  BMD_log_transfo = TRUE)
  
  ### Plot of 25th quantiles of BMD-zSD calculated by pathway
  ### in the order of the levels as defined in the group input
  levels(annotres$path_class)
  sensitivityplot(annotres, BMDtype = "zSD",
                  group = "path_class", ECDF_plot = FALSE, 
                  BMDsummary = "first.quartile")
  
  ### an ECDFplot of medians of BMD-zSD calculated by pathway
  sensitivityplot(annotres, BMDtype = "zSD",
                  group = "path_class",  
                  BMDsummary = "median")
  
  ### an ECDFplot of medians of BMD-zSD calculated by pathway
  ### with addition of interquartile ranges (IQRs) 
  sensitivityplot(annotres, BMDtype = "zSD",
                  group = "path_class",  
                  BMDsummary = "median.and.IQR") 
  
  # (2) 
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
  sensitivityplot(extendedres, BMDtype = "zSD",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "first.quartile")
  
  subextendedres <- selectgroups(extendedres, 
                              group = "path_class", 
                              explevel = "molecular.level",
                              BMDmax = 5, 
                              BMDtype = c("zSD", "xfold"), 
                              BMDsummary = "first.quartile",
                              nitemsmin = 10)
  
  sensitivityplot(subextendedres, BMDtype = "zSD",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "first.quartile")
  
  subextendedres <- selectgroups(extendedres, 
                              group = "path_class", 
                              explevel = "molecular.level",
                              BMDmax = 1, 
                              BMDtype = c("zSD", "xfold"), 
                              BMDsummary = "first.quartile",
                              nitemsmin = 3)
  
  sensitivityplot(subextendedres, BMDtype = "zSD",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "first.quartile")
  
#}