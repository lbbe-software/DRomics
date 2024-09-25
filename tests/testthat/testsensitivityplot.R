test_that("test sensitivityplot()", {
  skip_on_cran()
  
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
  
  # Plots on the BMDxfold (with NA values)
  sensitivityplot(annotres, BMDtype = "xfold",
                  group = "path_class")
  
  sensitivityplot(annotres, BMDtype = "xfold",
                  group = "path_class", 
                  BMDsummary = "median")
  
  sensitivityplot(annotres, BMDtype = "xfold",
                  group = "path_class", 
                  BMDsummary = "median.and.IQR")
  
  # after use of selectgroups
  nrow(annotres)
  annotres.s1 <- selectgroups(annotres, BMDtype = "xfold", 
                              group = "path_class", nitemsmin = 1)
  nrow(annotres.s1)
  sensitivityplot(annotres.s1, BMDtype = "xfold",
                  group = "path_class")
  
  # (2) 
  # An example with two molecular levels
  #
  ### Rename metabolomic results
  metabextendedres <- annotres
  str(metabextendedres)
  
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
  
  colnames(contigextendedres)
  colnames(metabextendedres)
  
  ## sensitivityplot on BMDxfold on contigextendedres
  sensitivityplot(contigextendedres, BMDtype = "xfold",
                  group = "path_class")
  nrow(contigextendedres)
  contigextendedres.s1 <- selectgroups(contigextendedres,
                                       BMDtype = "xfold", 
                                       group = "path_class", BMDmax = 2)
  nrow(contigextendedres.s1)
  sensitivityplot(contigextendedres.s1, BMDtype = "xfold",
                  group = "path_class")
  
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
  
  ### Plot of medians and IQRs of BMD-zSD 
  sensitivityplot(extendedres, BMDtype = "zSD",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "median.and.IQR")
  
  ### Plot of medians of BMD-zSD 
  sensitivityplot(extendedres, BMDtype = "zSD",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "median")
  
  ### Plot of first quartiles of BMD-xfold 
  sensitivityplot(extendedres, BMDtype = "xfold",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "first.quartile")
  
  ### Plot of medians of BMD-xfold 
  sensitivityplot(extendedres, BMDtype = "xfold",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "median")
  
  ### Plot of medians and IQRof BMD-xfold 
  sensitivityplot(extendedres, BMDtype = "xfold",
                  group = "path_class", colorby = "molecular.level", 
                  BMDsummary = "median.and.IQR")
  
})
