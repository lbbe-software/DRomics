test_that("test the bmdplotwithgradient function", {
  skip_on_cran()
  
  # (1) Plot of BMD values with color dose-response gradient
  # faceted by metabolic pathway (from annotation of the selected items)
  # and shaped by dose-response trend
  
  # An example from the paper published by Larras et al. 2020
  # in Journal of Hazardous Materials
  # https://doi.org/10.1016/j.jhazmat.2020.122727
  # A example of plot obtained with this function is in Figure 5 in Larras et al. 2020
  
  
  # the dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
  resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
  res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
  str(res)
  
  # the dataframe with annotation of each item identified in the previous file
  # each item may have more than one annotation (-> more than one line)
  annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
  annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
  str(annot)
  
  # Merging of both previous dataframes
  # in order to obtain an extenderes dataframe
  extendedres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
  head(extendedres)
  
  
  ### (1.a) BMDplot with gradient by pathway
  bmdplotwithgradient(extendedres, BMDtype = "zSD",
                      facetby = "path_class", 
                      shapeby = "trend") 
  
  # (1.b) BMDplot with gradient by pathway and trend
  bmdplotwithgradient(extendedres, BMDtype = "zSD",
                      facetby = "path_class",
                      facetby2 = "trend") 
  
  # (1.b) BMDplot with gradient by pathway
  # forcing the limits of the colour gradient at other 
  # values than observed minimal and maximal values of the signal
  bmdplotwithgradient(extendedres, BMDtype = "zSD",
                      facetby = "path_class", 
                      shapeby = "trend",
                      limits4colgradient = c(-1, 1)) 
  
  # (1.c) The same example changing the gradient colors and the line size
  bmdplotwithgradient(extendedres, BMDtype = "zSD",
                      facetby = "path_class", 
                      shapeby = "trend",
                      line.size = 3, 
                      lowercol = "darkgreen", uppercol = "orange") 
  
  # (1.d) The same example with only lipid metabolism pathclass
  # and identification of the metabolites
  LMres <- extendedres[extendedres$path_class == "Lipid metabolism", ]
  bmdplotwithgradient(LMres, BMDtype = "zSD",
                      line.size = 3, 
                      add.label = TRUE, label.size = 3) 
 
   # (1.e) The same example with only membrane transport pathclass
  # and identification of the metabolites
  LMres <- extendedres[extendedres$path_class == "Membrane transport", ]
  bmdplotwithgradient(LMres, BMDtype = "zSD",
                      line.size = 3, 
                      add.label = TRUE, label.size = 3) 
  bmdplotwithgradient(LMres, BMDtype = "zSD", xmax = 7.76,
                      line.size = 3, 
                      add.label = FALSE, label.size = 3) 
  curvesplot(LMres, facetby = "id", xmax = 7.76, scaling = TRUE) 
  LMres[LMres$id == "NP_92", ]
  
  
  
  # (2) 
  # An example on a microarray data set (a subsample of a greater data set)
  #
  datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
  (f <- drcfit(s_quad, progressbar = TRUE))
  (r <- bmdcalc(f))
  
  bmdplotwithgradient(r$res, BMDtype = "zSD",
                      facetby = "trend", 
                      shapeby = "model") 
  bmdplotwithgradient(r$res, BMDtype = "zSD",
                      xmax = max(f$omicdata$dose), facetby = "trend", 
                      shapeby = "model") 


})