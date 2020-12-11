# Tests on residuals and corresponding postfit filter
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  ### test on microarray data ######################
  datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (f <- drcfit(s_quad, progressbar = TRUE))
  
  # Look at the table of results for successful fits
  head(f$fitres)
  
  # Look at the table of results for unsuccessful fits
  head(f$unfitres)
  
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits
  targetplot(f$unfitres$id[f$unfitres$cause == "constant.model"], f)
  targetplot(f$unfitres$id[f$unfitres$cause == "trend.in.residuals"], f)
  nrow(f$fitres)
  nrow(f$unfitres)
  
  # Fit without postfit filtering
  (f2 <- drcfit(s_quad, postfitfilter = FALSE, progressbar = TRUE))
  nrow(f2$fitres)
  nrow(f2$unfitres)
  
  # count the number of unsuccessful fits for each cause
  table(f2$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits in f
  (itemseliminatedinf <- f$unfitres$id[f$unfitres$cause == "trend.in.residuals"])
  
  targetplot(itemseliminatedinf, f2)
  
  f2$residualtests
  (itemswithmeantrendinf2 <- 
      f2$fitres$id[f2$residualtests$resimeantrendP < 0.05])
  targetplot(itemswithmeantrendinf2[1:20], f2)
  
  (itemswithvartrendinf2 <- 
      f2$fitres$id[f2$residualtests$resivartrendP < 0.05])
  targetplot(itemswithvartrendinf2[1:20], f2)
  
  (itemsbothPB <- f2$fitres$id[f2$residualtests$resimeantrendP < 0.05 
                              & f2$residualtests$resivartrendP < 0.05])
  ### test on RNAseq data #################
  data(Zhou_kidney_pce)
  d <- Zhou_kidney_pce
  (o <- RNAseqdata(d))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (f <- drcfit(s_quad, progressbar = TRUE))
  # (f <- drcfit(s_quad, progressbar = FALSE, parallel = "snow", ncpus = 4))
  
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits
  targetplot(f$unfitres$id[f$unfitres$cause == "constant.model"], f)
  targetplot(f$unfitres$id[f$unfitres$cause == "trend.in.residuals"], f)
  nrow(f$fitres)
  nrow(f$unfitres)
  
  which(f$residualtests$resivartrendP < 0.05)
  # (itemswithvartrendinf <- 
  #     f$fitres$id[f$residualtests$resivartrendP < 0.05])
  # targetplot(itemswithvartrendinf, f)
  
 
}