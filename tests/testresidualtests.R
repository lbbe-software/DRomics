# Tests on residuals and corresponding postfit filter
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  IC <- "AICc"
  ### test on microarray data ######################
  datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")
  
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (f <- drcfit(s_quad, progressbar = TRUE, information.criterion = IC))
  plot(f, plot.type = "fitted_residuals")
  nrow(f$fitres)
  nrow(f$unfitres)
  length(which(f$residualtests$resivartrendP < 0.05))
  length(which(f$residualtests$resivartrendP < 0.05)) / length(f$residualtests$resivartrendP)
  
  # Look at the table of results for successful fits
  head(f$fitres)
  
  # Look at the table of results for unsuccessful fits
  head(f$unfitres)
  
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits
  targetplot(f$unfitres$id[f$unfitres$cause == "constant.model"], f)
  targetplot(f$unfitres$id[f$unfitres$cause == "trend.in.residuals"], f)

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
  (f <- drcfit(s_quad, progressbar = TRUE, information.criterion = IC))
  # (f <- drcfit(s_quad, progressbar = FALSE, parallel = "snow", ncpus = 4))
  plot(f, dose_log_transfo = TRUE) + scale_x_log10(limits = c(0.1, 10))
  
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits
  targetplot(f$unfitres$id[f$unfitres$cause == "constant.model"], f)
  targetplot(f$unfitres$id[f$unfitres$cause == "trend.in.residuals"], f)
  nrow(f$fitres)
  nrow(f$unfitres)
  length(which(f$residualtests$resivartrendP < 0.05))
  length(which(f$residualtests$resivartrendP < 0.05)) / length(f$residualtests$resivartrendP)
  
  ### test on RNAseqdata incorrectly entered as metabolomic data
  ### and with a high FDR
  data(Zhou_kidney_pce)
  d <- Zhou_kidney_pce
  (o <- continuousomicdata(d))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.20))
  (f <- drcfit(s_quad, progressbar = TRUE, information.criterion = IC))
  plot(f, items = 49)
  nrow(f$fitres)
  nrow(f$unfitres)
  length(which(f$residualtests$resivartrendP < 0.05))
  length(which(f$residualtests$resivartrendP < 0.05)) / length(f$residualtests$resivartrendP)
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  
  ### test on metabolomic data #################
  data(Scenedesmus_metab)
  (o <- continuousomicdata(Scenedesmus_metab))
  plot(o)
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (f <- drcfit(s_quad, progressbar = TRUE, information.criterion = IC))
  plot(f, plot.type = "fitted_residuals")
  nrow(f$fitres)
  nrow(f$unfitres)
  length(which(f$residualtests$resivartrendP < 0.05))
  length(which(f$residualtests$resivartrendP < 0.05)) / length(f$residualtests$resivartrendP)
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  which(f$residualtests$resivartrendP < 0.05)
  (itemswithvartrendinf <- 
       f$fitres$id[f$residualtests$resivartrendP < 0.05])
   targetplot(itemswithvartrendinf, f)
  
   ### test on metabolomic data not in log scale #################
   data(Scenedesmus_metab)
   metabnotinlog <- Scenedesmus_metab
   metabnotinlog[-1, -1] <- 10^metabnotinlog[-1, -1] 
   
   (o <- continuousomicdata(metabnotinlog))
   plot(o)
   (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
   (f <- drcfit(s_quad, progressbar = TRUE, information.criterion = IC))
   plot(f, plot.type = "fitted_residuals")
   nrow(f$fitres)
   nrow(f$unfitres)
   length(which(f$residualtests$resivartrendP < 0.05))
   length(which(f$residualtests$resivartrendP < 0.05)) / length(f$residualtests$resivartrendP)
   
   which(f$residualtests$resivartrendP < 0.05)
   (itemswithvartrendinf <- 
       f$fitres$id[f$residualtests$resivartrendP < 0.05])
   targetplot(itemswithvartrendinf, f)
   
}