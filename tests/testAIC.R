# Testof the impact of the three information criteria
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  ### test on microarray data ######################
  datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")

  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  table(fAIC$fitres$model)
  table(fAICc$fitres$model)
  table(fBIC$fitres$model)
  
  head(fAIC$information.criterion.val)
  head(fAICc$information.criterion.val)
  head(fBIC$information.criterion.val)

  # check of values on the linear model
  (npts <- length(o$dose))
  k <- 3 # mod lin
  # correction to get AICc
  fAIC$information.criterion.val$AIC.L + 2*k*(k+1)/(npts -k -1)
  fAICc$information.criterion.val$AIC.L
  # correction to get BIC
  fAIC$information.criterion.val$AIC.L - 2*k + log(npts)*k
  fBIC$information.criterion.val$AIC.L
  
  plot(fAIC)
  plot(fAICc)
  plot(fBIC)
  
  ################# on larger data sets
  datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  table(fAIC$fitres$model)
  table(fAICc$fitres$model)
  table(fBIC$fitres$model)

  ### test on metabolo data with 4 doses ######################
  data(Scenedesmus_metab)
  head(Scenedesmus_metab)
  # build of a dataset with 
  Scenedesmus_metab2 <- Scenedesmus_metab[, c(1:8,10, 12, 14, 15, 18, 19, 22, 23)]
  head(Scenedesmus_metab2)
  
  (o <- continuousomicdata(Scenedesmus_metab2))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  table(fAIC$fitres$model)
  table(fAICc$fitres$model)
  table(fBIC$fitres$model)
  
  table(fAIC$fitres$nbpar)
  table(fAICc$fitres$nbpar)
  table(fBIC$fitres$nbpar)
  
  plot(fAIC)
  plot(fAICc)
  plot(fBIC)
  
  ### test on RNAseq data with 5 doses ######################
  data(Zhou_kidney_pce)
  head(Zhou_kidney_pce)

  (o <- RNAseqdata(Zhou_kidney_pce))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  table(fAIC$fitres$model)
  table(fAICc$fitres$model)
  table(fBIC$fitres$model)
  
  table(fAIC$fitres$nbpar)
  table(fAICc$fitres$nbpar)
  table(fBIC$fitres$nbpar)
  
  plot(fAIC, 81)
  plot(fAICc, 81)
  plot(fBIC, 81)
  
  ###### test on apical data
  data(Scenedesmus_apical)
  head(Scenedesmus_apical)
  (o <- continuousanchoringdata(Scenedesmus_apical))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  plot(fAIC)
  plot(fAICc)
  plot(fBIC)
  
}