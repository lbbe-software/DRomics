context("testAIC")
test_that("Testof the impact of the three information criteria", {
  skip_on_cran()
  
  ### test on microarray data ######################
  datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")
  
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  (fAICcrelaxed <- drcfit(s_quad, information.criterion = "AICc", deltaAICminfromnullmodel = 0, progressbar = TRUE))
  
  
  table(fAIC$fitres$model)
  table(fAICc$fitres$model)
  table(fAICcrelaxed$fitres$model)
  table(fBIC$fitres$model)
  
  head(fAIC$information.criterion.val)
  head(fAICc$information.criterion.val)
  head(fBIC$information.criterion.val)
  
  # check of values on the linear model
  (npts <- length(o$dose))
  k <- 3 # mod lin
  # correction to get AICc
  fAIC$information.criterion.val$InfoCrit.L + 2*k*(k+1)/(npts -k -1)
  fAICc$information.criterion.val$InfoCrit.L
  # correction to get BIC
  fAIC$information.criterion.val$InfoCrit.L - 2*k + log(npts)*k
  fBIC$information.criterion.val$InfoCrit.L
  
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
  (fAICcrelaxed <- drcfit(s_quad, information.criterion = "AICc", 
                          deltaAICminfromnullmodel = 0, progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  table(fAIC$fitres$model)
  table(fAICc$fitres$model)
  table(fAICcrelaxed$fitres$model)
  table(fBIC$fitres$model)
  
  table(fAIC$fitres$nbpar)
  table(fAICc$fitres$nbpar)
  table(fBIC$fitres$nbpar)
  
  plot(fAIC, 81)
  plot(fAICc, 81)
  plot(fBIC, 81)
  
  # exploration of simplified biphasic models with f = 0
  f <- fAIC
  # f <- fBIC
  #  f <- AICc
  (id2explore <- f$fitres$id[f$fitres$model %in% c("Gauss-probit", "log-Gauss-probit") & 
                               f$fitres$f == 0])
  f$fitres[f$fitres$id %in%  id2explore, ]
  plot(f, items = id2explore)
  
  ###### test on apical data
  data(Scenedesmus_apical)
  head(Scenedesmus_apical)
  (o <- continuousanchoringdata(Scenedesmus_apical, backgrounddose = 0.1))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (fAIC <- drcfit(s_quad, information.criterion = "AIC", progressbar = TRUE))
  (fAICc <- drcfit(s_quad, information.criterion = "AICc", progressbar = TRUE))
  (fBIC <- drcfit(s_quad, information.criterion = "BIC", progressbar = TRUE))
  
  plot(fAIC)
  plot(fAICc)
  plot(fBIC)
  
  ###### test on in situ RNAseq data
  datafilename <- system.file("extdata", "insitu_RNAseq_sample.txt", package="DRomics")
  (o <- RNAseqdata(datafilename, backgrounddose = 2e-2, transfo.method = "rlog"))
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
  
  plot(fAIC, dose_log_transfo = TRUE)
  plot(fAICc, dose_log_transfo = TRUE)
  plot(fBIC, dose_log_transfo = TRUE)
  
  nrow(fAIC$fitres)
  nrow(fAICc$fitres)
  nrow(fBIC$fitres)
  
  id2compare <- fBIC$fitres$id[50:70]
  plot(fAIC, items = id2compare, dose_log_transfo = TRUE)
  plot(fAICc, items = id2compare, dose_log_transfo = TRUE)
  plot(fBIC, items = id2compare, dose_log_transfo = TRUE)
  
  # exploration of simplified biphasic models with f = 0
  # f <- fAIC
  # f <- fBIC
  f <- fAICc
  (id2explore <- f$fitres$id[f$fitres$model %in% c("Gauss-probit", "log-Gauss-probit") & 
                               f$fitres$f == 0])
  f$fitres[f$fitres$id %in%  id2explore, ]
  plot(f, items = id2explore, dose_log_transfo = TRUE)
  
  head(fAIC$information.criterion.val, 20)
  head(fAICc$information.criterion.val, 20)
  head(fBIC$information.criterion.val, 20)
  
  
})