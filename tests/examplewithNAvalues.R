# Test DRomics on datasets with NA values
# possible especially for apical data
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  ########### Example on apical data ################
  ###################################################
  data(Scenedesmus_apical)
  head(Scenedesmus_apical)
  set.seed(1234)
  
  # build of a dataset with NA values
  Scenedesmus_apical2 <- Scenedesmus_apical
  Scenedesmus_apical2[3, ] <- Scenedesmus_apical2[2, ]
  Scenedesmus_apical2[2, 2] <- NA
  Scenedesmus_apical2[3, 27] <- NA
  Scenedesmus_apical2[3, 1] <- "growthbis"
  head(Scenedesmus_apical2)
  
  (o <- continuousanchoringdata(Scenedesmus_apical2))
  plot(o)
  o$data
  complete.cases(o$data)
  sum(complete.cases(o$data))
  o$data.mean
  (s <- itemselect(o, select.method = "quadratic"))
  (f <- drcfit(s))
  plot(f)
  (r <- bmdcalc(f))
  r$res
  (b <- bmdboot(r))
  b$res
 
  (f.AIC <- drcfit(s, information.criterion = "AIC"))
  (f.BIC <- drcfit(s, information.criterion = "BIC"))
  
  ## comparison with  the individual fit for growth
  Scenedesmus_apical3 <- Scenedesmus_apical2[1:2, ]
  Scenedesmus_apical3 <- Scenedesmus_apical3[, -2] # remove of the column with NA
  head(Scenedesmus_apical3)
  
  (o3 <- continuousanchoringdata(Scenedesmus_apical3))
  plot(o3)
  (s3 <- itemselect(o3, select.method = "quadratic"))
  (f3 <- drcfit(s3))
  plot(f3)
  (r3 <- bmdcalc(f3))
  r3$res
  
  (b3 <- bmdboot(r3))
  b3$res
  b$res
  
  ########### Example on metabolomic data ################
  ########################################################
  data(Scenedesmus_metab)
  head(Scenedesmus_metab)
  set.seed(1234)
  
  # build of a dataset with NA values
  Scenedesmus_metab2 <- Scenedesmus_metab1 <- Scenedesmus_metab[1:50, ]
  Scenedesmus_metab2[, 1]
  # Put NA values on a non selected item (line)
  # and on a selected item (line 23 item "NAP_24")
  Scenedesmus_metab2[2, 2] <- NA
  Scenedesmus_metab2[23, 5] <- NA
  Scenedesmus_metab2[23, 4] <- NA
  
  (o1 <- continuousomicdata(Scenedesmus_metab1))
  plot(o1)
  (o2 <- continuousomicdata(Scenedesmus_metab2, check = FALSE))
  plot(o2)

  (s1 <- itemselect(o1, select.method = "quadratic"))
  (s2 <- itemselect(o2, select.method = "quadratic"))

  (f1 <- drcfit(s1))
  plot(f1, items = "NAP_24")
  (f2 <- drcfit(s2))
  plot(f2, items = "NAP_24")
  
  f1$fitres[1:3, ]
  f2$fitres[1:3, ]

  (f2.AIC <- drcfit(s2, information.criterion = "AIC"))
  (f2.BIC <- drcfit(s2, information.criterion = "BIC"))
  
  (r1 <- bmdcalc(f1))
  r1$res[1:2, ]
  (r2 <- bmdcalc(f2))
  r2$res[1:2, ]
  
  (b1 <- bmdboot(r1, niter = 100))
  b1$res[1:2, ]
  (b2 <- bmdboot(r2, niter = 100))
  b2$res[1:2, ]
  
  
  # Trial with NA values for each replicate of a dose
  Scenedesmus_metab3 <- Scenedesmus_metab1 
  Scenedesmus_metab3[c(1,23), ]
  # Put NA values for each replicate for dose 1.79 (line 23 item "NAP_24")
  Scenedesmus_metab3[23, 11] <- NA
  Scenedesmus_metab3[23, 20] <- NA
  Scenedesmus_metab3[23, 21] <- NA
  (o3 <- continuousomicdata(Scenedesmus_metab3))
  plot(o3)

  (s3 <- itemselect(o3, select.method = "quadratic"))

  (f3 <- drcfit(s3))
  plot(f3, items = "NAP_24")
  plot(f1, items = "NAP_24")
  
  f3$fitres[1:5, ]
  f1$fitres[1:5, ]

  (r3 <- bmdcalc(f3))

  (b3 <- bmdboot(r3, niter = 100))
  b3$res[1:5, ]
  b1$res[1:5, ]
  
  ################## example with microarray or RNAseq data #############
  ## It should stop
  data(Zhou_kidney_pce)
  Zhou <- Zhou_kidney_pce[1:1000, ]
  Zhou[10,10] <- NA
  try(RNAseqdata(Zhou))
  
  try(microarraydata(Zhou))
 }
