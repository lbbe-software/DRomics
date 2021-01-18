# Test DRomics on datasets with NA values
# possible especially for apical data
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  ## test of the selection step with limma
  data(Scenedesmus_apical)
  head(Scenedesmus_apical)
  set.seed(1234)
  
  # build of a dataset without control nor replicate
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
  
}
