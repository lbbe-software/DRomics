# Test DRomics on datasets without replicates and without control data
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  ## test of the selection step with limma
  data(Scenedesmus_metab)
  head(Scenedesmus_metab)
  set.seed(1234)
  
  # build of a dataset without control nor replicate
  Scenedesmus_metab2 <- Scenedesmus_metab[, c(1,14:25)]
  Scenedesmus_metab2[1, -1] <- Scenedesmus_metab2[1, -1] * runif(11, 0.9, 1.1)
  head(Scenedesmus_metab2)
  
  (o <- continuousomicdata(Scenedesmus_metab))
  plot(o)
  (s <- itemselect(o, select.method = "quadratic"))
  (f <- drcfit(s))
  plot(f)
  
  (o2 <- continuousomicdata(Scenedesmus_metab2))
  plot(o2)
  (s2 <- itemselect(o2, select.method = "quadratic"))
  (f2 <- drcfit(s2))
  plot(f2)
  
  ## Test of the selection step with DESeq2
  data(Zhou_kidney_pce)
  head(Zhou_kidney_pce)
  Zhou <- Zhou_kidney_pce[1:1000, ]
  
  # build of a dataset without control nor replicate
  Zhou2 <- Zhou[, c(1, 4:15)]
  Zhou2[1, -1] <- Zhou2[1, -1] * runif(11, 0.9, 1.1)
  head(Zhou2)
  
  (o <- RNAseqdata(Zhou))
  plot(o)
  (s <- itemselect(o, select.method = "quadratic"))
  (f <- drcfit(s))
  plot(f, dose_log_transfo = TRUE)
  
  (o2 <- RNAseqdata(Zhou2))
  plot(o2)
  (s2 <- itemselect(o2, select.method = "quadratic"))
  (f2 <- drcfit(s2))
  plot(f2, dose_log_transfo = TRUE)
  
}
