# Test DRomics on datasets without replicates and without control data
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots
niterboot <- 25
#niterboot <- 250

if (visualize)
{
  datafilename <- system.file("extdata", "insitu_RNAseq_sample.txt", package="DRomics")
  (o <- RNAseqdata(datafilename, backgrounddose = 2e-2, transfo.method = "vst"))
#  (o <- RNAseqdata(datafilename, backgrounddose = 2e-2, transfo.method = "rlog"))
  (s <- itemselect(o)) 
  (f <- drcfit(s))
  (fbis <- drcfit(s, enablesfequal0inGP = FALSE,
                  enablesfequal0inLGP = FALSE, 
                  preventsfitsoutofrange = FALSE))
  # Focus on eliminated curves (peak out of range)
  (idnotinf <- fbis$fitres$id[!(fbis$fitres$id %in% f$fitres$id)])
  plot(fbis, items = idnotinf, dose_log_transfo = TRUE)
  plot(fbis, items = idnotinf, dose_log_transfo = FALSE)

    # Focus on simplified biphasic models in monotonic models
  (id2explore <- f$fitres$id[f$fitres$model %in% c("Gauss-probit", "log-Gauss-probit") & 
                              f$fitres$f == 0])
  f$fitres[f$fitres$id %in%  id2explore, ]
  plot(f, items = id2explore, dose_log_transfo = TRUE)
  plot(fbis, items = id2explore, dose_log_transfo = TRUE)
  plot(f, items = id2explore, dose_log_transfo = FALSE)
  plot(fbis, items = id2explore, dose_log_transfo = FALSE)
  
  (r <- bmdcalc(f))
  (rbis <- bmdcalc(fbis))
  b <- bmdboot(r, niter = niterboot)
  bbis <- bmdboot(rbis, niter = niterboot)
  
  plot(f , items = id2explore,
       BMDoutput = b, dose_log_transfo = TRUE) 
  plot(fbis , items = id2explore, 
       BMDoutput = bbis, dose_log_transfo = TRUE) 

}
