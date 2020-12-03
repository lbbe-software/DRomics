library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots
doboot <- FALSE


if(visualize) 
{
  
  # Impact of ratio2switchinlog and minBMD on a toy example  
  #
  # datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")
  datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (f <- drcfit(s_quad, progressbar = TRUE))
  (r <- bmdcalc(f))
  plot(r) 
  r$res
  (r <- bmdcalc(f, ratio2switchinlog = 1))
  plot(r) 
  (r <- bmdcalc(f, minBMD = 2))
  plot(r) 
  (r <- bmdcalc(f, minBMD = 2, ratio2switchinlog = 1))
  plot(r)
  
  # look at BMR in the output
  (r <- bmdcalc(f, z = 2, x = 20))
  r$res
  r$minBMD
  
  # bootstrap after forcing minBMD to a high value
  # without anay interest, but just to test bmdboot
  (r <- bmdcalc(f, minBMD = 2))
  (b <- bmdboot(r, niter = 100)) # with a non reasonable value for niter 
  b$res
  plot(b) # plot of BMD.zSD after removing of BMDs with infinite upper bounds
  
  # and with minBMD to its default value (dosemin/100)
  (r <- bmdcalc(f))
  (b <- bmdboot(r, niter = 100)) # with a non reasonable value for niter 
  plot(b) # plot of BMD.zSD after removing of BMDs with infinite upper bounds
  

  # using an RNAseq example
  # subsample
  # datafilename <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
  # (o <- RNAseqdata(datafilename, check = TRUE, transfo.method = "vst"))
  
  # whole data
  data(Zhou_kidney_pce)
  d <- Zhou_kidney_pce
  (o <- RNAseqdata(d))
  
  (s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (f <- drcfit(s, progressbar = TRUE))
  head(f$fitres)
  
  if (require(ggplot2))
  {
    (r <- bmdcalc(f))
    plot(r) + scale_x_log10()
    (r <- bmdcalc(f, ratio2switchinlog = 1))
    plot(r) + scale_x_log10()
    (r <- bmdcalc(f, minBMD = 0.0001, ratio2switchinlog = 1))
    plot(r) + scale_x_log10()
    (r <- bmdcalc(f, minBMD = 1))
    plot(r) + scale_x_log10()
  }
}
