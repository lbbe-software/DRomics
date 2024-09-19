context("testbmdcalc")
test_that("testbmdcalc", {
  skip_on_cran()
  
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
  r$omicdata$dose
  (r <- bmdcalc(f, minBMD = 0.3))
  plot(r) 
  (r <- bmdcalc(f, minBMD = 0.3, ratio2switchinlog = 1))
  plot(r)
  
  # check of defensive prog
  # (r <- bmdcalc(f, minBMD = 0))
  # (r <- bmdcalc(f, minBMD = 1)) # 1 > minimal non null dose
  
  
  # look at BMR in the output
  (r <- bmdcalc(f, z = 2, x = 20))
  r$res
  r$minBMD
  
  # bootstrap after forcing minBMD to a high value
  # without anay interest, but just to test bmdboot
  (r <- bmdcalc(f, minBMD = 0.3))
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
  
  (r <- bmdcalc(f))
  r$omicdata$dose
  plot(r)
  (r <- bmdcalc(f, ratio2switchinlog = 1))
  plot(r) 
  (r <- bmdcalc(f, minBMD = 0.0001, ratio2switchinlog = 1))
  plot(r) 
  (r <- bmdcalc(f, minBMD = 0.1))
  plot(r) 
}
)