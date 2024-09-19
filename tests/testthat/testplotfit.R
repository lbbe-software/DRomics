context("testbmdplotfit")
test_that("testbmdplotfit", {
  skip_on_cran()
  
  niterboot <- 25
  # niterboot <- 250
  
  datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")
  # to test the multi-page of plotfit2pdf take the file below
  # datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
  s <- itemselect(o) 
  f <- drcfit(s)
  r <- bmdcalc(f)
  b <- bmdboot(r, niter = niterboot)
  plot(f , BMDoutput = r, BMDtype = "zSD") 
  plot(f , BMDoutput = b, BMDtype = "zSD") 
  plot(f , BMDoutput = r, BMDtype = "xfold") 
  plot(f , BMDoutput = b, BMDtype = "xfold") 
  plot(f , items = 3,  BMDoutput = b, BMDtype = "zSD") 
  plot(f , items = 6, BMDoutput = b,  BMDtype = "zSD") 
  plot(f , items = c("12.1", "4", "70"),  BMDoutput = b, BMDtype = "zSD")
  
  plotfit2pdf(f , BMDoutput = r, BMDtype = "zSD") 
  plotfit2pdf(f , BMDoutput = b, BMDtype = "zSD") 
  plotfit2pdf(f , BMDoutput = r, BMDtype = "xfold") 
  plotfit2pdf(f , BMDoutput = b, BMDtype = "xfold") 
  plotfit2pdf(f , items = 3,  BMDoutput = r, BMDtype = "zSD") 
  plotfit2pdf(f , items = 6, BMDoutput = b,  BMDtype = "xfold") 
  plotfit2pdf(f , items = c("12.1", "4", "70"),  BMDoutput = r, BMDtype = "zSD") 
  
  
  # Metabolo ex. case where there is a pb of items ordering 
  datafilename <- system.file("extdata", "metabolo_sample.txt", package="DRomics")
  o <- continuousomicdata(datafilename, backgrounddose = 0, check = TRUE)
  s <- itemselect(o, select.method = 'quadratic', FDR = 0.05)
  f <- drcfit(s, progressbar = FALSE, parallel = 'no')
  plot(f, plot.type = 'dose_fitted', dose_log_transfo = TRUE)
  r <- bmdcalc(f, z = 1, x = 10)
  plot(f, plot.type = 'dose_fitted', BMDoutput = r, BMDtype = 'zSD', dose_log_transfo = TRUE) # good order
  plotfit2pdf(f) # good order - initial order by p-value of the selection
  plotfit2pdf(f , BMDoutput = r, BMDtype = "zSD")  # alphabetic order WHY ??????????????? PB fixed - OK
  b <- bmdboot(r, niter = 10)
  plotfit2pdf(f , BMDoutput = b, BMDtype = "zSD") # alphabetic order WHY ??????????????? PB fixed - OK
  
})

