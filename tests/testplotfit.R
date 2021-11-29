library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots
# niterboot <- 25
niterboot <- 250

if (visualize)
{
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
  
}

