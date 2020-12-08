# Tests on residuals and corresponding postfit filter
library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if (visualize)
{
  datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
  
  (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
  (f <- drcfit(s_quad, progressbar = TRUE))
  
  # Look at the table of results for successful fits
  head(f$fitres)
  
  # Look at the table of results for unsuccessful fits
  head(f$unfitres)
  
  # count the number of unsuccessful fits for each cause
  table(f$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits
  targetplot(f$unfitres$id[f$unfitres$cause == "constant.model"], f)
  targetplot(f$unfitres$id[f$unfitres$cause == "trend.in.residuals"], f)
  nrow(f$fitres)
  nrow(f$unfitres)
  
  # Fit without postfit filtering
  (f2 <- drcfit(s_quad, postfitfilter = FALSE, progressbar = TRUE))
  nrow(f2$fitres)
  nrow(f2$unfitres)
  
  # count the number of unsuccessful fits for each cause
  table(f2$unfitres$cause)
  
  # Plot of the data corresponding to unsuccessful fits in f
  (itemseliminatedinf <- f$unfitres$id[f$unfitres$cause == "trend.in.residuals"])
  
  targetplot(itemseliminatedinf, f2)
  
  f2$residualtrendtests
  (itemswithmeantrendinf2 <- 
      f2$fitres$id[f2$residualtrendtests$resimeantrendadjP < 0.05])
  targetplot(itemswithmeantrendinf2, f2)
  
  (itemswithvartrendinf2 <- 
      f2$fitres$id[f2$residualtrendtests$resivartrendadjP < 0.05])
  targetplot(itemswithvartrendinf2, f2)
  
}