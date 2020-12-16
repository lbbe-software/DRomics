library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots
doboot <- FALSE

# importation and check of metabolomic data
datafilename <- system.file("extdata", "metabolo_sample.txt", package="DRomics")
(o <- continuousomicdata(datafilename, check = TRUE))
if (visualize)
plot(o)

# item selection using the quadratic method
# options to put in shiny : select.method (3 methods), FDR (numerical positive value < 1)
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
if (visualize)
{
  (s_lin <- itemselect(o, select.method = "linear", FDR = 0.001))
  (s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.001))
  
}

# no options in shiny
(f <- drcfit(s_quad, progressbar = TRUE))
f$fitres
if (visualize)
plot(f)


if (visualize) 
{
  # various plot of fitted curves (without data)
  curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
             facetby = "model", colorby = "model")
  
  curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
             facetby = "typology")
  
  # plot of selection of curves
  curvesplot(f$fitres[f$fitres$trend == "U", ], xmax = max(f$omicdata$dose), 
             facetby = "id")
  
}

# calculation of benchmark doses
# options in shiny : z (numerical positive value), x (numerical positive value : percentage)
(r <- bmdcalc(f, z = 1, x = 10))
if (visualize)
(r.2 <- bmdcalc(f, z = 2, x = 50))

# plot of BMD
# options in shiny : BMDtype (2 possibilities), plottype (3 possibilities), by (3 possibilities)
# hist.bins (integer for hist only)
if (visualize)
{
  plot(r, BMDtype = "zSD", plottype = "ecdf", by = "none")
  plot(r, BMDtype = "xfold", plottype = "ecdf", by = "none") 
  
  plot(r, plottype = "hist", by = "none", hist.bins = 10) 
  plot(r, plottype = "density", by = "none") 
  
  plot(r, plottype = "ecdf", by = "trend", hist.bins = 10) 
  
}

if (doboot)
{
  niter <- 1000
  niter <- 10
  
  # Calculation of confidence intervals on BMDs by Bootstrap
  b <- bmdboot(r, niter = niter) # niter should be fixed at least at 1000 to get a reasonable precision
  if (visualize)
    plot(b)
  
}

