library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

# importation and check of RNAseq data and normalization
# with respect to library size and transformation 
# options to put in shiny : transfo.method (2 methods, rlog or vst)
datafilename <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
(o <- RNAseqdata(datafilename, check = TRUE, transfo.method = "rlog"))
plot(o)

(o.blind <- RNAseqdata(datafilename, check = TRUE, transfo.method = "rlog",
                       transfo.blind = TRUE))
plot(o.blind)


if(FALSE) # too long computation !
{
  data(Zhou_kidney_pce)
  
  # variance stabilizing tranformation
  (o1 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst"))
  plot(o1)
  
  # regularized logarithm
  (o2 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog"))
  plot(o2)
  
  # variance stabilizing tranformation (blind to the experimental design)
  (o3 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst",
                    transfo.blind = TRUE))
  plot(o3)
  
  # regularized logarithm
  (o4 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog",
                    transfo.blind = TRUE))
  plot(o4)
  
}


# item selection using the quadratic method
# options to put in shiny : select.method (3 methods), FDR (numerical positive value < 1)
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))

(s_lin <- itemselect(o, select.method = "linear", FDR = 0.001))
(s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.001))

# no options in shiny
(f <- drcfit(s_quad, progressbar = TRUE))
f$fitres
plot(f)

# various plot of fitted curves (without data)
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
           facetby = "model", colorby = "model")

if (visualize) 
{
  curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
             facetby = "typology")
}

# plot of selection of curves
curvesplot(f$fitres[f$fitres$trend == "bell", ], xmax = max(f$omicdata$dose), 
           facetby = "id")
if (visualize) 
{
  curvesplot(f$fitres[f$fitres$trend == "U", ], xmax = max(f$omicdata$dose), 
             facetby = "id")
}


# calculation of benchmark doses
# options in shiny : z (numerical positive value), x (numerical positive value : percentage)
(r <- bmdcalc(f, z = 1, x = 10))
(r.2 <- bmdcalc(f, z = 2, x = 50))

# plot of BMD
# options in shiny : BMDtype (2 possibilities), plottype (3 possibilities), by (3 possibilities)
# hist.bins (integer for hist only)
plot(r, BMDtype = "zSD", plottype = "ecdf", by = "none") 
if (visualize) 
{
  plot(r, BMDtype = "xfold", plottype = "ecdf", by = "none") 
  
  plot(r, plottype = "hist", by = "none", hist.bins = 10) 
  plot(r, plottype = "density", by = "none") 
  
  plot(r, plottype = "hist", by = "trend", hist.bins = 10) 
}

# Calculation of confidence intervals on BMDs by Bootstrap
niter <- 1000
niter <- 50
b <- bmdboot(r, niter = niter) # niter should be fixed at least at 1000 to get a reasonable precision
plot(b)