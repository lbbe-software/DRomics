library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

# importation and check of apical anchoring data
# datafilename <- system.file("extdata", "apical_anchoring.txt", package="DRomics")
# (o <- continuousanchoringdata(datafilename, check = TRUE))
data("Scenedesmus_apical")
(o <- continuousanchoringdata(Scenedesmus_apical, check = TRUE)) 

# Use of only one endpoint
#(o <- continuousanchoringdata(Scenedesmus_apical[c(1,2), ], check = TRUE)) # growth
# (o <- continuousanchoringdata(Scenedesmus_apical[c(1,3), ], check = TRUE)) # photosynthesis
plot(o)

# item selection using the three methods
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(s_lin <- itemselect(o, select.method = "linear", FDR = 0.05))
(s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.05))

# fit
(f <- drcfit(s_quad, progressbar = TRUE))
f$fitres
plot(f)
if (visualize)
{
  plot(f, dose_log_transfo = TRUE)
  plot(f, plot.type = "dose_residuals")
  
}

# various plot of fitted curves (without data)
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
           facetby = "model", colorby = "model")

{
  curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
             facetby = "typology")
  
  # plot of selection of curves
  curvesplot(f$fitres[f$fitres$trend == "U", ], xmax = max(f$omicdata$dose), 
             facetby = "id")
  
}


# calculation of benchmark doses
# options in shiny : z (numerical positive value), x (numerical positive value : percentage)
(r <- bmdcalc(f, z = 1, x = 10))
r$res

# plot of BMD with gradient
bmdplotwithgradient(r$res, xmax = max(f$omicdata$dose))

# Calculation of confidence intervals on BMDs by Bootstrap
niter <- 1000
niter <- 50
(b <- bmdboot(r, niter = niter)) # niter should be fixed at least at 1000 to get a reasonable precision
plot(b)