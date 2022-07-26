# test scaling on curvesplot and bmdplotwithgradient

library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots

if(visualize) 
{
  

# A toy example on a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
                            package="DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01)
f <- drcfit(s_quad, progressbar = TRUE)
r <- bmdcalc(f)

# Plots without scaling 
#
curvesplot(r$res, xmax = max(f$omicdata$dose), facetby = "id")
bmdplotwithgradient(r$res, xmax = max(f$omicdata$dose),
           add.label = TRUE, label.size = 4)
# Plots with scaling 
#
curvesplot(r$res, xmax = max(f$omicdata$dose), facetby = "id",
           scaling = TRUE)
bmdplotwithgradient(r$res, xmax = max(f$omicdata$dose),
                    add.label = TRUE, label.size = 4,
                    scaling = TRUE)
}