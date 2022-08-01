library(DRomics)
visualize <- FALSE # put to TRUE for a manual check of plots
doboot <- FALSE

# importation and check of RNAseq data and normalization
# with respect to library size and transformation 
# options to put in shiny : transfo.method (2 methods, rlog or vst)
datafilename <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
# small data set 'less than 1000 items (999)
(o.vst <- RNAseqdata(datafilename, check = TRUE, transfo.method = "vst"))
plot(o.vst)

if (visualize) # too long computation !
{
  plot(o.vst, range = 1.5) # boxplot visualizing outliers
  (o.vst.notblind <- RNAseqdata(datafilename, check = TRUE, transfo.method = "vst",
                            transfo.blind = FALSE))
  plot(o.vst.notblind)
  
  (o.rlog <- RNAseqdata(datafilename, check = TRUE, transfo.method = "rlog"))
  plot(o.rlog)
  
  (o.rlog.notblind <- RNAseqdata(datafilename, check = TRUE, transfo.method = "rlog",
                                transfo.blind = FALSE))
  plot(o.rlog.notblind)
  
}

if(visualize) # too long computation !
{
  data(Zhou_kidney_pce)
  
  # variance stabilizing tranformation
  (o1 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst"))
  plot(o1)
  
  # regularized logarithm
  (o2 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog"))
  plot(o2)
  
  # variance stabilizing tranformation (not blind to the experimental design)
  (o3 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst",
                    transfo.blind = FALSE))
  plot(o3)
  
  # regularized logarithm (not blind to the experimental design)
  (o4 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog",
                    transfo.blind = FALSE))
  plot(o4)
  
}


if (visualize)
{
  # item selection using the quadratic method
# options to put in shiny : select.method (3 methods), FDR (numerical positive value < 1)
  o <- o.rlog
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (s_lin <- itemselect(o, select.method = "linear", FDR = 0.01))
  (s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.01))

  (f <- drcfit(s_quad, progressbar = TRUE))
  f$fitres
  plot(f)
  
  # various plot of fitted curves (without data)
  curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
             facetby = "model", colorby = "model")
  
  curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
             facetby = "typology")
  # plot of selection of curves
  curvesplot(f$fitres[f$fitres$trend == "bell", ], xmax = max(f$omicdata$dose), 
             facetby = "id")
  
  curvesplot(f$fitres[f$fitres$trend == "U", ], xmax = max(f$omicdata$dose), 
             facetby = "id")
}

if (visualize)
{
  # evaluate the impact of preventsfitsoutofrange, enablesfequal0inGP, enablesfequal0inlGP
  data(Zhou_kidney_pce)
  (o1 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog"))
  s_quad1 <- itemselect(o1, select.method = "quadratic", FDR = 0.1)
  (f1 <- drcfit(s_quad1, 
                preventsfitsoutofrange = FALSE,
                enablesfequal0inGP  = FALSE,
                enablesfequal0inLGP  = FALSE,
                progressbar = TRUE))
  (f1bis <- drcfit(s_quad1, 
                preventsfitsoutofrange = TRUE,
                enablesfequal0inGP  = FALSE,
                enablesfequal0inLGP  = FALSE,
                progressbar = TRUE))
  (f1ter <- drcfit(s_quad1, 
                   preventsfitsoutofrange = TRUE,
                   enablesfequal0inGP  = TRUE,
                   enablesfequal0inLGP  = TRUE,
                   progressbar = TRUE))
  
  (idremovedinf1bis <- f1$fitres$id[!is.element(f1$fitres$id, f1bis$fitres$id)])
  targetplot(items = idremovedinf1bis, f1) 
  
  (idchanged <- f1bis$fitres$id[which(f1bis$fitres$model != f1ter$fitres$model | 
                                        f1bis$fitres$f != f1ter$fitres$f)])
  targetplot(items = idchanged, f1bis, dose_log_transfo = TRUE)
  targetplot(items = idchanged, f1ter, dose_log_transfo = TRUE)
  
  f1bis$fitres[f1bis$fitres$id %in% idchanged, ]
  f1ter$fitres[f1ter$fitres$id %in% idchanged, ]
  
  
}


# calculation of benchmark doses
# options in shiny : z (numerical positive value), x (numerical positive value : percentage)
if (visualize) 
{
  (r <- bmdcalc(f, z = 1, x = 10))
  (r.2 <- bmdcalc(f, z = 2, x = 50))

# plot of BMD
# options in shiny : BMDtype (2 possibilities), plottype (3 possibilities), by (3 possibilities)
# hist.bins (integer for hist only)
  plot(r, BMDtype = "zSD", plottype = "ecdf", by = "none") 
  
  plot(r, BMDtype = "xfold", plottype = "ecdf", by = "none") 
  
  plot(r, plottype = "hist", by = "none", hist.bins = 10) 
  plot(r, plottype = "density", by = "none") 
  
  plot(r, plottype = "hist", by = "trend", hist.bins = 10) 
}

if (doboot)
{
  # Calculation of confidence intervals on BMDs by Bootstrap
  niter <- 1000
  niter <- 10
  b <- bmdboot(r, niter = niter) # niter should be fixed at least at 1000 to get a reasonable precision
  if (visualize) plot(b)
  
}

if(visualize) # too long computation !
{
  data(Zhou_kidney_pce)
  
  # exploration of an extreme case (BMD at 0)
  d <- Zhou_kidney_pce
  (o <- RNAseqdata(d))
  plot(o)
  (s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
  (f <- drcfit(s, progressbar = TRUE))
  head(f$fitres)
  
  r <- bmdcalc(f, z = 1)
  plot(r) 
  if (require(ggplot2))
    plot(r) + scale_x_log10() # same plot in log scale of BMD
  
  bmdplotwithgradient(r$res, BMDtype = "zSD") 
  bmdplotwithgradient(r$res, BMDtype = "zSD", BMD_log_transfo = TRUE) 
 
  # no more 0 BMD values using argument minBMD 
  # res0 <- r$res[r$res$BMD.zSD == 0, ]
  # curvesplot(res0, xmin =0.0000000001, xmax = max(f$omicdata$dose), 
  #            colorby = "model", dose_log_transfo = TRUE)
  # plot(f, items = r$res[r$res$BMD.zSD == 0, ]$id)
  # plot(f, items = r$res[r$res$BMD.zSD == 0, ]$id, dose_log_trans = TRUE)
}
