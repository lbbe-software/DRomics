context("examplewithametabolomicdata")
test_that("examplewithametabolomicdata", {
  skip_on_cran()
  
  # importation and check of metabolomic data
  datafilename <- system.file("extdata", "metabolo_sample.txt", package="DRomics")
  (o <- continuousomicdata(datafilename, check = TRUE))
  plot(o)
  
  # item selection using the quadratic method
  # options to put in shiny : select.method (3 methods), FDR (numerical positive value < 1)
  (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
  (s_lin <- itemselect(o, select.method = "linear", FDR = 0.001))
  (s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.001))
  
  # no options in shiny
  (f <- drcfit(s_quad, progressbar = TRUE))
  f$fitres
  plot(f)
  
  # evaluate the impact of preventsfitsoutofrange, enablesfequal0inGP, enablesfequal0inlGP
  data(Scenedesmus_metab)
  (o1 <- continuousomicdata(Scenedesmus_metab, check = TRUE))
  
  # datafilename <- system.file("extdata", "metabolo_sample.txt", package="DRomics")
  # (o1 <- continuousomicdata(datafilename, check = TRUE))
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
  #  targetplot(items = idremovedinf1bis, f1) 
  
  (idchanged <- f1bis$fitres$id[which(f1bis$fitres$model != f1ter$fitres$model | 
                                        f1bis$fitres$f != f1ter$fitres$f)])
  # targetplot(items = idchanged, f1bis, dose_log_transfo = TRUE)
  # targetplot(items = idchanged, f1ter, dose_log_transfo = TRUE)
  
  f1bis$fitres[f1bis$fitres$id %in% idchanged, ]
  f1ter$fitres[f1ter$fitres$id %in% idchanged, ]
  # no impact on those data
  
  # calculation of benchmark doses
  # options in shiny : z (numerical positive value), x (numerical positive value : percentage)
  (r <- bmdcalc(f, z = 1, x = 10))
  (r.2 <- bmdcalc(f, z = 2, x = 50))
  
  # plot of BMD
  # options in shiny : BMDtype (2 possibilities), plottype (3 possibilities), by (3 possibilities)
  # hist.bins (integer for hist only)
  plot(r, BMDtype = "zSD", plottype = "ecdf", by = "none")
  plot(r, BMDtype = "xfold", plottype = "ecdf", by = "none") 
  
  plot(r, plottype = "hist", by = "none", hist.bins = 10) 
  plot(r, plottype = "density", by = "none") 
  
  plot(r, plottype = "ecdf", by = "trend", hist.bins = 10) 
  
  # various plot of fitted curves (without data)
  curvesplot(r$res, xmax = max(r$omicdata$dose), 
             facetby = "model", colorby = "model")
  
  curvesplot(r$res, xmax = max(r$omicdata$dose), 
             facetby = "typology")
  
  # plot of selection of curves
  curvesplot(r$res[r$res$trend == "U", ], xmax = max(f$omicdata$dose), 
             facetby = "id")
  
  # niter <- 1000
  niter <- 10
  # Calculation of confidence intervals on BMDs by Bootstrap
  b <- bmdboot(r, niter = niter) # niter should be fixed at least at 1000 to get a reasonable precision
  plot(b)
})
