library(DRomics)
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, sigmoid.model = "log-probit", progressbar = TRUE))
itemslP <- as.character(f$fitres[f$fitres$model == "log-probit",]$id)
plot(f, items = itemslP)
(r <- bmdcalc(f, z = 1, x = 10))
r$res[r$res$id %in% itemslP,]
(b <- bmdboot(r, items = itemslP, tol = 0.05,
            niter = 1000, conf.level = 0.95, progressbar = TRUE))

b$res
