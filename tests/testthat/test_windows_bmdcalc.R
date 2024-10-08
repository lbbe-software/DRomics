test_that("bmdcalc works as expected on the BMD results",
  {
    skip_on_cran()
    skip_on_os(c("mac", "linux", "solaris"))
    
    datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
    o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
    s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001)
    f <- drcfit(s_quad, progressbar = TRUE, information.criterion = "AIC")
    r.1 <- bmdcalc(f, z = 1, x = 10)
    r.2 <- bmdcalc(f, z = 2, x = 50)
    BMD.zSD.1 <- r.1$res$BMD.zSD
    BMD.zSD.2 <- r.2$res$BMD.zSD
    BMD.xfold.1 <- r.1$res$BMD.xfold
    BMD.xfold.2 <- r.2$res$BMD.xfold
    expect_equal(sum(is.nan(BMD.zSD.1)), 0)
    expect_equal(sum(is.nan(BMD.zSD.2)), 0)
    expect_equal(sum(is.nan(BMD.xfold.1)), 1)
    expect_equal(sum(is.nan(BMD.xfold.2)), 13)
    expect_equal(sum(is.na(BMD.zSD.1)), 0)
    expect_equal(sum(is.na(BMD.zSD.2)), 5)
    expect_equal(sum(is.na(BMD.xfold.1)), 30)
    expect_equal(sum(is.na(BMD.xfold.2)), 65)
    expect_equal(round(mean(BMD.zSD.1, na.rm = TRUE), 4), 2.1421)
    expect_equal(round(mean(BMD.zSD.2, na.rm = TRUE), 4), 4.0185)
    expect_equal(round(mean(BMD.xfold.1, na.rm = TRUE), 4), 3.2477)
    expect_equal(round(mean(BMD.xfold.2, na.rm = TRUE), 4), 5.1536)
    
  })