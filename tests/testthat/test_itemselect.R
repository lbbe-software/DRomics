context("itemselect_count")
test_that("itemselect works as expected on the number of selected probes",
  {
    datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
    (o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
    (s_quad0p001 <- itemselect(o, select.method = "quadratic", FDR = 0.001))
    expect_equal(length(s_quad0p001$selectindex), 78)
    (s_quad0p05 <- itemselect(o, select.method = "quadratic", FDR = 0.05))
    expect_equal(length(s_quad0p05$selectindex), 318)
  })