# internally used function
calcBMD <- function(y0, delta, xext, yext, dosemin, dosemax, ydosemax, func, 
                    func_xinlog, b, c, d, e, g, minBMD, ratio2switchinlog){
  if ((dosemax / dosemin) > ratio2switchinlog)
  {
    workwithxinlog <- TRUE
    func4uniroot <- func_xinlog
    firstinterval <- c(log(minBMD), log(xext)) 
    secondinterval <- c(log(max(xext, minBMD)), log(dosemax))
  }
  else
  {
    workwithxinlog <- FALSE
    func4uniroot <- func
    firstinterval <- c(minBMD, xext)
    secondinterval <- c(max(xext, minBMD), dosemax)
  }
  finalroot <- NA
  if (g > 0)
  { # Umbrella shape
    threshold <- y0 + delta # we first seek the BMR above d
    #value of y - threshold at minBMD
    funcatminBMD <- 
      func(minBMD, b=b, c=c, d=d, e=e, g=g, threshold=threshold) 
    if (y0 < threshold & threshold < yext & xext!=0) # BMR in first phase
      # xext != 0 is for some rare Gaussprobit models with e = 0 so 
      # with ext = 0 and thus increasing
    {
      if ((xext <= minBMD) | (funcatminBMD >= 0))
      {
        finalroot <- workwithxinlog * log(minBMD) + (1 - workwithxinlog) * minBMD
      } else
      { # then we seek the BMR above y0 
        finalroot <- uniroot(func4uniroot, interval=firstinterval, b=b, c=c, 
                             d=d, e=e, g=g, threshold=threshold)$root
      }
    } else # BMR may be in the second phase 
    {
      threshold <- y0 - delta
      funcatminBMD <- 
        func(minBMD, b=b, c=c, d=d, e=e, g=g, threshold=threshold) 
      if (funcatminBMD <= 0)
      {
        finalroot <- workwithxinlog * log(minBMD) + (1 - workwithxinlog) * minBMD
      } else
      {
        if(c < y0 & threshold > ydosemax)
        {  # then we seek the BMR below y0 (possible only if c<y0) 
          finalroot <- uniroot(func4uniroot, interval=secondinterval, b=b, c=c, 
                               d=d, e=e, g=g, threshold=threshold)$root
        }
      }
    }
  }
  if(g < 0)
  { # U shape
    threshold <- y0 - delta # we first seek the BMR below y0
    funcatminBMD <- 
      func(minBMD, b=b, c=c, d=d, e=e, g=g, threshold=threshold) 
    if(y0 > threshold & threshold > yext & xext!=0) # BMR in first phase
      # xext != 0 is for some rare Gaussprobit models with e = 0 so 
      # with ext = 0 and thus decreasing
    {
      if ((xext <= minBMD) | (funcatminBMD <= 0))
      {
        finalroot <- workwithxinlog * log(minBMD) + (1 - workwithxinlog) * minBMD
      } else
      {
        finalroot <- uniroot(func4uniroot, interval=firstinterval, b=b, c=c, 
                             d=d, e=e, g=g, threshold=threshold)$root
      }  
    }
    else  # BMR may be in the second phase
    {
      threshold <- y0 + delta
      funcatminBMD <- 
        func(minBMD, b=b, c=c, d=d, e=e, g=g, threshold=threshold) 
      if (funcatminBMD >= 0)
      {
        finalroot <- workwithxinlog * log(minBMD) + (1 - workwithxinlog) * minBMD
      } else
      {
        if(c > y0 & threshold < ydosemax)
        {  # then we seek the BMR above y0 (possible only if c > y0) 
          finalroot <- uniroot(func4uniroot, interval=secondinterval, b=b, c=c, 
                               d=d, e=e, g=g, threshold=threshold)$root
        }
      }
    }
  }
  if (workwithxinlog)
  {
    return(list(threshold = threshold, BMD = exp(finalroot)))
  } else
  {
    return(list(threshold = threshold, BMD = finalroot))
  }
}

### Calculation of BMD values (x-fold or z-SD) from fitted dose-response curves
bmdcalc <- function(f, z = 1, x = 10, minBMD, ratio2switchinlog = 100)
{
  # Checks
  if (!inherits(f, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit.")
  
  dfitall <- f$fitres
  nselect <- length(dfitall$irow)
  dosemax <- max(f$omicdata$dose)
  dosemin <- min(f$omicdata$dose[f$omicdata$dose != 0])
  
  if (missing(minBMD)) minBMD <- dosemin / 100 # could be changed
  
  if (minBMD <= 0)
    stop("minBMD should be a stricly positive value.")

  dcalc <- data.frame(xextrem = dfitall$xextrem, 
                      yextrem = rep(NA,nselect), 
                      y0 = dfitall$y0,
                      ydosemax = rep(NA, nselect),
                      yp = rep(NA,nselect), 
                      ysd = rep(NA,nselect),
                      BMDp = rep(NA,nselect), 
                      BMDsd = rep(NA,nselect)
  )
  xdiv100 <- x/100 # x in relative value and not in percentage
  
  for(i in 1:nselect)
  {
    b <- dfitall$b[i]
    c <- dfitall$c[i]
    d <- dfitall$d[i]
    e <- dfitall$e[i]
    g <- dfitall$f[i] # f is renamed g
    y0 <- dcalc$y0[i]
    xext <- dcalc$xextrem[i]

    modeli <- dfitall$model[i]
    if(modeli == "linear") {
      ydosemax <- dcalc$ydosemax[i] <- flin(x=dosemax, b=b, d=d)
      dcalc$yp[i] <- y0 * ( 1 + xdiv100*sign(b))
      dcalc$BMDp[i] <- invlin(dcalc$yp[i], b, d)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(b)
      dcalc$BMDsd[i] <- invlin(dcalc$ysd[i], b, d)
    } else
    if(modeli == "exponential") {
      ydosemax <- dcalc$ydosemax[i] <- fExpo(x=dosemax, b=b, d=d, e=e)
      dcalc$yp[i] <- y0 * ( 1 + xdiv100*sign(e*b))
      dcalc$BMDp[i] <- invExpo(dcalc$yp[i], b, d, e)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(e*b)
      dcalc$BMDsd[i] <- invExpo(dcalc$ysd[i], b, d, e)
    } else
    if(modeli == "Hill") {
      ydosemax <- dcalc$ydosemax[i] <- fHill(x=dosemax, b=b, c=c, d=d, e=e)
      dcalc$yp[i] <- y0 * ( 1 + xdiv100*sign(c - d))
      dcalc$BMDp[i] <- invHill(dcalc$yp[i], b, c, d, e)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(c - d)
      dcalc$BMDsd[i] <- invHill(dcalc$ysd[i], b, c, d, e)
    } else
    if(modeli == "log-probit") {
      ydosemax <- dcalc$ydosemax[i] <- fLGauss5p(x=dosemax, b=b, c=c, d=d, e=e, f=0)
      dcalc$yp[i] <- y0 * ( 1 + xdiv100*sign(c - d))
      dcalc$BMDp[i] <- invLprobit(dcalc$yp[i], b, c, d, e)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(c - d)
      dcalc$BMDsd[i] <- invLprobit(dcalc$ysd[i], b, c, d, e)
    } else
    if(modeli == "Gauss-probit") {
      yext <- dcalc$yextrem[i] <- fGauss5p(xext, b=b, c=c, d=d, e=e, f=g) # g is renamed f
      ydosemax <- dcalc$ydosemax[i] <- fGauss5p(x=dosemax, b=b, c=c, d=d, e=e, f=g)
      
      deltap <- abs(y0) * xdiv100
      deltasd <- z * dfitall$SDres[i]
      
      resBMDp <- calcBMD(y0=y0, delta=deltap, xext=xext, yext=yext, 
                         dosemin = dosemin, dosemax=dosemax, ydosemax=ydosemax, 
                         func=fGauss5pBMR, func_xinlog=fGauss5pBMR_xinlog,
                         b=b, c=c, d=d, e=e, g=g, minBMD = minBMD, 
                         ratio2switchinlog = ratio2switchinlog)
      dcalc$yp[i] <- resBMDp$threshold
      dcalc$BMDp[i] <- resBMDp$BMD
      
      resBMDsd <- calcBMD(y0=y0, delta=deltasd, xext=xext, yext=yext, 
                          dosemin = dosemin, dosemax= dosemax, ydosemax = ydosemax, 
                          func = fGauss5pBMR, func_xinlog = fGauss5pBMR_xinlog,
                          b=b, c=c, d=d, e=e, g=g, minBMD = minBMD, 
                          ratio2switchinlog = ratio2switchinlog)
      dcalc$ysd[i] <- resBMDsd$threshold
      dcalc$BMDsd[i] <- resBMDsd$BMD
    } else
    if(modeli == "log-Gauss-probit") {
      yext <- dcalc$yextrem[i] <- fLGauss5p(xext, b=b, c=c, d=d, e=e, f=g) # g is renamed f
      ydosemax <- dcalc$ydosemax[i] <- fLGauss5p(x=dosemax, b=b, c=c, d=d, e=e, f=g)
      deltap <- abs(y0) * xdiv100
      deltasd <- z * dfitall$SDres[i]
      
      resBMDp <- calcBMD(y0 = y0, delta = deltap, xext = xext, yext = yext, 
                         dosemin = dosemin, dosemax = dosemax, ydosemax = ydosemax, 
                         func = fLGauss5pBMR, func_xinlog = fLGauss5pBMR_xinlog,
                         b=b, c=c, d=d, e=e, g=g, minBMD = minBMD, 
                         ratio2switchinlog = ratio2switchinlog)
      dcalc$yp[i] <- resBMDp$threshold
      dcalc$BMDp[i] <- resBMDp$BMD
      
      resBMDsd <- calcBMD(y0=y0, delta=deltasd, xext=xext, yext=yext, 
                          dosemin = dosemin, dosemax = dosemax, ydosemax = ydosemax, 
                          func = fLGauss5pBMR,  func_xinlog = fLGauss5pBMR_xinlog,
                          b=b, c=c, d=d, e=e, g=g, minBMD = minBMD, 
                          ratio2switchinlog = ratio2switchinlog)
      dcalc$ysd[i] <- resBMDsd$threshold
      dcalc$BMDsd[i] <- resBMDsd$BMD
    }
    dcalc$BMDsd[i] <- max(dcalc$BMDsd[i], minBMD)
    dcalc$BMDp[i] <- max(dcalc$BMDp[i], minBMD)
  }
  dcalc$BMDp[dcalc$BMDp > dosemax] <- NA
  dcalc$BMDsd[dcalc$BMDsd > dosemax] <- NA
  
  reslist <- list(res = as.data.frame(cbind(dfitall,
                            data.frame(BMD.zSD = dcalc$BMDsd, BMR.zSD = dcalc$ysd,
                                BMD.xfold = dcalc$BMDp, BMR.xfold = dcalc$yp))), 
                  z = z, x = x, minBMD = minBMD,
                  ratio2switchinlog = ratio2switchinlog, omicdata = f$omicdata) 
  
  return(structure(reslist, class = "bmdcalc"))
  
}


print.bmdcalc <- function(x, ...) 
{
  if (!inherits(x, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects.")
  
  # count of cases where BMD cannot be reached 
  # being outside the range of response values defined by the model
  nNaN.BMD.zSD <- sum(is.nan(x$res$BMD.zSD))
  nNaN.BMD.xfold <- sum(is.nan(x$res$BMD.xfold))
  if ((nNaN.BMD.zSD > 0) |  (nNaN.BMD.xfold > 0))
    cat(strwrap(paste0(nNaN.BMD.xfold, " BMD-xfold values and ", nNaN.BMD.zSD,
                       " BMD-zSD values are not defined (coded NaN as the BMR stands outside 
                       the range of response values defined by the model).")), fill = TRUE)
                
  # count of cases where BMD is not yet reached at the highest tested dose
  nNA.BMD.zSD <- sum(is.na(x$res$BMD.zSD) & !is.nan(x$res$BMD.zSD))
  nNA.BMD.xfold <- sum(is.na(x$res$BMD.xfold) & !is.nan(x$res$BMD.xfold))
  if ((nNA.BMD.zSD > 0) |  (nNA.BMD.xfold > 0))
    cat(strwrap(paste0(nNA.BMD.xfold, " BMD-xfold values and ", nNA.BMD.zSD,
                       " BMD-zSD values could not be calculated (coded NA as the BMR stands within the 
                       range of response values defined by the model but outside the range of tested doses).")), fill = TRUE)
  
  if ((nNA.BMD.zSD == 0) &  (nNA.BMD.xfold == 0) & (nNaN.BMD.zSD == 0) &  (nNaN.BMD.xfold == 0))
    cat(strwrap("BMD-xfold and BMD-SD values could be calculated on all the curves 
                (the BMR always stands within the range of response values defined by the model 
                and within the range of tested doses)."), fill = TRUE)
}

plot.bmdcalc <- function(x, BMDtype = c("zSD", "xfold"), 
                         plottype = c("ecdf", "hist", "density"), 
                         by = c("none", "trend", "model", "typology"), 
                         hist.bins = 30, ...) 
{
  if (!inherits(x, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects.")
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  plottype <- match.arg(plottype, c("ecdf", "hist", "density"))  
  by <- match.arg(by, c("none", "trend", "model", "typology"))  
  
  if (BMDtype == "zSD")
  {
    dwithNANaN <- data.frame(BMD = x$res$BMD.zSD, 
      trend = x$res$trend, model = x$res$model, typology = x$res$typology)
  } else
  {
    dwithNANaN <- data.frame(BMD = x$res$BMD.xfold, 
      trend = x$res$trend, model = x$res$model, typology = x$res$typology)
  }
  
  # Remove NA and NaN values if needed
  d <- dwithNANaN[!is.na(dwithNANaN$BMD) & !is.nan(dwithNANaN$BMD), ]
  nremoved <- nrow(dwithNANaN) - nrow(d)
  if (nremoved > 0)
    warning(strwrap(prefix = "\n", initial = "\n", paste0(nremoved,
      " BMD coded NA or NaN were removed before plotting.")))
  
  if (plottype == "hist") 
  {
    g <- ggplot(data = d, mapping = aes_(x = quote(BMD))) +
        geom_histogram(bins = hist.bins) 
  } else
  if (plottype == "density") 
  {
    g <- ggplot(data = d, mapping = aes_(x = quote(BMD))) + geom_density(fill = I("grey"))
  } else
  if (plottype == "ecdf") 
  {
    g <- ggplot(data = d, mapping = aes_(x = quote(BMD))) +
            stat_ecdf(geom = "step") + ylab("ECDF")
  }
  
  if (by == "trend")  g <- g + facet_wrap(~ trend) else
    if (by == "model") g <- g + facet_wrap(~ model) else
      if (by == "typology") g <- g + facet_wrap(~ typology)
    
  return(g)
}

