## Perform non parametric bootstrap on a selection of items in order to give
## a confidence interval for the BMD values
bmdboot <- function(r, items = r$res$id, niter = 1000, 
                    conf.level = 0.95, 
                    tol = 0.5, progressbar = TRUE, 
                    parallel = c("no", "snow", "multicore"), ncpus)
{
  # Checks
  if (!inherits(r, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects, created with the function bmdcalc")
  
  # bootmethod <- match.arg(bootmethod, c("nonparam", "param"))
  bootmethod <- "nonparam"
  
  if (niter < 1000)
    warning("A small number of iterations (less than 1000) may not be sufficient
            to ensure a good quality of bootstrap confidence intervals.")
    
    parallel <- match.arg(parallel, c("no", "snow", "multicore"))
  if (parallel == "multicore" & .Platform$OS.type == "windows")
  {
    parallel <- "snow"
    warning("As the multicore option is not supported on Windows it was replaced by snow")
  }
  if ((parallel == "snow" | parallel == "multicore") & missing(ncpus)) 
    stop("You have to specify the number of available processors to parallelize 
         the bootstrap")
  if (parallel != "no") progressbar <- FALSE
  
  if (progressbar)
    cat("The bootstrap may be long if the number of items and the number of bootstrap iterations is high.\n")

  i.items <- match(items, r$res$id)
  nitems <- length(items)
  
  if (!is.numeric(tol) | (tol > 1) | (tol < 0))
    stop("Wrong argument 'tol'. If not omitted it must be a number between 0 and 1 (the proportion
         of failure of model fit among bootstrap samples).")

  if (!is.numeric(conf.level) | (conf.level >= 1) | (conf.level <= 0))
    stop("Wrong argument 'conf.level'. If not omitted it must be a number between 0 and 1 
         (the confidence level of the bootstrap confidence intervals).")
  prob.lower <- (1 - conf.level) / 2
  prob.upper <- conf.level + prob.lower
  
  z <- r$z
  x <- r$x
  xdiv100 <- r$x/100
    
  dose <- r$omicdata$dose
  dosemax <- max(dose)
  
  # progress bar
  if (progressbar)
    pb <- txtProgressBar(min = 0, max = nitems, style = 3)

  ##### Bootstrap for one item ####################
  bootoneitem <- function(i)
  {
    resitem <- r$res[i.items[i], ]
    # parameter estimates as a named list for starting values
    estimpar <- unlist(resitem[c("b","c","d","e","f")])
    lestimpar <- as.list(estimpar[!is.na(estimpar)])
    
    # residual standard error
    SDresi <- unlist(resitem["SDres"])
    
    modeli <- as.character(unlist(resitem["model"]))
    nbpari <- unlist(resitem["nbpar"])

    # dataset
    datai <- r$omicdata$data[resitem$irow, ]
    dset <- data.frame(signal = datai, dose = dose)
    ndata <- nrow(dset)

    ############## Model expo ###########
    if (modeli == "exponential")
    {
      b1 <- lestimpar$b
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      fitted1 <- fExpo(x = dose, d = d1, b = b1, e = e1)
      resid1 <- datai - fitted1
      
      dsetboot <- dset
      fboot <- function(i)
      {
        if(bootmethod == "param")
        {
          dsetboot[, 1] <- fitted1 + rnorm(ndata, mean = 0, sd = SDresi)
        } else
        {
          dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        }
        # fit
        if (e1 < 0)
        {
          nlsboot <- suppressWarnings(try(nls(formula = formExp3p, data = dsetboot, start = lestimpar,
                             lower = c(-Inf, -Inf, -Inf), 
                             upper = c(Inf, Inf, 0), algorithm = "port"), 
                         silent = TRUE))
        } else
        {
          nlsboot <- suppressWarnings(try(nls(formula = formExp3p, data = dsetboot, start = lestimpar,
                             lower = c(-Inf, -Inf, 0), algorithm = "port"), 
                         silent = TRUE))
        }
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- sqrt(sum(residuals(nlsboot)^2)/(ndata - nbpari))
          bboot <- coef(nlsboot)["b"]
          dboot <- coef(nlsboot)["d"]
          eboot <- coef(nlsboot)["e"]
          y0boot <- dboot
          ydosemaxboot <- fExpo(x = dosemax, b = bboot, d = dboot, e = eboot)
          ypboot <- y0boot * ( 1 + xdiv100*sign(eboot * bboot))
          BMDpboot <- invExpo(ypboot, b= bboot, d = dboot, e = eboot)
          ysdboot <- y0boot + z*SDresboot * sign(eboot * bboot)
          BMDsdboot <- invExpo(ysdboot, b= bboot, d = dboot, e = eboot)
          return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
        }
      } # end fboot
    } else
    ############ END model expo ###################
    
    ############## Model Hill ###########
    if (modeli == "Hill")
    {
      b1 <- lestimpar$b
      c1 <- lestimpar$c
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      fitted1 <- fHill(x = dose, b = b1, c = c1, d = d1,  e = e1)
      resid1 <- datai - fitted1
      
      dsetboot <- dset
      fboot <- function(i)
      {
        if(bootmethod == "param")
        {
          dsetboot[, 1] <- fitted1 + rnorm(ndata, mean = 0, sd = SDresi)
        } else
        {
          dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        }
        # fit
        nlsboot <- suppressWarnings(try(nls(formula = formHill, data = dsetboot, start = lestimpar,
                                             lower = c(0, -Inf, -Inf, 0), algorithm = "port"), 
                                          silent = TRUE))
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- sqrt(sum(residuals(nlsboot)^2)/(ndata - nbpari))
          bboot <- coef(nlsboot)["b"]
          cboot <- coef(nlsboot)["c"]
          dboot <- coef(nlsboot)["d"]
          eboot <- coef(nlsboot)["e"]
          y0boot <- dboot
          ydosemaxboot <- fHill(x = dosemax, b = bboot, c = cboot, d = dboot, e = eboot)
          ypboot <- y0boot * ( 1 + xdiv100*sign(cboot * dboot))
          BMDpboot <- invHill(ypboot, b= bboot, c = cboot, d = dboot, e = eboot)
          ysdboot <- y0boot + z*SDresboot * sign(cboot * dboot)
          BMDsdboot <- invHill(ysdboot, b= bboot, c = cboot, d = dboot, e = eboot)

          return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
        }
      } # end fboot
    } else
    ############ END model Hill ###################

    ############## Model log-probit ###########
    if (modeli == "log-probit")
    {
      b1 <- lestimpar$b
      c1 <- lestimpar$c
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      fitted1 <- fLGauss5p(x = dose, b = b1, c = c1, d = d1, e = e1, f = 0)
      resid1 <- datai - fitted1
      
      dsetboot <- dset
      fboot <- function(i)
      {
        if(bootmethod == "param")
        {
          dsetboot[, 1] <- fitted1 + rnorm(ndata, mean = 0, sd = SDresi)
        } else
        {
          dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        }
        # fit
        nlsboot <- suppressWarnings(try(nls(formula = formLprobit, data = dsetboot, start = lestimpar,
                                            lower = c(0, -Inf, -Inf, 0), algorithm = "port"), 
                                        silent = TRUE))
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- sqrt(sum(residuals(nlsboot)^2)/(ndata - nbpari))
          bboot <- coef(nlsboot)["b"]
          cboot <- coef(nlsboot)["c"]
          dboot <- coef(nlsboot)["d"]
          eboot <- coef(nlsboot)["e"]
          y0boot <- dboot
          ydosemaxboot <- fLGauss5p(x = dosemax, b = bboot, c = cboot, d = dboot, e = eboot, f = 0)
          ypboot <- y0boot * ( 1 + xdiv100*sign(cboot * dboot))
          BMDpboot <- invLprobit(ypboot, b= bboot, c = cboot, d = dboot, e = eboot)
          ysdboot <- y0boot + z*SDresboot * sign(cboot * dboot)
          BMDsdboot <- invLprobit(ysdboot, b= bboot, c = cboot, d = dboot, e = eboot)
          
          return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
        }
      } # end fboot
    } else
    ############ END model log-probit ###################
    
    
    ############## Linear model ###########
    if (modeli == "linear")
    {
      b1 <- lestimpar$b
      d1 <- lestimpar$d
      fitted1 <- flin(x = dose, b = b1, d = d1)
      resid1 <- datai - fitted1
      
      dsetboot <- dset
      fboot <- function(i)
      {
        if(bootmethod == "param")
        {
          dsetboot[, 1] <- fitted1 + rnorm(ndata, mean = 0, sd = SDresi)
        } else
        {
          dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        }
        # fit
        linboot <- lm(signal ~ dose, data = dsetboot)
        SDresboot <- sqrt(sum(residuals(linboot)^2)/(ndata - nbpari))
        bboot <- coef(linboot)[2]
        dboot <- coef(linboot)[1]
        y0boot <- dboot
        ydosemaxboot <- flin(x = dosemax, b = bboot, d = dboot)
        ypboot <- y0boot * ( 1 + xdiv100*sign(bboot))
        BMDpboot <- invlin(ypboot, b= bboot, d = dboot)
        ysdboot <- y0boot + z*SDresboot * sign(bboot)
        BMDsdboot <- invlin(ysdboot, b= bboot, d = dboot)
          
        return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
      } # end fboot
    } else
      ############ END linear model ###################
    
    ##### Model Gauss-probit #######
    if (modeli == "Gauss-probit")
    {
      b1 <- lestimpar$b
      c1 <- lestimpar$c
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      f1 <- lestimpar$f
      fitted1 <- fGauss5p(x = dose, c = c1, d = d1, b = b1, e = e1, f = f1)
      resid1 <- datai - fitted1
       
      dsetboot <- dset
      fboot <- function(i)
      {
        if(bootmethod == "param")
        {
          dsetboot[, 1] <- fitted1 + rnorm(ndata, mean = 0, sd = SDresi)
        } else
        {
          dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        }
        # fit
        if (nbpari == 5)
        {
          nlsboot <- suppressWarnings(try(nls(formula = formGauss5p, data = dsetboot, start = lestimpar,
                                              lower = c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), 
                                          silent = TRUE))
        }
        else
        {
          lestimpar.4p <- list(b = lestimpar$b, d = lestimpar$d, e = lestimpar$e, f = lestimpar$f)
          nlsboot <- suppressWarnings(try(nls(formula = formGauss4p, data = dsetboot, start = lestimpar.4p,
                                              lower = c(0, -Inf, 0, -Inf), algorithm = "port"), 
                                          silent = TRUE))
        }
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- sqrt(sum(residuals(nlsboot)^2)/(ndata - nbpari))
          bboot <- coef(nlsboot)["b"]
          if (nbpari == 5) cboot <- coef(nlsboot)["c"] else cboot <- coef(nlsboot)["d"]
          dboot <- coef(nlsboot)["d"]
          eboot <- coef(nlsboot)["e"]
          fboot <- coef(nlsboot)["f"]
          y0boot <- fGauss5p(x = 0, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
          ydosemaxboot <- fGauss5p(x = dosemax, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
            
          xextrboot <- eboot + (cboot - dboot)*bboot/(fboot*sqrt(2*pi)) 
          yextrboot <- fGauss5p(x = xextrboot, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)

          deltapboot <- y0boot * xdiv100
          deltasdboot <- z * SDresboot
            
          resBMDp <- calcBMD(y0=y0boot, delta=deltapboot, xext=xextrboot, yext=yextrboot, 
                               dosemax = dosemax, ydosemax = ydosemaxboot, func = fGauss5pBMR, 
                               b = bboot, c = cboot, d = dboot, e = eboot, g = fboot)
          BMDpboot <- resBMDp$BMD
            
          resBMDsd <- calcBMD(y0=y0boot, delta=deltasdboot, xext=xextrboot, yext=yextrboot, 
                                dosemax = dosemax, ydosemax = ydosemaxboot, func = fGauss5pBMR, 
                                b = bboot, c = cboot, d = dboot, e = eboot, g = fboot)
          BMDsdboot <- resBMDsd$BMD
          return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
        }
      } # end fboot
    } else
    ############ END model Gauss probit ###################
    
    ############ Model log Gauss-probit #################
    if (modeli == "log-Gauss-probit")
    {
      b1 <- lestimpar$b
      c1 <- lestimpar$c
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      f1 <- lestimpar$f
      fitted1 <- fLGauss5p(x = dose, c = c1, d = d1, b = b1, e = e1, f = f1)
      resid1 <- datai - fitted1

      dsetboot <- dset
      fboot <- function(i)
      {
        if(bootmethod == "param")
        {
          dsetboot[, 1] <- fitted1 + rnorm(ndata, mean = 0, sd = SDresi)
        } else
        {
          dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        }
        # fit
        if (nbpari == 5)
        {
          nlsboot <- suppressWarnings(try(nls(formula = formLGauss5p, data = dsetboot, start = lestimpar,
                                              lower =  c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), 
                                          silent = TRUE))
        }
        else
        {
          lestimpar.4p <- list(b = lestimpar$b, d = lestimpar$d, e = lestimpar$e, f = lestimpar$f)
          nlsboot <- suppressWarnings(try(nls(formula = formLGauss4p, data = dsetboot, start = lestimpar.4p,
                                              lower = c(0, -Inf, 0, -Inf), algorithm = "port"), 
                                          silent = TRUE))
        }
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- sqrt(sum(residuals(nlsboot)^2)/(ndata - nbpari))
          bboot <- coef(nlsboot)["b"]
          if (nbpari == 5) cboot <- coef(nlsboot)["c"] else cboot <- coef(nlsboot)["d"]
          dboot <- coef(nlsboot)["d"]
          eboot <- coef(nlsboot)["e"]
          fboot <- coef(nlsboot)["f"]
          y0boot <- dboot
          ydosemaxboot <- fLGauss5p(x = dosemax, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
          
          xextrboot <-  exp(log(eboot) + (cboot - dboot)*bboot/(fboot*sqrt(2*pi))) 
          yextrboot <-  fLGauss5p(x = xextrboot, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot) 
          
          deltapboot <- y0boot * xdiv100
          deltasdboot <- z * SDresboot
          
          resBMDp <- calcBMD(y0=y0boot, delta=deltapboot, xext=xextrboot, yext=yextrboot, 
                             dosemax = dosemax, ydosemax = ydosemaxboot, func = fLGauss5pBMR, 
                             b = bboot, c = cboot, d = dboot, e = eboot, g = fboot)
          BMDpboot <- resBMDp$BMD
          
          resBMDsd <- calcBMD(y0=y0boot, delta=deltasdboot, xext=xextrboot, yext=yextrboot, 
                              dosemax = dosemax, ydosemax = ydosemaxboot, func = fLGauss5pBMR, 
                              b = bboot, c = cboot, d = dboot, e = eboot, g = fboot)
          BMDsdboot <- resBMDsd$BMD
          return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
        }
      } # end fboot
    }
    ############ END model log Gauss probit ###################
    
    
    ########### Bootstrap iterations on item i ################
    l1 <- lapply(1:niter, fboot)
    nboot.successful <- niter - sum(sapply(l1, is.null))
    if(nboot.successful < niter * tol) 
    {
      # warning(paste("Procedure aborted: the fit only converged for", nboot.successful, 
      #               "iterations during bootstrapping for item ", items[i]))
      return(c(NA, NA, NA, NA, nboot.successful))
    } else
    {
      BMDpbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$BMDp)
      BMDsdbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$BMDsd)

      BMDpbooti[is.na(BMDpbooti) | BMDpbooti > dosemax] <- Inf
      BMDsdbooti[is.na(BMDsdbooti) | BMDsdbooti > dosemax] <- Inf
      BMDp.CI <- quantile(BMDpbooti, probs = c(prob.lower, prob.upper))
      BMDplower <- BMDp.CI[1]
      BMDpupper <- BMDp.CI[2]
      
      BMDsd.CI <- quantile(BMDsdbooti, probs = c(prob.lower, prob.upper))
      BMDsdlower <- BMDsd.CI[1]
      BMDsdupper <- BMDsd.CI[2]
      
      
      if (progressbar)
      {
        setTxtProgressBar(pb, i)
      }
      return(c(BMDsdlower, BMDsdupper, BMDplower, BMDpupper, nboot.successful))
    }
   
  } 
  ##### END bootstrap for one item ####################
  
  # Loop on items
  # parallel or sequential computation
  if (parallel != "no") 
  {
    if (parallel == "snow") type <- "PSOCK"
    else if (parallel == "multicore") type <- "FORK"
    clus <- parallel::makeCluster(ncpus, type = type)
    res <- parallel::parSapply(clus, 1:nitems, bootoneitem)
    parallel::stopCluster(clus)
  }
  else
  {
    res <- sapply(1:nitems, bootoneitem)
  }
  
  # close progress bar
  if (progressbar) close(pb)
  
  dres <- as.data.frame(t(res))
  colnames(dres) <- c("BMD.zSD.lower", "BMD.zSD.upper", "BMD.xfold.lower", "BMD.xfold.upper", "nboot.successful")
  
  dres <- cbind(r$res[i.items, ], dres)
  reslist <- list(res = dres, 
                  z = z, x = x, tol = tol, niter = niter) 
  return(structure(reslist, class = "bmdboot"))
}
############################# END of bmdboot

print.bmdboot <- function(x, ...) 
{
  if (!inherits(x, "bmdboot"))
    stop("Use only with 'bmdboot' objects")
  
  ntot <- nrow(x$res)
  nNA.BMDboot <- sum(x$res$nboot.successful < x$tol * x$niter)
  if (nNA.BMDboot == 0)
  {
    cat("Bootstrap confidence interval computation was successful on ", nNA.BMDboot,
        "items among", ntot, ".\n")
  } else
  {
    cat("Bootstrap confidence interval computation failed on", nNA.BMDboot,
        "items among", ntot, 
        "due to lack of convergence of the model fit for a fraction of the bootstrapped samples greater than",
        x$tol, ".\n")
  }
  
  nInf.BMD.zSD.upper <- sum(is.infinite(x$res$BMD.zSD.upper))
  nInf.BMD.xfold.upper <- sum(is.infinite(x$res$BMD.xfold.upper))
  cat("For", nInf.BMD.zSD.upper, "BMD.zSD values and", nInf.BMD.xfold.upper,
      "BMD.xfold values among", ntot, 
      "at least one bound of the 95 percent confidence interval could not be
      computed due to some bootstrapped BMD values not reachable due to model asymptotes 
      or reached outside the range of tested doses (bounds coded Inf)).\n")
  }

plot.bmdboot <- function(x, BMDtype = c("zSD", "xfold"), remove.infinite = TRUE,
                         by = c("none", "trend", "model", "typology"), CI.col = "blue",  ...) 
{
  if (!inherits(x, "bmdboot"))
    stop("Use only with 'bmdboot' objects")
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  by <- match.arg(by, c("none", "trend", "model", "typology"))  
  
  res <- x$res

  if (BMDtype == "zSD")
  {
    dwithNA <- data.frame(BMD = res$BMD.zSD, BMD.lower = res$BMD.zSD.lower,
                          BMD.upper = res$BMD.zSD.upper)
  } else
  {
    dwithNA <- data.frame(BMD = res$BMD.xfold, BMD.lower = res$BMD.xfold.lower,
                          BMD.upper = res$BMD.xfold.upper)
  }
  nrow(dwithNA)
  
  if (by == "trend") dwithNA$by <- res$trend else
    if (by == "model") dwithNA$by <- res$model else
      if (by == "typology") dwithNA$by <- res$typology 
  
  # Remove NA values if needed
  d <- dwithNA[!is.na(dwithNA$BMD) & !is.na(dwithNA$BMD.lower) & !is.na(dwithNA$BMD.upper), ]
  nrow(d)
  
  # remove BMD with infinite lower bounds
  d <- d[is.finite(d$BMD.lower), ]
  nrow(d)
  
  # remove BMD with infinite upper bounds
  if (remove.infinite)
  {
    # remove BMD with infinite upper values
    d <- d[is.finite(d$BMD.upper), ]
    define.xlim <- FALSE
    nrow(d)
  } else
  {
    ind.infinite <- !is.finite(d$BMD.upper)
    if (any(ind.infinite))
    {
      allBMDval <- c(d$BMD, d$BMD.upper, d$BMD.lower)
      BMDmax <- max(allBMDval[is.finite(allBMDval)])
      BMDlimmax <- BMDmax * 1.5
      d$BMD.upper[ind.infinite] <- BMDlimmax # I did not manage to fix it at a higher value 
                                            # (not plotted by geom_errorbarh)
      define.xlim <- TRUE
    } else
    {
      define.xlim <- FALSE
    }
  }
  
  nplotted <- nrow(d)
  nremoved <- nrow(dwithNA) - nplotted
  
  if (nremoved > 0)
  {
    if (remove.infinite)
    {
      warning(nremoved,
    " BMD values for which lower and upper bounds were coded NA or with lower or upper infinite bounds were removed before plotting")
    } else
    {
      warning(nremoved,
    " BMD values for which lower and upper bounds were coded NA or with lower and upper infinite bounds were removed before plotting")
    }
  }
  
  if (by != "none") 
  {
    uniqueby <- unique(d$by)
    n.uniqueby <- length(uniqueby)
    d$ECDF <- rep(0, nplotted) # initialization
    for (i in 1:n.uniqueby)
    {
      indi <- which(d$by == uniqueby[i])
      ntoti <- length(indi)
      d$ECDF[indi] <- (rank(d$BMD[indi], ties.method = "first") - 0.5) / ntoti
    }
    if (!define.xlim)
    {
      g <- ggplot(data = d, mapping = aes_(x = quote(BMD), y = quote(ECDF))) + 
        facet_wrap(~ by) + 
        geom_errorbarh(aes_(xmin = quote(BMD.lower), xmax = quote(BMD.upper)), col = CI.col, 
                       alpha = 0.5, height = 0) + geom_point() 
    } else
    {
      g <- ggplot(data = d, mapping = aes_(x = quote(BMD), y = quote(ECDF))) + 
        facet_wrap(~ by) + 
        geom_errorbarh(aes_(xmin = quote(BMD.lower), xmax = quote(BMD.upper)), col = CI.col, 
              alpha = 0.5, height = 0) + geom_point() + xlim(0, BMDlimmax)
      
    }
  }  else
  { # global plot of BMDs
    d$ECDF <- (rank(d$BMD, ties.method = "first") - 0.5) / nplotted
    if (!define.xlim)
    {
      g <- ggplot(data = d, mapping = aes_(x = quote(BMD), y = quote(ECDF))) + 
        geom_errorbarh(aes_(xmin = quote(BMD.lower), xmax = quote(BMD.upper)), col = CI.col, 
                       alpha = 0.5,  height = 0) + geom_point() 
    } else
    {
      g <- ggplot(data = d, mapping = aes_(x = quote(BMD), y = quote(ECDF))) + 
        geom_errorbarh(aes_(xmin = quote(BMD.lower), xmax = quote(BMD.upper)), col = CI.col, 
                       alpha = 0.5,  height = 0) + geom_point() + xlim(0, BMDlimmax)
    }
  } 
  return(g)
}

