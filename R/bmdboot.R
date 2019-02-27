## Perform bootstrap on a selection of items in order to give
## a 95% confidence interval for the BMD values
bmdboot <- function(r, items = r$res$id, niter = 250, tol = 0.5, progressbar = TRUE, 
                    parallel = c("no", "snow", "multicore"), ncpus)
#  tol = percentage of bootstrap failure admitted
{
  # Checks
  if (!inherits(r, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects, created with the function bmdcalc")
  
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
    cat("The bootstrap may be long if the number of items is high.\n")

  i.items <- match(items, r$res$id)
  nitems <- length(items)
  
  if (!is.numeric(tol) | (tol > 1) | (tol < 0))
    stop("Wrong argument 'tol'. If not omitted it must be a number between 0 and 1 (the proportion
         of failure of model fit among bootstrap samples).")
  
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
    
    modeli <- as.character(unlist(resitem["model"]))
    nbpari <- unlist(resitem["nbpar"])

    # dataset
    datai <- r$omicdata$data[resitem$irow, ]
    dset <- data.frame(signal = datai, dose = dose)
    ndata <- nrow(dset)
    # plot(dset$dose, dset$signal)

    ############## Model expo ###########
    if (modeli == "exponential")
    {
      b1 <- lestimpar$b
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      fitted1 <- fExpo(x = dose, d = d1, b = b1, e = e1)
      resid1 <- datai - fitted1
      
      dsetboot <- dset
      # plot(dose, fitted1, type = "l")
      # points(dose, datai)
      fboot <- function(i)
      {
        dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        # plot(dsetboot[,2], dsetboot[,1])
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
          # return(list(coef = coef(nlsboot), SDres = SDresboot, BMDp = BMDpboot, BMDsd = BMDsdboot))
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
      # plot(dose, fitted1, type = "l")
      # points(dose, datai)
      fboot <- function(i)
      {
        dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        # plot(dsetboot[,2], dsetboot[,1])
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

          # return(list(coef = coef(nlsboot), SDres = SDresboot, BMDp = BMDpboot, BMDsd = BMDsdboot))
          return(list(BMDp = BMDpboot, BMDsd = BMDsdboot))
        }
      } # end fboot
    } else
    ############ END model Hill ###################

    ############## Linear model ###########
    if (modeli == "linear")
    {
      b1 <- lestimpar$b
      d1 <- lestimpar$d
      fitted1 <- flin(x = dose, b = b1, d = d1)
      resid1 <- datai - fitted1
      
      dsetboot <- dset
      # plot(dose, fitted1, type = "l")
      # points(dose, datai)
      fboot <- function(i)
      {
        dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        # plot(dsetboot[,2], dsetboot[,1])
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
          
          # return(list(coef = coef(nlsboot), SDres = SDresboot, BMDp = BMDpboot, BMDsd = BMDsdboot))
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
      # lines(dose, fitted1)
        
      dsetboot <- dset
      fboot <- function(i)
      {
        dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
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
          # return(list(coef = coef(nlsboot), SDres = SDresboot, BMDp = BMDpboot, BMDsd = BMDsdboot))
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
      # lines(dose, fitted1)
      
      dsetboot <- dset
      fboot <- function(i)
      {
        dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
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
          # return(list(coef = coef(nlsboot), SDres = SDresboot, BMDp = BMDpboot, BMDsd = BMDsdboot))
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
      # tabbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$coef)
      # SDresbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$SDres)
      BMDpbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$BMDp)
      BMDsdbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$BMDsd)

      BMDpbooti[is.na(BMDpbooti) | BMDpbooti > dosemax] <- Inf
      BMDsdbooti[is.na(BMDsdbooti) | BMDsdbooti > dosemax] <- Inf
      BMDp.CI95 <- quantile(BMDpbooti, probs = c(0.025, 0.975))
      BMDplower <- BMDp.CI95[1]
      BMDpupper <- BMDp.CI95[2]
      
      BMDsd.CI95 <- quantile(BMDsdbooti, probs = c(0.025, 0.975))
      BMDsdlower <- BMDsd.CI95[1]
      BMDsdupper <- BMDsd.CI95[2]
      
      
      if (progressbar)
      {
        setTxtProgressBar(pb, i)
      }
      return(c(BMDsdlower, BMDsdupper, BMDplower, BMDpupper, nboot.successful))
    }
   
    # pairs(as.data.frame(t(tabbooti)))
    # boxplot(SDresbooti)
    # boxplot(BMDpbooti)
    # boxplot(BMDsdbooti)
  } 
  ##### END bootstrap for one item ####################
  
  # trial
  # bootoneitem(1)
  # bootoneitem(2)
  
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
        "items among", ntot, "due to lack of convergence of the model fit for
        a fraction of the bootstrapped samples greater than", x$tol, ".\n")
  }
  
  nInf.BMD.zSD.upper <- sum(is.infinite(x$res$BMD.zSD.upper))
  nInf.BMD.xfold.upper <- sum(is.infinite(x$res$BMD.xfold.upper))
  cat("For", nInf.BMD.zSD.upper, "BMD.zSD values and", nInf.BMD.xfold.upper,
        "BMD.xfold values among", ntot, 
        "at least one bound of the 95 percent confidence interval could not be
          computed due to some bootstrapped BMD values not reachable due to model asymptotes 
          or reached outside the range of tested doses (bounds coded Inf)).\n")
 }

plot.bmdboot <- function(x, BMDtype = c("zSD", "xfold"), 
                         plottype = c("ecdf", "univariate"), bytypology = FALSE, ...) 
{
  if (!inherits(x, "bmdboot"))
    stop("Use only with 'bmdboot' objects")
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  plottype <- match.arg(plottype, c("ecdf", "univariate"))  
  
  # que faire des NA et NaN (enlever, les représenter en données censurées ?)
  
  if (BMDtype == "zSD")
  {
    dwithNA <- data.frame(BMD = x$res$BMD.zSD, BMD.lower = x$res$BMD.zSD.lower,
                          BMD.upper = x$res$BMD.zSD.upper, typology = x$res$typology)
  } else
  {
    dwithNA <- data.frame(BMD = x$res$BMD.xfold, BMD.lower = x$res$BMD.xfold.lower,
                          BMD.upper = x$res$BMD.xfold.upper,typology = x$res$typology)
  }
  
  # Remove NA values if needed
  d <- dwithNA[!is.na(dwithNA$BMD) & !is.na(dwithNA$BMD.lower) & !is.na(dwithNA$BMD.upper), ]
  n.nonNA <- nrow(d)
  allBMDval <- c(d$BMD, d$BMD.upper, d$BMD.lower)
  ymax <- max(allBMDval[is.finite(allBMDval)])
  ylimmax <- ymax * 1.2
  d$BMD.upper[!is.finite(d$BMD.upper)] <- ylimmax
  
  nremoved <- nrow(dwithNA) - nrow(d)
  if (nremoved > 0)
    warning(nremoved," BMD values for which lower and upper bounds were coded NA were removed before plotting")
  if (plottype == "univariate")
  {
    d$Index <- 1:n.nonNA
  }
  else
  {
    BMDorder <- order(d$BMD)
    d <- d[BMDorder,]
    d$ECDF <- ((1:n.nonNA) - 0.5)/n.nonNA
  }
  
  if (bytypology) 
  {
    if (plottype == "ecdf") 
    {
      g <- ggplot(data = d, mapping = aes(x = BMD, y = ECDF)) + 
        facet_wrap(~ typology) + 
        geom_errorbarh(aes(xmin = BMD.lower, xmax = BMD.upper), col = "blue", 
                      alpha = 0.5, height = 0) +
        geom_point() + xlim(0, ylimmax)
    } else
    {
      g <- ggplot(data = d, mapping = aes(x= Index, y = BMD)) + 
            facet_wrap(~ typology) + 
            geom_errorbar(aes(ymin = BMD.lower, ymax = BMD.upper), col = "blue", 
                          alpha = 0.5, width = 0) +
            geom_point() + ylim(0, ylimmax)
    }
  }  else
  { # global plot of BMDs
    if (plottype == "ecdf") 
    {
      g <- ggplot(data = d, mapping = aes(x = BMD, y = ECDF)) + 
        geom_errorbarh(aes(xmin = BMD.lower, xmax = BMD.upper), col = "blue", 
                      alpha = 0.5,  height = 0) +
        geom_point() + xlim(0, ylimmax)
    } else
    {
      g <- ggplot(data = d, mapping = aes(x= Index, y = BMD)) + 
        geom_errorbar(aes(ymin = BMD.lower, ymax = BMD.upper), col = "blue", 
                      alpha = 0.5, width = 0) +
        geom_point() + ylim(0, ylimmax)
    }
  } 
  return(g)
}

# plot.bootres <- function(x, ymax = 10)
#   # to greatly improve and not fix ymax at 7 but at an automatic value from the design
# {
#   indexFDR0p001 <- min(which(x$adjpvalue > 0.001))
#   modelu <- unique(x$model)
#   colmodel <- 2:6
#   
#   par(mfrow = c(2, 1), mar = c(3,4,0.1,0.1))
#   BMD.zSD.lower <- x$BMD.zSD.lower
#   BMD.zSD.upper <- x$BMD.zSD.upper
#   BMD.zSD.upper[is.infinite(BMD.zSD.upper)] <- ymax*2
#   BMD.zSD <- x$BMD.zSD
#   plot(BMD.zSD, ylim = c(0, ymax), ylab = "BMD.zSD",
#        col = colmodel[match(x$model, modelu)], pch = 16)
#   nitems <- nrow(x)
#   segments(x0 = 1:nitems, y0 = BMD.zSD.lower, x1 = 1:nitems, y1 = BMD.zSD.upper, 
#            col = colmodel[match(x$model, modelu)])
#   abline(v = indexFDR0p001, lwd = 2)
#   legend("topleft", legend = modelu, pch = 16, col = colmodel, bty = "n", cex = 0.75)
#   
#   BMD.xfold.lower <- x$BMD.xfold.lower
#   BMD.xfold.upper <- x$BMD.xfold.upper
#   BMD.xfold.upper[is.infinite(BMD.xfold.upper)] <- ymax*2
#   BMD.xfold <- x$BMD.xfold
#   plot(BMD.xfold, ylim = c(0, ymax), ylab = "BMD.xfold",
#        col = colmodel[match(x$model, modelu)], pch = 16)
#   nitems <- nrow(x)
#   segments(x0 = 1:nitems, y0 = BMD.xfold.lower, x1 = 1:nitems, y1 = BMD.xfold.upper,
#            col = colmodel[match(x$model, modelu)])
#   abline(v = indexFDR0p001, lwd = 2)
#   
# }
# ############## END of the plot function
# 
# plot.bootres.gg <- function(x, ymax = 10, type = c("zSD", "xplot"))
#   # to greatly improve and not fix ymax at 7 but at an automatic value from the design
# {
#   indexFDR0p001 <- min(which(x$adjpvalue > 0.001))
# 
#   if (type == "zSD")
#   {
#     BMD.lower <- x$BMD.zSD.lower
#     BMD.upper <- x$BMD.zSD.upper
#     BMD <- x$BMD.zSD
#   }
#   else
#   {
#     BMD.lower <- x$BMD.xfold.lower
#     BMD.upper <- x$BMD.xfold.upper
#     BMD <- x$BMD.xfold
#     
#   }
#   BMD.upper[is.infinite(BMD.upper)] <- ymax
#   
#   d2plot <- data.frame(BMD = BMD, lower = BMD.lower, 
#                        upper = BMD.upper, index = 1:nrow(x),
#                        model = x$model)
#   ggplot(data = d2plot, mapping = aes(x= index, y = BMD)) + 
#     facet_wrap(~ model) + geom_vline(xintercept = indexFDR0p001, col = "red") +
#     geom_errorbar(aes(ymin = lower, ymax = upper), col = "grey",alpha = 0.5) + 
#     geom_point() +
#     theme_bw() + ylim(0, ymax)
# }
