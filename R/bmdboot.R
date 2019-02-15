## Perform bootstrap on a selection of items in order to give
## a 95% confidence interval for the BMD values
bmdboot <- function(r, items, niter = 99, progressbar = TRUE, 
                    parallel = c("no", "snow", "multicore"), ncpus)
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
  z <- r$z
  xdiv100 <- r$x/100
    
  dose <- r$omicdata$dose
  dosemax <- max(dose)
  formlist <- list(Exp = formExp3p, 
                   Gauss4p = formGauss4p,  
                   Gauss5p = formGauss5p, 
                   Hill = formHill,
                   Lin = as.formula(signal ~ d + b * dose),
                   LGauss4p = formLGauss4p,
                   LGauss5p = formLGauss5p, 
                   Lprobit = formLprobit)
  # formlist and modelnbpar MUST BE IN THE SAME ORDER
  modelnbpar <- c("exponential3", "Gauss-probit4", "Gauss-probit5", "Hill4", "linear2", 
                  "log-Gauss-probit4", "log-Gauss-probit5", "log-probit4")
  
  # progress bar
  if (progressbar)
    pb <- txtProgressBar(min = 0, max = nitems, style = 3)

  bootoneitem <- function(i)
  {
    resitem <- r$res[i.items[i], ]
    # parameter estimates as a named list for starting values
    estimpar <- unlist(resitem[c("b","c","d","e","f")])
    lestimpar <- as.list(estimpar[!is.na(estimpar)])
    
    # formula of the model (formi) and associated function (funci)
    modelnamei <- as.character(unlist(resitem["model"]))
    nbpari <- as.character(unlist(resitem["nbpar"]))
    modelnbpari <- paste(modelnamei, nbpari, sep = "")
    formi <- formlist[[match(modelnbpari, modelnbpar)]]
    
    # dataset
    datai <- r$omicdata$data[resitem$irow, ]
    dset <- data.frame(signal = datai, dose = dose)
    # plot(dset$dose, dset$signal)

    # Model expo
    if (modelnbpari == "exponential3")
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
          nlsboot <- suppressWarnings(try(nls(formula = formi, data = dsetboot, start = lestimpar,
                             lower = c(-Inf, -Inf, -Inf), 
                             upper = c(Inf, Inf, 0), algorithm = "port"), 
                         silent = TRUE))
        } else
        {
          nlsboot <- suppressWarnings(try(nls(formula = formi, data = dsetboot, start = lestimpar,
                             lower = c(-Inf, -Inf, 0), algorithm = "port"), 
                         silent = TRUE))
        }
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- summary(nlsboot)$sigma
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
    # Model Gauss-probit 5p
    if (modelnbpari == "Gauss-probit5")
    {
      b1 <- lestimpar$b
      c1 <- lestimpar$c
      d1 <- lestimpar$d
      e1 <- lestimpar$e
      f1 <- lestimpar$f
      #### Y a un bug ici, ca donne toujours la même valeur !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      fitted1 <- fGauss5p(x = dose, c = c1, d = d1, b = b1, e = e1, f = f1)
      resid1 <- datai - fitted1
      # lines(dose, fitted1)
        
      dsetboot <- dset
      fboot <- function(i)
      {
        dsetboot[, 1] <- fitted1 + sample(scale(resid1, scale = FALSE), replace = TRUE)
        # fit
        nlsboot <- suppressWarnings(try(nls(formula = formi, data = dsetboot, start = lestimpar,
                                              lower = c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), 
                           silent = TRUE))
        if(inherits(nlsboot, "nls"))
        {
          SDresboot <- summary(nlsboot)$sigma
          bboot <- coef(nlsboot)["b"]
          cboot <- coef(nlsboot)["c"]
          dboot <- coef(nlsboot)["d"]
          eboot <- coef(nlsboot)["e"]
          fboot <- coef(nlsboot)["f"]
          y0boot <- fGauss5p(x = 0, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
          ydosemaxboot <- fGauss5p(x = dosemax, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
            
          xextrboot <- eboot + (cboot - dboot)*bboot/(fboot*sqrt(2*pi)) 
          yextrboot <- fGauss5p(x = xextrboot, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
          ydosemaxboot <- fGauss5p(x = dosemax, b = bboot, c = cboot, d = dboot, e = eboot, f = fboot)
            
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
    }
    
    # bootstrap iterations on item i
    l1 <- lapply(1:niter, fboot)
    if(sum(sapply(l1, is.null)) > niter/2) 
    {
      warning(paste("Procedure aborted: the fit only converged in", round(sum(sapply(l1, is.null))/niter), "% during bootstrapping for item ", items[i]))
      return(c(NA, NA, NA, NA))
    } else
    {
      # tabbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$coef)
      # SDresbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$SDres)
      BMDpbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$BMDp)
      BMDsdbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$BMDsd)
      
      BMDp.CI95 <- quantile(BMDpbooti, probs = c(0.025, 0.975), na.rm = TRUE)
      BMDplower <- BMDp.CI95[1]
      BMDpupper <- BMDp.CI95[2]
      
      BMDsd.CI95 <- quantile(BMDsdbooti, probs = c(0.025, 0.975), na.rm = TRUE)
      BMDsdlower <- BMDsd.CI95[1]
      BMDsdupper <- BMDsd.CI95[2]
      
      if (progressbar)
      {
        setTxtProgressBar(pb, i)
      }
      return(c(BMDsdlower, BMDsdupper, BMDplower, BMDpupper))
    }
   
    # pairs(as.data.frame(t(tabbooti)))
    # boxplot(SDresbooti)
    # boxplot(BMDpbooti)
    # boxplot(BMDsdbooti)
  } # end bootoneitem
  
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
  colnames(dres) <- c("BMD.zSD.lower", "BMD.zSD.upper", "BMD.xfold.lower", "BMD.xfold.upper")
  
  dres <- cbind(r$res[i.items, ], dres)
  return(dres)
}

# test
source("util-basicandfitfunc.R")
source("bmdcalc.R")

# check on expo
items <- "363.1" # expo e > 0
items <- "301.1" # expo e < 0
items <- c("301.1", "363.1")
items <- r$res[r$res$model == "exponential", "id"]
(bootres <- bmdboot(r, items))


# check on GP 5 p
# items <- "247.2" # GP 5 y en a qu'un dans le sous jeu de données
items <- r$res[r$res$model == "Gauss-probit" & r$res$nbpar == 5, "id"]
r$res[r$res$id == items, ]
(bootres <- bmdboot(r, items))



plot(bootres$BMD.zSD)
nitems <- nrow(bootres)
segments(x0 = 1:nitems, y0 = bootres$BMD.zSD.lower, x1 = 1:nitems, y1 = bootres$BMD.zSD.upper)
plot(bootres$BMD.xfold)
nitems <- nrow(bootres)
segments(x0 = 1:nitems, y0 = bootres$BMD.xfold.lower, x1 = 1:nitems, y1 = bootres$BMD.xfold.upper)
