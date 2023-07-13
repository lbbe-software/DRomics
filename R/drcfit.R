### fit different models to each dose-response curve and choose the best fit 
drcfit <- function(itemselect,  
                   information.criterion = c("AICc", "BIC", "AIC"),
                   postfitfilter = TRUE,
                   preventsfitsoutofrange = TRUE,
                   enablesfequal0inGP = TRUE,
                   enablesfequal0inLGP = TRUE,
                   progressbar = TRUE, 
                   parallel = c("no", "snow", "multicore"), ncpus)
{
  # Checks
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect.")
  
  parallel <- match.arg(parallel, c("no", "snow", "multicore"))
  if (parallel == "multicore" & .Platform$OS.type == "windows")
  {
    parallel <- "snow"
    warning(strwrap(prefix = "\n", initial = "\n",
      "As the multicore option is not supported on Windows it was replaced by snow."))
  }
  if ((parallel == "snow" | parallel == "multicore") & missing(ncpus)) 
    stop("You have to specify the number of available processors to parallelize the fitting.")
  if (parallel != "no") 
    progressbar <- FALSE
  
  if (progressbar)
    cat("The fitting may be long if the number of selected items is high.\n")
  
  # definition of necessary data
  selectindex <- itemselect$selectindex
  adjpvalue <- itemselect$adjpvalue
  dose <- itemselect$omicdata$dose
  doseranks <- as.numeric(as.factor(itemselect$omicdata$dose)) 
  data <- itemselect$omicdata$data 
  data.mean <- itemselect$omicdata$data.mean 
  
  containsNA <- itemselect$omicdata$containsNA
    
  # calculations for starting values and other uses
  dosemin <- min(dose)
  dosemax <- max(dose)
  dosemed <- stats::median(dose[dose!=0])
  doseu <- as.numeric(colnames(data.mean)) # sorted unique doses
  
  # number of points per dose-response curve
  npts <- length(dose)
  ndoses <- length(unique(dose))
  lessthan5doses <- ndoses < 5

  nselect <- length(selectindex)
  
  # Information criterion definition 
  AICdigits <- 2 # number of digits for rounding the AIC values
  information.criterion <- match.arg(information.criterion, c("AICc", "BIC", "AIC"))

  # kcrit gives the argument k to pass to function AIC()
  # depending of the number of parameters of the model 
  # (1 to 5, corresponding to the index of the vector)
  if (information.criterion == "AIC")
  { 
    kcrit <- rep(2, 5) 
  } else
  if (information.criterion == "AICc")
  {
    nparwithsigma <- 1:5 + 1
    kcrit <- 2*npts /(npts - nparwithsigma - 1)
  } else # BIC last choice
  {
    lnpts <- log(npts)
    kcrit <- rep(lnpts, 5)
  } 
  
  # progress bar
  if (progressbar)
    pb <- utils::txtProgressBar(min = 0, max = length(selectindex), style = 3)
  
  # function to fit all the models an choose the best on one item
  ################################################################
  fitoneitem <- function(i) 
  {
    keeplin <- TRUE
    keepExpo <- TRUE
    keepHill <- TRUE
    keepLGauss <- TRUE
    keepGauss <- TRUE
    
    equalcdG <- FALSE # use to define the value of c equal to d in the Gauss4p model if needed
    equalcdLG <- FALSE # use to define the value of c equal to d in the LGauss4p model if needed
    fequal0LG <- FALSE # use to define the value of f at 0 in the LGauss5p model if needed
    fequal0G <- FALSE # use to define the value of f at 0 in the Gauss5p model if needed
    
    signal <- data[selectindex[i], ]
    signalm <- as.vector(data.mean[selectindex[i],]) # means per dose
    signalmin <- min(signal)
    signalmax <- max(signal)
    
    # preparation of data for modelling with nls 
    dset <- data.frame(signal = signal, dose = dose, doseranks = doseranks)
    
    if (containsNA)
      if (any(!stats::complete.cases(dset)))
      {
        # removing lines with NA values for the signal
        dset <- dset[stats::complete.cases(dset$signal), ]
        npts <- nrow(dset)
        ndoses <- length(unique(dset$dose))
        lessthan5doses <- ndoses < 5
        
        # kcrit gives the argument k to pass to function AIC()
        # dependeing of the number of parameters of the model 
        # (1 to 5, corresponding to the index of the vector)
        if (information.criterion == "AIC")
        { 
          kcrit <- rep(2, 5) 
        } else
          if (information.criterion == "AICc")
          {
            nparwithsigma <- 1:5 + 1
            kcrit <- 2*npts /(npts - nparwithsigma - 1)
          } else # BIC last choice
          {
            lnpts <- log(npts)
            kcrit <- rep(lnpts, 5)
          } 
       } 
    
    # for choice of the linear trend (decreasing or increasing)
    modlin <- stats::lm(signal ~ doseranks, data = dset)
    increaseranks <- stats::coef(modlin)[2] >= 0
    increaseminmax <- dset$dose[which.min(dset$signal)] < dset$dose[which.max(dset$signal)]
    
    # for choice of the quadratic trend (Ushape or Umbrella shape)
    modquad <- stats::lm(signal ~ doseranks + I(doseranks^2), data = dset)
    Ushape <- stats::coef(modquad)[3] >= 0
    
    ################ Expo fit (npar = 3) ###############################
    if (keepExpo)
    {
      # fit of the exponential model with two starting values for abs(e)
      # 0.1*max(dose) or max(dose)
      startExpo3p.1 <- startvalExp3pnls.1(xm = doseu, ym = signalm,  
                                          increase = increaseranks,
                                          Ushape = Ushape)
      startExpo3p.2 <- startvalExp3pnls.2(xm = doseu, ym = signalm,  
                                          increase = increaseranks,
                                          Ushape = Ushape)
      if ((increaseranks & !Ushape) | (!increaseranks & Ushape)) # e < 0
      {
        # Fit of the 3 par model
        Expo3p.1 <- suppressWarnings(try(stats::nls(formExp3p, start = startExpo3p.1, data = dset, 
                                             lower = c(-Inf, -Inf, -Inf), 
                                             upper = c(Inf, Inf, 0),
                                             algorithm = "port"), silent = TRUE))
        
        Expo3p.2 <- suppressWarnings(try(stats::nls(formExp3p, start = startExpo3p.2, data = dset, 
                                             lower = c(-Inf, -Inf, -Inf), 
                                             upper = c(Inf, Inf, 0),
                                             algorithm = "port"), silent = TRUE))
      } else # e > 0
      {
        # Fit of the 3 par model
        Expo3p.1 <- suppressWarnings(try(stats::nls(formExp3p, start = startExpo3p.1, data = dset, 
                                             lower = c(-Inf, -Inf, 0), 
                                             algorithm = "port"), silent = TRUE))
        Expo3p.2 <- suppressWarnings(try(stats::nls(formExp3p, start = startExpo3p.2, data = dset, 
                                             lower = c(-Inf, -Inf, 0), 
                                             algorithm = "port"), silent = TRUE))
      }
      #### convergence of both models
      if ((!inherits(Expo3p.1, "try-error")) & (!inherits(Expo3p.2, "try-error")))
      {
        AICExpo3p.1 <- round(stats::AIC(Expo3p.1, k = kcrit[3]), digits = AICdigits)
        AICExpo3p.2 <- round(stats::AIC(Expo3p.2, k = kcrit[3]), digits = AICdigits)
        if (AICExpo3p.1 < AICExpo3p.2)
        {
          Expo <- Expo3p.1
          AICExpoi <- AICExpo3p.1
        } else
        {
          Expo <- Expo3p.2
          AICExpoi <- AICExpo3p.2
        }
      } else
        #### no convergence of both models
        if (inherits(Expo3p.1, "try-error") & inherits(Expo3p.2, "try-error"))
        {
          # keepExpo <- FALSE
          AICExpoi <- Inf
          Expo <- Expo3p.1 # we could have given Expo3p.2
        } else 
          #### convergence only of Expo3p.2
          if ((!inherits(Expo3p.2, "try-error")) & inherits(Expo3p.1, "try-error"))
          {
            Expo <- Expo3p.2
            AICExpoi <- round(stats::AIC(Expo3p.2, k = kcrit[3]), digits = AICdigits) 
          } else
            #### convergence only of Expo3p.1
            if ((!inherits(Expo3p.1, "try-error")) & inherits(Expo3p.2, "try-error"))
            {
              Expo <- Expo3p.1
              AICExpoi <- round(stats::AIC(Expo3p.1, k = kcrit[3]), digits = AICdigits) 
            }
    } else (AICExpoi <- Inf)
    
    ################## Hill fit (npar = 4) ##########################
    if (keepHill)
    {
      startHill <- startvalHillnls2(x = dose, y = signal, xm = doseu, ym = signalm,  
                                    increase = increaseminmax)
      Hill <- suppressWarnings(try(stats::nls(formHill, start = startHill, data = dset, 
                                       lower = c(0, -Inf, -Inf, 0), algorithm = "port"), silent = TRUE))
      if (!inherits(Hill, "try-error"))
      {
        AICHilli <- round(stats::AIC(Hill, k = kcrit[4]), digits = AICdigits)
      } else 
      {
        # keepHill <- FALSE
        AICHilli <- Inf
      }
    } else (AICHilli <- Inf)
    
    ################# LGauss fit ####################
    if (keepLGauss)
    {
      if (!lessthan5doses)
      {
        startLGauss5p <- startvalLGauss5pnls(xm = doseu, ym = signalm,  
                                           Ushape = Ushape)
        LGauss5p <- suppressWarnings(try(stats::nls(formLGauss5p, start = startLGauss5p, data = dset,
                                             lower = c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
        LGauss5psucces <- !inherits(LGauss5p, "try-error")
        # state a failure if the fitted model is out of the range of the signal
        if (LGauss5psucces & preventsfitsoutofrange)
          LGauss5psucces <- !fLGauss5poutofrange(LGauss5p, signalmin, signalmax)
      }
      startLGauss4p <- startvalLGauss4pnls(xm = doseu, ym = signalm,  
                                           Ushape = Ushape)
      LGauss4p <- suppressWarnings(try(stats::nls(formLGauss4p, start = startLGauss4p, data = dset,
                                           lower = c(0, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
      LGauss4psucces <- !inherits(LGauss4p, "try-error")
      # state a failure if the fitted model is out of the range of the signal
      if (LGauss4psucces & preventsfitsoutofrange)
        LGauss4psucces <- !fLGauss4poutofrange(LGauss4p, signalmin, signalmax)
      
      if (lessthan5doses)
      {
        if (LGauss4psucces)
        {
          equalcdLG <- TRUE
          LGauss <- LGauss4p
          AICLGaussi <- round(stats::AIC(LGauss4p, k = kcrit[4]), digits = AICdigits)
        } else (AICLGaussi <- Inf)
      } else # if (lessthan5doses)
      {
        #### convergence of both models
        if ((LGauss4psucces) & (LGauss5psucces))
        {
          AICLGauss4p <- round(stats::AIC(LGauss4p, k = kcrit[4]), digits = AICdigits)
          AICLGauss5p <- round(stats::AIC(LGauss5p, k = kcrit[5]), digits = AICdigits)
          if (AICLGauss5p < AICLGauss4p)
          {
            LGauss <- LGauss5p
            AICLGaussi <- AICLGauss5p
          } else
          {
            LGauss <- LGauss4p
            equalcdLG <- TRUE
            AICLGaussi <- AICLGauss4p
          }
        } else
          #### no convergence of both models
          if ((!LGauss4psucces) & (!LGauss5psucces))
          {
            # keepLGauss <- FALSE
            AICLGaussi <- Inf
            LGauss <- LGauss5p # we could have given LGauss4p
          } else 
            #### convergence only of LGauss4p
            if ((LGauss4psucces) & (!LGauss5psucces))
            {
              equalcdLG <- TRUE
              LGauss <- LGauss4p
              AICLGaussi <- round(stats::AIC(LGauss4p, k = kcrit[4]), digits = AICdigits)
            } else
              #### convergence only of LGauss5p
              if ((LGauss5psucces) & (!LGauss4psucces))
              {
                LGauss <- LGauss5p
                AICLGaussi <- round(stats::AIC(LGauss5p, k = kcrit[5]), digits = AICdigits)
              } else (AICLGaussi <- Inf)
        
        # If LGauss5p chosen, try with f = 0
        if (enablesfequal0inLGP & (is.finite(AICLGaussi)) & (!equalcdLG))
        {
          parLG5p <- stats::coef(LGauss)
          startLprobit <- list(b = parLG5p["b"], c = parLG5p["c"], d = parLG5p["d"], e = parLG5p["e"])
          Lprobit <- suppressWarnings(try(stats::nls(formLprobit, start = startLprobit, data = dset, 
                                                lower = c(0, -Inf, -Inf, 0), algorithm = "port"), silent = TRUE))
          if (!inherits(Lprobit, "try-error"))
          {
            AICwithfat0 <- round(stats::AIC(Lprobit, k = kcrit[4]), digits = AICdigits)
            if (AICwithfat0 <= AICLGaussi)
            {
              AICLGaussi <- AICwithfat0
              LGauss <- Lprobit
              fequal0LG <- TRUE
            }
          }          
        } 
      } # END of if (lessthan5doses) 
    } # END of if (keepLGauss)
        
      
    
    
    ################### Gauss fit ########################
    if (keepGauss)
    {
      if (!lessthan5doses)
      {
        startGauss5p <- startvalGauss5pnls(xm = doseu, ym = signalm,  
                                         Ushape = Ushape)
        Gauss5p <- suppressWarnings(try(stats::nls(formGauss5p, start = startGauss5p, data = dset, 
                                          lower = c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
        Gauss5psucces <- !inherits(Gauss5p, "try-error")
        # state a failure if the fitted model is out of the range of the signal
        if (Gauss5psucces & preventsfitsoutofrange)
          Gauss5psucces <- !fGauss5poutofrange(Gauss5p, signalmin, signalmax)
      }
      startGauss4p <- startvalGauss4pnls(xm = doseu, ym = signalm,  
                                         Ushape = Ushape)
      Gauss4p <- suppressWarnings(try(stats::nls(formGauss4p, start = startGauss4p, data = dset, 
                                          lower = c(0, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
      Gauss4psucces <- !inherits(Gauss4p, "try-error")
      # state a failure if the fitted model is out of the range of the signal
      if (Gauss4psucces & preventsfitsoutofrange)
        Gauss4psucces <- !fGauss4poutofrange(Gauss4p, signalmin, signalmax)
      
      if (lessthan5doses)
      {
        if (Gauss4psucces)
        {
          equalcdG <- TRUE
          Gauss <- Gauss4p
          AICGaussi <- round(stats::AIC(Gauss4p, k = kcrit[4]), digits = AICdigits)
        } else (AICGaussi <- Inf)
      } else # if (lessthan5doses)
      {
        #### convergence of both models
        if ((Gauss4psucces) & (Gauss5psucces))
        {
          AICGauss4p <- round(stats::AIC(Gauss4p, k = kcrit[4]), digits = AICdigits)
          AICGauss5p <- round(stats::AIC(Gauss5p, k = kcrit[5]), digits = AICdigits)
          if (AICGauss5p < AICGauss4p)
          {
            Gauss <- Gauss5p
            AICGaussi <- AICGauss5p
          } else
          {
            Gauss <- Gauss4p
            equalcdG <- TRUE
            AICGaussi <- AICGauss4p
          }
        } else
          #### no convergence of both models
          if ((!Gauss4psucces) & (!Gauss5psucces))
          {
            # keepGauss <- FALSE
            AICGaussi <- Inf
            Gauss <- Gauss5p # we could have given Gauss4p
          } else 
            #### convergence only of Gauss4p
            if ((Gauss4psucces) & (!Gauss5psucces))
            {
              equalcdG <- TRUE
              Gauss <- Gauss4p
              AICGaussi <- round(stats::AIC(Gauss4p, k = kcrit[4]), digits = AICdigits)
            } else
              #### convergence only of Gauss5p
              if ((Gauss5psucces) & (!Gauss4psucces))
              {
                Gauss <- Gauss5p
                AICGaussi <- round(stats::AIC(Gauss5p, k = kcrit[5]), digits = AICdigits)
              } else (AICGaussi <- Inf)

      # If Gauss5p chosen, try with f = 0
      if (enablesfequal0inGP & (is.finite(AICGaussi)) & (!equalcdG))
      {
        parG5p <- stats::coef(Gauss)
        startprobit <- list(b = parG5p["b"], c = parG5p["c"], d = parG5p["d"], e = parG5p["e"])
        probit <- suppressWarnings(try(stats::nls(formprobit, start = startprobit, data = dset, 
                                            lower = c(0, -Inf, -Inf, 0), algorithm = "port"), silent = TRUE))
        if (!inherits(probit, "try-error"))
        {
          AICwithfat0 <- round(stats::AIC(probit, k = kcrit[4]), digits = AICdigits)
          if (AICwithfat0 <= AICGaussi)
          {
            AICGaussi <- AICwithfat0
            Gauss <- probit
            fequal0G <- TRUE
          }
        }          
      } 
    } # END of if (lessthan5doses) 
  } # END of if (keepGauss)
  
    
    ######### Fit of the linear model ############################    
    if (keeplin)
    {
      lin <- stats::lm(signal ~ dose, data = dset)
      AIClini <- round(stats::AIC(lin, k = kcrit[2]), digits = AICdigits)
    } else (AIClii <- Inf)
    
    
    ######## Fit of the null model (constant) ###########################
    constmodel <- stats::lm(signal ~ 1, data = dset)
    AICconsti <-  round(stats::AIC(constmodel, k = kcrit[1]), digits = AICdigits)

    ######### Choice of the best fit #####################################
    ######################################################################
    AICvec <- c(AICGaussi, AICLGaussi, AICHilli, AICExpoi, AIClini)
    # the nb. of the model, 1 to 5, is used in the following with the last
    # being the null model : nb. 6
    indmodeli <- which.min(AICvec)
    AICmin <- AICvec[indmodeli]
    if (AICmin > AICconsti - 2) # we keep the null model
    {
      fit <- constmodel
      indmodeli <- 6 # constant model
      nbpari <- 1
      b.i <- NA
      c.i <- mean(dset$signal)
      d.i <- NA
      e.i <- NA
      f.i <- NA
      SDres.i <- stats::sigma(constmodel)
    } else
    {
      if (indmodeli == 1)
      {
        fit <- Gauss
        par <- stats::coef(fit)
        b.i <- par["b"]
        c.i <- ifelse(equalcdG, par["d"], par["c"])
        d.i <- par["d"]
        e.i <- par["e"]
        f.i <- ifelse(fequal0G, 0, par["f"])
        SDres.i <- stats::sigma(fit)
        if (enablesfequal0inGP)
        {
          nbpari <- ifelse(equalcdG | fequal0G, 4, 5)
        } else
        {
          nbpari <- ifelse(equalcdG, 4, 5)
        }
      } else
        if (indmodeli == 2)
        {
          fit <- LGauss
          par <- stats::coef(fit)
          b.i <- par["b"]
          c.i <- ifelse(equalcdLG, par["d"], par["c"])
          d.i <- par["d"]
          e.i <- par["e"]
          f.i <- ifelse(fequal0LG, 0, par["f"])
          SDres.i <- stats::sigma(fit)
          if (enablesfequal0inLGP)
          {
            nbpari <- ifelse(equalcdLG | fequal0LG, 4, 5)
          } else
          {
            nbpari <- ifelse(equalcdLG, 4, 5)
          }
        } else
          if (indmodeli == 3)
          {
            fit <- Hill
            par <- stats::coef(fit)
            b.i <- par["b"]
            c.i <- par["c"]
            d.i <- par["d"]
            e.i <- par["e"]
            f.i <- NA
            SDres.i <- stats::sigma(fit)
            nbpari <- 4
          } else
            if (indmodeli == 4)
            {
              fit <- Expo
              par <- stats::coef(fit)
              b.i <- par["b"]
              c.i <- NA
              d.i <- par["d"]
              e.i <- par["e"]
              f.i <- NA
              SDres.i <- stats::sigma(fit)
              nbpari <- 3
            } else
              if (indmodeli == 5)
              {
                fit <- lin
                par <- stats::coef(fit)
                b.i <- par[2]
                c.i <- NA
                d.i <- par[1]
                e.i <- NA
                f.i <- NA
                SDres.i <- stats::sigma(fit)
                nbpari <- 2
              } 
      
    }
    
    # diagnostics on residuals (quadratic trend on residuals) 
    # answer to the question: correct mean function ?
    dset$resi <- stats::residuals(fit)
    modquad.resi <- stats::lm(resi ~ doseranks + I(doseranks^2), data = dset)
    mod0.resi <- stats::lm(resi ~ 1, data = dset)
    resimeantrendPi <- stats::anova(modquad.resi, mod0.resi)[[6]][2]
 
    # diagnostics on absolute value of residuals 
    # answer to the question of homoscedasticity ?
    # (quadratic trend on abs(residuals)) 
    dset$absresi <-abs(dset$resi)
    modquad.absresi <- stats::lm(absresi ~ doseranks + I(doseranks^2), data = dset)
    mod0.absresi <- stats::lm(absresi ~ 1, data = dset)
    resivartrendPi <- stats::anova(modquad.absresi, mod0.absresi)[[6]][2]
    
    
    if (progressbar)
    {
      utils::setTxtProgressBar(pb, i)
    }
    
    return(c(indmodeli, nbpari, b.i, c.i, d.i, e.i, f.i, SDres.i,
             AIClini, AICExpoi, AICHilli, AICLGaussi, 
             AICGaussi,resimeantrendPi,resivartrendPi))
    
  } ##################################### END of fitoneitem
  
  # Loop on items
  # parallel or sequential computation
  if (parallel != "no") 
  {
    if (parallel == "snow") type <- "PSOCK"
    else if (parallel == "multicore") type <- "FORK"
    clus <- parallel::makeCluster(ncpus, type = type)
    res <- parallel::parSapply(clus, 1:nselect, fitoneitem)
    parallel::stopCluster(clus)
  }
  else
  {
    res <- sapply(1:nselect, fitoneitem)
  }
  
  # close progress bar
  if (progressbar) close(pb)
  
  dres <- as.data.frame(t(res))
  # colnames(dres) <- c("model", "nbpar", "b", "c", "d", "e", "f", "SDres",
  #                     "AIC.L", "AIC.E", "AIC.H", "AIC.lP", "AIC.lGP", "AIC.GP",
  #                     "resimeantrendP", "resivartrendP")
  colnames(dres) <- c("model", "nbpar", "b", "c", "d", "e", "f", "SDres",
                      "AIC.L", "AIC.E", "AIC.H", "AIC.lGP", "AIC.GP",
                      "resimeantrendP", "resivartrendP")
  
  dres <- cbind(data.frame(id = row.names(data)[selectindex], 
                           irow = selectindex, 
                           adjpvalue = adjpvalue),
                           dres)
  
  # correction of dres$resimeantrendP and dres$resivartrendP
  # using Benjamini Hochberg method
  # not done to be more cautious and alert about potential heteroscedasticity pb
  # and eliminate bad fits
  # dres$resimeantrendadjP <- p.adjust(dres$resimeantrendP, method = "BH")
  # dres$resivartrendadjP <- p.adjust(dres$resivartrendP, method = "BH")
  
    
  # removing of null models (const, model no 6) and 
  # fits eliminated by the quadratic trend test on residuals
  if (postfitfilter)
  {
    lines.success <- (dres$model != 6) & 
      ((dres$resimeantrendP > 0.05) | is.na(dres$resimeantrendP))
    # is.na(resimeantrendP because anova of two models with very close RSS
    # may return NA for pvalue)
  } else
  {
    lines.success <- (dres$model != 6) 
  }
  
  
  dres.failure <- dres[!lines.success, ]
  dfail <- dres.failure[, c("id", "irow", "adjpvalue")]
  dfail$cause <- character(length = nrow(dfail))
  dfail$cause[dres.failure$model == 6] <- "constant.model"
  dfail$cause[dres.failure$model != 6] <- "trend.in.residuals"
  
  dres <- dres[lines.success, ]

  # update of nselect
  nselect <- nrow(dres)
  
  dc <- dres[, c("id", "irow", "adjpvalue", "model", "nbpar", "b", "c", "d", "e", "f", "SDres")]
  dresitests <- dres[, c("resimeantrendP", "resivartrendP")]
  modelnames <- c("Gauss-probit", "log-Gauss-probit", "Hill", "exponential", "linear")
  dc$model <- modelnames[dc$model] 
  
  # Calculation of the theoretical value at the control : y0
  # of the theoretical value at the maximal dose : yatdosemax
  # of the theoretical signal range on the range of tested concentration : yrange
  # of the maximal change of y from its value at the minimal dose : maxychange
  # of the x-value that corresponds to the extremum for U and bell curves : xextrem
  ##################################################################################
  y0 <- numeric(length = nselect)
  yatdosemax <- numeric(length = nselect)
  yrange <- numeric(length = nselect)
  maxychange <- numeric(length = nselect)
  xextrem <- numeric(length = nselect)
  xextrem[1:nselect] <- NA # will remain at NA for monotonic curves
  yextrem <- numeric(length = nselect)
  yextrem[1:nselect] <- NA # will remain at NA for monotonic curves
  
  # calculation of y0, maxychange and yrange for linear curves
  indlin <- which(dc$model == "linear")
  vb <- dc$b[indlin]
  vd <- dc$d[indlin]
  ydosemin <- flin(dosemin, vb, vd)
  ydosemax <- flin(dosemax, vb, vd)
  yatdosemax[indlin] <- ydosemax
  yrange[indlin] <- abs(ydosemin - ydosemax)
  y0[indlin] <- vd
  maxychange[indlin] <- yrange[indlin]
  
  # calculation of y0 and yrange for exponential curves
  indExpo <- which(dc$model == "exponential")
  vb <- dc$b[indExpo]
  vd <- dc$d[indExpo]
  ve <- dc$e[indExpo]
  ydosemin <- fExpo(dosemin, vb, vd, ve) 
  ydosemax <- fExpo(dosemax, vb, vd, ve)
  yatdosemax[indExpo] <- ydosemax
  yrange[indExpo] <- abs(ydosemin - ydosemax)
  y0[indExpo] <- vd
  maxychange[indExpo] <- yrange[indExpo]
  
  # calculation of y0 and yrange for Hill curves
  indHill <- which(dc$model == "Hill")
  vb <- dc$b[indHill]
  vc <- dc$c[indHill]
  vd <- dc$d[indHill]
  ve <- dc$e[indHill]
  ydosemin <- fHill(dosemin, vb, vc, vd, ve)
  ydosemax <- fHill(dosemax, vb, vc, vd, ve)
  yatdosemax[indHill] <- ydosemax
  yrange[indHill] <- abs(ydosemin - ydosemax)
  y0[indHill] <- vd
  maxychange[indHill] <- yrange[indHill]
  
  # calculation of y0, xextrem and yrange for Gauss-probit curves
  # when f != 0
  indGP <- which(dc$model == "Gauss-probit"& dc$f != 0)
  vb <- dc$b[indGP]
  vc <- dc$c[indGP]
  vd <- dc$d[indGP]
  ve <- dc$e[indGP]
  vf <- dc$f[indGP]
  xextr <- xextrem[indGP] <- ve + (vc - vd)*vb/(vf*sqrt(2*pi)) 
  yextr <- yextrem[indGP] <- fGauss5p(xextr, vb, vc, vd, ve, vf)
  ydosemin <- fGauss5p(dosemin, vb, vc, vd, ve, vf)
  ydosemax <- fGauss5p(dosemax, vb, vc, vd, ve, vf)
  yatdosemax[indGP] <- ydosemax
  yrange[indGP] <- pmax(abs(ydosemin - yextr), abs(yextr - ydosemax))
  maxychange[indGP] <- pmax(abs(ydosemin - yextr), abs(ydosemax - ydosemin))
  y0[indGP] <- fGauss5p(0, vb, vc, vd, ve, vf)
  
  # when f == 0
  indGPf0 <- which((dc$model == "Gauss-probit" & dc$f == 0))
  vb <- dc$b[indGPf0]
  vc <- dc$c[indGPf0]
  vd <- dc$d[indGPf0]
  ve <- dc$e[indGPf0]
  vf <- dc$f[indGPf0]
  ydosemin <- fGauss5p(dosemin, vb, vc, vd, ve, vf)
  ydosemax <- fGauss5p(dosemax, vb, vc, vd, ve, vf)
  yatdosemax[indGPf0] <- ydosemax
  yrange[indGPf0] <- abs(ydosemin - ydosemax)
  y0[indGPf0] <- fGauss5p(0, vb, vc, vd, ve, vf)
  maxychange[indGPf0] <- yrange[indGPf0]
  
  
  # calculation of y0, xextrem and yrange for log-Gauss-probit curves
  # when f != 0
  indlGP <- which(dc$model == "log-Gauss-probit" & dc$f != 0)
  vb <- dc$b[indlGP]
  vc <- dc$c[indlGP]
  vd <- dc$d[indlGP]
  ve <- dc$e[indlGP]
  vf <- dc$f[indlGP]
  xextr <- xextrem[indlGP] <- exp(log(ve) + (vc - vd)*vb/(vf*sqrt(2*pi))) 
  yextr <- yextrem[indlGP] <- fLGauss5p(xextr, vb, vc, vd, ve, vf) 
  ydosemin <- fLGauss5p(dosemin, vb, vc, vd, ve, vf)
  ydosemax <- fLGauss5p(dosemax, vb, vc, vd, ve, vf)
  yatdosemax[indlGP] <- ydosemax
  yrange[indlGP] <- pmax(abs(ydosemin - yextr), abs(yextr - ydosemax))
  maxychange[indlGP] <- pmax(abs(ydosemin - yextr), abs(ydosemax - ydosemin))
  y0[indlGP] <- vd
  
  # when f == 0
  indlGPf0 <- which(dc$model == "log-Gauss-probit" & dc$f == 0)
  vb <- dc$b[indlGPf0]
  vc <- dc$c[indlGPf0]
  vd <- dc$d[indlGPf0]
  ve <- dc$e[indlGPf0]
  vf <- dc$f[indlGPf0]
  ydosemin <- fLGauss5p(dosemin, vb, vc, vd, ve, vf)
  ydosemax <- fLGauss5p(dosemax, vb, vc, vd, ve, vf)
  yatdosemax[indlGPf0] <- ydosemax
  yrange[indlGPf0] <- abs(ydosemin - ydosemax)
  y0[indlGPf0] <- vd
  maxychange[indlGPf0] <- yrange[indlGPf0]
  
  
  # definition of trend and typology
  typology <- character(length = nselect)
  trend <- character(length = nselect)

  if (nselect !=0)
  {
    for (i in 1:nselect) 
    {
      di <- dc[i,]
      if (di$model == "exponential" & di$e > 0 & di$b > 0) 
      {typology[i] <- "E.inc.convex"
      trend[i] <- "inc"} else
        if (di$model == "exponential" & di$e <= 0 & di$b > 0) 
        {typology[i] <- "E.dec.convex"
        trend[i] <- "dec"} else
          if (di$model == "exponential" & di$e <= 0 & di$b <= 0) 
          {typology[i] <- "E.inc.concave"
          trend[i] <- "inc"} else
            if (di$model == "exponential" & di$e > 0 & di$b <= 0) 
            {typology[i] <- "E.dec.concave"
            trend[i] <- "dec"} else
              if (di$model == "Hill" & di$c > di$d) 
              {typology[i] <- "H.inc"
              trend[i] <- "inc"} else
                if (di$model == "Hill" & di$c <= di$d) 
                {typology[i] <- "H.dec"
                trend[i] <- "dec"} else
                  if (di$model == "log-Gauss-probit" & di$f < 0) 
                  {typology[i] <- "lGP.U"
                  trend[i] <- "U"
                  } else
                    if (di$model == "log-Gauss-probit" & di$f >=0) 
                    {typology[i] <- "lGP.bell"
                    trend[i] <- "bell"
                    } else
                      if (di$model == "Gauss-probit" & di$f < 0) 
                      {typology[i] <- "GP.U"
                      trend[i] <- "U"
                      } else
                        if (di$model == "Gauss-probit" & di$f >=0) 
                        {typology[i] <- "GP.bell"
                        trend[i] <- "bell"
                        } else
                          if (di$model == "linear" & di$b > 0) 
                          {typology[i] <- "L.inc"
                          trend[i] <- "inc"} else
                            if (di$model == "linear" & di$b <= 0) 
                            {typology[i] <- "L.dec"
                            trend[i] <- "dec"} 
      
      if (enablesfequal0inLGP)
      {
        if (di$model == "log-Gauss-probit" & di$f == 0) 
        {     
          if (di$c > di$d) 
          { typology[i] <- "lGP.inc"
            trend[i] <- "inc"} else
            if (di$c <= di$d) 
            {typology[i] <- "lGP.dec"
            trend[i] <- "dec"} 
        }
      } 
      if (enablesfequal0inGP)
      {
        if (di$model == "Gauss-probit" & di$f == 0) 
        {     
          if (di$c > di$d) 
          { typology[i] <- "GP.inc"
          trend[i] <- "inc"} else
            if (di$c <= di$d) 
            {typology[i] <- "GP.dec"
            trend[i] <- "dec"} 
        }
      } 
      
    } # END of the for
  } else
  {
    warning(strwrap(prefix = "\n", initial = "\n", 
                    "THERE IS NO SUCCESSFUL FIT."))
    
  }
  dc$typology <- typology
  
  # correction of the trend and typology for Gauss-probit curves with xextrem == 0
  indnullxextr <- which((dc$model == "Gauss-probit") & (xextrem == 0))
  trend[indnullxextr] <- ifelse(dc$f[indnullxextr] > 0, "dec", "inc") 
  typology[indnullxextr] <- ifelse(dc$f[indnullxextr] > 0, "GP.dec", "GP.inc") 
  
  dc$trend <- factor(trend)
  dc$y0 <- y0
  dc$yatdosemax <- yatdosemax
  dc$yrange <- yrange
  dc$maxychange <- maxychange
  dc$xextrem <- xextrem
  dc$yextrem <- yextrem
  
  # number of null models
  n.failure <- length(itemselect$selectindex) - nrow(dc)
  
  dc$model <- factor(dc$model, # to specify the order
                     levels = c("Hill", "linear", "exponential", "Gauss-probit", "log-Gauss-probit"))
  dc$typology <- factor(dc$typology)
  dAIC <- dres[, c("AIC.L", "AIC.E", "AIC.H", "AIC.lGP", "AIC.GP")] 
  
  reslist <- list(fitres = dc, omicdata = itemselect$omicdata,  
                  information.criterion = information.criterion, information.criterion.val = dAIC,
                  n.failure = n.failure, unfitres = dfail, 
                  residualtests = dresitests ) 
  
  return(structure(reslist, class = "drcfit"))
}

print.drcfit <- function(x, ...)
{
  if (!inherits(x, "drcfit"))
    stop("Use only with 'drcfit' objects.")
  
  cat("Results of the fitting using the", x$information.criterion, "to select the best fit model\n")
  ttrend <- table(x$fitres$trend)
  tfit <- table(x$fitres$model)
  nsucces <- nrow(x$fitres)
  nfirstselect <- x$n.failure + nsucces
  if (x$n.failure > 0)
    cat(strwrap(paste(x$n.failure, "dose-response curves out of", nfirstselect, "previously selected were removed 
                       because no model could be fitted reliably.")), fill = TRUE)
  ncaseheterosced <- length(which(x$residualtests$resivartrendP < 0.05))
  ntot <- nrow(x$residualtests)
  pc.heterosced <- round(ncaseheterosced / ntot * 100)
  if (pc.heterosced > 50)
    cat(strwrap(paste0(pc.heterosced, "% of the fitted dose-response curves show a significant heteroscedasticity. 
                       (non constant variance).")), fill = TRUE)
  cat("Distribution of the chosen models among the", nsucces, "fitted dose-response curves:\n")
  print(tfit)
  cat("Distribution of the trends (curve shapes) among the", nsucces, "fitted dose-response curves:\n")
  print(ttrend)
  # ttypology <- table(x$fitres$typology)
  # cat("Distribution of the typology of the ",nsucces," fitted dose-response curves :\n")
  # print(ttypology)
}

plot.drcfit <- function(x, items, 
                plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"), 
                dose_log_transfo = FALSE, 
                BMDoutput, BMDtype = c("zSD", "xfold"), ...)
{
  plot.type <- match.arg(plot.type, c("dose_fitted", "dose_residuals","fitted_residuals"))  
  if (!inherits(x, "drcfit"))
    stop("Use only with 'drcfit' objects.")
  
  if(missing(items))
  {
    items <- 20
  }
  if(!( is.numeric(items) | is.character(items)) )
    stop("Wrong argument 'items'. It must be a number inferior or equal to 20 or
    a character vector indicating the identifiers of the items who want to plot.")
  if (is.numeric(items))
  {
    inditems <- 1:min(nrow(x$fitres),items)
    subd <- x$fitres[inditems, ]
  } else
  if (is.character(items))
  {
    inditems <- match(items, x$fitres$id)
    if (any(is.na(inditems)))
    stop("At least one of the chosen items was not selected as responding. You should use targetplot() in that case.")
    subd <- x$fitres[inditems, ]
  }
  subd$id <- factor(subd$id, levels = subd$id)
  g <- plotfitsubset(subd, 
                dose = x$omicdata$dose, 
                data = x$omicdata$data, 
                data.mean = x$omicdata$data.mean, 
                npts = 500,
                plot.type = plot.type, 
                dose_log_transfo = dose_log_transfo) + theme_classic()

  addBMD <- FALSE
  addCI <- FALSE
  ## optional add of BMD values on fits
  if (!(missing(BMDoutput)) & (plot.type == "dose_fitted"))
  {
    BMDtype <- match.arg(BMDtype, c("zSD", "xfold")) 
    addBMD <- TRUE
    if (inherits(BMDoutput, "bmdcalc") | inherits(BMDoutput, "bmdboot"))
    {
      bmdres <- BMDoutput$res
      subbmdres <- BMDoutput$res[inditems, ]
      if (inherits(BMDoutput, "bmdboot")) addCI <- TRUE else addCI <- FALSE
      
      if (any(subd$id != subbmdres$id) | any(subd$yrange != subbmdres$yrange))
      {
        warning(strwrap(prefix = "\n", initial = "\n",
                        "To add BMD values on the plot you must 
                        first apply bmdcalc() (and if you want also BMD confidence intervals
                        bmdboot()) on the R object of class drcfit that is specified as first
                        argument of the current plot function, and then 
                        give in the argument BMDoutput the R object given in output
                    of bmdcalc() or bmdboot()."))
        addBMD <- FALSE
      } else
      {
        if (BMDtype == "zSD")
        {
          zvalue <- BMDoutput$z
          subbmdres$lowhline <- subbmdres$y0 - zvalue * subbmdres$SDres
          subbmdres$uphline <- subbmdres$y0 + zvalue * subbmdres$SDres
          subbmdres$BMD <- subbmdres$BMD.zSD
          if (inherits(BMDoutput, "bmdboot"))
          {
            subbmdres$BMDlower <- subbmdres$BMD.zSD.lower
            subbmdres$BMDupper <- subbmdres$BMD.zSD.upper
          }
        } else  # so BMDxfold
        {
          xvalue <- BMDoutput$x
          subbmdres$lowhline <- subbmdres$y0 * (1 + xvalue/100)
          subbmdres$uphline <- subbmdres$y0 * (1 - xvalue/100)
          subbmdres$BMD <- subbmdres$BMD.xfold
          if (inherits(BMDoutput, "bmdboot"))
          {
            addCI <- TRUE
            subbmdres$BMDlower <- subbmdres$BMD.xfold.lower
            subbmdres$BMDupper <- subbmdres$BMD.xfold.upper
          }
        }
      }
    } else
    {
      warning(strwrap(prefix = "\n", initial = "\n",
                      "To add BMD values on the plot you must 
                        first apply bmdcalc() (and if you want also BMD confidence intervals
                        bmdboot()) on the R object of class drcfit that is specified as first
                        argument of the current plot function, and then 
                        give in the argument BMDoutput the R object given in output
                    of bmdcalc() or bmdboot()."))
      addBMD <- FALSE
    }
  }# END if !missing(BMDoutput)
  
  if (addBMD)
  {
    subbmdres$id <- factor(subbmdres$id, levels = subd$id)
    g <- g + geom_vline(data = subbmdres,
                        aes_(xintercept = quote(BMD)), 
                        linetype = 1, colour = "red") +
      # geom_ribbon(data = subbmdres,
      #             aes_(ymin = quote(lowhline), 
      #                  ymax = quote(uphline)), 
      #             fill = "red", alpha = 0.1)
      geom_hline(data = subbmdres, aes_(yintercept = quote(uphline)),
                 linetype = 3, colour = "red") +
      geom_hline(data = subbmdres, aes_(yintercept = quote(lowhline)),
                 linetype = 3,colour = "red")
    if (addCI)
    {
      g <- g + geom_vline(data = subbmdres, aes_(xintercept = quote(BMDlower)), 
                          linetype = 2,colour = "red") +
        geom_vline(data = subbmdres, aes_(xintercept = quote(BMDupper)), 
                   linetype = 2, colour = "red") 
    }
  }
  print(g)
  
}

plotfit2pdf <- function(x, items, 
                        plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"), 
                        dose_log_transfo = FALSE, 
                        BMDoutput, BMDtype = c("zSD", "xfold"),
                        nrowperpage = 6, ncolperpage = 4,
                        path2figs = getwd())
{
  plot.type <- match.arg(plot.type, c("dose_fitted", "dose_residuals","fitted_residuals"))  
  if (!inherits(x, "drcfit"))
    stop("Use only with 'drcfit' objects.")
  
  # a ggplot alternative
  if(missing(items))
  {
    items <- x$fitres$id
  }
  if(!( is.numeric(items) | is.character(items)) )
    stop("Wrong argument 'items'. It must be a number inferior or equal to 20 or
    a character vector indicating the identifiers of the items who want to plot.")
  if (is.numeric(items))
  {
    inditems <- 1:min(nrow(x$fitres),items)
    subd <- x$fitres[1:min(nrow(x$fitres),items), ]
  } else
  if (is.character(items))
  {
    inditems <- match(items, x$fitres$id)
    if (any(is.na(inditems)))
        stop("At least one of the chosen items was not selected as responding. You should use targetplot() in that case.")
      subd <- x$fitres[inditems, ]
  }
  
  file2plot <- paste0(path2figs, "/drcfitplot.pdf")
  message(strwrap(prefix = "\n", initial = "\n",
                  paste0("Figures are stored in ", normalizePath(path2figs), ".")))
  grDevices::pdf(file2plot, width = 7, height = 10, onefile = TRUE) # w and h in inches
  
  nplotsperpage <- nrowperpage * ncolperpage
  npage <- ceiling(nrow(subd) / nplotsperpage)
  
  addBMD <- FALSE
  addCI <- FALSE
  if (!(missing(BMDoutput)) & (plot.type == "dose_fitted"))
  {
    BMDtype <- match.arg(BMDtype, c("zSD", "xfold")) 
    addBMD <- TRUE
    if (inherits(BMDoutput, "bmdcalc") | inherits(BMDoutput, "bmdboot"))
    {
      bmdres <- BMDoutput$res
      subbmdres <- BMDoutput$res[inditems, ]
      if (inherits(BMDoutput, "bmdboot")) addCI <- TRUE else addCI <- FALSE

      if (any(subd$id != subbmdres$id) | any(subd$yrange != subbmdres$yrange))
      {
        warning(strwrap(prefix = "\n", initial = "\n",
                        "To add BMD values on the plot you must 
                        first apply bmdcalc() (and if you want also BMD confidence intervals
                        bmdboot()) on the R object of class drcfit that is specified as first
                        argument of the current plot function, and then 
                        give in the argument BMDoutput the R object given in output
                    of bmdcalc() or bmdboot()."))
        addBMD <- FALSE
      } else
      {
        if (BMDtype == "zSD")
        {
          zvalue <- BMDoutput$z
          subbmdres$lowhline <- subbmdres$y0 - zvalue * subbmdres$SDres
          subbmdres$uphline <- subbmdres$y0 + zvalue * subbmdres$SDres
          subbmdres$BMD <- subbmdres$BMD.zSD
          if (inherits(BMDoutput, "bmdboot"))
          {
            subbmdres$BMDlower <- subbmdres$BMD.zSD.lower
            subbmdres$BMDupper <- subbmdres$BMD.zSD.upper
          }
        } else  # so BMDxfold
        {
          xvalue <- BMDoutput$x
          subbmdres$lowhline <- subbmdres$y0 * (1 + xvalue/100)
          subbmdres$uphline <- subbmdres$y0 * (1 - xvalue/100)
          subbmdres$BMD <- subbmdres$BMD.xfold
          if (inherits(BMDoutput, "bmdboot"))
          {
            addCI <- TRUE
            subbmdres$BMDlower <- subbmdres$BMD.xfold.lower
            subbmdres$BMDupper <- subbmdres$BMD.xfold.upper
          }
        }
      }
    } else
    {
      warning(strwrap(prefix = "\n", initial = "\n",
                      "To add BMD values on the plot you must 
                        first apply bmdcalc() (and if you want also BMD confidence intervals
                        bmdboot()) on the R object of class drcfit that is specified as first
                        argument of the current plot function, and then 
                        give in the argument BMDoutput the R object given in output
                    of bmdcalc() or bmdboot()."))
      addBMD <- FALSE
    }
  }# END if !missing(BMDoutput)

  
  for (i in 1:npage)
  {
    if (i == npage) indmax <- nrow(subd) else indmax <- i*nplotsperpage
    ind2plot <- seq((i-1)*nplotsperpage + 1,indmax, 1)
    g <- plotfitsubset(subd[ind2plot, ], 
                                 dose = x$omicdata$dose, 
                                 data = x$omicdata$data, 
                                 data.mean = x$omicdata$data.mean, 
                                 npts = 500,
                                 plot.type = plot.type, 
                                 dose_log_transfo = dose_log_transfo, 
                                  nr = nrowperpage, 
                                  nc = ncolperpage) + theme_classic()
    if (addBMD)
    {
      g <- g + geom_vline(data = subbmdres[ind2plot, ],
                          aes_(xintercept = quote(BMD)), 
                          linetype = 1, colour = "red") +
        # geom_ribbon(data = subbmdres[ind2plot, ],
        #             aes_(ymin = quote(lowhline), 
        #                  ymax = quote(uphline)), 
        #             fill = "red", alpha = 0.1)
        geom_hline(data = subbmdres[ind2plot, ], aes_(yintercept = quote(uphline)),
                   linetype = 3, colour = "red") +
        geom_hline(data = subbmdres[ind2plot, ], aes_(yintercept = quote(lowhline)),
                   linetype = 3,colour = "red")
      if (addCI)
      {
        g <- g + geom_vline(data = subbmdres[ind2plot, ], aes_(xintercept = quote(BMDlower)), 
                            linetype = 2,colour = "red") +
          geom_vline(data = subbmdres[ind2plot, ], aes_(xintercept = quote(BMDupper)), 
                     linetype = 2, colour = "red") 
      }
    }
    print(g)
  }
  grDevices::dev.off()
  
}


