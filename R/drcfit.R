### fit different models to each dose-response curve and choose the best fit 
drcfit <- function(itemselect, sigmoid.model = c("Hill", "log-probit"), 
                   information.criterion = c("AIC", "BIC"),
                   progressbar = TRUE, saveplot2pdf = TRUE, 
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
  
  # Definition of the sigmoid model to fit
  sigmoid.model <- match.arg(sigmoid.model, c("Hill", "log-probit"))
  
  # definition of necessary data
  selectindex <- itemselect$selectindex
  adjpvalue <- itemselect$adjpvalue
  dose <- itemselect$omicdata$dose
  doseranks <- as.numeric(as.factor(itemselect$omicdata$dose)) 
  data <- itemselect$omicdata$data 
  data.mean <- itemselect$omicdata$data.mean 
  
  # calculations for starting values and other uses
  dosemin <- min(dose)
  dosemax <- max(dose)
  dosemed <- median(dose[dose!=0])
  doseu <- as.numeric(colnames(data.mean)) # sorted unique doses
  
  # number of points per dose-response curve
  nptsperDR <- ncol(data)
  nselect <- length(selectindex)
  
  # Information criterion definition 
  AICdigits <- 2 # number of digits for rounding the AIC values
  information.criterion <- match.arg(information.criterion, c("AIC", "BIC"))
  if (information.criterion == "AIC")
  { 
    kcrit <- 2 
  } else
  {
    kcrit <- log(nptsperDR)
  }

  # progress bar
  if (progressbar)
    pb <- txtProgressBar(min = 0, max = length(selectindex), style = 3)
  
  # function to fit all the models an choose the best on one item
  ################################################################
  fitoneitem <- function(i) 
  {
    keeplin <- TRUE
    keepExpo <- TRUE
    keepHill <- sigmoid.model == "Hill"
    keepLprobit <- sigmoid.model == "log-probit"
    keepLGauss <- TRUE
    keepGauss <- TRUE
    
    equalcdG <- FALSE # use to define the value of c equal to d in the Gauss4p model if needed
    equalcdLG <- FALSE # use to define the value of c equal to d in the LGauss4p model if needed
    
    signal <- data[selectindex[i], ]
    signalm <- as.vector(data.mean[selectindex[i],]) # means per dose
    
    # preparation of data for modelling with nls 
    dset <- data.frame(signal = signal, dose = dose)
    
    # for choice of the linear trend (decreasing or increasing)
    modlin <- lm(signal ~ doseranks)
    increaseranks <- coef(modlin)[2] >= 0
    increaseminmax <- dose[which.min(signal)] < dose[which.max(signal)]
    
    # for choice of the quadratic trend (Ushape or Umbrella shape)
    modquad <- lm(signal ~ doseranks + I(doseranks^2))
    Ushape <- coef(modquad)[3] >= 0
    
    ################ Expo fit ###############################
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
        Expo3p.1 <- suppressWarnings(try(nls(formExp3p, start = startExpo3p.1, data = dset, 
                                             lower = c(-Inf, -Inf, -Inf), 
                                             upper = c(Inf, Inf, 0),
                                             algorithm = "port"), silent = TRUE))
        
        Expo3p.2 <- suppressWarnings(try(nls(formExp3p, start = startExpo3p.2, data = dset, 
                                             lower = c(-Inf, -Inf, -Inf), 
                                             upper = c(Inf, Inf, 0),
                                             algorithm = "port"), silent = TRUE))
      } else # e > 0
      {
        # Fit of the 3 par model
        Expo3p.1 <- suppressWarnings(try(nls(formExp3p, start = startExpo3p.1, data = dset, 
                                             lower = c(-Inf, -Inf, 0), 
                                             algorithm = "port"), silent = TRUE))
        Expo3p.2 <- suppressWarnings(try(nls(formExp3p, start = startExpo3p.2, data = dset, 
                                             lower = c(-Inf, -Inf, 0), 
                                             algorithm = "port"), silent = TRUE))
      }
      #### convergence of both models
      if ((!inherits(Expo3p.1, "try-error")) & (!inherits(Expo3p.2, "try-error")))
      {
        AICExpo3p.1 <- round(AIC(Expo3p.1, k = kcrit), digits = AICdigits)
        AICExpo3p.2 <- round(AIC(Expo3p.2, k = kcrit), digits = AICdigits)
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
          keepExpo <- FALSE
          AICExpoi <- Inf
          Expo <- Expo3p.1 # we could have given Expo3p.2
        } else 
          #### convergence only of Expo3p.2
          if ((!inherits(Expo3p.2, "try-error")) & inherits(Expo3p.1, "try-error"))
          {
            Expo <- Expo3p.2
            AICExpoi <- round(AIC(Expo3p.2, k = kcrit), digits = AICdigits)
          } else
            #### convergence only of Expo3p.1
            if ((!inherits(Expo3p.1, "try-error")) & inherits(Expo3p.2, "try-error"))
            {
              Expo <- Expo3p.1
              AICExpoi <- round(AIC(Expo3p.1, k = kcrit), digits = AICdigits)
            }
    } else (AICExpoi <- Inf)
    
    ################## Hill fit ##########################
    if (keepHill)
    {
      startHill <- startvalHillnls2(x = dose, y = signal, xm = doseu, ym = signalm,  
                                    increase = increaseminmax)
      Hill <- suppressWarnings(try(nls(formHill, start = startHill, data = dset, 
                                       lower = c(0, -Inf, -Inf, 0), algorithm = "port"), silent = TRUE))
      if (!inherits(Hill, "try-error"))
      {
        AICHilli <- round(AIC(Hill, k = kcrit), digits = AICdigits)
      } else 
      {
        keepHill <- FALSE
        AICHilli <- Inf
      }
    } else (AICHilli <- Inf)
    
    ############### Lprobit fit #################
    if (keepLprobit)
    {
      startLprobit <- startvalLprobitnls2(x = dose, y = signal, xm = doseu, ym = signalm,  
                                          increase = increaseminmax)
      Lprobit <- suppressWarnings(try(nls(formLprobit, start = startLprobit, data = dset, 
                                          lower = c(0, -Inf, -Inf, 0), algorithm = "port"), silent = TRUE))
      if (!inherits(Lprobit, "try-error"))
      {
        AICLprobiti <- round(AIC(Lprobit, k = kcrit), digits = AICdigits)
      } else 
      {
        keepLprobit <- FALSE
        AICLprobiti <- Inf
      }
    } else (AICLprobiti <- Inf)
    
    
    ################# LGauss fit ####################
    if (keepLGauss)
    {
      startLGauss5p <- startvalLGauss5pnls(xm = doseu, ym = signalm,  
                                           Ushape = Ushape)
      startLGauss4p <- startvalLGauss4pnls(xm = doseu, ym = signalm,  
                                           Ushape = Ushape)
      LGauss5p <- suppressWarnings(try(nls(formLGauss5p, start = startLGauss5p, data = dset,
                                           lower = c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
      LGauss4p <- suppressWarnings(try(nls(formLGauss4p, start = startLGauss4p, data = dset,
                                           lower = c(0, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
      #### convergence of both models
      if ((!inherits(LGauss4p, "try-error")) & (!inherits(LGauss5p, "try-error")))
      {
        AICLGauss4p <- round(AIC(LGauss4p, k = kcrit), digits = AICdigits)
        AICLGauss5p <- round(AIC(LGauss5p, k = kcrit), digits = AICdigits)
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
        if (inherits(LGauss4p, "try-error") & inherits(LGauss5p, "try-error"))
        {
          keepLGauss <- FALSE
          AICLGaussi <- Inf
          LGauss <- LGauss5p # we could have given LGauss4p
        } else 
          #### convergence only of LGauss4p
          if ((!inherits(LGauss4p, "try-error")) & inherits(LGauss5p, "try-error"))
          {
            equalcdLG <- TRUE
            LGauss <- LGauss4p
            AICLGaussi <- round(AIC(LGauss4p, k = kcrit), digits = AICdigits)
          } else
            #### convergence only of LGauss5p
            if ((!inherits(LGauss5p, "try-error")) & inherits(LGauss4p, "try-error"))
            {
              LGauss <- LGauss5p
              AICLGaussi <- round(AIC(LGauss5p, k = kcrit), digits = AICdigits)
            }
    } else (AICLGaussi <- Inf)
    
    
    ################### Gauss fit ########################
    if (keepGauss)
    {
      startGauss5p <- startvalGauss5pnls(xm = doseu, ym = signalm,  
                                         Ushape = Ushape)
      Gauss5p <- suppressWarnings(try(nls(formGauss5p, start = startGauss5p, data = dset, 
                                          lower = c(0, -Inf, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
      startGauss4p <- startvalGauss4pnls(xm = doseu, ym = signalm,  
                                         Ushape = Ushape)
      Gauss4p <- suppressWarnings(try(nls(formGauss4p, start = startGauss4p, data = dset, 
                                          lower = c(0, -Inf, 0, -Inf), algorithm = "port"), silent = TRUE))
      
      #### convergence of both models
      if ((!inherits(Gauss4p, "try-error")) & (!inherits(Gauss5p, "try-error")))
      {
        AICGauss4p <- round(AIC(Gauss4p, k = kcrit), digits = AICdigits)
        AICGauss5p <- round(AIC(Gauss5p, k = kcrit), digits = AICdigits)
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
        if (inherits(Gauss4p, "try-error") & inherits(Gauss5p, "try-error"))
        {
          keepGauss <- FALSE
          AICGaussi <- Inf
          Gauss <- Gauss5p # we could have given Gauss4p
        } else 
          #### convergence only of Gauss4p
          if ((!inherits(Gauss4p, "try-error")) & inherits(Gauss5p, "try-error"))
          {
            equalcdG <- TRUE
            Gauss <- Gauss4p
            AICGaussi <- round(AIC(Gauss4p, k = kcrit), digits = AICdigits)
          } else
            #### convergence only of Gauss5p
            if ((!inherits(Gauss5p, "try-error")) & inherits(Gauss4p, "try-error"))
            {
              Gauss <- Gauss5p
              AICGaussi <- round(AIC(Gauss5p, k = kcrit), digits = AICdigits)
            }
    } else (AICGaussi <- Inf)
    
    ######### Fit of the linear model ############################    
    if (keeplin)
    {
      lin <- lm(signal ~ dose, data = dset)
      AIClini <- round(AIC(lin, k = kcrit), digits = AICdigits)
    } else (AIClii <- Inf)
    
    
    ######## Fit of the null model (constant) ###########################
    constmodel <- lm(signal ~ 1, data = dset)
    AICconsti <-  round(AIC(constmodel, k = kcrit), digits = AICdigits)
    
    # Choice of the best fit
    AICvec <- c(AICGaussi, AICLGaussi, AICHilli, AICLprobiti, AICExpoi, AIClini)
    # Order in the default choice you want in case of equality of AIC values
    indmodeli <- which.min(AICvec)
    AICmin <- AICvec[indmodeli]
    if (AICmin > AICconsti - 2) # we keep the null model
    {
      fit <- constmodel
      indmodeli <- 7 # constant model
      nbpari <- 1
      b.i <- NA
      c.i <- mean(dset$signal)
      d.i <- NA
      e.i <- NA
      f.i <- NA
      SDres.i <- sigma(constmodel)
    } else
    {
      if (indmodeli == 1)
      {
        fit <- Gauss
        par <- coef(fit)
        b.i <- par["b"]
        c.i <- ifelse(equalcdG, par["d"], par["c"])
        d.i <- par["d"]
        e.i <- par["e"]
        f.i <- par["f"]
        SDres.i <- sigma(fit)
        nbpari <- ifelse(equalcdG, 4, 5)
      } else
        if (indmodeli == 2)
        {
          fit <- LGauss
          par <- coef(fit)
          b.i <- par["b"]
          c.i <- ifelse(equalcdLG, par["d"], par["c"])
          d.i <- par["d"]
          e.i <- par["e"]
          f.i <- par["f"]
          SDres.i <- sigma(fit)
          nbpari <- ifelse(equalcdLG, 4, 5)
        } else
          if (indmodeli == 3)
          {
            fit <- Hill
            par <- coef(fit)
            b.i <- par["b"]
            c.i <- par["c"]
            d.i <- par["d"]
            e.i <- par["e"]
            f.i <- NA
            SDres.i <- sigma(fit)
            nbpari <- 4
          } else
            if (indmodeli == 4)
            {
              fit <- Lprobit
              par <- coef(fit)
              b.i <- par["b"]
              c.i <- par["c"]
              d.i <- par["d"]
              e.i <- par["e"]
              f.i <- 0 # to enable the use of the LGauss function to plot the model and calculate the BMD 
              SDres.i <- sigma(fit)
              nbpari <- 4
            } else
              if (indmodeli == 5)
              {
                fit <- Expo
                par <- coef(fit)
                b.i <- par["b"]
                c.i <- NA
                d.i <- par["d"]
                e.i <- par["e"]
                f.i <- NA
                SDres.i <- sigma(fit)
                nbpari <- 3
              } else
                if (indmodeli == 6)
                {
                  fit <- lin
                  par <- coef(fit)
                  b.i <- par[2]
                  c.i <- NA
                  d.i <- par[1]
                  e.i <- NA
                  f.i <- NA
                  SDres.i <- sigma(fit)
                  nbpari <- 2
                } 
    }
    
    # diagnostics on residuals (quadratic trend on residuals) 
    resi <- residuals(fit)
    modquad.resi <- lm(resi ~ doseranks + I(doseranks^2))
    mod0.resi <- lm(resi ~ 1)
    trendPi <- anova(modquad.resi, mod0.resi)[[6]][2]
    
    
    if (progressbar)
    {
      setTxtProgressBar(pb, i)
    }
    
    return(c(indmodeli, nbpari, b.i, c.i, d.i, e.i, f.i, SDres.i,
             AIClini, AICExpoi, AICHilli, AICLprobiti, AICLGaussi, 
             AICGaussi,trendPi))
    
  } ##################################### and of fitoneitem
  
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
  colnames(dres) <- c("model", "nbpar", "b", "c", "d", "e", "f", "SDres",
                      "AIC.L", "AIC.E", "AIC.H", "AIC.lP", "AIC.lGP", "AIC.GP",
                      "trendP")

  dres <- cbind(data.frame(id = row.names(data)[selectindex], 
                           irow = selectindex, 
                           adjpvalue = adjpvalue),
                           dres)
    
  # removing of null models (const, model no 7) and 
  # fits eliminated by the quadratic trend test on residuals
  lines.success <- (dres$model != 7) & 
    ((dres$trendP > 0.05) | is.na(dres$trendP))
  # is.na(trendP because anova of two models with very close RSS
  # may return NA for pvalue)
  
  # dres <- dres[(dres$model != 7) & 
  #                ((dres$trendP > 0.05) | is.na(dres$trendP)) , ]
  dres.failure <- dres[!lines.success, ]
  dfail <- dres.failure[, c("id", "irow", "adjpvalue")]
  dfail$cause <- character(length = nrow(dfail))
  dfail$cause[dres.failure$model == 7] <- "constant.model"
  dfail$cause[dres.failure$model != 7] <- "trend.in.residuals"
  
  dres <- dres[lines.success, ]

  # update of nselect
  nselect <- nrow(dres)
  
  dc <- dres[, c("id", "irow", "adjpvalue", "model", "nbpar", "b", "c", "d", "e", "f", "SDres")]

  # Model names in the order of indmodel
  modelnames <- c("Gauss-probit", "log-Gauss-probit", "Hill", "log-probit", "exponential", "linear")
  dc$model <- modelnames[dc$model] 
  
  # Calculation of the theoretical value at the control : y0
  # of the theoretical signal range on the range of tested concentration : yrange
  # of the x-value that corresponds to the extremum for U and bell curves : xextrem
  ##################################################################################
  y0 <- numeric(length = nselect)
  yrange <- numeric(length = nselect)
  xextrem <- numeric(length = nselect)
  xextrem[1:nselect] <- NA # will remain at NA for monotonic curves
  yextrem <- numeric(length = nselect)
  yextrem[1:nselect] <- NA # will remain at NA for monotonic curves
  
  # calculation of y0 and yrange for linear curves
  indlin <- which(dc$model == "linear")
  vb <- dc$b[indlin]
  vd <- dc$d[indlin]
  yrange[indlin] <- abs(flin(dosemin, vb, vd) - flin(dosemax, vb, vd))
  y0[indlin] <- vd

  # calculation of y0 and yrange for exponential curves
  indExpo <- which(dc$model == "exponential")
  vb <- dc$b[indExpo]
  vd <- dc$d[indExpo]
  ve <- dc$e[indExpo]
  yrange[indExpo] <- 
    abs(fExpo(dosemin, vb, vd, ve) - fExpo(dosemax, vb, vd, ve))
  y0[indExpo] <- vd
  
  # calculation of y0 and yrange for Hill curves
  indHill <- which(dc$model == "Hill")
  vb <- dc$b[indHill]
  vc <- dc$c[indHill]
  vd <- dc$d[indHill]
  ve <- dc$e[indHill]
  yrange[indHill] <- 
    abs(fHill(dosemin, vb, vc, vd, ve) - fHill(dosemax, vb, vc, vd, ve))
  y0[indHill] <- vd
  
  # calculation of y0, xextrem and yrange for Gauss-probit curves
  indGP <- which(dc$model == "Gauss-probit")
  vb <- dc$b[indGP]
  vc <- dc$c[indGP]
  vd <- dc$d[indGP]
  ve <- dc$e[indGP]
  vf <- dc$f[indGP]
  xextr <- xextrem[indGP] <- ve + (vc - vd)*vb/(vf*sqrt(2*pi)) 
  yextr <- yextrem[indGP] <- fGauss5p(xextr, vb, vc, vd, ve, vf)
  yrange[indGP] <- pmax(
    abs(fGauss5p(dosemin, vb, vc, vd, ve, vf) - yextr),
    abs(yextr - fGauss5p(dosemax, vb, vc, vd, ve, vf))
  )
  y0[indGP] <- fGauss5p(0, vb, vc, vd, ve, vf)

  # calculation of y0, xextrem and yrange for log-Gauss-probit and log-probit curves
  indlGP <- which(dc$model == "log-Gauss-probit" | dc$model == "log-probit")
  vb <- dc$b[indlGP]
  vc <- dc$c[indlGP]
  vd <- dc$d[indlGP]
  ve <- dc$e[indlGP]
  vf <- dc$f[indlGP]
  xextr <- xextrem[indlGP] <- exp(log(ve) + (vc - vd)*vb/(vf*sqrt(2*pi))) 
  yextr <- yextrem[indlGP] <- fLGauss5p(xextr, vb, vc, vd, ve, vf) 
  yrange[indlGP] <- pmax(
    abs(fLGauss5p(dosemin, vb, vc, vd, ve, vf) - yextr),
    abs(yextr - fLGauss5p(dosemax, vb, vc, vd, ve, vf))
  )
  y0[indlGP] <- vd
  
  # definition of the typology
  typology <- character(length = nselect)
  trend <- character(length = nselect)
 # pf <- 0.1 # if abs(f) < pf * abs(c - d) gaussian trends are considered roughly monotonous
  
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
                if (di$model == "log-probit" & di$c > di$d) 
                  {typology[i] <- "lP.inc"
                    trend[i] <- "inc"} else
                  if (di$model == "log-probit" & di$c <= di$d) 
                    {typology[i] <- "lP.dec"
                    trend[i] <- "dec"} else
                    if (di$model == "log-Gauss-probit" & di$f < 0) 
                      {typology[i] <- "lGP.U"
                      trend[i] <- "U"
                        #ifelse(abs(di$f) > pf * abs(di$d - di$c), "U", ifelse(di$c > di$d, "inc", "dec"))
                      } else
                      if (di$model == "log-Gauss-probit" & di$f >=0) 
                        {typology[i] <- "lGP.bell"
                        trend[i] <- "bell"
                          #ifelse(abs(di$f) > pf * abs(di$d - di$c), "bell", ifelse(di$c > di$d, "inc", "dec"))
                          } else
                        if (di$model == "Gauss-probit" & di$f < 0) 
                          {typology[i] <- "GP.U"
                          trend[i] <- "U"
                            #ifelse(abs(di$f) > pf * abs(di$d - di$c), "U", ifelse(di$c > di$d, "inc", "dec"))
                            } else
                          if (di$model == "Gauss-probit" & di$f >=0) 
                            {typology[i] <- "GP.bell"
                            trend[i] <- "bell"
                              #ifelse(abs(di$f) > pf * abs(di$d - di$c), "bell", ifelse(di$c > di$d, "inc", "dec"))
                              } else
                            if (di$model == "linear" & di$b > 0) 
                              {typology[i] <- "L.inc"
                              trend[i] <- "inc"} else
                              if (di$model == "linear" & di$b <= 0) 
                                {typology[i] <- "L.dec"
                                trend[i] <- "dec"} 
  }
  dc$typology <- typology
  
  # correction of the trend for Gauss-probit curves with xextrem == 0
  indnullxextr <- which((dc$model == "Gauss-probit") & (xextrem == 0))
  trend[indnullxextr] <- ifelse(dc$f[indnullxextr] > 0, "dec", "inc") 
  
  dc$trend <- factor(trend)
  dc$y0 <- y0
  dc$yrange <- yrange
  dc$xextrem <- xextrem
  dc$yextrem <- yextrem
  
  # number of null models
  n.failure <- length(itemselect$selectindex) - nrow(dc)
  
  if (sigmoid.model == "Hill")
  {
    dc$model <- factor(dc$model, # to specify the order
                       levels = c("Hill", "linear", "exponential", "Gauss-probit", "log-Gauss-probit"))
    dc$typology <- factor(dc$typology,
                          levels = c("H.inc", "H.dec", "L.inc", "L.dec", 
                                     "E.inc.convex","E.dec.concave", "E.inc.concave", "E.dec.convex",
                                     "GP.U", "GP.bell", "lGP.U", "lGP.bell"))
    dAIC <- dres[, c("AIC.L", "AIC.E", "AIC.H", "AIC.lGP", "AIC.GP")] 
  } else
  {
    dc$model <- factor(dc$model, # to specify the order
                       levels = c("log-probit", "linear", "exponential", "Gauss-probit", "log-Gauss-probit")) 
    dc$typology <- factor(dc$typology,
                          levels = c("lP.inc", "lP.dec", "L.inc", "L.dec", 
                                     "E.inc.convex","E.dec.concave", "E.inc.concave", "E.dec.convex",
                                     "GP.U", "GP.bell", "lGP.U", "lGP.bell"))
    dAIC <- dres[, c("AIC.L", "AIC.E", "AIC.lP", "AIC.lGP", "AIC.GP")] 
  }
  
  # Plot of fitted DRCs
  if(saveplot2pdf) 
  {
    pathToFigs <- tempdir()
    pdf(paste0(pathToFigs, "/drcfitplot.pdf"), width = 7, height = 10) # w and h in inches
    message(strwrap(prefix = "\n", initial = "\n",
      paste0("Figures are stored in ", pathToFigs, ". This directory is temporary. It will be removed when the R session is closed.")))
    plotfit(dc, 
            dose = dose, 
            data = data, 
            data.mean = data.mean, 
            xlog10 = FALSE, 
            allpoints = TRUE)
    dev.off()
  }
  
  reslist <- list(fitres = dc, omicdata = itemselect$omicdata,  
                  information.criterion = information.criterion, information.criterion.val = dAIC,
                  n.failure = n.failure, unfitres = dfail) 
  
  return(structure(reslist, class = "drcfit"))
}

print.drcfit <- function(x, ...)
{
  if (!inherits(x, "drcfit"))
    stop("Use only with 'drcfit' objects.")
  
  ttrend <- table(x$fitres$trend)
  tfit <- table(x$fitres$model)
  nsucces <- nrow(x$fitres)
  nfirstselect <- x$n.failure + nsucces
  if (x$n.failure > 0)
    cat(x$n.failure,"dose-response curves out of ",nfirstselect, " previously selected were removed
        because no model could be fitted reliably.\n")
  cat("Distribution of the chosen models among the ",nsucces," fitted dose-response curves :\n")
  print(tfit)
  cat("Distribution of the trends (curve shapes) among the ",nsucces," fitted dose-response curves :\n")
  print(ttrend)
  ttypology <- table(x$fitres$typology)
  cat("Distribution of the typology of the ",nsucces," fitted dose-response curves :\n")
  print(ttypology)
}

plot.drcfit <- function(x, items, 
                plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"), 
                dose_log_transfo = FALSE, ...)
{
  if (!inherits(x, "drcfit"))
    stop("Use only with 'drcfit' objects.")
  
  # a ggplot alternative
  if(missing(items))
  {
    items <- 20
  }
  if(!( is.numeric(items) | is.character(items)) )
    stop("Wrong argument 'items'. It must be a number inferior or equal to 20 or
    a character vector indicating the identifiers of the items who want to plot.")
  if (is.numeric(items))
  {
    subd <- x$fitres[1:min(nrow(x$fitres),items), ]
  } else
  if (is.character(items))
  {
    inditems <- match(items, x$fitres$id)
    if (any(is.na(inditems)))
    stop("At least one of the chosen items was not selected as responding. You should use targetplot() in that case.")
    subd <- x$fitres[inditems, ]
  }
  plotfitsubset(subd, 
                dose = x$omicdata$dose, 
                data = x$omicdata$data, 
                data.mean = x$omicdata$data.mean, 
                npts = 500,
                plot.type = plot.type, 
                dose_log_transfo = dose_log_transfo) 
  
}

plotfit2pdf <- function(x, items, 
                        plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"), 
                        dose_log_transfo = FALSE, nrowperpage = 6, ncolperpage = 4)
{
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
    subd <- x$fitres[1:min(nrow(x$fitres),items), ]
  } else
  if (is.character(items))
  {
    inditems <- match(items, x$fitres$id)
    if (any(is.na(inditems)))
        stop("At least one of the chosen items was not selected as responding. You should use targetplot() in that case.")
      subd <- x$fitres[inditems, ]
  }
  
  pathToFigs <- tempdir()
  file2plot <- paste0(pathToFigs, "/drcfitplot.pdf")
  message(strwrap(prefix = "\n", initial = "\n",
                  paste0("Figures are stored in ", pathToFigs, ". This directory is temporary. It will be removed when the R session is closed.")))
  pdf(file2plot, width = 7, height = 10, onefile = TRUE) # w and h in inches
  
  nplotsperpage = nrowperpage * ncolperpage
  npage <- ceiling(nrow(subd) / nplotsperpage)
  
  for (i in 1:npage)
  {
    if (i == npage) indmax <- nrow(subd) else indmax <- i*nplotsperpage
    g <- plotfitsubset(subd[seq((i-1)*nplotsperpage + 1,indmax, 1), ], 
                                 dose = x$omicdata$dose, 
                                 data = x$omicdata$data, 
                                 data.mean = x$omicdata$data.mean, 
                                 npts = 500,
                                 plot.type = plot.type, 
                                 dose_log_transfo = dose_log_transfo, 
                                  nr = nrowperpage, 
                                  nc = ncolperpage) 
    print(g)
  }
  dev.off()
  
}


