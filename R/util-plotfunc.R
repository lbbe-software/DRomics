# Function used in plot.drcfit()
# uses ggplot2
plotfitsubset <- function(subd, dose, data, data.mean, npts = 50, 
                        plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"),
                        dose_pseudo_log_transfo = FALSE)
{
  plot.type <- match.arg(plot.type, c("dose_fitted", "dose_residuals", "fitted_residuals"))
  
  if ((dose_pseudo_log_transfo) & (plot.type == "fitted_residuals"))
  {
    warning("The pseudo-log transformation of the dose axis cannot be used for 
              this type of plot: residuals as fonction of fitted values")
  }
  
######################### Dose_fitted plot ##########################
  if (plot.type == "dose_fitted")
  {
    nobs <- length(dose)
    doseu <- as.numeric(colnames(data.mean)) # sorted unique doses
    ndose <- length(doseu)
    
    if (dose_pseudo_log_transfo)
    {
      minx <- min(dose[dose != 0]) 
      maxx <- max(dose)
      xplot <- c(0, 10^seq(log10(minx), log10(maxx), length.out = npts))
    } else
    {
      xplot <- seq(0, max(dose), length.out = npts)
    }
    nitems <- nrow(subd)
    dataobs <- data.frame(dose = numeric(), signal = numeric(), 
                          id = character())
    dataobsmean <- data.frame(dose = numeric(), signal = numeric(), 
                              id = character())
    datatheo <- data.frame(dose = numeric(), signal = numeric(), 
                           id = character())
    if (dose_pseudo_log_transfo)
    {
      datatheo0 <- data.frame(dose = numeric(), signal = numeric(), 
                              id = character())
    }
    
    for (i in 1:nitems)
    {
      irow <- subd$irow[i]
      ident <- subd$id[i]
      datai <- data[irow, ]
      datameani <- data.mean[irow, ]
      # fitted curves
      if (subd$model[i] == "exponential") datapred <- fExpo(x = xplot, d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i]== "Hill") datapred <- fHill(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i]== "log-Gauss-probit" | subd$model[i]== "log-probit") datapred <- fLGauss5p(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i]== "Gauss-probit") datapred <- fGauss5p(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i]== "linear") datapred <- xplot * subd$b[i] + subd$d[i]
      if (subd$model[i]== "const") datapred <- rep(mean(datai), length(xplot))
      
      dataobs <- rbind(dataobs, 
                       data.frame(dose = dose, signal = datai, id = rep(ident, nobs)))
      dataobsmean <- rbind(dataobsmean, 
                           data.frame(dose = doseu, signal = datameani, id = rep(ident, ndose)))
      
      if (dose_pseudo_log_transfo)
      {
        datatheo <- rbind(datatheo,
                          data.frame(dose = xplot[-1], signal = datapred[-1], id = rep(ident, npts)))
        datatheo0 <- rbind(datatheo0,
                           data.frame(dose = xplot[1], signal = datapred[1], id = ident))
        
      } else
      {
        datatheo <- rbind(datatheo,
                          data.frame(dose = xplot, signal = datapred, id = rep(ident, npts)))
      }
    }
    
    dataobs$id <- factor(dataobs$id, levels = subd$id)
    dataobsmean$id <- factor(dataobsmean$id, levels = subd$id)
    
    g <- ggplot(dataobs, aes_(x = quote(dose), y = quote(signal))) + geom_point(shape = 1) +
      facet_wrap(~ id, scales = "free_y") +
      geom_point(data = dataobsmean, shape = 19)
    
    
    datatheo$id <- factor(datatheo$id, levels = subd$id)
    if (dose_pseudo_log_transfo) 
    {
      datatheo0$id <- factor(datatheo0$id, levels = subd$id)
      g <- g + geom_line(data = datatheo, colour = "red") +
        geom_point(data = datatheo0, colour = "red") 
      g <- g + scale_x_log10()
    } else
    {
      g <- g + geom_line(data = datatheo, colour = "red") 
    }
  } else
######################### residuals plots ##########################
  {
    nobs <- length(dose)
    xplot <- dose
    nitems <- nrow(subd)
    dataresiduals <- data.frame(dose = numeric(), residuals = numeric(), 
                                fitted = numeric(), id = character())
    for (i in 1:nitems)
    {
      irow <- subd$irow[i]
      ident <- subd$id[i]
      datai <- data[irow, ]
      # fitted curves
      if (subd$model[i] == "exponential") datapred <- fExpo(x = xplot, d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i]== "Hill") datapred <- fHill(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i]== "log-Gauss-probit" | subd$model[i]== "log-probit") datapred <- fLGauss5p(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i]== "Gauss-probit") datapred <- fGauss5p(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i]== "linear") datapred <- xplot * subd$b[i] + subd$d[i]
      if (subd$model[i]== "const") datapred <- rep(mean(datai), length(xplot))
      
      dataresiduals <- rbind(dataresiduals, 
                       data.frame(dose = dose, residuals = datai - datapred, 
                                  fitted_values = datapred, id = rep(ident, nobs)))
    }
    
    dataresiduals$id <- factor(dataresiduals$id, levels = subd$id)
    if (plot.type == "dose_residuals")
    {
      g <- ggplot(dataresiduals, aes_(x = quote(dose), y = quote(residuals))) + 
        geom_point(shape = 1) +
        facet_wrap(~ id) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "red")
      if (dose_pseudo_log_transfo)
      {
        g <- g + scale_x_log10()
      }
      
      
    } else
    if (plot.type == "fitted_residuals")
    {
      g <- ggplot(dataresiduals, aes_(x = quote(fitted_values), y = quote(residuals))) + 
        geom_point(shape = 1) +
        facet_wrap(~ id, scales = "free_x") + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "red")
      
    }
  }
  
  return(g)
}

# Function used to generate the pdf file in drcfit when saveplot2pdf == TRUE
# uses graphics
plotfit <- function(subd, dose, data, data.mean, pmfrow = c(7,5), pmar = c(2.2, 2.2, 2.2, 0.7),
                    xlog10 = FALSE, npts = 100, allpoints = TRUE, 
                    addBMD.xfold = FALSE, addBMD.SD = FALSE, view.axes = TRUE)
{
  def.par <- par(no.readonly = TRUE)
  par(mfrow = pmfrow) 
  if (view.axes)
  {
    par(mar = pmar)
  } else
  {
    par(mar = c(0.2, 0.2, 2.2, 0.2))
  }
  
  doseu <- as.numeric(colnames(data.mean)) # sorted unique doses
  
  if (xlog10)
  {
    log10dosecoru <- log10doseu <- log10(doseu)
    log10dosecor <- log10dose <- log10(dose)
    valuelog0 <- log10doseu[2] - 3*(log10doseu[3] - log10doseu[2])
    log10dosecor[log10dose == -Inf] <- valuelog0
    log10dosecoru[log10dosecoru == -Inf] <- valuelog0
    
    xplot <- seq(min(log10dosecoru), max(log10dosecoru), length.out = npts)
    for (i in 1:nrow(subd))
    {
      irow <- subd$irow[i]
      datai <- data[irow, ]
      datameani <- data.mean[irow, ]
      
      # fitted curves
      if (subd$model[i] == "exponential") 
        datapred <- fExpo(x = 10^xplot, d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i] == "Hill") 
        datapred <- fHill(x = 10^xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i] == "log-Gauss-probit" | subd$model[i] == "log-probit") 
        datapred <- fLGauss5p(x = 10^xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i] == "Gauss-probit") 
        datapred <- fGauss5p(x = 10^xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i] == "linear") 
        datapred <- 10^xplot * subd$b[i] + subd$d[i]
      if (subd$model[i] == "const") 
        datapred <- rep(mean(datai), length(xplot))
      
      if (allpoints)
      {
        plot(log10dosecor, datai, main = subd$id[i], col = ifelse(dose > 0, "black","grey"))
        lines(xplot, datapred, col = "red")
        points(log10dosecor, datai, col = ifelse(dose > 0, "black","grey"))
        points(log10dosecoru, datameani, pch = 16, col = ifelse(doseu > 0, "black","grey"))
      } else
      {
        plot(log10dosecoru, datameani, pch = 16, main = subd$id[i], col = ifelse(dose > 0, "black","grey"))
        lines(xplot, datapred, col = "red")
        points(log10dosecoru, datameani, pch = 16, col = ifelse(doseu > 0, "black","grey"))
      }
      if (!is.finite(dose))
        
        if (addBMD.xfold)
        {
          segments(x0=log10(subd$BMD.xfold[i]), x1=log10(subd$BMD.xfold[i]), y0=0, y1=subd$yp[i], col = "blue")
          segments(x0=-1, x1=log10(subd$BMD.xfold[i]), y0=subd$yp[i], y1=subd$yp[i], col = "blue")
        }
      if (addBMD.SD)
      {
        segments(x0=log10(subd$BMD.SD[i]), x1=log10(subd$BMD.SD[i]), y0=0, y1=subd$ysd[i], col = "blue")
        segments(x0=-1, x1=log10(subd$BMD.SD[i]), y0=subd$ysd[i], y1=subd$ysd[i], col = "blue")
      }
      
    }
    
  } else
  {
    xplot <- seq(0, max(dose), length.out = npts)
    for (i in 1:nrow(subd))
    {
      irow <- subd$irow[i]
      datai <- data[irow, ]
      datameani <- data.mean[irow, ]
      # fitted curves
      if (subd$model[i] == "exponential") datapred <- fExpo(x = xplot, d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i]== "Hill") datapred <- fHill(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i])
      if (subd$model[i]== "log-Gauss-probit" | subd$model[i]== "log-probit") datapred <- fLGauss5p(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i]== "Gauss-probit") datapred <- fGauss5p(x = xplot, c = subd$c[i], d = subd$d[i], b = subd$b[i], e = subd$e[i], f = subd$f[i])
      if (subd$model[i]== "linear") datapred <- xplot * subd$b[i] + subd$d[i]
      if (subd$model[i]== "const") datapred <- rep(mean(datai), length(xplot))
      
      if (allpoints)
      {
        plot(dose, datai, main = subd$id[i])
        lines(xplot, datapred, col = "red")
        points(dose, datai)
        points(doseu, datameani, pch = 16)
      } else
      {
        plot(doseu, datameani, main = subd$id[i], pch = 16)
        lines(xplot, datapred, col = "red")
        points(doseu, datameani, main = subd$id[i], pch = 16)
      }
      
      
      if (addBMD.xfold)
      {
        segments(x0=subd$BMD.xfold[i], x1=subd$BMD.xfold[i], y0=0, y1=subd$yp[i], col = "blue")
        segments(x0=0, x1=subd$BMD.xfold[i], y0=subd$yp[i], y1=subd$yp[i], col = "blue")
      }
      if (addBMD.SD)
      {
        segments(x0=subd$BMD.SD[i], x1=subd$BMD.SD[i], y0=0, y1=subd$ysd[i], col = "green")
        segments(x0=0, x1=subd$BMD.SD[i], y0=subd$ysd[i], y1=subd$ysd[i], col = "grenn")
      }
    }
  }
  par(def.par)    
  
}


