# Function used in plot.drcfit()
# uses ggplot2
plotfitsubset <- function(subd, dose, data, data.mean, npts = 50, 
                        plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"),
                        dose_log_transfo = FALSE, nr = NULL, nc = NULL)
{
  plot.type <- match.arg(plot.type, c("dose_fitted", "dose_residuals", "fitted_residuals"))
  
  if ((dose_log_transfo) & (plot.type == "fitted_residuals"))
  {
    warning(strwrap(prefix = "\n", initial = "\n", 
      "The log transformation of the dose axis cannot be used for 
      this type of plot: residuals as fonction of fitted values."))
  }
  
  lev <- if((!is.null(nr) & !is.null(nc)) && (length(subd$id) < (nr * nc))) {c(subd$id, strrep(" ", 1:(nr * nc - length(subd$id))))} else {subd$id}
  
######################### Dose_fitted plot ##########################
  if (plot.type == "dose_fitted")
  {
    nobs <- length(dose)
    doseu <- as.numeric(colnames(data.mean)) # sorted unique doses
    ndose <- length(doseu)
    
    if (dose_log_transfo)
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
    if (dose_log_transfo)
    {
      datatheo0 <- data.frame(dose = numeric(), signal = numeric(), 
                              id = character())
    }
    
    for (i in 1:nitems)
    {
      irow <- subd$irow[i]
      ident <- lev[i]
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
      
      if (dose_log_transfo)
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
    
    dataobs$id <- factor(dataobs$id, levels = lev)
    dataobsmean$id <- factor(dataobsmean$id, levels = lev)
    
    g <- ggplot(dataobs, aes_(x = quote(dose), y = quote(signal))) + geom_point(shape = 1) +
      facet_wrap(~ id, scales = "free_y", nrow = nr, ncol = nc, drop = FALSE) +
      geom_point(data = dataobsmean, shape = 19)
    
    
    datatheo$id <- factor(datatheo$id, levels = lev)
    if (dose_log_transfo) 
    {
      datatheo0$id <- factor(datatheo0$id, levels = lev)
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
      ident <- lev[i]
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
    
    dataresiduals$id <- factor(dataresiduals$id, levels = lev)
    if (plot.type == "dose_residuals")
    {
      g <- ggplot(dataresiduals, aes_(x = quote(dose), y = quote(residuals))) + 
        geom_point(shape = 1) +
        facet_wrap(~ id, nrow = nr, ncol = nc, drop = FALSE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red")
      if (dose_log_transfo)
      {
        g <- g + scale_x_log10()
      }
      
      
    } else
    if (plot.type == "fitted_residuals")
    {
      g <- ggplot(dataresiduals, aes_(x = quote(fitted_values), y = quote(residuals))) + 
        geom_point(shape = 1) +
        facet_wrap(~ id, scales = "free_x", nrow = nr, ncol = nc, drop = FALSE) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red")
      
    }
  }
  
  return(g)
}



