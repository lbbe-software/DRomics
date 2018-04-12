plotfit <- function(subd, dose, data, data.mean, pmfrow = c(7,5), pmar = c(2.2, 2.2, 2.2, 0.7),
                    xlog10 = FALSE, npts = 500, allpoints = TRUE, 
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
        segments(x0=log10(subd$BMD.xfold[i]), x1=log10(subd$BMD.xfold[i]), y0=0, y1=subd$yp[i], col = colBMD.xfold)
        segments(x0=-1, x1=log10(subd$BMD.xfold[i]), y0=subd$yp[i], y1=subd$yp[i], col = colBMD.xfold)
      }
      if (addBMD.SD)
      {
        segments(x0=log10(subd$BMD.SD[i]), x1=log10(subd$BMD.SD[i]), y0=0, y1=subd$ysd[i], col = colBMD.SD)
        segments(x0=-1, x1=log10(subd$BMD.SD[i]), y0=subd$ysd[i], y1=subd$ysd[i], col = colBMD.SD)
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




