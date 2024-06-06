# Plot of fitted curves using columns of on extended dataframe to optionnally code 
# for color and or facet 
curvesplot <- function(extendedres, xmin, xmax, 
                       y0shift = TRUE, scaling = TRUE,
                       facetby, facetby2, free.y.scales = FALSE, 
                       ncol4faceting, colorby, removelegend = FALSE,  
                        npoints = 500, line.size = 0.5, line.alpha = 0.8,
                       dose_log_transfo = TRUE,
                       addBMD = TRUE, BMDtype = c("zSD", "xfold"), 
                       point.size = 1, point.alpha = 0.8)
{
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
    
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of curvesplot must be a dataframe 
    (see ?curvesplot for details).")
  if(scaling & !y0shift)
  {
    y0shift <- TRUE
    warning(strwrap(prefix = "\n", initial = "\n",
        "y0shift is forced to TRUE when scaling is TRUE."))
  }

  cnames <- colnames(extendedres)

  if (BMDtype == "zSD" & !is.element(c("BMD.zSD"), cnames))
  {
    stop("The first argument of curvesplot must be a dataframe
    containing at least columns named id, model, b, c, d, e, f, y0 and maxychange and BMD.zSD.")
    
  } else
  { 
    if (!is.element(c("BMD.xfold"), cnames))
      stop("The first argument of curvesplot must be a dataframe
    containing at least columns named id, model, b, c, d, e, f, y0 and maxychange and BMD.xfold.")
  }

    if (scaling)
  {
    if (any(!is.element(c("id", "model", "b", "c", "d", "e", "f", "y0", "maxychange"), cnames)))
      stop("The first argument of curvesplot must be a dataframe
    containing at least columns named id, model, b, c, d, e, f, y0 and maxychange and BMD.zSD or BMD.xfold.")
  } else
  {
    if (any(!is.element(c("id", "model", "b", "c", "d", "e", "f", "y0"), cnames)))
      stop("The first argument of curvesplot must be a dataframe
    containing at least columns named id, model, b, c, d, e, f and y0 and BMD.zSD or BMD.xfold.")
  }
  
    if (missing(xmax))
    {
      if (BMDtype == "zSD")
      { 
        xmax <- max(extendedres$BMD.zSD, na.rm = FALSE) * 1.1
      } else
      {
        xmax <- max(extendedres$BMD.xfold, na.rm = FALSE) * 1.1
      }
    }
    # stop("xmax must be given. You can fix it at max(f$omicdata$dose)} 
    # with f the output of drcfit().")
      
  if (dose_log_transfo)
  {
    if (missing(xmin))
    {
      if (BMDtype == "zSD")
      { 
        xmin <- min(extendedres$BMD.zSD, na.rm = FALSE) * 0.9
      } else
      {
        xmin <- min(extendedres$BMD.xfold, na.rm = FALSE) * 0.9
      } 
    } else if (xmin <= 0) stop("xmin cannot be fixed at 0 using a dose log scale")
      
     x2plot <- 10^seq(log10(xmin), log10(xmax), length.out = npoints)
  } else
  {
    if (missing(xmin)) xmin <- 0
    x2plot <- seq(xmin, xmax, length.out = npoints)
  }
  
  ns <- nrow(extendedres)
  N <- ns * npoints
  
  if (free.y.scales)
  {
    scales.arg <- "free_y"
  } else
  {
    scales.arg <- "fixed"
  }
  
  curves2plot <- data.frame(x = rep(x2plot, ns), 
                            id = rep(extendedres$id, each = npoints),
                            y = numeric(length = N))
  
  if (!missing(facetby)) 
  {
    if (!is.character(facetby)) 
      stop("facetby should be a character string for the name of the column used for facetting.")
    if (!is.element(facetby, cnames))
      stop("facetby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
    
    if (!missing(facetby2)) 
    {
      if (!is.character(facetby2)) 
        stop("facetby2 should be a character string for the name of the column used for facetting.")
      if (!is.element(facetby2, cnames))
        stop("facetby2 should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
      curves2plot$facetby2 <- rep(extendedres[, facetby2], each = npoints)
    }     
  }
  
  if (!missing(colorby))
  {
    if (!is.character(colorby)) 
      stop("colorby should be a character string for the name of the column coding for the point color.")
    if (!is.element(colorby, cnames))
      stop("colorby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  }
  
    
  for (i in 1:ns)
  {
    modeli <- extendedres$model[i]
    if (modeli == "linear")
    {
      b <- extendedres$b[i]
      d <- extendedres$d[i]
      curves2plot$y[(i-1)*npoints + 1:npoints] <- flin(x2plot, b = extendedres$b[i], 
                             d = extendedres$d[i]) 
    } else
      if (modeli == "exponential")
      {
        curves2plot$y[(i-1)*npoints + 1:npoints] <- fExpo(x2plot, b = extendedres$b[i], 
                       d = extendedres$d[i], e = extendedres$e[i]) 
      } else
        if (modeli == "Hill")
        {
          curves2plot$y[(i-1)*npoints + 1:npoints] <- fHill(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i]) 
        } else
          if (modeli == "Gauss-probit")
          {
            curves2plot$y[(i-1)*npoints + 1:npoints] <- fGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                       d = extendedres$d[i], e = extendedres$e[i], 
                       f = extendedres$f[i]) 
          } else
            if (modeli == "log-Gauss-probit")
            {
              curves2plot$y[(i-1)*npoints + 1:npoints] <- fLGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i], 
                      f = extendedres$f[i]) 
            }
    if (y0shift) 
    {
      if (scaling) 
      {
        curves2plot$y[(i-1)*npoints + 1:npoints] <- 
          (curves2plot$y[(i-1)*npoints + 1:npoints] - extendedres$y0[i]) / extendedres$maxychange[i]
      } else
      {
        curves2plot$y[(i-1)*npoints + 1:npoints] <- 
          curves2plot$y[(i-1)*npoints + 1:npoints] - extendedres$y0[i]
      }
    }
    
  }
  
  # no color no facet
  if (missing(colorby) & missing(facetby))
  {
    gg <- ggplot(data = curves2plot, mapping = aes(x = .data$x, y = .data$y, group = .data$id)) +
      geom_line(linewidth = line.size, alpha = line.alpha) 
  } else
    # facet only
    if (missing(colorby))
    { 
      curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
      gg <- ggplot(data = curves2plot, mapping = aes(x = .data$x, y = .data$y, group = .data$id)) +
        geom_line(linewidth = line.size, alpha = line.alpha) 
      # + 
      #   facet_wrap(~ facetby, scales = scales.arg) 
    } else
      # color only
      if (missing(facetby))
      {
        curves2plot$colorby <- rep(extendedres[, colorby], each = npoints)
        gg <- ggplot(data = curves2plot, mapping = aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colorby)) +
          geom_line(linewidth = line.size, alpha = line.alpha)  
      } else
        # color and facet
      {
        curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
        curves2plot$colorby <- rep(extendedres[, colorby], each = npoints)
        gg <- ggplot(data = curves2plot, mapping = aes(x = .data$x, y = .data$y, group = .data$id, colour = .data$colorby)) +
          geom_line(linewidth = line.size, alpha = line.alpha)   
      }
  if (!missing(facetby))
  {
    if (!missing(facetby2)) 
    {
      gg <- gg + facet_grid(facetby2 ~ facetby, scales = scales.arg) 
    } else
    {
      if (missing(ncol4faceting))
      {
        gg <- gg + facet_wrap(~ facetby, scales = scales.arg) 
      } else
      {
        gg <- gg + facet_wrap(~ facetby, scales = scales.arg, ncol = ncol4faceting) 
      }
    }
  }
  
  
  if (removelegend) gg <- gg + theme(legend.position = "none") 
  
  if (dose_log_transfo)
    gg <- gg + scale_x_log10()

  if (scaling)
  {
    gg <- gg + ylab("scaled signal") + xlab("dose")
  } else
  {
    gg <- gg + ylab("signal") + xlab("dose")
  }

  if (!missing(colorby))
  {
    gg <- gg + labs(color = colorby)
  }
  
  if (addBMD)
  {
    if (BMDtype == "zSD")
    {  
      if (any(!is.element(c("BMD.zSD", "BMR.zSD"), cnames)))
      {
        stop("To add BMD-zSD values on the curves, the first argument of curvesplot must be a dataframe
      containing the columns BMD.zSD and BMR.zSD.")
      }
      BMD2plot <- data.frame(x = extendedres$BMD.zSD, y = extendedres$BMR.zSD, id = extendedres$id)
    } else 
    {
      if (any(!is.element(c("BMD.xfold", "BMR.xfold"), cnames)))
      {
        stop("To add BMD-xfold values on the curves, the first argument of curvesplot must be a dataframe
      containing the columns BMD.xfold and BMR.xfold.")
      }
      BMD2plot <- data.frame(x = extendedres$BMD.xfold, y = extendedres$BMR.xfold, id = extendedres$id)
    }
    
    if (y0shift) 
    {
      if (scaling) 
      {
        BMD2plot$y <- (BMD2plot$y - extendedres$y0) / extendedres$maxychange
      } else
      {
        BMD2plot$y <- (BMD2plot$y - extendedres$y0) 
      }
    }
    
    
    if (!missing(colorby))
    {
      BMD2plot$colorby <- extendedres[, colorby]
    }
    if (!missing(facetby)) 
    {
      BMD2plot$facetby <- extendedres[, facetby]
      if (!missing(facetby2)) 
      {
        BMD2plot$facetby2 <- extendedres[, facetby2]
      }
    }
    gg <- gg + geom_point(data = BMD2plot, size = point.size, alpha = point.alpha)
  }
        

  return(gg)
}