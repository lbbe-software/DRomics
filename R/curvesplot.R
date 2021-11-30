# Plot of fitted curves using columns of on extended dataframe to optionnally code 
# for color and or facet 
curvesplot <- function(extendedres, xmin = 0, xmax, y0shift = TRUE, 
                       facetby, facetby2, free.y.scales = FALSE, 
                       ncol4faceting, colorby, removelegend = FALSE,  
                        npoints = 500, line.size = 0.2, line.alpha = 1,
                       dose_log_transfo = FALSE)
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of curvesplot must be a dataframe 
    (see ?curvesplot for details).")

  cnames <- colnames(extendedres)
  if (any(!is.element(c("id", "model", "b", "c", "d", "e", "f"), cnames)))
    stop("The first argument of curvesplot must be a dataframe
    containing at least columns named id, model, b, c, d, e and f.")
  
    if (missing(xmax)) 
    stop("xmax must be given. You can fix it at max(f$omicdata$dose)} 
    with f the output of drcfit().")
  if (dose_log_transfo)
  {
    if (xmin == 0)
      stop("When using a log scale for the dose, a strictly positive value must be given for xmin.")
    x2plot <- 10^seq(log10(xmin), log10(xmax), length.out = npoints)
  } else
  {
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
                             d = extendedres$d[i]) - extendedres$y0[i]*y0shift
    } else
      if (modeli == "exponential")
      {
        curves2plot$y[(i-1)*npoints + 1:npoints] <- fExpo(x2plot, b = extendedres$b[i], 
                       d = extendedres$d[i], e = extendedres$e[i]) - extendedres$y0[i]*y0shift
      } else
        if (modeli == "Hill")
        {
          curves2plot$y[(i-1)*npoints + 1:npoints] <- fHill(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i]) - extendedres$y0[i]*y0shift
        } else
          if (modeli == "Gauss-probit")
          {
            curves2plot$y[(i-1)*npoints + 1:npoints] <- fGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                       d = extendedres$d[i], e = extendedres$e[i], 
                       f = extendedres$f[i]) - extendedres$y0[i]*y0shift
          } else
            if (modeli == "log-Gauss-probit")
            {
              curves2plot$y[(i-1)*npoints + 1:npoints] <- fLGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i], 
                      f = extendedres$f[i]) - extendedres$y0[i]*y0shift
            }
  }
  
  # no color no facet
  if (missing(colorby) & missing(facetby))
  {
    gg <- ggplot(data = curves2plot, mapping = aes_(x = quote(x), y = quote(y), group = quote(id))) +
      geom_line(size = line.size, alpha = line.alpha) 
  } else
    # facet only
    if (missing(colorby))
    { 
      curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
      gg <- ggplot(data = curves2plot, mapping = aes_(x = quote(x), y = quote(y), group = quote(id))) +
        geom_line(size = line.size, alpha = line.alpha) 
      # + 
      #   facet_wrap(~ facetby, scales = scales.arg) 
    } else
      # color only
      if (missing(facetby))
      {
        curves2plot$colorby <- rep(extendedres[, colorby], each = npoints)
        gg <- ggplot(data = curves2plot, mapping = aes_(x = quote(x), y = quote(y), group = quote(id), colour = quote(colorby))) +
          geom_line(size = line.size, alpha = line.alpha)  
      } else
        # color and facet
      {
        curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
        curves2plot$colorby <- rep(extendedres[, colorby], each = npoints)
        gg <- ggplot(data = curves2plot, mapping = aes_(x = quote(x), y = quote(y), group = quote(id), colour = quote(colorby))) +
          geom_line(size = line.size, alpha = line.alpha)   
      }
  if (!missing(facetby))
  {
    if (!missing(facetby2)) 
    {
      gg <- gg + facet_grid(facetby2 ~ facetby, scales = scales.arg) 
    }
    else
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
  
  return(gg)
}