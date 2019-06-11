# Plot of fitted curves using columns of on extended dataframe to optionnally code 
# for color and or facet 
curvesplot <- function(extendedres, xmin = 0, xmax, y0shift = TRUE, 
                       facetby, colorby, removelegend = FALSE,  
                        npoints = 50, line.size = 0.2, line.alpha = 1)
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of curvesplot must be a dataframe 
         (see ?curvesplot for details).")
  # ajouter un test sur le nom des colonnes indispensables !!!!!!!!!!!!!!!!!!!!!!!
  if (missing(xmax)) 
    stop("xmax must be given. You can fix it at max(f$omicdata$dose)} 
         with f the output of drcfit()")
  x2plot <- seq(xmin, xmax, length.out = npoints)
  ns <- nrow(extendedres)
  N <- ns * npoints
  
  curves2plot <- data.frame(x = rep(x2plot, ns), 
                            id = rep(extendedres$id, each = npoints),
                            y = numeric(length = N))
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
            if ((modeli == "log-Gauss-probit") | (modeli == "log-probit"))
            {
              curves2plot$y[(i-1)*npoints + 1:npoints] <- fLGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i], 
                      f = extendedres$f[i]) - extendedres$y0[i]*y0shift
            }
  }
  
  # no color no facet
  if (missing(colorby) & missing(facetby))
  {
    gg <- ggplot(data = curves2plot, aes(x, y, group = id)) +
      geom_line(size = line.size, alpha = line.alpha) 
  } else
    # facet only
    if (missing(colorby))
    { 
      if (!is.character(facetby)) 
        stop("facetby should be a character string for the name of the column used for facetting")
      curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
      gg <- ggplot(data = curves2plot, aes(x, y, group = id)) +
        geom_line(size = line.size, alpha = line.alpha) + 
        facet_wrap(~ facetby) 
    } else
      # color only
      if (missing(facetby))
      {
        if (!is.character(colorby)) 
          stop("colorby should be a character string for the name of the column used for coloring curves")
        curves2plot$colorby <- rep(extendedres[, colorby], each = npoints)
        gg <- ggplot(data = curves2plot, aes(x, y, group = id, colour = colorby)) +
          geom_line(size = line.size, alpha = line.alpha)  
      } else
        # color and facet
      {
        if (!is.character(facetby)) 
          stop("facetby should be a character string for the name of the column used for facetting")
        if (!is.character(colorby)) 
          stop("colorby should be a character string for the name of the column used for coloring curves")
        curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
        curves2plot$colorby <- rep(extendedres[, colorby], each = npoints)
        gg <- ggplot(data = curves2plot, aes(x, y, group = id, colour = colorby)) +
          geom_line(size = line.size, alpha = line.alpha) + facet_wrap(~ facetby)  
      }
  if (removelegend) gg <- gg + theme(legend.position = "none") 
  return(gg)
}