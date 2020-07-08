# Plot of BMD values as an ECDF plot, with an horizontal color gradient coding 
# for the signal level at each dose (or concentration)for each item,
# form an extended results dataframe (e.g. with annotation of items)
# with optionnal use of columns for shape and or facet 
bmdplotwithgradient <- function(extendedres, BMDtype = c("zSD", "xfold"),
                                   xmin = 0, xmax, y0shift = TRUE, 
                                   facetby, shapeby, npoints = 50, 
                                   line.size, point.size = 1,
                                   ncol4faceting, limits4colgradient,
                                   lowercol = "darkgreen", uppercol = "darkred")
{
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))

  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of bmdplotwithgradient must be a dataframe 
         (see ?bmdplotwithgradient for details).")
  
  cnames <- colnames(extendedres)
 
  if (BMDtype == "zSD")
  {  
    if (any(!is.element(c("id", "model", "b", "c", "d", "e", "f", "BMD.zSD"), cnames)))
      stop("The first argument of bmdplotwithgradient must be a dataframe
    containing at least columns named id, model, b, c, d, e, f and BMD.zSD.")
    
    BMD2plot <- data.frame(x = extendedres$BMD.zSD, id = extendedres$id)
  }
  else 
  {
    if (any(!is.element(c("id", "model", "b", "c", "d", "e", "f", "BMD.xfold"), cnames)))
      stop("The first argument of bmdplotwithgradient must be a dataframe
    containing at least columns named id, model, b, c, d, e, f and BMD.xfold")
    
    BMD2plot <- data.frame(x = extendedres$BMD.xfold, id = extendedres$id)
  }
  
  if (missing(xmax)) 
    stop("xmax must be given. You can fix it at max(f$omicdata$dose)} 
         with f the output of drcfit()")
  
  
  # ajouter un test sur le nom des colonnes de extendedres !!!!!!!!!!!!!!!!!!!!!!!
  
  if (!missing(shapeby))
  {
    if (!is.character(shapeby)) 
      stop("shapeby should be a character string for the name of the column coding for the point shape")
    BMD2plot$shapeby <- extendedres[, shapeby]
  }
  
  # calculation of ECDF by facetby
  ntot <- nrow(BMD2plot)
  if (!missing(facetby)) 
  {
    if (!is.character(facetby)) 
      stop("facetby should be a character string for the name of the column used for facetting")
    BMD2plot$facetby <- extendedres[, facetby]

    if (missing(line.size)) line.size <- 24 / max(table(BMD2plot$facetby)) 

    uniqueby <- unique(BMD2plot$facetby)
    n.uniqueby <- length(uniqueby)
    BMD2plot$ECDF <- rep(0, ntot) # initialization
    for (i in 1:n.uniqueby)
    {
      indi <- which(BMD2plot$facetby == uniqueby[i])
      ntoti <- length(indi)
      BMD2plot$ECDF[indi] <- (rank(BMD2plot$x[indi], ties.method = "first") - 0.5) / ntoti
    }
    g <- ggplot(data = BMD2plot, mapping = aes_(x = quote(x), y = quote(ECDF))) + facet_wrap(~ facetby) 
    
  } else
  {
    if (missing(line.size)) line.size <- 24 / nrow(BMD2plot) 
    
    BMD2plot$ECDF <- (rank(BMD2plot$x, ties.method = "first") - 0.5) / ntot
    g <- ggplot(data = BMD2plot, mapping = aes_(x = quote(x), y = quote(ECDF)))
  }

  # Calculation of theoretical signal to color the lines
  x2plot <- seq(xmin, xmax, length.out = npoints)
  ns <- nrow(extendedres)
  N <- ns * npoints
  
  curves2plot <- data.frame(x = rep(x2plot, ns), 
                            id = rep(BMD2plot$id, each = npoints),
                            ECDF = rep(BMD2plot$ECDF, each = npoints),
                            signal = numeric(length = N))
  for (i in 1:ns)
  {
    modeli <- extendedres$model[i]
    if (modeli == "linear")
    {
      b <- extendedres$b[i]
      d <- extendedres$d[i]
      curves2plot$signal[(i-1)*npoints + 1:npoints] <- flin(x2plot, b = extendedres$b[i], 
                                                       d = extendedres$d[i]) - extendedres$y0[i]*y0shift
    } else
      if (modeli == "exponential")
      {
        curves2plot$signal[(i-1)*npoints + 1:npoints] <- fExpo(x2plot, b = extendedres$b[i], 
                                                          d = extendedres$d[i], e = extendedres$e[i]) - extendedres$y0[i]*y0shift
      } else
        if (modeli == "Hill")
        {
          curves2plot$signal[(i-1)*npoints + 1:npoints] <- fHill(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                                                            d = extendedres$d[i], e = extendedres$e[i]) - extendedres$y0[i]*y0shift
        } else
          if (modeli == "Gauss-probit")
          {
            curves2plot$signal[(i-1)*npoints + 1:npoints] <- fGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                                                                 d = extendedres$d[i], e = extendedres$e[i], 
                                                                 f = extendedres$f[i]) - extendedres$y0[i]*y0shift
          } else
            if ((modeli == "log-Gauss-probit") | (modeli == "log-probit"))
            {
              curves2plot$signal[(i-1)*npoints + 1:npoints] <- fLGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                                                                    d = extendedres$d[i], e = extendedres$e[i], 
                                                                    f = extendedres$f[i]) - extendedres$y0[i]*y0shift
            }
  }

  # no shape no facet
  if (missing(facetby))
  {
    gg <- g + geom_line(data = curves2plot, 
              mapping = aes_(x = quote(x), y = quote(ECDF), 
                             group = quote(id), color = quote(signal)),
              size = line.size) 
  } else
  { 
    curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
    gg <- g + geom_line(data = curves2plot, 
                        mapping = aes_(x = quote(x), y = quote(ECDF), 
                                       group = quote(id), color = quote(signal)),
                        size = line.size)
    if (missing(ncol4faceting))
    {
        gg <- gg + facet_wrap(~ facetby) 
    } else
    {
      gg <- gg + facet_wrap(~ facetby, ncol = ncol4faceting) 
    }
    
  } 
  # Add of the color gradient
  if (missing(limits4colgradient))
  {
    gg <- gg +
      # scale_colour_gradient2(low = "darkblue", mid = "white",
      #                        high = "darkred", midpoint = median(curves2plot$signal), space = "Lab",
      #                        na.value = "grey50", guide = "colourbar", aesthetics = "colour")
      scale_colour_gradient2(low = lowercol, mid = "white",
                             high = uppercol, midpoint = 0, space = "Lab",
                             na.value = "grey50", guide = "colourbar", aesthetics = "colour")
    
  } else
  {
    gg <- gg +
      scale_colour_gradient2(low = lowercol, mid = "white",
                             high = uppercol, midpoint = 0, space = "Lab",
                             na.value = "grey50", guide = "colourbar", aesthetics = "colour",
                             limits = limits4colgradient)
    
  }
  
  # Add of points (BMD values)
  if (!missing(shapeby)) 
  {
    gg <- gg + geom_point(data = BMD2plot, mapping = aes_(shape = quote(shapeby)),
                        size = point.size)
  } else
  {
    gg <- gg + geom_point(data = BMD2plot, size = point.size)
  }
  gg <- gg + theme_classic()
  
  return(gg)
}