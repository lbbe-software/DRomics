# Plot of BMD values as an ECDF plot, with an horizontal color gradient coding
# for the signal level at each dose (or concentration)for each item,
# form an extended results dataframe (e.g. with annotation of items)
# with optionnal use of columns for shape and or facet
bmdplotwithgradient <- function(extendedres, BMDtype = c("zSD", "xfold"),
                                xmin, xmax, y0shift = TRUE, scaling = TRUE,
                                facetby, facetby2, shapeby, npoints = 50,
                                line.size, point.size = 1,
                                ncol4faceting, limits4colgradient,
                                lowercol = "darkblue", uppercol = "darkred",
                                add.label = FALSE, label.size = 2,
                                BMD_log_transfo = TRUE) {
  
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  
  if (missing(extendedres) || !is.data.frame(extendedres))
    stop("The first argument of bmdplotwithgradient must be a dataframe 
    (see ?bmdplotwithgradient for details).")
  
  if (scaling && !y0shift) {
    y0shift <- TRUE
    warning(strwrap(prefix = "\n", initial = "\n",
                    "y0shift is forced to TRUE when scaling is TRUE."))
  }
  
  cnames <- colnames(extendedres)
  
  if (BMDtype == "zSD") {
    if (scaling) {
      if (!all(is.element(c("id", "model", "b", "c", "d", "e", "f", "y0", "maxychange", "BMD.zSD"), cnames)))
        stop("The first argument of bmdplotwithgradient must be a dataframe
      containing at least columns named id, model, b, c, d, e, f, y0, maxychange and BMD.zSD.")
    } else {
      if (!all(is.element(c("id", "model", "b", "c", "d", "e", "f", "y0", "BMD.zSD"), cnames)))
        stop("The first argument of bmdplotwithgradient must be a dataframe
      containing at least columns named id, model, b, c, d, e, f, y0 and BMD.zSD.")
    }
    
    BMD2plot <- data.frame(x = extendedres$BMD.zSD, id = extendedres$id)
  } else {
    if (scaling) {
      if (!all(is.element(c("id", "model", "b", "c", "d", "e", "f", "y0", "maxychange", "BMD.xfold"), cnames)))
        stop("The first argument of bmdplotwithgradient must be a dataframe
      containing at least columns named id, model, b, c, d, e, f, y0, maxychange and BMD.xfold.")
    } else {
      if (!all(is.element(c("id", "model", "b", "c", "d", "e", "f", "y0", "BMD.xfold"), cnames)))
        stop("The first argument of bmdplotwithgradient must be a dataframe
      containing at least columns named id, model, b, c, d, e, f, y0 and BMD.xfold.")
    }
    
    BMD2plot <- data.frame(x = extendedres$BMD.xfold, id = extendedres$id)
  }
  
  if (BMD_log_transfo) {
    if (missing(xmin)) {
      xmin <- min(BMD2plot$x[is.finite(BMD2plot$x) & BMD2plot$x != 0])
    } else {
      if (xmin == 0) {
        warning(strwrap(prefix = "\n", initial = "\n",
                        "When using a log scale for the BMD plot, it is not possible to fix xmin at 0. 
          If the default value does not suit you, you can define a strictly positive value for xmin."))
        xmin <- min(BMD2plot$x[is.finite(BMD2plot$x) & BMD2plot$x != 0])
      }
    }
  } else {
    if (missing(xmin)) xmin <- 0
  }
  
  if (missing(xmax)) {
    warning(strwrap(prefix = "\n", initial = "\n",
                    "By default xmax was fixed at the maximal BMD value, but you could also 
                    fix it at the maximal tested dose."))
    xmax <- max(BMD2plot$x[is.finite(BMD2plot$x)])
  }
  
  if (!missing(shapeby)) {
    if (!is.character(shapeby))
      stop("shapeby should be a character string for the name of the column coding for the point shape.")
    if (!is.element(shapeby, cnames))
      stop("shapeby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    BMD2plot$shapeby <- extendedres[, shapeby]
  }
  
  # calculation of ECDF by facetby
  ntot <- nrow(BMD2plot)
  if (!missing(facetby)) {
    if (!is.character(facetby))
      stop("facetby should be a character string for the name of the column used for facetting.")
    if (!is.element(facetby, cnames))
      stop("facetby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    BMD2plot$facetby <- extendedres[, facetby]
    
    if (!missing(facetby2)) {
      if (!is.character(facetby2))
        stop("facetby2 should be a character string for the name of the column used for facetting.")
      if (!is.element(facetby2, cnames))
        stop("facetby2 should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
      BMD2plot$facetby2 <- extendedres[, facetby2]
      BMD2plot$group <- paste(extendedres[, facetby], extendedres[, facetby2], sep = "_")
    } else {
      BMD2plot$group  <-  BMD2plot$facetby
    }
    
    if (missing(line.size)) {
      line.size <- 24 / max(table(BMD2plot$group))
    }
    
    uniqueby <- unique(BMD2plot$group)
    n.uniqueby <- length(uniqueby)
    BMD2plot$ECDF <- rep(0, ntot) # initialization
    for (i in 1:n.uniqueby) {
      indi <- which(BMD2plot$group == uniqueby[i])
      ntoti <- length(indi)
      BMD2plot$ECDF[indi] <- (rank(BMD2plot$x[indi], ties.method = "first") - 0.5) / ntoti
    }
    
    g <- ggplot(data = BMD2plot, mapping = aes(x = .data$x, y = .data$ECDF,
                                               label = .data$id))
    if (missing(facetby2)) {
      g <- g + facet_wrap(~ facetby)
    } else {
      g <- g + facet_grid(facetby2 ~ facetby)
    }
  } else {
    if (missing(line.size)) {
      line.size <- 24 / nrow(BMD2plot)
    }
    
    BMD2plot$ECDF <- (rank(BMD2plot$x, ties.method = "first") - 0.5) / ntot
    g <- ggplot(data = BMD2plot, mapping = aes(x = .data$x, y = .data$ECDF, label = .data$id))
  }
  
  # Calculation of theoretical signal to color the lines
  if (BMD_log_transfo) {
    x2plot <- 10^seq(log10(xmin), log10(xmax), length.out = npoints)
  } else {
    x2plot <- seq(xmin, xmax, length.out = npoints)
  }
  ns <- nrow(extendedres)
  N <- ns * npoints
  
  curves2plot <- data.frame(x = rep(x2plot, ns),
                            id = rep(BMD2plot$id, each = npoints),
                            ECDF = rep(BMD2plot$ECDF, each = npoints),
                            signal = numeric(length = N))
  for (i in 1:ns) {
    modeli <- extendedres$model[i]
    if (modeli == "linear") {
      curves2plot$signal[(i - 1) * npoints + 1:npoints] <- flin(x2plot, b = extendedres$b[i],
                                                                d = extendedres$d[i])
      
    } else if (modeli == "exponential") {
      curves2plot$signal[(i - 1) * npoints + 1:npoints] <- fExpo(x2plot, b = extendedres$b[i],
                                                                 d = extendedres$d[i], e = extendedres$e[i])
      
    } else if (modeli == "Hill") {
      curves2plot$signal[(i - 1) * npoints + 1:npoints] <- fHill(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                                                                 d = extendedres$d[i], e = extendedres$e[i])
      
    } else if (modeli == "Gauss-probit") {
      curves2plot$signal[(i - 1) * npoints + 1:npoints] <- fGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                                                                    d = extendedres$d[i], e = extendedres$e[i],
                                                                    f = extendedres$f[i])
      
    } else if (modeli == "log-Gauss-probit") {
      curves2plot$signal[(i - 1) * npoints + 1:npoints] <- fLGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                                                                     d = extendedres$d[i], e = extendedres$e[i],
                                                                     f = extendedres$f[i])
    }
    
    if (y0shift) {
      if (scaling) {
        curves2plot$signal[(i - 1) * npoints + 1:npoints] <-
          (curves2plot$signal[(i - 1) * npoints + 1:npoints] - extendedres$y0[i]) / extendedres$maxychange[i]
      } else {
        curves2plot$signal[(i - 1) * npoints + 1:npoints] <-
          curves2plot$signal[(i - 1) * npoints + 1:npoints] - extendedres$y0[i]
      }
    }
  }
  
  # no shape no facet
  if (!missing(facetby)) {
    curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
    if (!missing(facetby2)) {
      curves2plot$facetby2 <- rep(extendedres[, facetby2], each = npoints)
    }
  }
  
  gg <- g + geom_line(data = curves2plot,
                      mapping = aes(x = .data$x, y = .data$ECDF,
                                    group = .data$id, color = .data$signal),
                      size = line.size)
  
  if (!missing(facetby)) {
    if (!missing(facetby2)) {
      gg <- gg + facet_grid(facetby2 ~ facetby)
    } else {
      if (missing(ncol4faceting)) {
        gg <- gg + facet_wrap(~ facetby)
      } else {
        gg <- gg + facet_wrap(~ facetby, ncol = ncol4faceting)
      }
    }
  }
  
  # Add of the color gradient
  if (missing(limits4colgradient)) {
    gg <- gg +
      # scale_colour_gradient2(low = "darkblue", mid = "white",
      #                        high = "darkred", midpoint = median(curves2plot$signal), space = "Lab",
      #                        na.value = "grey50", guide = "colourbar", aesthetics = "colour")
      scale_colour_gradient2(low = lowercol, mid = "white",
                             high = uppercol, midpoint = 0, space = "Lab",
                             na.value = "grey50", guide = "colourbar", aesthetics = "colour")
    
  } else {
    gg <- gg +
      scale_colour_gradient2(low = lowercol, mid = "white",
                             high = uppercol, midpoint = 0, space = "Lab",
                             na.value = "grey50", guide = "colourbar", aesthetics = "colour",
                             limits = limits4colgradient)
    
  }
  
  # Add of points (BMD values)
  if (!missing(shapeby)) {
    gg <- gg + geom_point(data = BMD2plot, mapping = aes(shape = .data$shapeby),
                          size = point.size)
  } else {
    gg <- gg + geom_point(data = BMD2plot, size = point.size)
  }
  gg <- gg + theme_classic()
  
  if (add.label) {
    if (!missing(shapeby))
      warning(strwrap(prefix = "\n", initial = "\n",
                      "The type of points will not be seen when points are replaced by labels.
        You should omit it in this case."))
    gg <- gg + geom_label(size = label.size)
  }
  
  if (BMD_log_transfo) {
    gg <- gg + scale_x_log10() + xlab("BMD (in log scale)")
  } else {
    gg <- gg + xlab("BMD")
  }
  
  if (scaling) {
    gg <- gg + labs(color = "scaled signal")
  } else {
    gg <- gg + labs(color = "signal")
  }
  
  if (!missing(shapeby)) {
    gg <- gg + labs(shape = shapeby)
  }
  
  return(gg)
}
