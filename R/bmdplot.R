# Plot of BMD values as an ECDF plot, optionally with confidence intervals,
# form an extended results dataframe (e.g. with annotation of items)
# with optionnal use of columns for shape and or facet 
bmdplot <- function(extendedres, BMDtype = c("zSD", "xfold"),
                                    add.CI = FALSE, 
                                    facetby, facetby2, 
                                    shapeby,  colorby,
                                   point.size = 1,
                                   ncol4faceting, 
                                   add.label = FALSE, label.size = 2,
                                    BMD_log_transfo = TRUE)
{
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))

  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of bmdplot must be a dataframe 
    (see ?bmdplot for details).")
  
  cnames <- colnames(extendedres)
 
  if (BMDtype == "zSD")
  {  
    if (any(!is.element(c("id", "BMD.zSD"), cnames)))
      stop("The first argument of bmdplot must be a dataframe
      containing at least columns named id and BMD.zSD.")
    
    if (add.CI)
    {
      BMD2plot <- data.frame(x = extendedres$BMD.zSD, id = extendedres$id,
                             upper = extendedres$BMD.zSD.upper,
                             lower = extendedres$BMD.zSD.lower)
    } else
    {
      BMD2plot <- data.frame(x = extendedres$BMD.zSD, id = extendedres$id)
    }
  } else 
  {
    if (any(!is.element(c("id", "BMD.xfold"), cnames)))
      stop("The first argument of bmdplot must be a dataframe
      containing at least columns named id and BMD.xfold.")
    if (add.CI)
      BMD2plot <- data.frame(x = extendedres$BMD.xfold, id = extendedres$id,
                             upper = extendedres$BMD.xfold.upper,
                             lower = extendedres$BMD.xfold.lower) else
      BMD2plot <- data.frame(x = extendedres$BMD.xfold, id = extendedres$id)
  }
  
  if (!missing(shapeby))
  {
    if (!is.character(shapeby)) 
      stop("shapeby should be a character string for the name of the column coding for the point shape.")
    if (!is.element(shapeby, cnames))
      stop("shapeby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    BMD2plot$shapeby <- extendedres[, shapeby]
  }

  if (!missing(colorby))
  {
    if (!is.character(colorby)) 
      stop("colorby should be a character string for the name of the column coding for the point color.")
    if (!is.element(colorby, cnames))
      stop("colorby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    BMD2plot$colorby <- extendedres[, colorby]
  }
  
  # calculation of ECDF by facetby
  ntot <- nrow(BMD2plot)
  if (!missing(facetby)) 
  {
    if (!is.character(facetby)) 
      stop("facetby should be a character string for the name of the column used for facetting.")
    if (!is.element(facetby, cnames))
      stop("facetby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    BMD2plot$facetby <- extendedres[, facetby]
    
    if (!missing(facetby2)) 
    {
      if (!is.character(facetby2)) 
        stop("facetby2 should be a character string for the name of the column used for facetting.")
      if (!is.element(facetby2, cnames))
        stop("facetby2 should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
      BMD2plot$facetby2 <- extendedres[, facetby2]
      BMD2plot$group <- paste(extendedres[, facetby], extendedres[, facetby2], sep = "_")
    } else
    {
      BMD2plot$group  <-  BMD2plot$facetby
    }
    
    uniqueby <- unique(BMD2plot$group)
    n.uniqueby <- length(uniqueby)
    BMD2plot$ECDF <- rep(0, ntot) # initialization
    for (i in 1:n.uniqueby)
    {
      indi <- which(BMD2plot$group == uniqueby[i])
      ntoti <- length(indi)
      BMD2plot$ECDF[indi] <- (rank(BMD2plot$x[indi], ties.method = "first") - 0.5) / ntoti
    }
    gg <- ggplot(data = BMD2plot, mapping = aes_(x = quote(x), y = quote(ECDF), 
                                                label = quote(id))) 
    if (missing(facetby2)) gg <- gg + facet_wrap(~ facetby) else gg <- gg + facet_grid(facetby2 ~ facetby)
    
  } else
  {
    BMD2plot$ECDF <- (rank(BMD2plot$x, ties.method = "first") - 0.5) / ntot
    gg <- ggplot(data = BMD2plot, mapping = aes_(x = quote(x), y = quote(ECDF),
                                                label = quote(id)))
  }

  if (!missing(facetby))
  {
    if (!missing(facetby2)) 
    {
      gg <- gg + facet_grid(facetby2 ~ facetby) 
    } else
    {
      if (missing(ncol4faceting))
      {
        gg <- gg + facet_wrap(~ facetby) 
      } else
      {
        gg <- gg + facet_wrap(~ facetby, ncol = ncol4faceting) 
      }
    }
  }
  

  # Add of points (BMD values)
  if (!missing(shapeby))
  {
    if (!missing(colorby))
    {
      gg <- gg + geom_point(data = BMD2plot, mapping = aes_(shape = quote(shapeby),
                 color = quote(colorby)), size = point.size) 
    } else
    {
      gg <- gg + geom_point(data = BMD2plot, mapping = aes_(shape = quote(shapeby)),
                            size = point.size)
    }
  } else
  {
    if (!missing(colorby))
    {
      gg <- gg + geom_point(data = BMD2plot, 
                            mapping = aes_(color = quote(colorby)),size = point.size)
    } else
    {
      gg <- gg + geom_point(data = BMD2plot, size = point.size)
    }
  }
  
  # Add of CIs
  if (add.CI)
  {
    if (!missing(colorby))
    {
      gg <- gg + geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper),
                                     color = quote(colorby)), height = 0)
    } else
    {
      gg <- gg + geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper)),  
                                height = 0)
    }                
  }
  
  # Add of labels
  if(add.label)
  {
    if (!missing(shapeby) | !missing(colorby))
      warning(strwrap(prefix = "\n", initial = "\n",
        "The type and color of points will not be seen when points are replaced by labels.
        You should omit it in this case."))
    gg <- gg + geom_label(size = label.size)
  }
  
  if (BMD_log_transfo)
    gg <- gg + scale_x_log10()
  
  gg <- gg + xlab("BMD")
  
  if (!missing(shapeby))
  {
    gg <- gg + labs(shape = shapeby)
  }
  
  if (!missing(colorby))
  {
    gg <- gg + labs(color = colorby)
  }
  
    
  return(gg)
}