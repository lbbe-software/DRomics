# Sensitivity plot
# Plot of a summary of BMD values per group of items 
# (e.g. from biological annotation), 
# and optionally per molecular level 
# (or per another additional grouping level )
sensitivityplot <- function(extendedres, BMDtype = c("zSD", "xfold"),
                            group, ECDF_plot = TRUE, colorby,
                            BMDsummary = c("first.quartile", "median" , "median.and.IQR"),
                            BMD_log_transfo = TRUE,
                            line.size = 0.5, line.alpha = 0.5, point.alpha = 0.5)
{
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  BMDsummary <- match.arg(BMDsummary, c("first.quartile", "median", "median.and.IQR"))
  
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of sensitivityplot must be a dataframe 
    (see ?sensitivityplot for details).")
  
  cnames <- colnames(extendedres)
  
  if (BMDtype == "zSD")
  {  
    if (!all(is.element(c("BMD.zSD"), cnames)))
      stop("The first argument of sensitivityplot must be a dataframe
      containing a column named BMD.zSD and other columns coding for group of items.")
    variable <- extendedres[, "BMD.zSD"]
  } else 
  {
    if (!all(is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of sensitivityplot must be a dataframe
      containing a column named BMD.xfold and other columns coding for groups of items.")
    variable <- extendedres[, "BMD.xfold"]
  }
  
  if (!is.character(group)) 
    stop("group should be a character string for the name of the column defining groups.")
  if (!is.element(group, cnames))
    stop("group should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  groupby <- as.factor(extendedres[, group])
  
  if (!missing(colorby))
  {
    if (!is.character(colorby)) 
      stop("colorby should be a character string for the name of the column coding for the point color.")
    if (!is.element(colorby, cnames))
      stop("colorby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  }
  
  
   firstquartilefun <- function(x) stats::quantile(x, probs = 0.25, na.rm = TRUE)
   secondquartilefun <- function(x) stats::quantile(x, probs = 0.5, na.rm = TRUE)
   thirdquartilefun <- function(x) stats::quantile(x, probs = 0.75, na.rm = TRUE)
  
  if (missing(colorby))
  {
    dnb <- as.data.frame(table(groupby))
    colnames(dnb) <- c("groupby", "nb_of_items")
    dnb$firstquartile <- tapply(variable, groupby, firstquartilefun)
    dnb$secondquartile <- tapply(variable, groupby, secondquartilefun)
    dnb$thirdquartile <- tapply(variable, groupby, thirdquartilefun)

  } else
  {
    level <- extendedres[, colorby]
    dnb <- as.data.frame(table(groupby, level))
    colnames(dnb) <- c("groupby", "level","nb_of_items")
    dnb$firstquartile <- tapply(variable, level:groupby, firstquartilefun)
    dnb$secondquartile <- tapply(variable, level:groupby, secondquartilefun)
    dnb$thirdquartile <- tapply(variable, level:groupby, thirdquartilefun)
  }
  dnb <- dnb[dnb$nb_of_items != 0, ]
  
  if (!missing(colorby)) ECDF_plot <- FALSE
  
  if (ECDF_plot)
  {
    # order by chosen summary
    if (BMDsummary == "median" | BMDsummary == "median.and.IQR")
      dnb <- dnb[order(dnb$secondquartile), ] else 
    if (BMDsummary == "first.quartile")
      dnb <- dnb[order(dnb$firstquartile), ]
          
    # fix the order of the modalities of by as in the ordered data set
    dnb$groupby <- factor(dnb$groupby, levels = dnb$groupby)
  }
  
  if (BMDsummary == "first.quartile")
  {
    if (missing(colorby))
    {
      gg <- ggplot(dnb, aes(x = .data$groupby, y = .data$firstquartile, 
                             size = .data$nb_of_items))
    } else
    {  
      gg <- ggplot(dnb, aes(x = .data$groupby, y = .data$firstquartile, 
                             color = .data$level, 
                             size = .data$nb_of_items))
    }
    
    gg <- gg + geom_point(stat = 'identity', alpha = point.alpha)  +  coord_flip() 

    if (BMD_log_transfo) 
    {
      gg <- gg + labs(x = "", y = "BMD 25th quantiles (in log scale)")
    } else
    {
      gg <- gg + labs(x = "", y = "BMD 25th quantiles")
    }
    
  } else 
  {
    if (missing(colorby))
    {
      gg <- ggplot(dnb, aes(x = .data$groupby, y = .data$secondquartile, 
                             size = .data$nb_of_items)) 
    } else 
    {
      if (BMDsummary == "median") {
        gg <- ggplot(dnb, aes(x = .data$groupby, y = .data$secondquartile, 
                               color = .data$level, 
                               size = .data$nb_of_items))
      } else 
      {
        gg <- ggplot(dnb, aes(x = .data$groupby, y = .data$secondquartile, 
                               color = .data$level, 
                               size = .data$nb_of_items))
      }
    }
    gg <- gg + geom_point(stat = 'identity', alpha = point.alpha) + coord_flip() 
    
    if (BMDsummary == "median")
    {
      if (BMD_log_transfo) 
      {
        gg <- gg + labs(x = "", y = "BMD medians (in log scale)")
      } else
      {
        gg <- gg + labs(x = "", y = "BMD medians")
      }
    } else 
    {
      gg <- gg + geom_errorbar(aes(ymin = .data$firstquartile, 
                                    ymax = .data$thirdquartile),
                               linewidth = line.size, 
                               alpha = line.alpha,
                               width = 0) +
        # line to remove lines on the size legend
      guides(size = guide_legend(override.aes = list(linetype = 0))) 
      if (BMD_log_transfo) 
      {
        gg <- gg + labs(x = "", y = "BMD medians and IQRs (in log scale)")
      } else
      {
        gg <- gg + labs(x = "", y = "BMD medians and IQRs")
      }
    }
  }
  
  if (BMD_log_transfo)
    gg <- gg + scale_y_log10() 
  
  if (!missing(colorby))
  {
    gg <- gg + labs(color = colorby)
  }
  
  round.quartiles.minmax <- unique(round(quantile(dnb$nb_of_items, probs = c(0, 0.25, 0.5, 0.75, 1))))
  gg <- gg + scale_size_continuous(breaks = as.numeric(round.quartiles.minmax)) + 
            labs(size = "nb. of items")
  return(gg)
}
