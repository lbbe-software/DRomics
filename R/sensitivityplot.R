# Sensitivity plot
# Plot of a summary of BMD values per group of items 
# (e.g. from biological annotation), 
# and optionally per molecular level 
# (or per another additional grouping level )
sensitivityplot <- function(extendedres, BMDtype = c("zSD", "xfold"),
                            group, ECDF_plot = TRUE, colorby,
                            plottype = c("first.quartile", "median" , "median.and.IQR"),
                            BMD_log_transfo = FALSE)
{
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  plottype <- match.arg(plottype, c("median", "first.quartile", "median.and.IQR"))
  
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of bmdplotwithgradient must be a dataframe 
    (see ?bmdplotwithgradient for details).")
  
  cnames <- colnames(extendedres)
  
  if (BMDtype == "zSD")
  {  
    if (any(!is.element(c("BMD.zSD"), cnames)))
      stop("The first argument of bmdplotwithgradient must be a dataframe
      containing a column named BMD.zSD and other columns coding for group of items.")
    variable <- extendedres[, "BMD.zSD"]
  }
  else 
  {
    if (any(!is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of bmdplotwithgradient must be a dataframe
      containing a column named BMD.xfold and other columns coding for group of items.")
    variable <- extendedres[, "BMD.xfold"]
  }
  groupby <- as.factor(extendedres[, group])
  
  # if (!missing(colorby))
  # {
  #   if (!is.character(colorby)) 
  #     stop("colorby should be a character string for the name of the column coding for the point shape.")
  #   summary2plot$colorby <- extendedres[, colorby]
  # }
  firstquartilefun <- function(x) quantile(x, probs = 0.25)
  thirdquartilefun <- function(x) quantile(x, probs = 0.75)
  
  if (missing(colorby))
  {
    dnb <- as.data.frame(table(groupby))
    colnames(dnb) <- c("groupby", "nb_of_items")
    dnb$firstquartile <- tapply(variable, groupby, firstquartilefun)
    dnb$secondquartile <- tapply(variable, groupby, median)
    dnb$thirdquartile <- tapply(variable, groupby, thirdquartilefun)

  } else
  {
    if (!is.character(colorby)) 
     stop("colorby should be a character string for the name of the column coding for the point shape.")
    level <- extendedres[, colorby]
    dnb <- as.data.frame(table(groupby, level))
    colnames(dnb) <- c("groupby", "level","nb_of_items")
    dnb$firstquartile <- tapply(variable, level:groupby, firstquartilefun)
    dnb$secondquartile <- tapply(variable, level:groupby, median)
    dnb$thirdquartile <- tapply(variable, level:groupby, thirdquartilefun)
  }
  dnb <- dnb[dnb$nb_of_items != 0, ]
  
  if (!missing(colorby)) ECDF_plot <- FALSE
  
  if (ECDF_plot)
  {
    # order by chosen summary
    if (plottype == "median" | plottype == "median.and.IQR")
      dnb <- dnb[order(dnb$secondquartile), ] else 
    if (plottype == "first.quartile")
      dnb <- dnb[order(dnb$firstquartile), ]
          
    # fix the order of the modalities of by as in the ordered data set
    dnb$groupby <- factor(dnb$groupby, levels = dnb$groupby)
  }
  if (plottype == "first.quartile")
  {
    if (missing(colorby))
    gg <- ggplot(dnb, aes_(x = quote(groupby), y = quote(firstquartile), 
                           size = quote(nb_of_items))) else
    gg <- ggplot(dnb, aes_(x = quote(groupby), y = quote(firstquartile), 
                          color = quote(level), alpha = I(0.8),
                          size = quote(nb_of_items))) 
                                                      
    gg <- gg + geom_point(stat = 'identity')  +  coord_flip() +
      labs(x = "", y = "BMD 25th quantiles") 
        
  } else
  
  if (plottype == "median" | plottype == "median.and.IQR")
  {
    if (missing(colorby))
      gg <- ggplot(dnb, aes_(x = quote(groupby), y = quote(secondquartile), 
                           size = quote(nb_of_items))) else
      gg <- ggplot(dnb, aes_(x = quote(groupby), y = quote(secondquartile), 
                             color = quote(level), alpha = I(0.8),
                             size = quote(nb_of_items)))
      gg <- gg + geom_point(stat = 'identity') + coord_flip() 
    if (plottype == "median")
      gg <- gg + labs(x = "", y = "BMD medians") else
      gg <- gg + geom_errorbar(aes_(ymin = quote(firstquartile), 
                                      ymax = quote(thirdquartile), size = I(1)), 
                                      width = 0) +
          # line to remove lines on the size legend
          guides(size = guide_legend(override.aes = list(linetype = 0))) +
          labs(x = "", y = "BMD medians and IQRs")
   }    
  if (BMD_log_transfo)
    gg <- gg + scale_y_log10()
  

  return(gg)
}