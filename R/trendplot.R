# Plot of trend repartition per group of items, 
# (e.g. from biological annotation), 
# and optionally per molecular level 
# (or per another additional grouping level )
trendplot <- function(extendedres, group,
                      facetby, ncol4faceting, add.color = TRUE)
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of trendplot must be a dataframe 
    (see ?trendplot for details).")
  
  cnames <- colnames(extendedres)
  
  if (!is.character(group)) 
    stop("group should be a character string for the name of the column defining groups.")
  if (!is.element(group, cnames))
    stop("group should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  
  
  if (any(!is.element(c("trend"), cnames)))
    stop("The first argument of trendplot must be a dataframe
    containing a column named trend and other columns coding for group of items.")

  if (!missing(facetby)) 
  {
    if (!is.character(facetby)) 
      stop("facetby should be a character string for the name of the column used for facetting.")
    if (!is.element(facetby, cnames))
      stop("facetby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  }
      
  if (missing(facetby))
  {
    dtab <- as.data.frame(table(extendedres[, group], 
                                extendedres[, "trend"] ) )
    colnames(dtab) <- c("group","trend", "nb_of_items")
    dtab <- dtab[dtab$nb_of_items != 0, ]
    if (add.color)
    {
      gg <- ggplot(dtab, aes_(x = quote(trend), y = quote(group), colour = quote(trend))) + 
        geom_point(aes_(size = quote(nb_of_items)))
      
    } else
    {
      gg <- ggplot(dtab, aes_(x = quote(trend), y = quote(group))) +
        geom_point(aes_(size = quote(nb_of_items)))
    }
  } else {
    dtab <- as.data.frame(table(extendedres[, group], 
                                extendedres[, "trend"],
                                extendedres[, facetby]) )
    colnames(dtab) <- c("group", "trend", "facetby", "nb_of_items")
    dtab <- dtab[dtab$nb_of_items != 0, ]
    if (add.color)
    {
      gg <- ggplot(dtab, aes_(x = quote(trend), y = quote(group), colour = quote(trend))) +
        geom_point(aes_(size = quote(nb_of_items))) 
    } else
    {
      gg <- ggplot(dtab, aes(x = quote(trend), y = quote(group))) +
        geom_point(aes_(size = quote(nb_of_items)))
    }
    
    if (missing(ncol4faceting)) 
      {gg <- gg + facet_wrap(~ facetby)} else
      {gg <- gg + facet_wrap(~ facetby, ncol = ncol4faceting)}
  }
  
  round.quartiles.minmax <- unique(round(quantile(dtab$nb_of_items, probs = c(0, 0.25, 0.5, 0.75, 1))))
  gg <- gg + scale_size_continuous(breaks = as.numeric(round.quartiles.minmax)) + 
    labs(size = "nb. of items")
  
  return(gg)
}