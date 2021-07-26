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
 
  if (any(!is.element(c("trend"), cnames)))
    stop("The first argument of trendplot must be a dataframe
    containing a column named trend and other columns coding for group of items.")
  
  if (missing(facetby))
  {
    dtab <- as.data.frame(table(extendedres[, group], 
                                extendedres[, "trend"] ) )
    colnames(dtab) <- c("group","trend", "nitems")
    dtab <- subset(dtab, nitems != 0)
    if (add.color)
      gg <- ggplot(dtab, aes(x = trend, y = group, colour = trend)) +
      geom_point(aes(size = nitems))  else
        
        gg <- ggplot(dtab, aes(x = trend, y = group)) +
      geom_point(aes(size = nitems)) 
   } else
  {
    dtab <- as.data.frame(table(extendedres[, group], 
                                extendedres[, "trend"],
                                extendedres[, facetby]) )
    colnames(dtab) <- c("group","trend", "facetby", "nitems")
    dtab <- subset(dtab, nitems != 0)
    if (add.color)
    gg <- ggplot(dtab, aes(x = trend, y = group, colour = trend)) +
      geom_point(aes(size = nitems))  else
        
        gg <- ggplot(dtab, aes(x = trend, y = group)) +
      geom_point(aes(size = nitems)) 
    if (missing(ncol4faceting)) gg <- gg + facet_wrap(~ facetby) else
      gg <- gg + facet_wrap(~ facetby, ncol = ncol4faceting)
  }
  return(gg)
}