ecdfplotwithCI <- function(variable, CI.lower, CI.upper, by, CI.col = "blue", CI.alpha = 1, 
                           add.point = TRUE, point.size = 1)
{
  if (!missing(by)) 
  {
    d <- data.frame(variable = variable, lower = CI.lower, upper = CI.upper, by = by)
    ntot = nrow(d)
    uniqueby <- unique(d$by)
    n.uniqueby <- length(uniqueby)
    d$ECDF <- rep(0, ntot) # initialization
    for (i in 1:n.uniqueby)
    {
      indi <- which(d$by == uniqueby[i])
      ntoti <- length(indi)
      d$ECDF[indi] <- (rank(d$variable[indi], ties.method = "first") - 0.5) / ntoti
      # not strictly equivalent with ecdf (i / n)
      # d$ECDF[indi] <- ecdf(d$variable[indi])(d$variable[indi])
    }
    g <- ggplot(data = d, mapping = aes_(x = variable, y = quote(ECDF))) + 
      facet_wrap(~ by) 
    
    if (is.factor(CI.col))
    {
      g <- g + 
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper), color = quote(CI.col)),  
                       alpha = CI.alpha, height = 0)  
    } else
    {
      g <- g + 
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper)), col = CI.col, 
                       alpha = CI.alpha, height = 0)  
    }
  } else
  {
    d <- data.frame(variable = variable, lower = CI.lower, upper = CI.upper)
    ntot = nrow(d)
    d$ECDF <- (rank(d$variable, ties.method = "first") - 0.5) / ntot
    # not strictly equivalent with ecdf (i / n)
    # d$variable <- ecdf(d$variable)(d$variable)
    g <- ggplot(data = d, mapping = aes_(x = variable, y = quote(ECDF)))
    if (is.factor(CI.col))
    {
      g <- g + 
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper), color = quote(CI.col)),  
                       alpha = CI.alpha, height = 0)  
    } else
    {
      g <- g + 
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper)), col = CI.col, 
                       alpha = CI.alpha, height = 0)  
    }
  }
  if (add.point) g <- g + geom_point(size = point.size)
  return(g)
}