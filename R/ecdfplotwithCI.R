ecdfplotwithCI <- function(variable, CI.lower, CI.upper, by, CI.col = "blue")
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
    g <- ggplot(data = d, mapping = aes(x = variable, y = ECDF)) + 
      facet_wrap(~ by) + 
      geom_errorbarh(aes(xmin = lower, xmax = upper), col = CI.col, 
                     alpha = 0.5, height = 0) + geom_point() 
  } else
  {
    d <- data.frame(variable = variable, lower = CI.lower, upper = CI.upper)
    ntot = nrow(d)
    d$ECDF <- (rank(d$variable, ties.method = "first") - 0.5) / ntot
    # not strictly equivalent with ecdf (i / n)
    # d$variable <- ecdf(d$variable)(d$variable)
    g <- ggplot(data = d, mapping = aes(x = variable, y = ECDF)) +
        geom_errorbarh(aes(xmin = lower, xmax = upper), col = CI.col,
                       alpha = 0.5,  height = 0) + geom_point()
  }
  return(g)
}