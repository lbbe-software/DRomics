ecdfplotwithCI <- function(variable, CI.lower, CI.upper, by, CI.col = "blue", CI.alpha = 1, 
                           add.point = TRUE, point.size = 1, point.type = 16)
{
  if (!is.numeric(variable))
    stop("Arguments variable, CI.lower and CI.upper must be numeric vectors of the same size")
  leng <- length(variable)
  if (!is.numeric(CI.lower) | (length(CI.lower) != leng) |
      !is.numeric(CI.upper) | (length(CI.upper) != leng))
    stop("Arguments variable, CI.lower and CI.upper must be numeric vectors of the same size")
  d <- data.frame(variable = variable, lower = CI.lower, upper = CI.upper)
  
  if (!missing(by)) 
  {
    if (length(by) != leng)
      stop("Argument by must be a factor of the same length as argument variable")
    d$by <- as.factor(by)
  } 
  if (is.factor(CI.col)) d$CI.col <- CI.col
  if (is.factor(point.type)) d$point.type <- point.type
  
  if (!missing(by)) 
  {
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
    g <- ggplot(data = d, mapping = aes_(x = variable, y = quote(ECDF))) + facet_wrap(~ by) 
    
    if (is.factor(CI.col))
    {
      g <- g + 
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper), color = quote(CI.col)),  
                       alpha = CI.alpha, height = 0)  
    } else
    {
      g <- g + 
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper)), color = CI.col, 
                       alpha = CI.alpha, height = 0)  
    }
  } else
  {
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
        geom_errorbarh(aes_(xmin = quote(lower), xmax = quote(upper)), color = CI.col, 
                       alpha = CI.alpha, height = 0)  
    }
  }
  if (add.point) 
  {
    if (is.factor(point.type)) 
    {
      g <- g + geom_point(aes_(shape = quote(point.type)),
                          size = point.size)
    } else
    {
      g <- g + geom_point(shape = point.type,
                          size = point.size)
    }
  }
  return(g)
}