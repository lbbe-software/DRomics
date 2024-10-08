ecdfquantileplot <- function(variable, by, quantile.prob = 0.5, title) {
  if (!is.numeric(variable))
    stop("Arguments variable must be a numeric vector.")
  leng <- length(variable)
  if (missing(by))
    stop("Argument 'by' is mandatory and must be a factor of the same length as argument 'variable'.")
  if (length(by) != leng)
    stop("Argument 'by' must be a factor of the same length as argument 'variable'.")
  group <- as.factor(by)
  
  if (!is.numeric(quantile.prob) || (quantile.prob >= 1) || (quantile.prob <= 0))
    stop("Wrong argument 'quantile.prob'. If not omitted it must be a number between 0 and 1 
    e(the probability defining the quantile).")
  
  # faire un titre par défaut incluant quantile.prob
  quantilepc <- quantile.prob * 100
  if (missing(title)) {
    title <- paste("ECDF plot of ", quantilepc, "th quantiles")
  }
  
  dnb <- as.data.frame(table(group))
  colnames(dnb) <- c("group", "nb_of_items")
  quantilefun <- function(x) stats::quantile(x, probs = quantile.prob, na.rm = FALSE)
  dnb$quantiles <- tapply(variable, group, quantilefun)
  dnb$ecdf <- (rank(dnb$quantiles) - 0.5) / nrow(dnb)
  # order by quantile
  dnb <- dnb[order(dnb$quantiles), ]
  # fix the order of the modalities of by as in the ordered data set
  dnb$group <- factor(dnb$group, levels = dnb$group)
  g <- ggplot(dnb, aes(x = .data$group, y = .data$quantiles, size = .data$nb_of_items)) +
    geom_point(stat = "identity") +
    coord_flip() + labs(title = title)
  
  round.quartiles.minmax <- unique(round(quantile(dnb$nb_of_items, probs = c(0, 0.25, 0.5, 0.75, 1))))
  g <- g + scale_size_continuous(breaks = as.numeric(round.quartiles.minmax)) +
    labs(size = "nb. of items")
  
  return(g)
}
