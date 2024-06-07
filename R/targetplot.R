# Plot of raw data and fitted curves if there is for target items 
targetplot <- function(items, f, add.fit = TRUE, dose_log_transfo = TRUE)
{
  if (!inherits(f, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit.")

    if(!(is.character(items)) )
    stop("Wrong argument 'items'. It must be a character vector indicating the identifiers of the items who want to plot.")
  
  o <- f$omicdata
  dose <- o$dose
  irowitems <- match(items, o$item)

  nobs <- length(dose)
  doseu <- as.numeric(colnames(o$data.mean)) # sorted unique doses
  ndose <- length(doseu)
  npts <- 100
  if (dose_log_transfo)
  {
    minx <- min(dose[dose != 0]) 
    maxx <- max(dose)
    xplot <- c(0, 10^seq(log10(minx), log10(maxx), length.out = npts))
  } else
  {
    xplot <- seq(0, max(dose), length.out = npts)
  }
  nitems <- length(irowitems)
  dataobs <- data.frame(dose = numeric(), signal = numeric(), 
                        id = character())
  dataobsmean <- data.frame(dose = numeric(), signal = numeric(), 
                            id = character())
  if (add.fit)
  {
    datatheo <- data.frame(dose = numeric(), signal = numeric(), 
                           id = character())
    if (dose_log_transfo)
    {
      datatheo0 <- data.frame(dose = numeric(), signal = numeric(), 
                             id = character())
    }
  }
  for (i in 1:nitems)
  {
    irow <- irowitems[i]
    ident <- o$item[irow]
    datai <- o$data[irow, ]
    datameani <- o$data.mean[irow, ]
    dataobs <- rbind(dataobs, 
                     data.frame(dose = dose, signal = datai, id = rep(ident, nobs)))
    dataobsmean <- rbind(dataobsmean, 
                         data.frame(dose = doseu, signal = datameani, id = rep(ident, ndose)))
    if (add.fit)
    {
      irowfitres <- match(ident, f$fitres$id)
      if (!is.na(irowfitres))
      {
        rowfitres <- f$fitres[irowfitres, ]
        if (rowfitres$model == "exponential") datapred <- fExpo(x = xplot, d = rowfitres$d, b = rowfitres$b, e = rowfitres$e)
        if (rowfitres$model == "Hill") datapred <- fHill(x = xplot, c = rowfitres$c, d = rowfitres$d, b = rowfitres$b, e = rowfitres$e)
        if (rowfitres$model == "log-Gauss-probit") datapred <- fLGauss5p(x = xplot, c = rowfitres$c, d = rowfitres$d, b = rowfitres$b, e = rowfitres$e, f = rowfitres$f)
        if (rowfitres$model == "Gauss-probit") datapred <- fGauss5p(x = xplot, c = rowfitres$c, d = rowfitres$d, b = rowfitres$b, e = rowfitres$e, f = rowfitres$f)
        if (rowfitres$model == "linear") datapred <- xplot * rowfitres$b + rowfitres$d
        
        if (dose_log_transfo)
        {
          datatheo <- rbind(datatheo,
                            data.frame(dose = xplot[-1], signal = datapred[-1], id = rep(ident, npts)))
          datatheo0 <- rbind(datatheo0,
                            data.frame(dose = xplot[1], signal = datapred[1], id = ident))
          
        } else
        {
          datatheo <- rbind(datatheo,
                            data.frame(dose = xplot, signal = datapred, id = rep(ident, npts)))
        }
       }
    }
  }
  
  dataobs$id <- factor(dataobs$id, levels = items)
  dataobsmean$id <- factor(dataobsmean$id, levels = items)

  g <- ggplot(dataobs, aes(x = .data$dose, y = .data$signal)) + geom_point(shape = 1) +
    facet_wrap(~ id, scales = "free_y") +
    geom_point(data = dataobsmean, shape = 19) + theme_classic()
  
  if (add.fit) 
  {
    datatheo$id <- factor(datatheo$id, levels = items)
    if (dose_log_transfo) 
    {
      datatheo0$id <- factor(datatheo0$id, levels = items)
      g <- g + geom_line(data = datatheo, colour = "red") +
        geom_point(data = datatheo0, colour = "red") 
      g <- g + scale_x_log10() + xlab("dose (in log scale)")
    } else
    {
      g <- g + geom_line(data = datatheo, colour = "red") 
    }
  }
  
  return(g)
}