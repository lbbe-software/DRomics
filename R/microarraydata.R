### import, check and optionnally normalize single-channel microarray data

microarraydata <- function(file, backgrounddose, check = TRUE, 
                     norm.method = c("cyclicloess", "quantile", "scale", "none"))
{
  if (is.data.frame(file))
  {
    d <- file
  } else
  {
    if (check)
    {
      # check argument file
      if (!is.character(file))
        stop("The argument file must be a character string.")
      le.file <- nchar(file)
      suffix <- substr(file, le.file - 3, le.file)
      if (suffix != ".txt")
        stop("The argument file must be a character string ending by .txt.")
    }
    d <- utils::read.table(file, header = FALSE)
    colnames(d) <- c("item",paste("S", 1:(ncol(d)-1), sep = ""))
  }  
  nrowd <- nrow(d)
  ncold <- ncol(d)
  data <- as.matrix(d[2:nrowd, 2:ncold]) 

    if(any(!stats::complete.cases(data)))
    stop("microarraydata() should not be used with data including NA values.")
  
  if (check)
  {  
    if (any(data > 100))
    warning(strwrap(prefix = "\n", initial = "\n",
                    "Your data contain high values (> 100). 
      Make sure that your data (microarray signal) are in log-scale.\n"))
    if (nrowd < 100)
      warning(strwrap(prefix = "\n", initial = "\n",
                      "Your dataset contains less than 100 lines. Are you sure you really
      work on microarray data ? This function should
      not be used with another type of data."))
    
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[,2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric 
      dose in the firt line and the numeric response of each item in the other lines.")
  }
  
  # Normalization using limma
  norm.method <- match.arg(norm.method, c("cyclicloess", "quantile", "scale", "none"))
  if(norm.method == "cyclicloess")
    cat("Just wait, the normalization using cyclicloess may take a few minutes.\n")
  data.beforenorm <- data
  data <- normalizeBetweenArrays(data, method = norm.method)  
  
  # definition of doses and item identifiers
  (dose <- as.vector(unlist(d[1, 2:ncold])))
  row.names(data) <- item <- as.character(d[2:nrowd, 1])
  (nitems <- nrow(data))
  
  if (!missing(backgrounddose))
  {
    dose <- dose * (dose > backgrounddose)
  }
  
  # control of the design
  if (!any(dose == 0))
    stop(strwrap(prefix = "\n", initial = "\n",
                 "DRomics cannot be used on a design with no dose at zero. 
            In case of observational data, to prevent calculation of BMDs by extrapolation, 
            doses considered as corresponding to the background exposition (control) must 
                 be fixed at 0. You can use the argument backgrounddose for that purpose."))
  if (any(dose < 0))
    stop("DRomics cannot be used with negative values of doses.")
  design <- table(dose, dnn = "")
  nbdoses <- length(design)
  nbpts <- sum(design)
  if ((nbdoses < 4)| (nbpts < 8))
    stop("Dromics cannot be used with a dose-response design 
    with less than four tested doses/concentrations or less than eight data points
         per dose-response curve.")
  if (nbdoses < 6)
    warning(strwrap(prefix = "\n", initial = "\n",
      "To optimize the dose-response modelling, it is recommended to use
      a dose-response design with at least six different tested doses."))
  
  fdose <- as.factor(dose)
  tdata <- t(data)
  calcmean <- function(i)
  {
  #   tapply(data[i,], fdose, mean)
    tapply(tdata[, i], fdose, mean)
  }
  s <- sapply(1:(nrowd - 1), calcmean)
  data.mean <- as.matrix(t(s))
  
  reslist <- list(data = data, dose = dose, item = item, 
                  design = design, data.mean = data.mean, 
                  norm.method = norm.method, data.beforenorm = data.beforenorm,
                  containsNA = FALSE)  
  
  return(structure(reslist, class = "microarraydata"))
}


print.microarraydata <- function(x, ...)
{
  if (!inherits(x, "microarraydata"))
    stop("Use only with 'microarraydata' objects.")
  
  cat("Elements of the experimental design in order to check the coding of the data:\n")
  cat("Tested doses and number of replicates for each dose:\n")
  print(x$design)
  cat("Number of items:", length(x$item), "\n")
  
  if (length(x$item) > 20)
  {
    cat("Identifiers of the first 20 items:\n")
    print(x$item[1:20])
  } else
  {
    cat("Identifiers of the items:\n")
    print(x$item)
  }
  if (x$norm.method != "none")
    cat("Data were normalized between arrays using the following method:", x$norm.method, "\n")
}

plot.microarraydata <- function(x, range4boxplot = 1e6, ...) 
{
  if (!inherits(x, "microarraydata"))
    stop("Use only with 'microarraydata' objects.")

  def.par <- graphics::par(no.readonly = TRUE)
  if (x$norm.method != "none")
  {
    ymin <- min(x$data.beforenorm, x$data)
    ymax <- max(x$data.beforenorm, x$data)
    graphics::par(mfrow = c(1,2), xaxt = "n")
    graphics::boxplot(x$data.beforenorm, xlab = "Samples", ylab = "Signal", range = range4boxplot,
            main = paste("Microarray data before normalization"), ylim = c(ymin, ymax), ...) 
    graphics::boxplot(x$data, xlab = "Samples", ylab = "Signal", range = range4boxplot,
            main = paste("Microarray data after", x$norm.method,"normalization"), 
            ylim = c(ymin, ymax), ...) 
    
  } else
  {
    graphics::par(xaxt = "n")
    graphics::boxplot(x$data, xlab = "Samples", ylab = "Signal", range = range4boxplot, 
            main = paste("Microarray data without normalization")) 
  }
  graphics::par(def.par)    
}


