### import, check and optionnally normalize single-channel microarray data

microarraydata <- function(file, check = TRUE, 
                     norm.method = c("cyclicloess", "quantile", "scale", "none"))
{
  if (check)
  {
    # check argument file
    if (!is.character(file))
      stop("The argument file must be a character string")
    le.file <- nchar(file)
    suffix <- substr(file, le.file - 3, le.file)
    if (suffix != ".txt")
      stop("The argument file must be a character string ending by .txt")
  }
  
  d <- read.table(file, header = FALSE)
  nrowd <- nrow(d)
  ncold <- ncol(d)
  data <- as.matrix(d[2:nrowd, 2:ncold]) 
  
  if (check)
  {
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[,2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric 
           dose in the firt line and the numeric response of each item in the other
           lines.")
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
  
  # control of the design
  design <- table(dose, dnn = "")
  if (length(design) < 5)
    stop("Dromics cannot be used with a dose-response design 
         with less than five tested doses/concentrations")
  
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
                  norm.method = norm.method, data.beforenorm = data.beforenorm)  
  
  return(structure(reslist, class = "microarraydata"))
}


print.microarraydata <- function(x, ...)
{
  if (!inherits(x, "microarraydata"))
    stop("Use only with 'microarraydata' objects")
  
  cat("Elements of the experimental design in order to check the coding of the data :\n")
  cat("Tested doses and number of replicates for each dose:\n")
  print(x$design)
  cat("Number of items: ", length(x$item),"\n")
  
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
    cat("Data were normalized between arrays using the following method: ", x$norm.method," \n")
}

plot.microarraydata <- function(x, ...) 
{
  if (!inherits(x, "microarraydata"))
    stop("Use only with 'microarraydata' objects")

  def.par <- par(no.readonly = TRUE)
  if (x$norm.method != "none")
  {
    ymin <- min(x$data.beforenorm, x$data)
    ymax <- max(x$data.beforenorm, x$data)
    par(mfrow = c(1,2), xaxt = "n")
    boxplot(x$data.beforenorm, xlab = "Samples", ylab = "Signal", 
            main = paste("Microarray data before normalization"), ylim = c(ymin, ymax)) 
    boxplot(x$data, xlab = "Samples", ylab = "Signal", 
            main = paste("Microarray data after", x$norm.method,"normalization"), 
            ylim = c(ymin, ymax)) 
    
  } else
  {
    par(xaxt = "n")
    boxplot(x$data, xlab = "Samples", ylab = "Signal", 
            main = paste("Microarray data without normalization")) 
  }
  par(def.par)    
}


