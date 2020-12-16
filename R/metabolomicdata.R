### import, check metabolomic data
### or other continuous omic data

metabolomicdata <- function(file, check = TRUE)
{
  continuousomicdata(file, check = check)
}
  
continuousomicdata <- function(file, check = TRUE)
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
    d <- read.table(file, header = FALSE)
  } 
  nrowd <- nrow(d)
  ncold <- ncol(d)
  data <- as.matrix(d[2:nrowd, 2:ncold]) 
  if (any(data > 100))
    warning(strwrap(prefix = "\n", initial = "\n",
      "Your data contain high values (> 100). 
      Make sure that your data are in a reasonable scale 
      (e.g. log-scale for metabolomic signal).\n"))
  if (nrowd < 100)
    warning(strwrap(prefix = "\n", initial = "\n",
      "Your dataset contains less than 100 lines. Are you sure you really
      work on omic data ? If the different lines of your data set contain
      different endpoints you should use the function continuousanchoringdata()."))
  
  if (check)
  {
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[, 2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric 
      dose in the firt line and the numeric response of each item in the other lines.")
    warning(strwrap(prefix = "\n", initial = "\n", 
      "We recommend you to check that your omic data were correctly pretreated
      before importation. In particular data (e.g. metabolomic signal)
      should have been log-transformed, without replacing 0 values by NA values
      (consider using the half minimum method instead for example). \n"))
  }
  
  # definition of doses and item identifiers
  (dose <- as.vector(unlist(d[1, 2:ncold])))
  row.names(data) <- item <- as.character(d[2:nrowd, 1])
  (nitems <- nrow(data))
  
  # control of the design
  if (any(dose < 0))
    stop("DRomics cannot be used with negative values of doses.")
  design <- table(dose, dnn = "")
  if (length(design) < 4)
    stop("Dromics cannot be used with a dose-response design 
    with less than four tested doses/concentrations.")
  if (length(design) == 4)
    warning(strwrap(prefix = "\n", initial = "\n",
      "When using DRomics with a dose-response design with only four tested doses/concentrations, 
      it is recommended to check after the modelling step that all selected models have no more 
      than 4 parameters."))
  
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
                  design = design, data.mean = data.mean)  
  
  return(structure(reslist, class = "continuousomicdata"))
}


print.continuousomicdata <- function(x, ...)
{
  if (!inherits(x, "continuousomicdata"))
    stop("Use only with 'continuousomicdata' objects.")
  
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
}

plot.continuousomicdata <- function(x, ...) 
{
  if (!inherits(x, "continuousomicdata"))
    stop("Use only with 'continuousomicdata' objects.")

  def.par <- par(no.readonly = TRUE)
    par(xaxt = "n")
    boxplot(x$data, xlab = "Samples", ylab = "Signal", 
            main = paste("Continuous omics data"), ...) 
  par(def.par)    
}


