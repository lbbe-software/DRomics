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
  
  if (check)
  {
    if(any(!complete.cases(data)))
      warning(strwrap(prefix = "\n", initial = "\n",
                      "Your data contain NA values. 
      Make sure that those NA values correspond to missing
        values at random. If this is not the case 
        (e.g. missing values correspond to values 
        under a limit of quantification),
        you should
        consider an imputation of missing values.\n"))
    if (any(data > 100, na.rm = TRUE))
      warning(strwrap(prefix = "\n", initial = "\n",
                      "Your data contain high values (> 100). 
      Make sure that your data are in a reasonable scale 
      (e.g. log-scale for metabolomic signal).\n"))
    if (nrowd < 100)
      warning(strwrap(prefix = "\n", initial = "\n",
                      "Your dataset contains less than 100 lines. Are you sure you really
      work on omic data ? If the different lines of your data set contain
      different endpoints you should use the function continuousanchoringdata()."))
    
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
  meanwithnarm <- function(v) mean(v, na.rm = TRUE)
  calcmean <- function(i)
  {
    tapply(tdata[, i], fdose, meanwithnarm)
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

  nitems <- length(x$item)
  nitemswithNA <- nitems - sum(complete.cases(x$data))
  
  cat("Elements of the experimental design in order to check the coding of the data :\n")
  cat("Tested doses and number of replicates for each dose:\n")
  print(x$design)
  cat("Number of items: ", nitems,"\n")
  if (nitemswithNA > 0) 
  {
    cat("Number of items with at least one missing data: ", nitemswithNA,"\n")
    cat(strwrap(prefix = "\n", initial = "\n",
        "BE CAREFUL ! MISSING VALUES ARE CONSIDERED AS MISSING AT RANDOM ! IF THIS IS NOT THE CASE
        CONSIDER AN IMPUTATION METHOD FOR NON RANDOM MISSING DATA.\n"))
    cat("\n")
  }
  
  if (length(x$item) > 20)
  {
    cat(strwrap(prefix = "\n", initial = "\n",
                "Identifiers of the first 20 items:\n"))
    cat("\n")
    print(x$item[1:20])
  } else
  {
    cat(strwrap(prefix = "\n", initial = "\n",
                "Identifiers of the items:\n"))
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


