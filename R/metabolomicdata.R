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
  
  if(any(!complete.cases(data)))
  {
    containsNA <- TRUE
  } else containsNA <- FALSE
  
  
  if (check)
  {
    if(containsNA)
    {
      warning(strwrap(prefix = "\n", initial = "\n",
                      "Your data contain NA values. 
      Make sure that those NA values correspond to missing
        values at random. If this is not the case 
        (e.g. missing values correspond to values 
        under a limit of quantification),
        you should
        consider an imputation of missing values.\n"))
    }
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

  # control of the design including on rows with NA values
  if(containsNA)
  {
    nonNAdata <- !is.na(data)
    minnonNAnbpts <- min(rowSums(nonNAdata))
    if (minnonNAnbpts < 8)
      stop(strwrap(prefix = "\n", initial = "\n",
      "Dromics cannot be used with a dose-response design 
    with less than eight data points
         per dose-response curve for each item. You should check your data
           to eliminate items with too many NA values or impute NA values if 
           they do not correspond to missing values at random 
           (e.g. missing values correspond to values 
        under a limit of quantification)."))
  }
  
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
                  design = design, data.mean = data.mean,
                  containsNA = containsNA)  
  
  return(structure(reslist, class = "continuousomicdata"))
}


print.continuousomicdata <- function(x, ...)
{
  if (!inherits(x, "continuousomicdata"))
    stop("Use only with 'continuousomicdata' objects.")

  nitems <- length(x$item)
  nitemswithNA <- nitems - sum(complete.cases(x$data))
  
  cat("Elements of the experimental design in order to check the coding of the data:\n")
  cat("Tested doses and number of replicates for each dose:\n")
  print(x$design)
  cat("Number of items:", nitems, "\n")
  if (nitemswithNA > 0) 
  {
    cat("Number of items with at least one missing data:", nitemswithNA, "\n")
    cat(strwrap("BE CAREFUL ! MISSING VALUES ARE CONSIDERED AS MISSING AT RANDOM ! IF THIS IS NOT THE CASE
        CONSIDER AN IMPUTATION METHOD FOR NON RANDOM MISSING DATA."), fill = TRUE)
    cat("\n")
  }
  
  if (length(x$item) > 20)
  {
    cat(strwrap("Identifiers of the first 20 items:"), fill = TRUE)
    cat("\n")
    print(x$item[1:20])
  } else
  {
    cat(strwrap("Identifiers of the items:"), fill = TRUE)
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


