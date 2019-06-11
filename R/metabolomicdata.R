### import, check metabolomic data

metabolomicdata <- function(file, check = TRUE)
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
    warning("We recommend you to check that your metabolomic data were 
correctly pretreated before importation especially so that 
the hypothesis of Gaussian error model used during the selection
and model fitting processes could be considered valid \n")
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
                  design = design, data.mean = data.mean)  
  
  return(structure(reslist, class = "metabolomicdata"))
}


print.metabolomicdata <- function(x, ...)
{
  if (!inherits(x, "metabolomicdata"))
    stop("Use only with 'metabolomic' objects")
  
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
    cat("We recommend you to check that your metabolomic data were 
        correctly pretreated
        before importation epecially so that the hypothesis of Gaussian error model 
        used during the selection and model fitting processes could 
        be considered valid \n")
}

plot.metabolomicdata <- function(x, ...) 
{
  if (!inherits(x, "metabolomicdata"))
    stop("Use only with 'metabolomicdata' objects")

  def.par <- par(no.readonly = TRUE)
    par(xaxt = "n")
    boxplot(x$data, xlab = "Samples", ylab = "Signal", 
            main = paste("Metabolomic data")) 
  par(def.par)    
}


