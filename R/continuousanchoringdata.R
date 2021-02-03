### import, check continuous anchoring data

continuousanchoringdata <- function(file, check = TRUE)
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
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[,2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric 
      dose in the firt line and the numeric response of each endpoint in the other lines.")
    warning(strwrap(prefix = "\n", initial = "\n",
      "We recommend you to check that your anchoring data are continuous and
      defined in a scale that enable the use of a normal error model (needed at each step
      of the workflow including the selection step). \n"))
  }
  
  # definition of doses and endpoint identifiers
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
  if(any(!complete.cases(data)))
  {
    containsNA <- TRUE
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
  } else containsNA <- FALSE
  
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
  
  return(structure(reslist, class = "continuousanchoringdata"))
}


print.continuousanchoringdata <- function(x, ...)
{
  if (!inherits(x, "continuousanchoringdata"))
    stop("Use only with 'continuousanchoringdata' objects.")
  
  nitems <- length(x$item)
  nitemswithNA <- nitems - sum(complete.cases(x$data))
  
  cat("Elements of the experimental design in order to check the coding of the data:\n")
  cat("Tested doses and number of replicates for each dose:\n")
  print(x$design)
  cat("Number of endpoints:", nitems,"\n")
  if (nitemswithNA > 0) cat("Number of endpoints with at least one missing data:", nitemswithNA,"\n")
  
  if (length(x$item) > 20)
  {
    cat("Identifiers of the first 20 endpoints:\n")
    print(x$item[1:20])
  } else
  {
    cat("Names of the endpoints:\n")
    print(x$item)
  }
}

plot.continuousanchoringdata <- function(x, ...) 
{
  if (!inherits(x, "continuousanchoringdata"))
    stop("Use only with 'continuousanchoringdata' objects.")

  nitems <- nrow(x$data)
  dataobs <- data.frame(dose = numeric(), measure = numeric(), 
                        endpoint = character())
  for (i in 1:nitems)
  {
    dataobs <- rbind(dataobs, 
                     data.frame(dose = x$dose, 
                                measure = x$data[i, ], 
                                endpoint = x$item[i]))
  }
  
  dataobs$endpoint <- factor(dataobs$endpoint)
  
  g <- ggplot(dataobs, aes_(x = quote(dose), y = quote(measure))) + geom_point(shape = 1) +
    facet_wrap(~ endpoint, scales = "free_y") 
  return(g)
}


