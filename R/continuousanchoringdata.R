### import, check metabolomic data

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
        stop("The argument file must be a character string")
      le.file <- nchar(file)
      suffix <- substr(file, le.file - 3, le.file)
      if (suffix != ".txt")
        stop("The argument file must be a character string ending by .txt")
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
           dose in the firt line and the numeric response of each endpoint in the other
           lines.")
    warning("We recommend you to check that your anchoring data are continuous and
      defined in a scale that enable the use of a Gaussian error model (needed at each step
      of the workflow including the selection step). \n")
    
  }
  
  # definition of doses and endpoint identifiers
  (dose <- as.vector(unlist(d[1, 2:ncold])))
  row.names(data) <- item <- as.character(d[2:nrowd, 1])
  (nitems <- nrow(data))
  
  # control of the design
  design <- table(dose, dnn = "")
  if (length(design) < 4)
    stop("Dromics cannot be used with a dose-response design 
         with less than four tested doses/concentrations")
  if (length(design) == 4)
    warning("When using DRomics with a dose-response design with only four tested doses/concentrations, 
            it is recommended to check after the modelling step that all selected models have no more 
            than 4 parameters")  
  
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
  
  return(structure(reslist, class = "continuousanchoringdata"))
}


print.continuousanchoringdata <- function(x, ...)
{
  if (!inherits(x, "continuousanchoringdata"))
    stop("Use only with 'continuousanchoringdata' objects")
  
  cat("Elements of the experimental design in order to check the coding of the data :\n")
  cat("Tested doses and number of replicates for each dose:\n")
  print(x$design)
  cat("Number of endpoints: ", length(x$item),"\n")
  
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
    stop("Use only with 'continuousanchoringdata' objects")

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


