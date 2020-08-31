### import, check normalize and transform RNAseq data

RNAseqdata <- function(file, check = TRUE, 
                     transfo.method = c("rlog", "vst"), transfo.blind = FALSE)
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
  subdata4check <- data[1:min(nrow(data), 10), ]
  subdata4checkT <- trunc(subdata4check)
  if (!identical(subdata4check, subdata4checkT))
    stop("Your data contain non integer values. 
            Make sure that your RNAseq data are imported in raw counts.\n") 
  if (nrowd < 100)
    warning("Your dataset contains less than 100 lines. Are you sure you really
            work on RNAseq data ? This function should
            not be used with another type of data.")
  
  
  if (check)
  {
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[,2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric 
           dose in the firt line and the read counts (integer values corresponding 
            to raw counts) of each item in the other lines.")
  }
  
  # Normalization and count data transformation using DESeq2
  transfo.method <- match.arg(transfo.method, c("rlog", "vst"))
  if(transfo.method == "rlog")
    cat("Just wait, the transformation using regularized logarithm (rlog)
        may take a few minutes.\n")
  
  raw.counts <- data
  (dose <- as.vector(unlist(d[1, 2:ncold])))
  fdose <- as.factor(dose)
  
  if(!transfo.blind)
  {
    coldata <- data.frame(fdose = fdose)
    rownames(coldata) <- colnames(data)
    dds <- DESeqDataSetFromMatrix(
      countData = raw.counts,
      colData = coldata,
      design = ~ fdose)
  }
    
  if (transfo.method == "rlog")
  {
    if (transfo.blind) 
      {
        data <- rlog(raw.counts)
      } else
      {
        data <- assay(rlog(dds, blind = FALSE))  
      }
  } else # transfo.method == "vst"
  {
    if (transfo.blind) 
    {
      data <- vst(raw.counts)
    } else
    {
      data <- assay(vst(dds, blind = FALSE))  
    }
  }
  
  # definition of doses and item identifiers
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

  # calculation of the means per dose
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
                  transfo.method = transfo.method, raw.counts = raw.counts)  
  
  return(structure(reslist, class = "RNAseqdata"))
}


print.RNAseqdata <- function(x, ...)
{
  if (!inherits(x, "RNAseqdata"))
    stop("Use only with 'RNAseqdata' objects")
  
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
  cat("Data were normalized with respect to library size
        and  tranformed using the following method: ", x$transfo.method," \n")
}

plot.RNAseqdata <- function(x, ...) 
{
  if (!inherits(x, "RNAseqdata"))
    stop("Use only with 'RNAseqdata' objects")

  def.par <- par(no.readonly = TRUE)
  ymin.rc <- min(x$raw.counts)
  ymax.rc <- max(x$raw.counts)
  par(mfrow = c(1,2), xaxt = "n")
  boxplot(x$raw.counts, xlab = "Samples", ylab = "Raw counts", 
          main = paste("Raw data"), 
          ylim = c(ymin.rc, ymax.rc), ...) 
  ymin.log <- min(x$data)
  ymax.log <- max(x$data)
  boxplot(x$data, xlab = "Samples", ylab = "Signal", 
          main = paste("Normalized and transformed data"), 
          ylim = c(ymin.log, ymax.log), ...)   
  par(def.par)    
}


