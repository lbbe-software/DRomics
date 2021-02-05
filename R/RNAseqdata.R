### import, check normalize and transform RNAseq data

RNAseqdata <- function(file, check = TRUE, 
                       transfo.method = c("rlog", "vst"), 
                       transfo.blind = TRUE, round.counts = FALSE)
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
  nrowdata <- nrowd - 1

  if(any(!complete.cases(data)))
    stop("RNAseqdata() should not be used with data including NA values.")
  
  if (round.counts)
  {
    data <- round(data)
  } else
  {
    subdata4check <- as.numeric(data[1:min(nrowdata, 10), ])
    subdata4checkT <- trunc(subdata4check)
    if (!identical(subdata4check, subdata4checkT))
      stop("Your data contain non integer values. Make sure that your RNAseq data are imported in raw counts.
      If your counts come from Kallisto or Salmon put the argument round.counts of RNAseqdata at TRUE to round them.\n") 
  }
  
  if (check)
  {
    if (nrowdata < 100)
      warning(strwrap(prefix = "\n", initial = "\n",
                      "Your dataset contains less than 100 lines. Are you sure you really
      work on RNAseq data ? This function should
      not be used with another type of data."))
    
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[,2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric dose in the firt line 
      and the read counts (integer values corresponding to raw counts) of each item in the other lines.")
  }
  
  # Normalization and count data transformation using DESeq2
  transfo.method <- match.arg(transfo.method, c("rlog", "vst"))
  if(transfo.method == "rlog")
    cat(strwrap(paste0("Just wait, the transformation using regularized logarithm (rlog) may take a few minutes.\n")), fill = TRUE)
  
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
    nsub <- 1000 # parameter of vst to speed computation
    if (transfo.blind) 
    {
      if (nrowdata < nsub)
      {
        data <- varianceStabilizingTransformation(raw.counts)
      } else
      {
        tryvst <- try(vst(raw.counts, nsub = nsub), silent = TRUE)
        if (!inherits(tryvst, "try-error"))
        {
          data <- tryvst
        } else
        {
          data <- varianceStabilizingTransformation(raw.counts)
        }
      }
    } else # VST is not blind to the experimental design
    {
      if (nrowdata < nsub)
      {
        data <- assay(varianceStabilizingTransformation(dds, blind = FALSE))
      } else
      {
        tryvst <- try(vst(dds, nsub = nsub, blind = FALSE), silent = TRUE)
        if (!inherits(tryvst, "try-error"))
        {
          data <- assay(tryvst)
        } else
        {
          data <- assay(varianceStabilizingTransformation(dds, blind = FALSE))
        }
      }
    }
  }
  
  # definition of doses and item identifiers
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
                  transfo.method = transfo.method, raw.counts = raw.counts,
                  containsNA = FALSE)  
  
  return(structure(reslist, class = "RNAseqdata"))
}


print.RNAseqdata <- function(x, ...)
{
  if (!inherits(x, "RNAseqdata"))
    stop("Use only with 'RNAseqdata' objects.")
  
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
  cat(strwrap(paste0("Data were normalized with respect to library size 
                     and tranformed using the following method: ", x$transfo.method)), fill = TRUE)
}

plot.RNAseqdata <- function(x, ...) 
{
  if (!inherits(x, "RNAseqdata"))
    stop("Use only with 'RNAseqdata' objects.")
  
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


