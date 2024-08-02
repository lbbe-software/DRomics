# Plot of trend repartition per group of items, 
# (e.g. from biological annotation), 
# and optionally per molecular level 
# (or per another additional grouping level )
PCAdataplot <- function(omicdata, batch, label)
{
  if (!(inherits(omicdata, "microarraydata") | 
        inherits(omicdata, "RNAseqdata") |
        inherits(omicdata, "continuousomicdata")))
    stop("Use only with 'microarraydata', 'RNAseqdata' or 'continuousomicdata' 
    objects, respectively created with functions 'microarraydata()', 'RNAseqdata()' 
    or 'continuousomicdata()'.")

  dosef <- as.factor(omicdata$dose)
  if (!missing(batch)) 
  {
    if (length(batch) != length(dosef))
      stop("Argument batch must be a factor of length the number of samples in
           your omic data set.")
    datacondition <- data.frame(batch = as.factor(batch), 
                                dose = dosef)
    
   } else
   {
     datacondition <- data.frame(dose = dosef)
     
   }
  
  pseudologdata <- as.data.frame(omicdata$data) # data after log 2 scale 
  if (missing(label))
  {
    label <- FALSE
  } 
  if (length(label) > 1) 
  {
    add.label <- TRUE
    if (length(label) != length(dosef))
    {
      stop("Argument label must be TRUE, FALSE of a character vector 
       giving the name the samples, so of length 
            the number of samples in your omic data set.")
    } else
    {
      colnames(pseudologdata) <- as.character(label)
      
    }
  } else
  {
    if (label) {add.label <- TRUE} else {add.label <- FALSE}
  }
  
  # colnames(pseudologdata) <- 1:ncol(pseudologdata)
  # prcomp{stats}
  # autoplot du package ggfortify qui est un ajout de ggplot2
  # https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
  pca.info <- stats::prcomp(t(pseudologdata), scale. = FALSE)
  if (add.label)
  {
    pca.plot <- autoplot(pca.info, 
                         data = datacondition, 
                         colour = "dose",
                         shape = FALSE,
                         label = TRUE)
    
  } else
  {
    if (missing(batch))
    {
      pca.plot <- autoplot(pca.info, 
                           data = datacondition, 
                           colour = "dose")
      
    } else
    {
      pca.plot <- autoplot(pca.info, 
                           data = datacondition, 
                           shape = "batch", 
                           colour = "dose")
    }
    
  }
  return(pca.plot)
}
