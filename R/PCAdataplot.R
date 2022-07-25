# Plot of trend repartition per group of items, 
# (e.g. from biological annotation), 
# and optionally per molecular level 
# (or per another additional grouping level )
PCAdataplot <- function(omicdata, batch)
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
    data4autoplot <- data.frame(batch = as.factor(batch), 
                                dose = dosef)
    
   } else
   {
     data4autoplot <- data.frame(dose = dosef)
     
   }
  
    
  pseudologdata <- as.data.frame(omicdata$data) # data after log 2 scale 
  # colnames(pseudologdata) <- 1:ncol(pseudologdata)
  # prcomp{stats}
  # autoplot du package ggfortify qui est un ajout de ggplot2
  # https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
  pca.info <- prcomp(t(pseudologdata), scale. = FALSE)
  if (missing(batch))
  {
    pca.plot <- autoplot(pca.info, 
                         data = data4autoplot, 
                         colour = "dose")
    
  } else
  {
    pca.plot <- autoplot(pca.info, 
                         data = data4autoplot, 
                         shape = "batch", 
                         colour = "dose")
  }
  # pca.plot <- autoplot(pca.info, 
  #                      data = data4autoplot, 
  #                      shape = FALSE, 
  #                      label = TRUE,
  #                      colour = quote(dose))
  return(pca.plot)
}