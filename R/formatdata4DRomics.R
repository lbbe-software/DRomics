formatdata4DRomics <- function(signalmatrix, dose, samplenames)
{
  signalmatrix <- as.matrix(signalmatrix)
  if (missing(samplenames)) samplenames <- colnames(signalmatrix)
  ncond <- ncol(signalmatrix)
  if (missing(dose)) 
  {
    stop("You must specify dose as a numeric vector giving the dose of each sample.")
    
  } else
  {
    if ((length(dose) != ncond) | !is.numeric(dose))
      stop("The input dose must be a numeric vector of length equal to the number of columns of
           signal.matrix, giving the dose of each sample (so each column of signal.matrix).")
  }
  itemnames <- rownames(signalmatrix)
  column1 <- data.frame(V1 = as.factor(c("item", itemnames)))
  othercolumns <- as.data.frame(as.matrix(rbind(dose, signalmatrix)))
  data4DRomics <- cbind(column1, othercolumns)
  colnames(data4DRomics) <- c("", samplenames)
  rownames(data4DRomics) <- seq_len(nrow(data4DRomics))
  return(data4DRomics)
}
