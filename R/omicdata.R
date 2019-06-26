### deprecated function that was replaced by microarraydata()

omicdata <- function(file, check = TRUE, 
                     norm.method = c("cyclicloess", "quantile", "scale", "none"))
{
  warning("omicdata() is a deprecated function that was replaced by microarraydata(). 
          You should replace it by microarraydata(), RNAseqdata() or metabolomicdata()
          depending of the type of data you handle. \n")
  microarraydata(file = file, check = check, norm.method = norm.method)
}


