bmdfilter <- function(res,
                        BMDfilter = c("definedCI", "finiteCI", "definedBMD", "none"),
                        BMDtype = c("zSD", "xfold")
                        )
{
  if (missing(res) | !is.data.frame(res))
    stop("The first argument of bmdfilter must be a dataframe 
    (see ?bmdfilter for details).")
  
  BMDfilter <- match.arg(BMDfilter, c("definedCI", "finiteCI", "definedBMD", "none"))
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  cnames <- colnames(res)
  
  # Definition of the filter to apply 
  if ((BMDtype == "zSD") & (BMDfilter != "none"))
  {  
    if (!all(is.element(c("BMD.zSD"), cnames)))
      stop("The first argument of bmdfilter must be a dataframe
      containing a column named BMD.zSD.")
    BMD <- res$BMD.zSD
    if ((BMDfilter ==  "definedCI") | (BMDfilter ==  "finiteCI"))
    {
      if (!all(is.element(c("BMD.zSD.upper", "BMD.zSD.lower"), cnames)) )
      stop("To apply a filter on BMD.zSD confidence intervals, the first argument of bmdfilter 
      must be a dataframe containing columns named BMD.zSD, BMD.zSD.lower, BMD.zSD.upper.")
      BMDupper <- res$BMD.zSD.upper
      BMDlower <- res$BMD.zSD.lower
    }
  } else #so if (BMDtype == "xfold")
  if ((BMDtype == "xfold") & (BMDfilter != "none"))
  {
    if (!all(is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of bmdfilter must be a dataframe
      containing a column named BMD.xfold.")
    BMD <- res$BMD.xfold
    if ((BMDfilter ==  "definedCI") | (BMDfilter ==  "finiteCI"))
    {
      if (!all(is.element(c("BMD.xfold.upper","BMD.xfold.lower"), cnames)))
        stop("To apply a filter on BMD.xfold confidence intervals, the first argument of bmdfilter 
      must be a dataframe containing columns named BMD.xfold, BMD.xfold.lower, BMD.xfold.upper.")
      BMDupper <- res$BMD.xfold.upper
      BMDlower <- res$BMD.xfold.lower
    }
  }
  
  # Filtering
  if (BMDfilter == "definedCI")
  {
    subres <- res[!is.na(BMD) & !is.na(BMDupper) & !is.na(BMDlower), ]
  } else
    if (BMDfilter == "finiteCI")
    {
      subres <- res[is.finite(BMD) & is.finite(BMDupper) & is.finite(BMDlower), ]
    } else
      if (BMDfilter == "definedBMD")
      {
        subres <- res[!is.na(BMD), ]
      } else
        if (BMDfilter == "none")
        {
          subres <- res
        }  
  return(subres)
}

