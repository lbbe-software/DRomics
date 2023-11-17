selectitems <- function(extendedres,
                        BMDfilter = c("definedCI", "finiteCI", "definedBMD", "none"),
                        BMDtype = c("zSD", "xfold")
                        )
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of selectitems must be a dataframe 
    (see ?selectitems for details).")
  
  BMDfilter <- match.arg(BMDfilter, c("definedCI", "finiteCI", "definedBMD", "none"))
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  cnames <- colnames(extendedres)
  
  # Definition of the filter to apply 
  if ((BMDtype == "zSD") & (BMDfilter != "none"))
  {  
    if (any(!is.element(c("BMD.zSD"), cnames)))
      stop("The first argument of selectitems must be a dataframe
      containing at least columns named id and BMD.zSD.")
    BMD <- extendedres$BMD.zSD
    if ((BMDfilter ==  "definedCI") | (BMDfilter ==  "finiteCI"))
    {
      if (any(!is.element(c("BMD.zSD.upper", "BMD.zSD.lower"), cnames)) )
      stop("To apply a filter on BMD.zSD confidence intervals, the first argument of selectitems 
      must be a dataframe containing at least columns named id and BMD.zSD, BMD.zSD.lower, BMD.zSD.upper.")
      BMDupper <- extendedres$BMD.zSD.upper
      BMDlower <- extendedres$BMD.zSD.lower
    }
  } else #so if (BMDtype == "xfold")
  if ((BMDtype == "xfold") & (BMDfilter != "none"))
  {
    if (any(!is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of selectitems must be a dataframe
      containing at least columns named id and BMD.xfold.")
    BMD <- extendedres$BMD.xfold
    if ((BMDfilter ==  "definedCI") | (BMDfilter ==  "finiteCI"))
    {
      if (any(!is.element(c("BMD.xfold.upper","BMD.xfold.lower"), cnames)))
        stop("To apply a filter on BMD.xfold confidence intervals, the first argument of selectitems 
      must be a dataframe containing at least columns named id and BMD.xfold, BMD.xfold.lower, BMD.xfold.upper.")
      BMDupper <- extendedres$BMD.xfold.upper
      BMDlower <- extendedres$BMD.xfold.lower
    }
  }
  
  # Filtering
  if (BMDfilter == "definedCI")
  {
    subextendedres <- extendedres[!is.na(BMD) & !is.na(BMDupper) & !is.na(BMDlower), ]
  } else
    if (BMDfilter == "finiteCI")
    {
      subextendedres <- extendedres[is.finite(BMD) & is.finite(BMDupper) & is.finite(BMDlower), ]
    } else
      if (BMDfilter == "definedBMD")
      {
        subextendedres <- extendedres[!is.na(BMD), ]
      } else
        if (BMDfilter == "none")
        {
          subextendedres <- extendedres
        }  
  return(subextendedres)
}

