# Select groups (corresponding to different biological annotations or groups of annotations)
# that we want to focus on using trendplot, sensitivityplot, bmdplotwithgradient ...
# - the selection can be based on the minimal nb of item in each group
# and optionnally on the BMDsummary value in each group
# - if there is more than one experimental level, the selection of groups 
# is made separately in each experimental level, so a group may be selected
# for one experimental level and removed for another one
# 
selectgroups <- function(extendedres, group, explevel,
                              BMDmax, 
                              BMDtype = c("zSD", "xfold"), 
                              BMDsummary = c("first.quartile", "median" ),
                              nitemsmin = 3
                         )
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of selectgroups must be a dataframe 
    (see ?selectgroups for details).")
  
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  BMDsummary <- match.arg(BMDsummary, c("first.quartile", "median" ))
  cnames <- colnames(extendedres)
  
  if (BMDtype == "zSD")
  {  
    if (any(!is.element(c("BMD.zSD"), cnames)))
      stop("The first argument of selectgroups must be a dataframe
      containing at least columns named id and BMD.zSD.")
    variable <- extendedres[, "BMD.zSD"]
  }
  else 
  {
    if (any(!is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of selectgroups must be a dataframe
      containing at least columns named id and BMD.xfold.")
    variable <- extendedres[, "BMD.xfold"]
  }
  if (!is.character(group)) 
    stop("group should be a character string for the name of the column defining groups.")
  if (!is.element(group, cnames))
    stop("group should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  
  if (!missing(explevel))
  {
    if (!is.character(explevel)) 
      stop("explevel should be a character string for the name of the column coding for the experimental level.")
    if (!is.element(explevel, cnames))
      stop("explevel should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  }
  
  firstquartilefun <- function(x) quantile(x, probs = 0.25)

  if (missing(explevel))
  {
    group_explevel <- as.factor(extendedres[, group])

  } else
  {
    group_explevel <- as.factor(paste(extendedres[, group], extendedres[, explevel], sep = "_"))
    
  }
  dnb <- as.data.frame(table(group_explevel))
  colnames(dnb) <- c("group_explevel", "nb_of_items")
  if (BMDsummary == "first.quartile")
    dnb$BMDsummary <- tapply(variable, group_explevel, firstquartilefun) else
    dnb$BMDsummary <- tapply(variable, group_explevel, median)
  
  if (!missing(BMDmax))
  {
    if (!is.numeric(BMDmax) | (BMDmax < 0))
      stop("Wrong argument 'BMDmax'. If not omitted it must be a positive number.")
    g_e2keep <- dnb[(dnb$nb_of_items >= nitemsmin) & (dnb$BMDsummary <= BMDmax), "group_explevel"]
  } else
  {
    g_e2keep <- dnb[dnb$nb_of_items >= nitemsmin, "group_explevel"]
  }
  
  subextendedres <- extendedres[group_explevel %in% g_e2keep, ]

  return(subextendedres)
}
