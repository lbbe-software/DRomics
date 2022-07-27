# Select groups (corresponding to different biological annotations or groups of annotations)
# that we want to focus on using trendplot, sensitivityplot, bmdplotwithgradient ...
# - the selection can be based on the minimal nb of items in each group
# and/or on the BMDsummary value in each group.
# - if there is more than one experimental level, the selection of groups 
# is made separately in each experimental level, so a group may be selected
# for one experimental level and removed for another one
# 
selectgroups <- function(extendedres, group, explev,
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

    # Elimination of rows with NA values 
    extendedreswithoutNA <- extendedres[!is.na(extendedres$BMD.zSD),]
    variable <- extendedreswithoutNA$BMD.zSD
  } else 
  {
    if (any(!is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of selectgroups must be a dataframe
      containing at least columns named id and BMD.xfold.")

    # Elimination of rows with NA values 
    extendedreswithoutNA <- extendedres[!is.na(extendedres$BMD.xfold),]
    variable <- extendedreswithoutNA$BMD.xfold
  }
  if (!is.character(group)) 
    stop("group should be a character string for the name of the column defining groups.")
  if (!is.element(group, cnames))
    stop("group should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  
  if (!missing(explev))
  {
    if (!is.character(explev)) 
      stop("explev should be a character string for the name of the column coding for the experimental level.")
    if (!is.element(explev, cnames))
      stop("explev should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
  }
  
    
  firstquartilefun <- function(x) quantile(x, probs = 0.25)

  if (missing(explev))
  {
    group_explev <- factor(extendedreswithoutNA[, group])

  } else
  {
    group_explev <- factor(paste(extendedreswithoutNA[, group], extendedreswithoutNA[, explev], sep = "_"))
    
  }
  dnb <- as.data.frame(table(group_explev))
  colnames(dnb) <- c("group_explev", "nb_of_items")
  if (BMDsummary == "first.quartile")
    dnb$BMDsummary <- tapply(variable, group_explev, firstquartilefun) else
    dnb$BMDsummary <- tapply(variable, group_explev, median)
  
  if (!missing(BMDmax))
  {
    if (!is.numeric(BMDmax) | (BMDmax < 0))
      stop("Wrong argument 'BMDmax'. If not omitted it must be a positive number.")
    g_e2keep <- dnb[(dnb$nb_of_items >= nitemsmin) & (dnb$BMDsummary <= BMDmax), "group_explev"]
  } else
  {
    g_e2keep <- dnb[dnb$nb_of_items >= nitemsmin, "group_explev"]
  }
  
  subextendedres <- extendedreswithoutNA[group_explev %in% g_e2keep, ]

  return(subextendedres)
}
