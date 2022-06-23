library(shiny, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(shinyBS, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
library(DRomics, quietly = TRUE)
library(sortable, quietly = TRUE)
options(shiny.maxRequestSize=30*1024^2)

texthelpnblevel <- "The maximal number of experimental levels is 10. The experimental levels can be for example different molecular levels (transcriptomics, metabolomis, ...), different experimental time points or different biological models (different species, different experimental settings), ..."
helplabel1step1 <- "Each file for annotation data must have exactly two columns."
helplabel2step1 <- "If there are multiple experimental levels, the labels chosen must be unique. All spaces in the labels are removed."

helplabel1step2 <- "To limit the number of annotation groups you can use the thresholds on the number of items representing the group and/or the BMDsummary value of the group."

helplabel1step4 <- "blabla"


##################################################################################################################################
##################################################################################################################################
##################################################################################################################################

# Select groups (corresponding to different biological annotations or groups of annotations)
# that we want to focus on using trendplot, sensitivityplot, bmdplotwithgradient ...
# - the selection can be based on the minimal nb of item in each group
# and optionnally on the BMDsummary value in each group
# - if there is more than one experimental level, the selection of groups 
# is made separately in each experimental level, so a group may be selected
# for one experimental level and removed for another one
# 
selectgroups <- function(extendedres, group, explev,
                         BMDmax, 
                         BMDtype = c("zSD", "xfold"), 
                         BMDsummary = c("first.quartile", "median" ),
                         nitemsmin = 3, selectateachexplev = TRUE
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
    group_explev <- as.factor(extendedres[, group])
    
  } else
  {
    group_explev <- as.factor(paste(extendedres[, group], extendedres[, explev], sep = "_"))
    
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
  
  subextendedres <- extendedres[group_explev %in% g_e2keep, ]
  
  return(subextendedres)
}
