selectitems <- function(extendedres,
                        BMDtype = c("zSD", "xfold"),
                        BMDfilter = c("definedBMD", "definedCI", "finiteCI") 
                         )
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of selectitems must be a dataframe 
    (see ?selectitems for details).")
  
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  cnames <- colnames(extendedres)
  
  if (BMDtype == "zSD")
  {  
    if (any(!is.element(c("BMD.zSD"), cnames)))
      stop("The first argument of selectitems must be a dataframe
      containing at least columns named id and BMD.zSD.")

    # Elimination of rows with NA values 
    extendedreswithoutNA <- extendedres[!is.na(extendedres$BMD.zSD),]
    variable <- extendedreswithoutNA$BMD.zSD
  } else 
  {
    if (any(!is.element(c("BMD.xfold"), cnames)))
      stop("The first argument of selectitems must be a dataframe
      containing at least columns named id and BMD.xfold.")

    # Elimination of rows with NA values 
    extendedreswithoutNA <- extendedres[!is.na(extendedres$BMD.xfold),]
    variable <- extendedreswithoutNA$BMD.xfold
  }

  # if (!missing(BMDmax))
  # {
  #   if (!is.numeric(BMDmax) | (BMDmax < 0))
  #     stop("Wrong argument 'BMDmax'. If not omitted it must be a positive number.")
  #   g_e2keep <- dnb[(dnb$nb_of_items >= nitemsmin) & (dnb$BMDsummary <= BMDmax), "group_explev"]
  # } else
  # {
  #   g_e2keep <- dnb[dnb$nb_of_items >= nitemsmin, "group_explev"]
  # }
  # 
  # if (keepallexplev & !missing(explev))
  # {
  #   g2keep <- unique(group_allexplev[group_explev %in% g_e2keep])
  #   subextendedres <- extendedreswithoutNA[group_allexplev %in% g2keep, ]
  # } else
  # {
  #   subextendedres <- extendedreswithoutNA[group_explev %in% g_e2keep, ]
  # }

  return(subextendedres)
}

############## Examples ##################
# (1) a toy example (a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt",
                            package = "DRomics")

# to test the package on a small but not very small data set
# use the following commented line
# datafilename <- system.file("extdata", "transcripto_sample.txt", package = "DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05)
f <- drcfit(s_quad, progressbar = TRUE)
r <- bmdcalc(f)
set.seed(1234) # to get reproducible results with a so small number of iterations
(b <- bmdboot(r, niter = 50)) # with a non reasonable value for niter
# !!!! TO GET CORRECT RESULTS
# !!!! niter SHOULD BE FIXED FAR LARGER , e.g. to 1000 
# !!!! but the run will be longer 

# Trial on BMD.zSD  ??
(BMD <- b$res$BMD.zSD)
(BMDupper <- b$res$BMD.zSD.upper)
(BMDlower <- b$res$BMD.zSD.lower)

length(BMD)

bmdplot(b$res, BMDtype = "zSD", point.size = 3, add.CI = TRUE)

definedBMD <- !is.na(BMD)
sum(definedBMD)

bmdplot(b$res[definedBMD, ], BMDtype = "zSD", point.size = 3, add.CI = TRUE)

plot(b, BMDtype = "zSD", remove.infinite = FALSE) # nom d'argument trompeur car il enlève les NA dans tous les cas
# et son warning n'est pas cohérent avec ce qui est fait (il dit toujours qu'il enlève les infinis même s'il ne le fait pas
# et on ne fait pas la différence entre les NA et les infinis)
############### A CORRIGER !!!!!!!!!!!!!!!!!!

definedCI <- !is.na(BMD) & !is.na(BMDupper) & !is.na(BMDlower)
sum(definedCI)

bmdplot(b$res[definedCI, ], BMDtype = "zSD", point.size = 3, add.CI = TRUE)


finiteCI <- is.finite(BMD) & is.finite(BMDupper) & is.finite(BMDlower)

bmdplot(b$res[finiteCI, ], BMDtype = "zSD", point.size = 3, add.CI = TRUE)

plot(b, BMDtype = "zSD", remove.infinite = TRUE) # Ca fait bien la même chose 


# Trial on BMD.xfold OK
(BMD <- b$res$BMD.xfold)
(BMDupper <- b$res$BMD.xfold.upper)
(BMDlower <- b$res$BMD.xfold.lower)

length(BMD)

bmdplot(b$res, BMDtype = "xfold", point.size = 3, add.CI = TRUE)

definedBMD <- !is.na(BMD)
sum(definedBMD)

bmdplot(b$res[definedBMD, ], BMDtype = "xfold", point.size = 3, add.CI = TRUE)

plot(b, BMDtype = "xfold", remove.infinite = FALSE) # nom d'argument trompeur car il enlève les NA dans tous les cas
# et son warning n'est pas cohérent avec ce qui est fait (il dit toujours qu'il enlève les infinis même s'il ne le fait pas
# et on ne fait pas la différence entre les NA et les infinis)
############### A CORRIGER !!!!!!!!!!!!!!!!!!

definedCI <- !is.na(BMD) & !is.na(BMDupper) & !is.na(BMDlower)
sum(definedCI)

bmdplot(b$res[definedCI, ], BMDtype = "xfold", point.size = 3, add.CI = TRUE)


finiteCI <- is.finite(BMD) & is.finite(BMDupper) & is.finite(BMDlower)

bmdplot(b$res[finiteCI, ], BMDtype = "xfold", point.size = 3, add.CI = TRUE)

plot(b, BMDtype = "xfold", remove.infinite = TRUE) # Ca fait bien la même chose 


# Two examples from the paper published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# (1) the dataframe with metabolomic results 
metabresfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
metabres <- read.table(metabresfilename, header = TRUE, stringsAsFactors = TRUE)
str(metabres)

## Filter on BMD defined
metabres$definedBMD <- !is.na(metabres$BMD.zSD)
# Number of non NA BMDs
sum(metabres$definedBMD)

## Filter on defined BMDCIs
metabres$definedCI <- !is.na(metabres$BMD.zSD) &
  !is.na(metabres$BMD.zSD.lower) &
  !is.na(metabres$BMD.zSD.upper)
# Number of CIs with no NA bounds
sum(metabres$definedCI)

## Filter on finite BMD CIs
metabres$finiteCI <- is.finite(metabres$BMD.zSD) &
  is.finite(metabres$BMD.zSD.lower) &
  is.finite(metabres$BMD.zSD.upper)
# Number of CIs with no NA bounds
sum(metabres$finiteCI)

################### rien n'est éliminé

# (2) 
# the dataframe with transcriptomic results 
contigresfilename <- system.file("extdata", "triclosanSVcontigres.txt", package = "DRomics")
contigres <- read.table(contigresfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigres)

## Filter on BMD defined
contigres$definedBMD <- !is.na(contigres$BMD.zSD)
# Number of non NA BMDs
sum(contigres$definedBMD)

## Filter on defined BMDCIs
contigres$definedCI <- !is.na(contigres$BMD.zSD) &
  !is.na(contigres$BMD.zSD.lower) &
  !is.na(contigres$BMD.zSD.upper)
# Number of CIs with no NA bounds
sum(contigres$definedCI)

## Filter on finite BMD CIs
contigres$finiteCI <- is.finite(contigres$BMD.zSD) &
  is.finite(contigres$BMD.zSD.lower) &
  is.finite(contigres$BMD.zSD.upper)
# Number of CIs with no NA bounds
sum(contigres$finiteCI)

