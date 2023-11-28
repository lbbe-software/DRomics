\name{bmdfilter}
\alias{bmdfilter}
\title{Filtering BMDs according to estimation quality}

\description{
Filtering BMDs in DRomics workflow output according to estimation quality,
to retain the best estimated BMDs for subsequent biological annotation and interpretation.
}

\usage{
bmdfilter(res, 
              BMDfilter = c("definedCI", "finiteCI", "definedBMD", "none"),
              BMDtype = c("zSD", "xfold"))
}

\arguments{
\item{res}{The dataframe of results provided by 
 \code{\link{bmdboot}} or \code{\link{bmdcalc}} (\code{res}) 
 or a subset of this data frame. 
 
 Even if this function is intended to be used just after the calculation
 of BMD values, before the biological annotation, it can also be used
 within the interpretation workflow, on an extended dataframe
 with additional columns coming for example from the biological annotation of items, 
 and with some lines replicated for items with more than one annotation. 
 
 In any case the dataframe
 must at least contain the column giving the BMD values (\code{BMD.zSD} or \code{BMD.xfold}
 depending on the chosen BMDtype), identification of each curve (\code{id}),
 and if \code{BMDfilter} is set to \code{"CIdefined"} or \code{"CIfinite"},
 the columns \code{BMD.zSD.lower}, \code{BMD.zSD.upper} or
 \code{BMD.xfold.lower}, \code{BMD.xfold.upper} depending on the argument \code{BMDtype}.}

\item{BMDfilter}{If not \code{"none"}, the type of filter applied, based on BMD estimation.
If \code{"definedCI"} (default choice), all items for which point and interval estimates
of the BMD were successfully calculated are retained 
(so items for which the bootstrap procedure failed are excluded). 
If \code{"finiteCI"}, all items for which point and interval estimates of the BMD 
were successfully calculated and gave values within the range of tested/observed doses are retained.
If \code{"definedBMD"}, all items for which the point estimate of the BMD 
was estimated at a value within the range of tested/observed doses are retained. 
}

\item{BMDtype}{The type of BMD used for the previously
described filtering procedure, \code{"zSD"} (default choice) or \code{"xfold"}.}

}

\details{
Using the argument \code{BMDfilter} three filters are proposed to retain
only the items associated to the best estimated BMD values.
By default we recommend to retain only the items for which the BMD and its
confidence interval are defined (using \code{"CIdefined"}) 
(so excluding items for which the bootstrap procedure failed).
One can be even more restrictive by 
retaining items only if the BMD confidence interval is within the range of
tested/observed doses (using \code{"CIfinite"}), or less restrictive 
(using \code{"BMDIdefined"}) requiring that the BMD
point estimate only must be defined within the range of tested/observed doses 
(let us recall that in the \code{\link{bmdcalc}} output, 
if it is not the case the BMD is coded \code{NA}).

We propose an option \code{"none"} only in case, in the future, we add
other filters not based on the BMD.
}

\value{ 
   A dataframe corresponding to a subset of res given in input, that 
   can be used for biological annotation and further exploration.
}

\seealso{
    See \code{\link{selectgroups}}, \code{\link{bmdboot}} and 
    \code{\link{bmdcalc}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) a toy example 
# on a very small subsample of a microarray data set
# and a very smal number of bootstrap iterations 
# (clearly not sufficient, but it is just for illustration)
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
(b <- bmdboot(r, niter = 10)) # with a non reasonable value for niter

# !!!! TO GET CORRECT RESULTS
# !!!! niter SHOULD BE FIXED FAR LARGER , e.g. to 1000 
# !!!! but the run will be longer

### (1.a) Examples on BMD.xfold (with some undefined BMD.xfold values)

# Plot of BMDs with no filtering
subres <- bmdfilter(b$res, BMDfilter = "none")
bmdplot(subres, BMDtype = "xfold", point.size = 3, add.CI = TRUE)

# Plot of items with defined BMD point estimate
subres <- bmdfilter(b$res, BMDtype = "xfold", BMDfilter = "definedBMD")
bmdplot(subres, BMDtype = "xfold", point.size = 3, add.CI = TRUE)

# Plot of items with defined BMD point estimate and CI bounds
subres <- bmdfilter(b$res, BMDtype = "xfold", BMDfilter = "definedCI")
bmdplot(subres, BMDtype = "xfold", point.size = 3, add.CI = TRUE)

# Plot of items with finite BMD point estimate and CI bounds
subres <- bmdfilter(b$res, BMDtype = "xfold", BMDfilter = "finiteCI") 
bmdplot(subres, BMDtype = "xfold", point.size = 3, add.CI = TRUE)

\donttest{

### (1.b) Examples on BMD.zSD (with no undefined BMD.zSD values)

# Plot of BMDs with no filtering
subres <- bmdfilter(b$res, BMDfilter = "none")
bmdplot(subres, BMDtype = "zSD", point.size = 3, add.CI = TRUE)

# Plot items with defined BMD point estimate (the same on this ex.)
subres <- bmdfilter(b$res, BMDtype = "zSD", BMDfilter = "definedBMD")
bmdplot(subres, BMDtype = "zSD", point.size = 3, add.CI = TRUE)

# Plot of items with defined BMD point estimate and CI bounds
subres <- bmdfilter(b$res, BMDtype = "zSD", BMDfilter = "definedCI")
bmdplot(subres, BMDtype = "zSD", point.size = 3, add.CI = TRUE)

# Plot of items with finite BMD point estimate and CI bounds
subres <- bmdfilter(b$res, BMDtype = "zSD", BMDfilter = "finiteCI") 
bmdplot(subres, BMDtype = "zSD", point.size = 3, add.CI = TRUE)
}

}
