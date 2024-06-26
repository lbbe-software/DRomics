\name{drcfit}
\alias{drcfit}
\alias{print.drcfit}
\alias{plot.drcfit}
\alias{plotfit2pdf}
\title{Dose response modelling for responsive items}

\description{
Fits dose reponse models to responsive items.
}

\usage{
drcfit(itemselect, 
  information.criterion = c("AICc", "BIC", "AIC"),
  deltaAICminfromnullmodel = 2,
  postfitfilter = TRUE, preventsfitsoutofrange = TRUE, 
  enablesfequal0inGP = TRUE, enablesfequal0inLGP = TRUE,
  progressbar = TRUE, parallel = c("no", "snow", "multicore"), ncpus)

\method{print}{drcfit}(x, \dots)

\method{plot}{drcfit}(x, items, 
  plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"), 
  dose_log_transfo = TRUE, BMDoutput, BMDtype = c("zSD", "xfold"), \dots)
   
plotfit2pdf(x, items, 
  plot.type = c("dose_fitted", "dose_residuals", "fitted_residuals"), 
  dose_log_transfo = TRUE, BMDoutput, BMDtype = c("zSD", "xfold"),
  nrowperpage = 6, ncolperpage = 4, path2figs = getwd())
}

\arguments{
\item{itemselect}{An object of class \code{"itemselect"} returned by the function \code{itemselect}.}

\item{information.criterion}{The information criterion used to select the best fit model, \code{"AICc"} as recommended and default
  choice (the corrected version of the AIC that is recommended
  for small samples (see Burnham and Anderson 2004), \code{"BIC"} or \code{"AIC"}.}

\item{deltaAICminfromnullmodel}{The minimal difference on the chosen information
criterion (AICc, AIC or BIC) between the null model and the best fit model,
requested to accept the fit with the bestfit model.
It is by default fixed at 2 to keep only models which fit the data 
clearly better than the null model, but it can be fixed at 0 to be less stringent.}

\item{postfitfilter}{If \code{TRUE} fits with significant trends on residuals 
  (showing a global significant quadratic trend of the residuals
  as a function of the dose (in rank-scale)) are considered as failures and so eliminated.
  It is strongly recommended to let it at \code{TRUE}, its default value.}

\item{preventsfitsoutofrange}{If \code{TRUE} fits of Gaussian or log-Gaussian models
  that give an extremum value outside the range of the observed signal for an item
  are eliminated from the candidate models for this item, 
  before the choice of the best.
  It is strongly recommended to let it at \code{TRUE}, its default value.}

\item{enablesfequal0inGP}{If \code{TRUE} when the fit of a Gauss-probit model with 5 parameters
  is successful, its simplified version with \code{f = 0} is also fitted and included in the candidate
  models. This submodel of the log-Gauss-probit model corresponds to the probit model.
  We recommend to let this argument at \code{TRUE}, its default value, in order to prevent
  overfitting, and prefer the description of a monotonic curve
  when the parameter f is not necessary to model the data according to the information criterion.}

\item{enablesfequal0inLGP}{If \code{TRUE} when the fit of a log-Gauss-probit model with 5 parameters
  is successful, its simplified version with \code{f = 0} is also fitted and included in the candidate
  models. This submodel of the log-Gauss-probit model corresponds to the log-probit model.
  We recommend to let this argument at \code{TRUE}, its default value, in order to prevent
  overfitting and prefer the description of a monotonic curve
  when the parameter f is not necessary to model the data according to the information criterion.}

\item{progressbar}{If \code{TRUE} a progress bar is used to follow the fitting process.}

\item{parallel}{The type of parallel operation to be used, \code{"snow"} or \code{"multicore"}
  (the second one not being available on Windows),
  or \code{"no"} if no parallel operation.}
  
\item{ncpus}{Number of processes to be used in parallel operation : 
  typically one would fix it to the number of available CPUs.}

\item{x}{An object of class \code{"drcfit"}.}

\item{items}{Argument of the \code{plot.drcfit} function : the number of the first fits to
  plot (20 items max) or the character vector
  specifying the identifiers of the items to plot (20 items max).}
  
\item{plot.type}{The type of plot, by default \code{"dose_fitted"} for the plot of 
  fitted curves with the observed points added to the plot and the observed means at each dose
  added as black plain circles, \code{"dose_residuals"} for the plot of the residuals as function
  of the dose, and \code{"fitted_residuals"} for the plot of the residuals as function of 
  the fitted value.}

\item{dose_log_transfo}{By default at \code{TRUE} to use a log transformation
  for the dose axis (only used if the dose is in x-axis, so not
  for \code{plot.type} \code{"fitted_residuals"}).}

\item{BMDoutput}{Argument that can be used to add BMD values
  and optionally their confidence intervals on a plot of type \code{"dose_fitted"}.
  To do that you must previously apply \code{\link{bmdcalc}} and optionally
  \code{\link{bmdboot}}
  on \code{x} of class \code{drcfit} 
  and then give in this argument the output of \code{\link{bmdcalc}} or
  \code{\link{bmdboot}}.}

\item{BMDtype}{The type of BMD to add on the plot, \code{"zSD"} (default choice) or   \code{"xfold"} (only used if BMDoutput is not missing).}

\item{nrowperpage}{Number of rows of plots when plots are saved in a pdf file using
  plotfit2pdf() (passed to \code{facet_wrap()}).}

\item{ncolperpage}{Number of columns of plots when plots are saved in a pdf file using
  plotfit2pdf() (passed to \code{facet_wrap()}).}
  
\item{path2figs}{File path when plots are saved in a pdf file using plotfit2pdf()}

\item{\dots}{Further arguments passed to graphical or print functions.}
}

\details{
For each selected item, five dose-response models (linear, Hill, exponential, 
Gauss-probit and log-Gauss-probit, see Larras et al. 2018
for their definition) are fitted by non linear regression,
using the \code{\link{nls}} function. If a fit of a biphasic model gives a extremum
value out of the range of the observed signal, it is eliminated (this may happen in rare cases,
especially on observational data  when the number of samples is high and the dose in uncontrolled,
if doses are not distributed all along the dose range).
The best fit is chosen as the one giving the lowest AICc 
(or BIC or AIC) value. The use of the AICc (second-order Akaike criterion)
instead of the AIC
is strongly recommended to prevent the overfitting that may occur with dose-response designs
with a small number of data points (Hurvich and Tsai, 1989; Burnham and Anderson DR, 2004). 
Note that in the extremely rare cases where the number of 
data points would be great, the AIC would converge to the AICc and both procedures would be equivalent.
Items with the best AICc value not lower than the AICc value of the null model (constant model) minus 2
are eliminated. Items with the best fit showing a global significant quadratic trend of the residuals
as a function of the dose (in rank-scale) are also eliminated (the best fit is considered as 
not reliable in such cases). 

Each retained item is classified in four classes by its global trend,
which can be used to roughly describe the shape of each dose-response curve:
  \itemize{
  \item inc for increasing curves,
  \item dec for decreasing curves ,
  \item U for U-shape curves,
  \item bell for bell-shape curves.
 }

Some curves fitted by a Gauss-probit model can be classified as increasing or decreasing when the 
dose value at which their extremum is reached is at zero or if their simplified version with \code{f = 0}
is retained (corresponding to the probit model).
Some curves fitted by a log-Gauss-probit model can be classified as increasing or decreasing 
if their simplified version with \code{f = 0}
is retained (corresponding to the log-probit model).

Each retained item is thus classified in a 16 class typology depending of the
chosen model and of its parameter values : 
  \itemize{
  \item H.inc for increasing Hill curves,
  \item H.dec for decreasing Hill curves,
  \item L.inc for increasing linear curves,
  \item L.dec for decreasing linear curves,
  \item E.inc.convex for increasing convex exponential curves,
  \item E.dec.concave for decreasing concave exponential curves,
  \item E.inc.concave for increasing concave exponential curves,
  \item E.dec.convex for decreasing convex exponential curves,
  \item GP.U for U-shape Gauss-probit curves,
  \item GP.bell for bell-shape Gauss-probit curves,
  \item GP.inc for increasing Gauss-probit curves,
  \item GP.dec for decreasing Gauss-probit curves,
  \item lGP.U for U-shape log-Gauss-probit curves,
  \item lGP.bell for bell-shape log-Gauss-probit curves.
  \item lGP.inc for increasing log-Gauss-probit curves,
  \item lGP.dec for decreasing log-Gauss-probit curves,
 }
 

}

\value{ 
   \code{drcfit} returns an object of class \code{"drcfit"}, a list with 4 components:
    
   \item{fitres}{ a data frame reporting the results of the fit on each selected item 
   for which a successful fit is reached (one line per item) sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The different columns correspond to the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), the adjusted p-value of the selection step (\code{adjpvalue}), the name of the best fit model (\code{model}), the number of fitted parameters (\code{nbpar}), the values of the parameters \code{b}, \code{c}, \code{d}, \code{e} and \code{f}, (\code{NA} for non used parameters), the residual standard deviation (\code{SDres}), the typology of the curve (\code{typology}), the
   rough trend of the curve (\code{trend}) defined with four classes (U, bell, increasing or decreasing shape), the
   theoretical y value at the control \code{y0}), the
   theoretical y value at the maximal dose \code{yatdosemax}), the theoretical y range for x within the range of 
   tested doses (\code{yrange}), the maximal absolute y change (up or down) from the control(\code{maxychange}) 
   and for biphasic curves 
   the x value at which their extremum is reached (\code{xextrem}) 
   and the corresponding y value (\code{yextrem}).
  }

\item{omicdata}{The object containing all the data, as given in input of \code{itemselect()}
which is also a component of the output of \code{itemselect()}.}

\item{information.criterion}{The information criterion used to select the best fit model as given in input.}

\item{information.criterion.val}{A data frame reporting the IC values (AICc, BIC or AIC) values for each selected item (one line per item) and each fitted model (one colum per model with the IC value fixed at \code{Inf} when the fit failed).}

\item{n.failure}{The number of previously selected items on which the workflow failed to fit an acceptable model.}

\item{unfitres}{A data frame reporting the results on each selected item 
for which no successful fit is reached (one line per item) sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The different columns correspond to the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), the adjusted p-value of the selection step (\code{adjpvalue}), and code for the reason of the fitting failure (\code{cause}, equal to \code{"constant.model"} if the best fit model is a constant model or \code{"trend.in.residuals"} if the best fit model is rejected due to quadratic trend on residuals.)}

\item{residualtests}{A data frame of P-values of the tests performed on residuals,
on the mean trend (\code{resimeantrendP} ) and on the variance trend (\code{resivartrendP}). The first one tests a global significant quadratic trend of the residuals as a function of the dose in rank-scale (used to
eliminate unreliable fits) and the second one a global significant quadratic trend of the residuals in absolute value as a function of the dose in rank-scale
(used to alert in case of heteroscedasticity).}
}

\seealso{
  See \code{\link{nls}} for details about the non linear regression function and 
  \code{\link{targetplot}} to plot target items (even if non responsive or unfitted).
}

\references{ 
Burnham, KP, Anderson DR (2004). Multimodel inference: understanding AIC and BIC in model selection. Sociological methods & research, 33(2), 261-304.

Hurvich, CM, Tsai, CL (1989). Regression and time series model selection in small samples. Biometrika, 76(2), 297-307.

Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M,
Schmitt-Jansen M and Delignette-Muller ML (2018). DRomics: a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment. Environmental science & technology.\doi{10.1021/acs.est.8b04752}
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) a toy example (a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package = "DRomics")

# to test the package on a small (for a quick calculation) but not very small data set
# use the following commented line
# datafilename <- system.file("extdata", "transcripto_sample.txt", package = "DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05)
(f <- drcfit(s_quad, progressbar = TRUE))

# Default plot
plot(f)
\donttest{
# The same plot without log transformation of the doses 
# (in raw scale of doses)
plot(f, dose_log_transfo = FALSE)

# The same plot in x log scale choosing x limits for plot
if (require(ggplot2))
  plot(f, dose_log_transfo = TRUE) + 
    scale_x_log10(limits = c(0.1, 10))

# Plot of residuals as function of the dose
plot(f, plot.type = "dose_residuals")

# Same plot of residuals without log transformation of the doses
plot(f, plot.type = "dose_residuals", dose_log_transfo = FALSE)

# plot of residuals as function of the fitted value
plot(f, plot.type = "fitted_residuals")


# (2) an example on a microarray data set (a subsample of a greater data set) 
#
datafilename <- system.file("extdata", "transcripto_sample.txt", package = "DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))

# Default plot
plot(f)

# save all plots to pdf using plotfit2pdf()
plotfit2pdf(f, path2figs = tempdir())
plotfit2pdf(f, plot.type = "fitted_residuals", 
  nrowperpage = 9, ncolperpage = 6, path2figs = tempdir())

# Plot of the fit of the first 12 most responsive items
plot(f, items = 12)

# Plot of the chosen items in the chosen order
plot(f, items = c("301.2", "363.1", "383.1"))

# Look at the table of results for successful fits
head(f$fitres)

# Look at the table of results for unsuccessful fits
head(f$unfitres)

# count the number of unsuccessful fits for each cause
table(f$unfitres$cause)


# (3) Comparison of parallel and non paralell implementations on a larger selection of items
#
if(!requireNamespace("parallel", quietly = TRUE)) {
  if(parallel::detectCores() > 1) {
    s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05)
    system.time(f1 <- drcfit(s_quad, progressbar = TRUE))
    system.time(f2 <- drcfit(s_quad, progressbar = FALSE, parallel = "snow", ncpus = 2))
}}

}}
