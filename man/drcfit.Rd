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
drcfit(itemselect, sigmoid.model = c("Hill", "log-probit"),
  information.criterion = c("AICc", "BIC", "AIC"), postfitfilter = TRUE,
  progressbar = TRUE, parallel = c("no", "snow", "multicore"), ncpus)

\method{print}{drcfit}(x, \dots)

\method{plot}{drcfit}(x, items, 
  plot.type = c("dose_fitted", "dose_residuals","fitted_residuals"), 
  dose_log_transfo = FALSE, \dots)
   
plotfit2pdf(x, items, 
  plot.type = c("dose_fitted", "dose_residuals", "fitted_residuals"), 
  dose_log_transfo = FALSE, nrowperpage = 6, ncolperpage = 4, path2figs = getwd())
}

\arguments{
\item{itemselect}{An object of class \code{"itemselect"} returned by the function \code{itemselect}.}

\item{sigmoid.model}{The chosen sigmoid model, \code{"Hill"} (default
  choice) or \code{"log-probit"}.}

\item{information.criterion}{The information criterion used to select the best fit model, \code{"AICc"} as recommended and default
  choice (the corrected version of the AIC that is recommended
  for small samples (see Burnham and Anderson 2004), \code{"BIC"} or \code{"AIC"}.}

\item{postfitfilter}{If \code{TRUE} fits with significant trends on residuals 
  (showing a global significant quadratic trend of the residuals
  as a function of the dose (in rank-scale)) are considered as failures and so eliminated.
  It is strongly recommended to let it at \code{TRUE}, its default value.}

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

\item{dose_log_transfo}{Put at \code{TRUE} to use a log transformation
  for the dose axis (only available if the dose is in x-axis, so not available
  for \code{plot.type} \code{"fitted_residuals"}).}

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
for their definition) were fitted by non linear regression,
using the \code{\link{nls}} function. The best one was chosen as the one giving the lowest AICc 
(or BIC or AIC) value. The use of the AICc (second-order Akaike criterion)
instead of the AIC
is strongly recommended to prevent the overfitting that may occur with dose-response designs
with a small number of data points (Hurvich and Tsai, 1989; Burnham and Anderson DR, 2004). 
Note that in the extremely rare cases where the number of 
data points would be great, the AIC would converge to the AICc and both procedures would be equivalent.
Items with the best AICc value not lower than the AICc value of the null model (constant model) minus 2
were eliminated. Items with the best fit showing a global significant quadratic trend of the residuals
as a function of the dose (in rank-scale) were also eliminated (the best fit is considered as 
not reliable in such cases). Each retained item is classified in a twelve class typology depending of the
chosen model and of its parameter values : 
  \itemize{
  \item H.inc for increasing Hill curves (or lP.inc if \code{sigmoid.model = "log-probit"}),
  \item H.dec for decreasing Hill curves (or lP.dec if \code{sigmoid.model = "log-probit"}),
  \item L.inc for increasing linear curves,
  \item L.dec for decreasing linear curves,
  \item E.inc.convex for increasing convex exponential curves,
  \item E.dec.concave for decreasing concave exponential curves,
  \item E.inc.concave for increasing concave exponential curves,
  \item E.dec.convex for decreasing convex exponential curves,
  \item GP.U for U-shape Gauss-probit curves,
  \item GP.bell for bell-shape Gauss-probit curves,
  \item lGP.U for U-shape log-Gauss-probit curves,
  \item lGP.bell for bell-shape log-Gauss-probit curves.
 }
 
Each retained item is also classified in four classes by its global trend,
which can be used to roughly describe the shape of each dose-response curve:
  \itemize{
  \item inc for increasing curves,
  \item dec for decreasing curves ,
  \item U for U-shape curves,
  \item bell for bell-shape curves.
 }
Some curves fitted by a Gauss-probit model can be classified as increasing or decreasing when the 
dose value at which their extremum is reached is at zero.

}

\value{ 
   \code{drcfit} returns an object of class \code{"drcfit"}, a list with 4 components:
    
   \item{fitres}{ a data frame reporting the results of the fit on each selected item 
   for which a successful fit is reached (one line per item) sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The different columns correspond to the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), the adjusted p-value of the selection step (\code{adjpvalue}), the name of the best fit model (\code{model}), the number of fitted parameters (\code{nbpar}), the values of the parameters \code{b}, \code{c}, \code{d}, \code{e} and \code{f}, (\code{NA} for non used parameters), the residual standard deviation (\code{SDres}), the typology of the curve (\code{typology}), the
   rough trend of the curve (\code{trend}) defined with four classes (U, bell, increasing or decreasing shape), the
   theoretical value at the control \code{y0}), the theoretical y range for x within the range of 
   tested doses (\code{yrange}), 
   for biphasic curves 
   the x value at which their extremum is reached (\code{xextrem}) 
   and the corresponding y value (\code{yextrem}).
  }

\item{omicdata}{The corresponding object of class \code{"microarraydata"} given in input 
(component of itemselect).}

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
# The same plot with log transformation of the doses
plot(f, dose_log_transfo = TRUE)

# The same plot in x log scale choosing x limits for plot
if (require(ggplot2))
  plot(f, dose_log_transfo = TRUE) + 
    scale_x_log10(limits = c(0.1, 10))

# Plot of residuals as function of the dose
plot(f, plot.type = "dose_residuals")

# Same plot of residuals with log transformation of the doses
plot(f, plot.type = "dose_residuals", dose_log_transfo = TRUE)

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
