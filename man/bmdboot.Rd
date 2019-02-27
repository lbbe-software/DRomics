\name{bmdboot}
\alias{bmdboot}
\alias{print.bmdboot}
\alias{plot.bmdboot}
\title{Computation of confidence interval on benchmark doses by bootstrap}

\description{
Computes  95 percent confidence intervals on x-fold and z-SD benchmark doses by bootstrap.
}

\usage{
bmdboot(r, items = r$res$id, niter = 250, tol = 0.5, progressbar = TRUE, 
                    parallel = c("no", "snow", "multicore"), ncpus)

\method{print}{bmdboot}(x, \dots)

\method{plot}{bmdboot}(x, BMDtype = c("zSD", "xfold"), bytypology = FALSE, \dots)
}

\arguments{
\item{r}{An object of class \code{"bmdcalc"} returned by the function \code{bmdcalc}.}

\item{items}{A character vector specifying the identifiers of the items for which you want the
computation of confidence intervals. If omitted the computation is done for all the items.}

\item{niter}{The number of samples drawn by bootstrap.}

\item{tol}{The tolerance in term of proportion of bootstrap samples
on which the fit of the model is successful (if this proportion is below the tolerance,
NA values are given for the limits of the confidence interval.}

\item{progressbar}{If \code{TRUE} a progress bar is used to follow the bootstrap process.}

\item{parallel}{The type of parallel operation to be used, \code{"snow"} or \code{"multicore"}
  (the second one not being available on Windows),
  or \code{"no"} if no parallel operation.}
  
\item{ncpus}{Number of processes to be used in parallel operation : 
  typically one would fix it to the number of available CPUs.}

\item{x}{An object of class \code{"bmdboot"}.}

\item{BMDtype}{The type of BMD to plot, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{bytypology}{If \code{TRUE} the plot is split by typology.}

\item{\dots}{ further arguments passed to graphical or print functions.}

}

\details{

Non-parametric bootstrapping is used. Mean centered residuals are bootstrapped. For each item, bootstrapped parameter estimates are obtained by fitting the model on each of these resampled data sets. If the fitting procedure fails to converge in more than tol*100\% of the cases, NA values are given for the confidence interval. Otherwise, bootstraped
BMD are computed from bootstrapped parameter estimates using the same method as in \code{\link{bmdcalc}}.
95 percent confidence intervals on BMD are then
computed using 2.5 and 97.5 percentiles of the bootstrapped BMD. 
In cases where the bootstrapped BMD cannot be estimated as
not reached at the highest tested dose or not reachable due to model asymptotes,
it was given an infinite value \code{Inf}, so as to enable the computation
of the lower limit of the BMD confidence interval if a sufficient number of bootstrapped BMD values were estimated
to finite values.
}

\value{ 
   \code{bmdboot} returns an object of class \code{"bmdboot"}, a list with 3 components:
    
   \item{res}{ a data frame reporting the results of the fit, BMD computation and bootstrap
   on each specified item sorted in the ascending order of the adjusted p-values. The different columns correspond to 
   the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), 
   the adjusted p-value of the selection step (\code{adjpvalue}), the name of the best fit model (\code{model}), 
   the number of fitted parameters (\code{nbpar}), the values of the parameters \code{b}, \code{c}, \code{d}, 
   \code{e} and \code{f}, (\code{NA} for non used parameters), the residual standard deviation (\code{SDres}), 
   the typology of the curve (\code{typology}, (twelve class typology described in the help of the \code{drcfit} 
   function)), the rough trend of the curve (\code{trend}) defined with four classes 
   (U, bell, increasing or decreasing shape), the theoretical value at the control (\code{y0}), 
   the theoretical y range for x within the range of tested doses 
   (\code{yrange}) and for biphasic curves 
   the x value at which their extremum is reached (\code{xextrem})
   and the corresponding y value (\code{yextrem}), the BMD-zSD value (\code{BMD.zSD}) and 
   the BMD-xfold value (\code{BMD.xfold}), 
   \code{BMD.zSD.lower} and  \code{BMD.zSD.upper}the lower and upper bounds of the 95 percent
   confidence intervals of the BMD-zSD value, 
   \code{BMD.xfold.lower} and  \code{BMD.xfold.upper} the lower and upper bounds of the 95 percent
   confidence intervals of the BMD-xfold value and
   \code{nboot.successful} the number of successful fits on bootstrapped samples for each item.}

  \item{z}{Value of z given in input to define the BMD-zSD.}

  \item{x}{Value of x given in input as a percentage to define the BMD-xfold.}

  \item{tol}{The tolerance given in input in term of tolerated proportion of failures of
  fit on bootstrapped samples.}

  \item{niter}{The number of samples drawn by bootstrap (given in input).}
}

\seealso{
    See \code{\link{bmdcalc}} for details about the computation of benchmark doses.
}

\references{ 
Huet S, Bouvier A, Poursat M-A, Jolivet E (2003) Statistical tools for nonlinear regression: a practical guide with S-PLUS and R examples. Springer, Berlin, Heidelberg, New York.
}


\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) a toy example (a very small subsample of a transcriptomics data set) 
#
datatxt <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")

# to test the package on a small but not very small data set
# use the following commented line
# datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
(b <- bmdboot(r))
b$res
plot(b) 

# bootstrap on only a subsample of items, here those best fitted by the linear model
# with a greater numbre of iterations
(b.lin <- bmdboot(r, items = r$res$id[r$res$model == "linear"],
                       niter = 1000, progressbar = TRUE))
b.lin$res

# (2) an example on a transcriptomics data set (a subsample of a greater data set) 
#
\donttest{
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a better precision

# different plots of BMD-zSD
plot(b, plottype = "ecdf", bytypology = FALSE) 
plot(b, plottype = "univariate", bytypology = FALSE) 
plot(b, plottype = "univariate", bytypology = TRUE) 

# a plot of BMD-xfold (by default BMD-zSD is plotted)
plot(b, BMDtype = "xfold", plottype = "ecdf", bytypology = FALSE) 
}

# (3) Comparison of parallel and non parallel implementations 
#
  \donttest{
  # to be tested with a greater number of iterations
   system.time(b1 <- bmdboot(r, niter = 100, progressbar = TRUE))
   system.time(b2 <- bmdboot(r, niter = 100, progressbar = FALSE, parallel = "snow", ncpus = 2))
}


}
