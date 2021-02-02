\name{bmdcalc}
\alias{bmdcalc}
\alias{print.bmdcalc}
\alias{plot.bmdcalc}
\title{Computation of benchmark doses for responsive items}

\description{
Computes x-fold and z-SD benchmark doses for each responsive item using the best fit dose-reponse model.
}

\usage{
bmdcalc(f, z = 1, x = 10, minBMD, ratio2switchinlog = 100)

\method{print}{bmdcalc}(x, \dots)

\method{plot}{bmdcalc}(x, BMDtype = c("zSD", "xfold"), 
            plottype = c("ecdf", "hist", "density"), 
            by = c("none", "trend", "model", "typology"),
            hist.bins = 30, \dots)
}

\arguments{
\item{f}{An object of class \code{"drcfit"} returned by the function \code{drcfit}.}

\item{z}{Value of z defining the BMD-zSD as the dose at which the response is reaching 
y0 +/- z * SD, with y0 the level at the control given by the dose-response fitted model and SD the 
residual standard deviation of the dose-response fitted model.}

\item{x}{Value of x given as a percentage and defining the BMD-xfold as 
the dose at which the response is reaching 
y0 +/- (x/100) * y0, with y0 the level at the control given by the dose-response fitted model.

For \code{print} and \code{plot} functions, an object of class \code{"bmdcalc"}.}

\item{minBMD}{minimal value for calculated BMDs, so a value considered negligible
compared to the tested doses. If not given by the user this argument is fixed
at the minimal non null tested dose divided by 100.}

\item{ratio2switchinlog}{ratio between maximal and minimal tested doses above which
the numerical computation (when the use of \code{\link{uniroot}} is necessary) 
of the BMD is performed 
on a log scale of dose.}

\item{BMDtype}{The type of BMD to plot, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{plottype}{The type plot, \code{"ecdf"} for an empirical cumulative distribution plot 
(default choice), \code{"hist"} for a histogram or \code{"density"} for a density plot.}

\item{by}{If different from \code{"none"} the plot is split by trend (if \code{"trend"}), by model (if \code{"model"}) or by typology (if \code{"typology"}).}

\item{hist.bins}{The number of bins, only used for histogram(s).}

\item{\dots}{ further arguments passed to graphical or print functions.}

}

\details{
The two types of benchmark doses (BMD) proposed by the EFSA (2017)
were computed for each responsive item using 
the best fit dose-reponse model previously obtained using the \code{\link{drcfit}} function
(see Larras et al. 2018 for details):
\itemize{
  \item the BMD-zSD defined as the dose at which the response is reaching 
    y0 +/- z * SD, with y0 the level at the control given by the dose-response model, SD the 
    residual standard deviation of the dose response model fit and z given as an input 
    (\code{z} fixed to 1  by default),
  \item the BMD-xfold defined as the dose at which the response is reaching 
  y0 +/- (x/100) * y0, with y0 the level at the control given by the dose-response fitted model 
  and x the percentage given as an input (\code{x} fixed at 10 by default.)
  }

When there is no analytical solution for the BMD, it is numerically searched along the fitted 
curve using the \code{\link{uniroot}} function.

In cases where the BMD cannot be reached due to the asymptote at high doses, \code{NaN} is returned.
In cases where the BMD is not reached at the highest tested dose, \code{NA} is returned.
}

\value{ 
   \code{bmdcalc} returns an object of class \code{"bmdcalc"}, a list with 4 components:
    
   \item{res}{ a data frame reporting the results of the fit and BMD computation
   on each selected item sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The different columns correspond to the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), the adjusted p-value of the selection step (\code{adjpvalue}), the name of the best fit model (\code{model}), the number of fitted parameters (\code{nbpar}), the values of the parameters \code{b}, \code{c}, \code{d}, \code{e} and \code{f}, (\code{NA} for non used parameters), the residual standard deviation (\code{SDres}), the typology of the curve (\code{typology}, (twelve class typology described in the help of the \code{drcfit} 
   function)), the
   rough trend of the curve (\code{trend}) defined with four classes (U, bell, increasing or decreasing shape), the
   theoretical value at the control (\code{y0}), the theoretical y range 
   for x within the range of tested doses 
   (\code{yrange}) and 
   for biphasic curves 
   the x value at which their extremum is reached (\code{xextrem})
   and the corresponding y value (\code{yextrem}), the BMD-zSD value (\code{BMD.zSD})
   with the corresponding BMR-zSD value (reached or not, \code{BMR.zSD}) and 
   the BMD-xfold value (\code{BMD.xfold})
   with the corresponding BMR-xfold value (reached or not, \code{BMR.xfold}).}

  \item{z}{Value of z given in input to define the BMD-zSD.}

  \item{x}{Value of x given in input as a percentage to define the BMD-xfold.}
  
  \item{minBMD}{minimal value for calculated BMDs given in input or fixed
  at the minimal non null tested dose divided by 100.}
  
  \item{ratio2switchinlog}{ratio between maximal and minimal tested doses above which
the numerical computations are performed in a log scale (as given in input).}

  \item{omicdata}{ The corresponding object given in input 
    (component of itemselect).}
 
}

\seealso{
    See \code{\link{uniroot}} for details about the function used for the numerical 
    search of the benchmark dose for cases where there is no analytical solution.
}

\references{ 
EFSA Scientific Committee, Hardy A, Benford D, Halldorsson T, Jeger MJ, Knutsen KH,
... & Schlatter JR  (2017). Update: use of the benchmark dose approach in risk assessment.
EFSA Journal, 15(1), e04658.

Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M,
Schmitt-Jansen M and Delignette-Muller ML (2018). DRomics: a turnkey tool to support 
the use of the dose-response framework for omics data in ecological risk assessment. 
Environmental science & technology.\doi{10.1021/acs.est.8b04752}
}


\author{ 
Marie-Laure Delignette-Muller and Elise Billoir
}

\examples{

# (1) a toy example (a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")

# to test the package on a small (for a quick calculation) but not very small data set
# use the following commented line
# datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
plot(r) 

# changing the values of z and x for BMD calculation
\donttest{
(rb <- bmdcalc(f, z = 2, x = 50))
plot(rb)
}

# (2) an example on a microarray data set (a subsample of a greater data set) 
#
\donttest{
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

# to test the package on a small (for a quick calculation) but not very small data set
# use the following commented line
# datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
plot(r) 
if (require(ggplot2))
  plot(r, plottype = "ecdf") + scale_x_log10() # with log10 dose scale 

# different plots of BMD-zSD

plot(r, plottype = "hist") 
plot(r, plottype = "density") 
plot(r, plottype = "density", by = "trend") 
plot(r, plottype = "ecdf", by = "trend") 
plot(r, plottype = "ecdf", by = "model") 
plot(r, plottype = "ecdf", by = "typology") 

# a plot of BMD-xfold (by default BMD-zSD is plotted)
plot(r, BMDtype = "xfold", plottype = "hist", by = "typology", hist.bins = 10) 
}

}
