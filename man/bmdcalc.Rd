\name{bmdcalc}
\alias{bmdcalc}
\alias{print.bmdcalc}
\alias{plot.bmdcalc}
\title{Computation of benchmark doses for responsive items}

\description{
Computes x-fold and z-SD benchmark doses for each responsive item using the best fit dose-reponse model.
}

\usage{
bmdcalc(f, z = 1, x = 10)

\method{print}{bmdcalc}(x, \dots)

\method{plot}{bmdcalc}(x, BMDtype = c("zSD", "xfold"), 
            plottype = c("ecdf", "hist", "density"), bytypology = FALSE, 
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

\item{BMDtype}{The type of BMD to plot, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{plottype}{The type plot, \code{"ecdf"} for an empirical cumulative distribution plot 
(default choice), \code{"hist"} for a histogram or \code{"density"} for a density plot.}

\item{bytypology}{If \code{TRUE} the plot is split by typology.}

\item{hist.bins}{The number of bins, only used for histogram(s).}

\item{\dots}{ further arguments passed to graphical or print functions.}

}

\details{
Two types of benchmark doses (BMD) were computed for each responsive item using 
the best fit dose-reponse model previously obtained using the \code{\link{drcfit}} function :
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
   on each selected item sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The given results are: id, the item identifier; irow, the row number in the initial dataset; adjpvalue, the adjusted p-values returned by function \code{itemselect}; model, the best model fitted; nbpar, the number of parameters of this best model; b, c, d, e, and f, the model parameter values (see Larras et al. for their interpretation); SDres, the residual standard deviation of the best model; typology, the class of typology (twelve class typology described in the help of the \code{drcfit} function); trend, the
   rough trend of the curve defined with four classes (U, bell, increasing or decreasing shape), BMD.zSD, the BMD-zSD value; BMD.xfold, the BMD-xfold value.}

  \item{z}{Value of z given in input to define the BMD-zSD.}

  \item{x}{Value of x given in input as a percentage to define the BMD-xfold.}

    \item{omicdata}{ The corresponding object of class \code{"omicdata"} given in input 
    (component of itemselect).}
 
}

\seealso{
    See \code{\link{uniroot}} for details about the function used for the numerical 
    search of the benchmark dose for cases where there is no analytical solution.
}

\references{ 
Larras and co, our paper when it will be published - TO COMPLETE !!!!!!!!!!!
}


\author{ 
Marie-Laure Delignette-Muller and Elise Billoir
}

\examples{
\dontrun{

# (1) an example on a transcriptomics data (a subsample of a greater data set) 
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
plot(r) 

# different plots of BMD-zSD
plot(r, plottype = "hist", bytypology = FALSE) 
plot(r, plottype = "density", bytypology = FALSE) 
plot(r, plottype = "hist", bytypology = TRUE) 

# a plot of BMD-xfold (by default BMD-zSD is plotted)
plot(r, BMDtype = "xfold", plottype = "hist", bytypology = TRUE, hist.bins = 10) 

# changing the values of z and x for BMD calculation
(rb <- bmdcalc(f, z = 2, x = 50))
plot(rb) 
  }
}
