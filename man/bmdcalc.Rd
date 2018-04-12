\name{bmdcalc}
\alias{bmdcalc}
\alias{print.bmdcalc}
\alias{plot.bmdcalc}
\title{Computation of benchmark doses for responsive items}

\description{
Computes x-fold and z-SD benchmark doses for each responsive item using the best fit dose reponse model.
}

\usage{
bmdcalc(f, z = 1, x = 10)

\method{print}{bmdcalc}(x, \dots)

\method{plot}{bmdcalc}(x, BMDtype = c("zSD", "xfold"), 
            plottype = c("ecdf", "hist", "density"), bytypology = FALSE, 
            hist.bins = 30, \dots)
}

\arguments{
\item{f}{An object of class \code{"drcfit"} returned by the function\code{drcfit}.}

\item{z}{Threshold defining the BMD_zSD as the dose at which the response is reaching 
y0 +/- z * SD with y0 the level at the control given by the dose response curve and SD the 
residual standard deviation of the dsoe response curve.}

\item{x}{Threshold given as a percentage and defining the BMD_xfold as 
the dose at which the response is reaching 
y0 +/- (x/100)*y0 with y0 the level at the control given by the dose response curve.}


\item{x}{An object of class \code{"bmdcalc"}.}

\item{BMDtype}{The type of BMD to plot, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{plottype}{The type plot, \code{"ecdf"} for an empirical cumulative distribution plot 
(default choice), \code{"hist"} for a histogram or \code{"density"} for a density plot.}

\item{bytypology}{If \code{TRUE} the plotted is splitted by typology.}

\item{hist.bins}{The number of bins, only used for histogram(s).}

\item{\dots}{ further arguments passed to graphical or print functions.}

}

\details{
TO WRITE !!!!!!!!!!!!!!!!!!!!
}

\value{ 
   \code{bmdcalc} returns an object of class \code{"bmdcalc"}, a list with 4 components:
    
   \item{res}{ a data frame reporting the results of the fit and BMD computation
   on each selected item
  (I WILL HAVE TO DETAIL THIS !!!!!!!!!!!) 
  sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}.}

  \item{z}{Threshold given in input to define the BMD_zSD as the dose at which the response is reaching 
y0 +/- z * SD with y0 the level at the control given by the dose response curve and SD the 
residual standard deviation of the dsoe response curve.}

  \item{x}{Threshold given in input as a percentage and defining the BMD_xfold as 
the dose at which the response is reaching 
y0 +/- (x/100)*y0 with y0 the level at the control given by the dose response curve.}

    \item{omicdata}{ The corresponding object of class \code{"omicdata"} given in input 
    (component of itemselect).}
 

  The print of a \code{"bmdcalc"} object gives TO WRITE !!!!!!!!!!!!!!.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) example on a metabolomic data (which has been normalized before)
#
datatxt <- system.file("extdata", "metabolo_norm.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "none"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
plot(r) 

# different plots of BMD_zSD
plot(r, plottype = "hist", bytypology = FALSE) 
plot(r, plottype = "hist", bytypology = FALSE, hist.bins = 40) 
plot(r, plottype = "density", bytypology = FALSE) 

plot(r, plottype = "ecdf", bytypology = TRUE) 
plot(r, plottype = "hist", bytypology = TRUE) 
plot(r, plottype = "hist", bytypology = TRUE, hist.bins = 10) 
plot(r, plottype = "density", bytypology = TRUE) 

# a plot of BMD_xfold (by default BMD_zSD is plotted)
plot(r, BMDtype = "xfold", plottype = "hist", bytypology = TRUE, hist.bins = 10) 

# changing the values of z and x for BMD calculation
(rb <- bmdcalc(f, z = 2, x = 50))
plot(rb) 


# (2) example on a transcriptomic data (a subsample of a greater data set) 
#
\dontrun{
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
plot(r)
}

}
