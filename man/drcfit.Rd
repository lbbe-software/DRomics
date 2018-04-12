\name{drcfit}
\alias{drcfit}
\alias{print.drcfit}
\alias{plot.drcfit}
\title{Dose response modelling for responsive items}

\description{
Fits dose reponse models to responsive items.
}

\usage{
drcfit(itemselect, sigmoid.model = c("Hill", "log-probit"), 
                   progressbar = TRUE, saveplot2pdf = TRUE, 
                   parallel = c("no", "snow", "multicore"), ncpus)

\method{print}{drcfit}(x, \dots)

\method{plot}{drcfit}(x, \dots)
}

\arguments{
\item{itemselect}{An object of class \code{"itemselect"} returned by the function \code{itemselect}.}

\item{sigmoid.model}{The chosen sigmoid model, \code{"Hill"} (default
choice) or \code{"probit"}.}

\item{progressbar}{If \code{TRUE} a progress is used to follow the fitting process.}

\item{saveplot2pdf}{If \code{TRUE} a pdf file named drcfitplot.pdf is saved containing
	all the fitted dose-response curves sorted by adjusted p-values of the selection step.}

\item{parallel}{The type of parallel operation to be used, \code{"snow"} or \code{"multicore"}
  (the second one not being available on Windows),
  or \code{"no"} if no parallel operation.}
  
\item{ncpus}{Number of processes to be used in parallel operation : 
  typically one would fix it to the number of available CPUs.}


\item{x}{An object of class \code{"drcfit"}.}

\item{\dots}{ further arguments passed to graphical or print functions.}
}

\details{
TO WRITE !!!!!!!!!!!!!!!!!!!!
}

\value{ 
   \code{drcfit} returns an object of class \code{"drcfit"}, a list with 3 components:
    
   \item{fitres}{ a data frame reporting the results of the fit on each selected item
  (I WILL HAVE TO DETAIL THIS !!!!!!!!!!!) 
  sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}.}

    \item{omicdata}{ The corresponding object of class \code{"omicdata"} given in input 
    (component of itemselect).}
 
    \item{n.failure}{ The number of previously selected items on which the workflow failed to 
    fit an acceptable model.}

  The print of a \code{"drcfit"} object gives TO WRITE !!!!!!!!!!!!!!.
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
plot(f)

# (2) example on a transcriptomic data (a subsample of a greater data set) 
#
\dontrun{
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))
plot(f)
}


}
