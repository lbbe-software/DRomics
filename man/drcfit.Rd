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

\method{plot}{drcfit}(x, items, \dots)
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

\item{items}{Argument of the \code{plot.drcfit} function : the number of the first fits to
  plot (20 items max) or the character vector
  specifying the identifiers of the items to plot (20 items max).}

\item{\dots}{ further arguments passed to graphical or print functions.}
}

\details{
TO WRITE !!!!!!!!!!!!!!!!!!!!
}

\value{ 
   \code{drcfit} returns an object of class \code{"drcfit"}, a list with 4 components:
    
   \item{fitres}{ a data frame reporting the results of the fit on each selected item (one line per item) sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The different columns correspond to the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), the adjusted p-value of the selection step (\code{adjpvalue}), the name of the best fit model (\code{model}), the number of fitted parameters (\code{nbpar}), the values of the parameters \code{b}, \code{c}, d,\code{d}, \code{e} and \code{f}, (\code{NA} for non used parameters), the residual standard deviation (\code{SDres}), the typology of the curve \code{typology} and the
   rough trend of the curve \code{trend}.
  }

    \item{omicdata}{ The corresponding object of class \code{"omicdata"} given in input 
    (component of itemselect).}
 
    \item{n.failure}{ The number of previously selected items on which the workflow failed to 
    fit an acceptable model.}

    \item{AIC.val}{ a data frame reporting AIC values for each selected item (one line per item) and each fitted model (one colum per model with the AIC value fixed at \code{Inf} when the fit failed).}

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
