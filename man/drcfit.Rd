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
choice) or \code{"log-probit"}.}

\item{progressbar}{If \code{TRUE} a progress bar is used to follow the fitting process.}

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
For each selected item, five dose-response models (linear, Hill, exponential, 
Gauss-probit and log-Gauss-probit, see Larras et al. ????
for their definition) were fitted by non linear regression,
using the \code{\link{nls}} function. The best one was chosen as the one giving the lowest AIC value.
Items with the best AIC value not lower than the AIC value of the null model (constant model) minus 2
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
 
Each retained is also classified in four classes by its global trend :
  \itemize{
  \item inc for increasing curves,
  \item dec for decreasing curves ,
  \item U for U-shape curves,
  \item bell for bell-shape curves.
 }

}

\value{ 
   \code{drcfit} returns an object of class \code{"drcfit"}, a list with 4 components:
    
   \item{fitres}{ a data frame reporting the results of the fit on each selected item (one line per item) sorted in the ascending order of the adjusted p-values returned by function \code{itemselect}. The different columns correspond to the identifier of each item (\code{id}), the row number of this item in the initial data set (\code{irow}), the adjusted p-value of the selection step (\code{adjpvalue}), the name of the best fit model (\code{model}), the number of fitted parameters (\code{nbpar}), the values of the parameters \code{b}, \code{c}, d,\code{d}, \code{e} and \code{f}, (\code{NA} for non used parameters), the residual standard deviation (\code{SDres}), the typology of the curve \code{typology} and the
   rough trend of the curve \code{trend} defined with four classes (U, bell, increasing or decreasing shape).
  }

    \item{omicdata}{ The corresponding object of class \code{"omicdata"} given in input 
    (component of itemselect).}
 
    \item{n.failure}{ The number of previously selected items on which the workflow failed to 
    fit an acceptable model.}

    \item{AIC.val}{ a data frame reporting AIC values for each selected item (one line per item) and each fitted model (one colum per model with the AIC value fixed at \code{Inf} when the fit failed).}

}

\seealso{
    See \code{\link{nls}} for details about the non linear regression function.
}

\references{ 
Larras and co, our paper when it will be published - TO COMPLETE !!!!!!!!!!!
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) an example on a transcriptomics data (a subsample of a greater data set) 
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))

# Default plot
plot(f)

# Plot of the first 12 most responsive items
plot(f, items = 12)

# Plot of the chosen items in the chosen order
plot(f, items = c("301.2", "363.1", "383.1"))

# (2) Comparison of parallel and non paralell implementations on a 
#     larger selection of items
#
  \dontrun{
   s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05)
    system.time(f1 <- drcfit(s_quad, progressbar = TRUE))
   system.time(f2 <- drcfit(s_quad, progressbar = FALSE, parallel = "snow", ncpus = 4))
  }
}
