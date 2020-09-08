\name{targetplot}
\alias{targetplot}
\title{Dose-reponse plot for target items}

\description{
Plots dose-response raw data of target items (whether or not their response is considered significant)
with fitted curves if available.
}

\usage{
targetplot(items, f, add.fit = TRUE, dose_log_transfo = FALSE)
}

\arguments{
\item{items}{A character vector
  specifying the identifiers of the items to plot.}

\item{f}{An object of class \code{"drcfit"}.}

\item{add.fit}{If \code{TRUE} the fitted curve is added for items which were selected as responsive items 
and for which a best fit model was obtained.}

\item{dose_log_transfo}{to put at \code{TRUE} to use a pseudo log transformation
for the dose axis. See \code{\link{pseudo_log_trans}} of the \code{scales} package
for details on this
transformation that enables to plot zero values in a pseudo log scale.}

}

\value{ 
   a ggplot object.
}

\seealso{
    See \code{\link{plot.drcfit}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# A toy example on a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
package="DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01)
f <- drcfit(s_quad, progressbar = TRUE)

# Plot of chosen items with fitted curves when available 
#
targetitems <- c("88.1", "1", "3", "15")
targetplot(targetitems, f = f)
targetplot(targetitems, f = f, dose_log_transfo = TRUE)

# Same plot without fitted curves 
#
targetplot(targetitems, f = f, add.fit = FALSE)

}
