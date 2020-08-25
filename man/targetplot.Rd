\name{targetplot}
\alias{targetplot}
\title{Dose-reponse plot for target items}

\description{
Plots dose-response raw data of target items (whether or not their response is considered significant)
with fitted curves if available.
}

\usage{
targetplot(items, f, add.fit = TRUE)
}

\arguments{
\item{items}{A character vector
  specifying the identifiers of the items to plot.}

\item{f}{An object of class \code{"drcfit"}.}

\item{add.fit}{If \code{TRUE} the fitted curve is added for items which were selected as responsive items 
and for which a best fit model was obtained.}

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

# Same plot without fitted curves 
#
targetplot(targetitems, f = f, add.fit = FALSE)

}
