\name{ecdfplotwithCI}
\alias{ecdfplotwithCI}
\title{ECDF plot of a variable with given confidence intervals on this variable}

\description{
Plots the distribution of a variable as an ecdf, with x-error bars for given confidence 
intervals on this variable
}

\usage{
ecdfplotwithCI(variable, CI.lower, CI.upper, by, CI.col = "blue")
}

\arguments{
\item{variable}{A numeric vector of the variable to plot.}

\item{CI.lower}{A corresponding numeric vector (same length) with the lower bounds of the confidence intervals.}

\item{CI.upper}{A corresponding numeric vector (same length) with the upper bounds of the confidence intervals.}

\item{by}{A factor of the same length for split of the plot by this factor (no split if omitted).}
  
\item{CI.col}{The color to draw the confidence intervals.}
}

\details{

TO WRITE !!!!!!!!!!!!!!!!!!!!!}

\value{ 
   a ggplot object.
}

\seealso{
    See \code{\link{plot.bmdboot}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) an example on a transcriptomics data set (a subsample of a greater data set) 
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a better precision

# manual ecdf plot of the bootstrap results as an ecdf distribution by trend 
# that could also be obtained with plot(b, by = "trend") in this simple case
a <- b$res[is.finite(b$res$BMD.zSD.upper), ]
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$trend, CI.col = "red")
plot(b, by = "trend")
}
