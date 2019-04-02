\name{ecdfplotwithCI}
\alias{ecdfplotwithCI}
\title{ECDF plot of a variable with given confidence intervals on this variable}

\description{
Plots the distribution of a variable as an ecdf, with x-error bars for given confidence 
intervals on this variable
}

\usage{
ecdfplotwithCI(variable, CI.lower, CI.upper, by, CI.col = "blue", 
CI.alpha = 1, add.point = TRUE)
}

\arguments{
\item{variable}{A numeric vector of the variable to plot.}

\item{CI.lower}{A corresponding numeric vector (same length) with the lower bounds of the confidence intervals.}

\item{CI.upper}{A corresponding numeric vector (same length) with the upper bounds of the confidence intervals.}

\item{by}{A factor of the same length for split of the plot by this factor (no split if omitted).}
  
\item{CI.col}{The color to draw the confidence intervals (unique color) of a factor coding for the color.}

\item{CI.alpha}{Optional transparency of the lines used to draw the confidence intervals}

\item{add.point}{If \code{TRUE} points are added to confidence intervals}

}

\details{

A function to plot a variable as an ECDF plot, potentially splitted by the groups coded in the argument 
\code{by}, with the confidence intervals associated to each value of the variable, given in \code{CI.lower}
and \code{CI.upper}. 
}

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

# an example on a transcriptomics data set (a subsample of a greater data set) 
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a better precision

# (1) 
# manual ecdf plot of the bootstrap results as an ecdf distribution 
# on BMD for each trend
# plot that could also be obtained with plot(b, by = "trend") 
# in this simple case
#
a <- b$res[is.finite(b$res$BMD.zSD.upper), ]
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$trend, CI.col = "red")
plot(b, by = "trend")

# (2)
# ecdf plot of the bootstrap results as an ecdf distribution 
# on BMD for each model
# with the color of the confidence intervals coding for the trend
#
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$model, CI.col = a$trend)

# (3)
# ecdf plot of the bootstrap results as an ecdf distribution on 
# on BMD_L (lower value of the confidence interval) for each trend
# 
ecdfplotwithCI(variable = a$BMD.zSD.lower, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$model, CI.col = a$trend, 
              add.point = FALSE)

}
