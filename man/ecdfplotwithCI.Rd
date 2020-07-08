\name{ecdfplotwithCI}
\alias{ecdfplotwithCI}
\title{ECDF plot of a variable with given confidence intervals on this variable}

\description{
Plots the distribution of a variable as an ecdf, with x-error bars for given confidence 
intervals on this variable.
}

\usage{
ecdfplotwithCI(variable, CI.lower, CI.upper, by, CI.col = "blue", 
CI.alpha = 1, add.point = TRUE, point.size = 1, point.type = 16)
}

\arguments{
\item{variable}{A numeric vector of the variable to plot.}

\item{CI.lower}{A corresponding numeric vector (same length) with the lower bounds of the confidence intervals.}

\item{CI.upper}{A corresponding numeric vector (same length) with the upper bounds of the confidence intervals.}

\item{by}{A factor of the same length for split of the plot by this factor (no split if omitted).}
  
\item{CI.col}{The color to draw the confidence intervals (unique color) of a factor coding for the color.}

\item{CI.alpha}{Optional transparency of the lines used to draw the confidence intervals.}

\item{add.point}{If \code{TRUE} points are added to confidence intervals.}

\item{point.size}{Size of the added points in case \code{add.point} is \code{TRUE}.}

\item{point.type}{Shape of the added points in case \code{add.point} is \code{TRUE}
  defined as an integer coding for a unique common shape or
  as a factor coding for the shape.}

}

\details{

A function to plot a variable as an ECDF plot, optionally splitted by the groups coded 
in the argument \code{by}, with the confidence intervals associated to each value of the variable, given in \code{CI.lower}
and \code{CI.upper}, and optionally differentially colored according to the factor
given in the argument \code{CI.col}. 
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

# (1) a toy example (a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt",
package="DRomics")
o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001)
f <- drcfit(s_quad, progressbar = TRUE)
r <- bmdcalc(f)
set.seed(1) # to get reproducible results with a so small number of iterations
(b <- bmdboot(r, niter = 10)) # with a non reasonable value for niter 
# !!!! TO GET CORRECT RESULTS
# !!!! niter SHOULD BE FIXED FAR LARGER , e.g. to 1000 
# !!!! but the run will be longer 
b$res
# manual ecdf plot of the bootstrap results as an ecdf distribution 
# on BMD, plot that could also be obtained with plot(b) 
# in this simple case
#
a <- b$res[is.finite(b$res$BMD.zSD.upper), ]
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, CI.col = "red")


# (2) An example from data published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# a dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
str(res)

# a dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
# bootstrap results and annotation
annotres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(annotres)

### an ECDFplot with confidence intervals by pathway
# with color coding for dose-response trend
ecdfplotwithCI(variable = annotres$BMD.zSD, 
               CI.lower = annotres$BMD.zSD.lower, 
               CI.upper = annotres$BMD.zSD.upper, 
               by = annotres$path_class,
               CI.col = annotres$trend) 


# (3) an example on a microarray data set (a subsample of a greater data set) 
#
\donttest{
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))
(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a better precision

# (3.a) 
# manual ecdf plot of the bootstrap results as an ecdf distribution 
# on BMD for each trend
# plot that could also be obtained with plot(b, by = "trend") 
# in this simple case
#
a <- b$res[is.finite(b$res$BMD.zSD.upper), ]
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$trend, CI.col = "red")

# (3.b)
# ecdf plot of the bootstrap results as an ecdf distribution 
# on BMD for each model
# with the color of the confidence intervals coding for the trend
#
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$model, CI.col = a$trend)

# changing the size of the points and the transparency of CI lines
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$model, CI.col = a$trend,
              CI.alpha = 0.5, point.size = 0.5)

# with the model coding for the type of points
ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, CI.col = a$trend,
              CI.alpha = 0.5, point.size = 0.5, point.type = a$model)


# (3.c)
# ecdf plot of the bootstrap results as an ecdf distribution on 
# on BMD_L (lower value of the confidence interval) for each trend
# 
ecdfplotwithCI(variable = a$BMD.zSD.lower, CI.lower = a$BMD.zSD.lower, 
              CI.upper = a$BMD.zSD.upper, by = a$model, CI.col = a$trend, 
              add.point = FALSE)
  }

}
