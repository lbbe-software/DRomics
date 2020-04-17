\name{curvesplot}
\alias{curvesplot}
\title{Plot of fitted curves}

\description{
Plots fitted curves from an extended dataframe of the fitted results
}

\usage{
curvesplot(extendedres, xmin = 0, xmax, y0shift = TRUE,
                       facetby, colorby, removelegend = FALSE,  
                        npoints = 50, line.size = 0.2, 
                        line.alpha = 1)}

\arguments{
\item{extendedres}{the dataframe of results provided by bmdcalc (res) or drcfit (fitres) 
 or a subset of this data frame (selected lines). This dataframe can be extended 
 with additional columns coming for example from the annotation of items, and some lines 
 can be replicated if their corresponding item has more than one annotation. 
 This extended dataframe
 must at least contain the column giving the identification of each curve (\code{id}),
 the column \code{model} naming the fitted model and the values of 
the parameters (columns \code{b}, \code{c}, \code{d}, \code{e}, \code{f}).}

\item{xmin}{Minimal dose/concentration for definition of the x range (by default 0).}

\item{xmax}{Maximal dose/concentration for definition of the x range (can be defined 
 as \code{max(f$omicdata$dose)} with \code{f} the output of \code{drcfit()}).}

\item{y0shift}{If \code{TRUE} (default choice) curves are all shifted to have the theoretical
 signal at the control at 0.}

\item{facetby}{optional argument naming the column of \code{extendedres} chosen 
  to split the plot in facets (no split if omitted).}

\item{colorby}{optional argument naming the column of \code{extendedres} chosen 
  to color the curves (no color if omitted).}
  
\item{removelegend}{If \code{TRUE} the color legend is removed (useful if the number
  of colors is great.}

\item{npoints}{Number of points computed on each curve to plot it.}

\item{line.size}{Size of the lines for plotting curves.}

\item{line.alpha}{Transparency of the lines for plotting curves.}

}

\details{
For each item of the extended dataframe, the name of the model 
(column \code{model}) and the values of 
the parameters (columns \code{b}, \code{c}, \code{d}, \code{e}, \code{f})
are used to compute theoretical dose-response curves in the range 
\code{[xmin ; xmax].}
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

# A toy example on a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
package="DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01)
f <- drcfit(s_quad, progressbar = TRUE)

# (1) 
# Default plot of all the curves 
#
curvesplot(f$fitres, xmax = max(f$omicdata$dose))

# the equivalent using the output of bmdcalc
(r <- bmdcalc(f))
curvesplot(r$res, xmax = max(f$omicdata$dose))

# plot of only U-shape curves colored by model
resU <- r$res[r$res$trend == "U", ]
curvesplot(resU, xmax = max(f$omicdata$dose), colorby = "model")

\donttest{

# (2) 
# Plot of all the curves without shifting y0 values to 0
#
curvesplot(f$fitres, xmax = max(f$omicdata$dose), y0shift = FALSE)

# (3) 
# Plot of all the curves colored by model, with one facet per trend
#
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", colorby = "model")

# playing with size and transparency of lines
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", colorby = "model", 
  line.size = 1, line.alpha = 0.5)
  
# (4) an example on a microarray data set (a subsample of a greater data set)
#
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))

curvesplot(f$fitres, xmax = max(f$omicdata$dose), facetby = "typology")
}

}
