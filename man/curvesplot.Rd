\name{curvesplot}
\alias{curvesplot}
\title{Plot of fitted curves}

\description{
Provides a plot of all the fitted curves from a dataframe of the main workflow
results, possibly extended with 
additional information (e.g. groups from functional annotation) used to color and/or split the curves. }

\usage{
curvesplot(extendedres, xmin = 0, xmax, y0shift = TRUE, scaling = TRUE,
                       facetby, facetby2, free.y.scales = FALSE, ncol4faceting,
                       colorby, removelegend = FALSE,  
                        npoints = 500, line.size = 0.2, 
                        line.alpha = 1, dose_log_transfo = FALSE,
                       addBMD = FALSE, BMDtype = c("zSD", "xfold"), 
                       point.size = 1, point.alpha = 1)}

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

\item{scaling}{If \code{TRUE}, deafult choice, curves are all shifted to have the theoretical
 signal at the control at 0 \code{y0} and scaled by dividing by 
 the maximal absolute signal change (up or down) from the signal at the control
 \code{maxychange}.}

\item{facetby}{optional argument naming the column of \code{extendedres} chosen 
  to split the plot in facets (no split if omitted).}

\item{facetby2}{optional argument naming the column of \code{extendedres} chosen 
  as an additional argument to split the plot in facets using 
  \code{ggplot2::facet_grid}, with columns defined by \code{facetby}
  and rows defined by \code{facetby2} (no split if omitted).}

\item{free.y.scales}{if TRUE the y scales are free in the different facets.}

\item{ncol4faceting}{Number of columns for facetting (not used if \code{facetby2} 
  is also provided.}

\item{colorby}{optional argument naming the column of \code{extendedres} chosen 
  to color the curves (no color if omitted).}
  
\item{removelegend}{If \code{TRUE} the color legend is removed (useful if the number
  of colors is great).}

\item{npoints}{Number of points computed on each curve to plot it.}

\item{line.size}{Size of the lines for plotting curves.}

\item{line.alpha}{Transparency of the lines for plotting curves.}

\item{dose_log_transfo}{If TRUE a log transformation of the dose is used in the plot. 
  This option needs a definition of a strictly positive value of xmin in input.}

\item{addBMD}{If TRUE points are added on the curve at BMD-BMR values (requires to have
BMD and BMD values in the first argument extendedres).}

\item{BMDtype}{The type of BMD to add, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{point.size}{Size of the BMD-BMR points added on the curves.}

\item{point.alpha}{Transparency of the BMD-BMR points added on the curves.}

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

# (1) A toy example on a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
  package = "DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01)
f <- drcfit(s_quad, progressbar = TRUE)

# (1.a) 
# Default plot of all the curves 
#
curvesplot(f$fitres, xmax = max(f$omicdata$dose))

\donttest{
# the equivalent using the output of bmdcalc
# which enables the add of BMD values
r <- bmdcalc(f)
curvesplot(r$res, xmax = max(f$omicdata$dose), addBMD = TRUE)

# use of line size, point size, transparency
curvesplot(r$res, xmax = max(f$omicdata$dose), 
  line.alpha = 0.2, line.size = 1,
  addBMD = TRUE, point.alpha = 0.3, point.size = 1.8)

# the same plot with dose in log scale (need xmin != 0 in input)
curvesplot(r$res, xmin = 0.1, xmax = max(f$omicdata$dose),
  dose_log_transfo = TRUE, addBMD = TRUE)

# plot of curves colored by models
curvesplot(r$res, xmax = max(f$omicdata$dose), colorby = "model")

# plot of curves facetted by item
curvesplot(r$res, xmax = max(f$omicdata$dose), facetby = "id")

# plot of curves facetted by trends
curvesplot(r$res, xmax = max(f$omicdata$dose), facetby = "trend")

# the same plot with free y scales
curvesplot(r$res, xmax = max(f$omicdata$dose), facetby = "trend",
  free.y.scales =  TRUE)
  
# (1.b) 
# Plot of all the curves without shifting y0 values to 0
# and without scaling
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  scaling = FALSE, y0shift = FALSE)

# (1.c) 
# Plot of all the curves colored by model, with one facet per trend
#
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", colorby = "model")

# changing the number of columns
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", colorby = "model", ncol4faceting = 4)

# playing with size and transparency of lines
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", colorby = "model", 
  line.size = 0.5, line.alpha = 0.8)
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", colorby = "model", 
  line.size = 0.8, line.alpha = 0.2)
curvesplot(f$fitres, xmax = max(f$omicdata$dose), 
  facetby = "trend", line.size = 1, line.alpha = 0.2)
  
# (2) an example on a microarray data set (a subsample of a greater data set)
#
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))

# plot split by trend and model with BMR-BMD points added on curves
# adding transparency
curvesplot(r$res, xmax = max(f$omicdata$dose), 
  line.alpha = 0.2, line.size = 0.8,
  addBMD = TRUE, point.alpha = 0.2, point.size = 1.5,
  facetby = "trend", facetby2 = "model")

# same plot without scaling 
curvesplot(r$res, xmax = max(f$omicdata$dose), 
  line.alpha = 0.2, line.size = 0.8,
  addBMD = TRUE, point.alpha = 0.2, point.size = 1.5,
  scaling = FALSE, facetby = "trend", facetby2 = "model")

# (3) An example from data published by Larras et al. 2020
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
extendedres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(extendedres)

# Plot of the dose-response curves by pathway colored by trend
# with BMR-BMD points added on curves
curvesplot(extendedres, facetby = "path_class", npoints = 100, line.size = 0.5,
  colorby = "trend", xmin = 0, xmax = 8, addBMD = TRUE) 

# Plot of the dose-response curves without scaling, by pathway colored by trend
curvesplot(extendedres, facetby = "path_class", npoints = 100, line.size = 0.5,
  colorby = "trend", scaling = FALSE, xmin = 0, xmax = 8) 

# Plot of the dose-response curves split by pathway and by trend
# for a selection pathway
chosen_path_class <- c("Membrane transport", "Lipid metabolism")
ischosen <- is.element(extendedres$path_class, chosen_path_class)
curvesplot(extendedres[ischosen, ],
  facetby = "trend", facetby2 = "path_class",
  npoints = 100, line.size = 0.5, xmin = 0, xmax = 8) 

# Plot of the dose-response curves for a specific pathway
# in this example the "lipid metabolism" pathclass
LMres <- extendedres[extendedres$path_class == "Lipid metabolism", ]
curvesplot(LMres, facetby = "id", npoints = 100, line.size = 0.8,
  colorby = "trend", xmin = 0, xmax = 8) 
}
}
