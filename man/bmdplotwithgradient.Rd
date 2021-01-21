\name{bmdplotwithgradient}
\alias{bmdplotwithgradient}
\title{BMD plot with color gradient}

\description{
Provides an ECDF plot of BMD values with a horizontal color gradient coding, for each item,
for the theoretical signal as a function of the dose (concentration).
The idea is to display the amplitude and the intensity of the response of each item
on the BMD ECDF plot, in addition to the BMD ordered values. This plot is of interest especially when not too much items are presented. To maximize the lisibility of the plot, one can manually pre-select items based on its own criteria (e.g. functional group of interest).

}

\usage{
bmdplotwithgradient(extendedres, BMDtype = c("zSD", "xfold"),
                                   xmin, xmax, y0shift = TRUE, 
                                   facetby, shapeby, npoints = 50, 
                                   line.size, point.size = 1,
                                   ncol4faceting, limits4colgradient,
                                   lowercol = "darkblue", uppercol = "darkred",
                                   add.label, label.size = 2,
                                   BMD_log_transfo = FALSE)
}

\arguments{
\item{extendedres}{the dataframe of results provided by bmdcalc (res) 
 or a subset of this data frame (selected lines). This dataframe can be extended 
 with additional columns coming for example from the functional annotation of items, and some lines 
 can be replicated if their corresponding item has more than one annotation. 
 This extended dataframe
 must at least contain the column giving the BMD values (\code{BMD.zSD} or \code{BMD.xfold}
 depending of chosen BMDtype), identification of each curve (\code{id}),
 the column \code{model} naming the fitted model and the values of 
the parameters (columns \code{b}, \code{c}, \code{d}, \code{e}, \code{f}).}

\item{BMDtype}{The type of BMD to plot, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{xmin}{Optional minimal dose/concentration for definition of the x range.}

\item{xmax}{Optional maximal dose/concentration for definition of the x range (can be defined 
 as \code{max(f$omicdata$dose)} with \code{f} the output of \code{drcfit()} for example).}

\item{y0shift}{If \code{TRUE} (default choice) for each item the signal is shifted
to have the theoretical signal at the control at 0.}

\item{facetby}{optional argument naming the column of \code{extendedres} chosen 
  to split the plot in facets (no split if omitted).}

\item{shapeby}{optional argument naming the column of \code{extendedres} chosen 
  to shape the BMD points (no difference if shapeby if omitted).}
  
\item{npoints}{Number of points computed on each curve in order to define the 
  signal color gradient (= number of doses or concentrations for which the theoretical
  signal is computed from the fitted model for each item).}

\item{line.size}{Size of the horizontal lines for plotting each signal color gradient.}

\item{point.size}{Size of the BMD points.}

\item{ncol4faceting}{Number of columns for facetting.}

\item{limits4colgradient}{Optional vector giving minimal and maximal value of the signal
  for the color gradient.}

\item{lowercol}{Chosen color for the lower values of the signal.}

\item{uppercol}{Chosen color for the upper values of the signal.}

\item{add.label}{Points are replaced  by labels of items if TRUE.}

\item{label.size}{Size of labels if add.label is TRUE.}

\item{BMD_log_transfo}{If TRUE a log transformation of the BMD is used in the plot. 
  This option cannot be used with a null value of xmin in input.}


}

\details{
BMD values are plotted as an ECDF plot, as with \code{\link{plot.bmdcalc}}
using "ecdf" as plottype. In addition is plotted an horizontal color gradient 
for each item coding for the signal level at each dose 
(or concentration). The optional use of columns to code for shape and/or 
facets for each item is particularly intended to give a view of all the dose-response 
per group (e.g. metabolic pathways). Those groups must be coded in a column
of extendedres. In case where one item is allocated to more than one group
during the annotation process, the line of this item must be replicated in 
extendedres as many times as the number of annotation groups in which it was
allocated.

For each item of the extended dataframe, the name of the model 
(column \code{model}) and the values of 
the parameters (columns \code{b}, \code{c}, \code{d}, \code{e}, \code{f})
are used to compute theoretical dose-response curves, and so the 
corresponding signal color gradient, in the range \code{[xmin ; xmax].}
}

\value{ 
   a ggplot object.
}

\seealso{
    See \code{\link{plot.bmdcalc}} and \code{\link{plot.bmdboot}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) 
# A toy example on a very small subsample of a microarray data set. 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
package="DRomics")

o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01)
f <- drcfit(s_quad, progressbar = TRUE)
r <- bmdcalc(f)

# Plot of all the BMD values with color dose-response gradient 
#
bmdplotwithgradient(r$res, BMDtype = "zSD") 

\donttest{

# Plot of all the BMD values with color dose-response gradient 
# with definition of xmax from the maximal tested dose
#
bmdplotwithgradient(r$res, BMDtype = "zSD",
                    xmax = max(f$omicdata$dose)) 

# Add of item labels
bmdplotwithgradient(r$res, BMDtype = "zSD",
                    xmax = max(f$omicdata$dose), add.label = TRUE) 

# The same plot in log scale (we have to define xmin in this case) 
#
bmdplotwithgradient(r$res, BMDtype = "zSD",
                    BMD_log_transfo = TRUE) 

# The same plot in log scale with defining xmin and xmax at a chosen values 
#
bmdplotwithgradient(r$res, BMDtype = "zSD",
                    xmin =  min(f$omicdata$dose[f$omicdata$dose != 0] / 2), 
                    xmax = max(f$omicdata$dose),
                    BMD_log_transfo = TRUE) 

# Plot of all the BMD values with color dose-response gradient 
# faceted by response trend and shaped by model
#
bmdplotwithgradient(r$res, BMDtype = "zSD",
                      facetby = "trend", shapeby = "model") 
                      
# same plot changing the names of the facets 
levels(r$res$trend)
levels(r$res$trend) <- c("bell shape", "decreasing", "increasing", "U shape")

bmdplotwithgradient(r$res, BMDtype = "zSD",
                      facetby = "trend", shapeby = "model")

# same plot changing the labels of the legends 
# and inversing the two guides
if (require(ggplot2)) bmdplotwithgradient(r$res, BMDtype = "zSD",
                      facetby = "trend", shapeby = "model") + 
                      labs(col = "signal value", shape = "model") +
                      guides(colour = guide_colourbar(order = 2),
                      shape = guide_legend(order = 1))
                      
# (2)
# Plot of BMD values with color dose-response gradient
# faceted by metabolic pathway (from annotation of the selected items)
# and shaped by dose-response trend

# An example from the paper published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727
# A example of plot obtained with this function is in Figure 5 in Larras et al. 2020


# the dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
str(res)

# the dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
extendedres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(extendedres)


### (2.a) BMDplot with gradient by pathway
bmdplotwithgradient(extendedres, BMDtype = "zSD",
                     facetby = "path_class", 
                       shapeby = "trend") 

# (2.b) The same example forcing the limits of the colour gradient at other 
# values than observed minimal and maximal values of the signal
bmdplotwithgradient(extendedres, BMDtype = "zSD",
                      facetby = "path_class", 
                       shapeby = "trend",
                       limits4colgradient = c(-1, 1)) 

# (2.c) The same example changing the gradient colors and the line size
bmdplotwithgradient(extendedres, BMDtype = "zSD",
                      facetby = "path_class", 
                       shapeby = "trend",
                       line.size = 3, 
                       lowercol = "darkgreen", uppercol = "orange") 

# (2.d) The same example with only lipid metabolism pathclass
# and identification of the metabolites
LMres <- extendedres[extendedres$path_class == "Lipid metabolism", ]
bmdplotwithgradient(LMres, BMDtype = "zSD",
                       line.size = 3, 
                       add.label = TRUE, label.size = 3) 


# (3) 
# An example on a microarray data set (a subsample of a greater data set)
#
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))
(f <- drcfit(s_quad, progressbar = TRUE))
(r <- bmdcalc(f))

bmdplotwithgradient(r$res, BMDtype = "zSD",
                      facetby = "trend", 
                      shapeby = "model") 
bmdplotwithgradient(r$res, BMDtype = "zSD",
                      xmax = max(f$omicdata$dose), facetby = "trend", 
                      shapeby = "model") 
}

}
