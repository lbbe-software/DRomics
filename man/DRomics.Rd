\name{DRomics-package}
\alias{DRomics}
\alias{DRomics-package}
\title{Overview of the \pkg{DRomics} package}

\description{
DRomics provides several functions for dose-response (or concentration-response) characterization from omics data. It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses (or concentrations, at least 5, and the more the better) rather than a great number of replicates (no need of three replicates). DRomics provides seven main functions described as follows:
\itemize{
\item \code{\link{microarraydata}}, \code{\link{RNAseqdata}} and 
\code{\link{metabolomicdata}} to check, normalize and transform data dependending of the type of omics data (see next paragraph for details),
\item \code{\link{itemselect}} to select monotonic or biphasic significant responses,
\item \code{\link{drcfit}} to choose the best-fit model among a predefined family of monotonic and biphasic models to describe each significant response and classify it in a typology of response, 
\item and \code{\link{bmdcalc}} to derive a benchmark dose or concentration from each fitted curve.
}
and other functions to help the biological interpretation of the worflow results.

The available version supports three types of data and should not be used 
with other types of data:
\itemize{
\item Single-channel microarray data (previously transformed in log2)
that must imported  using the function \code{\link{microarraydata}}, 
\item RNAseq (in raw counts) that must imported using the function 
\code{\link{RNAseqdata}} and 
\item metabolomic data that must imported using the function 
\code{\link{metabolomicdata}}.
For metabolomic data, all the pretreatment steps must be done before importation
of data and data should be imported in log scale
so that data can be directly fitted by least-square regression
(assuming a Gaussian error model valid)
without any transformation.
}


Below is proposed an example including each step or the workflow on microarray data 
(sample data from Larras et al. 2018) and 
another example on metabolomic data (results from Larras et al. 2020)
to show how some functions provided in the package can be used to help to link 
the worflow results with the biological annotation of selected items.

}

\seealso{
    See \code{\link{microarraydata}}, \code{\link{RNAseqdata}},
    \code{\link{metabolomicdata}}, \code{\link{itemselect}},
    \code{\link{drcfit}}, \code{\link{bmdcalc}}, 
    \code{\link{bmdboot}}, \code{\link{bmdplotwithgradient}},
    \code{\link{ecdfplotwithCI}}, \code{\link{ecdfquantileplot}},
    \code{\link{curvesplot}} 
    for details about each function.
}

\author{ 
Marie-Laure Delignette-Muller, Elise Billoir, Floriane Larras and Aurelie Siberchicot.
}

\references{ 
Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M,
  Schmitt-Jansen M and Delignette-Muller ML (2018). DRomics: a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment. Environmental science & technology.\href{https://doi.org/10.1021/acs.est.8b04752}{https://doi.org/10.1021/acs.est.8b04752}

Larras F, Billoir E, Scholz S, Tarkka M, Wubet T, Tarkka M, Delignette-Muller ML and
  Schmitt-Jansen M and  (2020). A multi-omics concentration-response framework uncovers 
  novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus. 
  Journal of Hazardous Materials.\href{https://doi.org/10.1016/j.jhazmat.2020.122727}{https://doi.org/10.1016/j.jhazmat.2020.122727}
}


\examples{
  \donttest{

#### Different steps of the workflow on an example with microarray data ####

# Step 1: importation, check and normalization of data if need
#
## here cyclicloess normalization of a small microarray data set
## (sample of a real data set)

datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
plot(o)

# Step 2: item selection using the quadratic method
#
## the quadratic method is the one we preconize to select both
## monotonic and biphasic curves from
## a typical dose-response design (with few replicates per dose)

(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))


# Step 3: fit of dose-response models, choice of the best fit for each curve
# and definition of the typology of response
#

(f <- drcfit(s_quad, progressbar = TRUE))
f$fitres
plot(f)

# Step 4: calculation of x-fold and z-SD benchmark doses 
#

(r <- bmdcalc(f, z = 1, x = 10))
plot(r, BMDtype = "zSD", plottype = "ecdf") 
plot(r, BMDtype = "xfold", plottype = "ecdf") 
plot(r, BMDtype = "zSD", plottype = "hist", hist.bins = 10) 
plot(r, BMDtype = "zSD", plottype = "density") 
plot(r, BMDtype = "zSD", plottype = "ecdf", by = "trend") 
bmdplotwithgradient(r$res, BMDtype = "zSD",
                      xmax = max(f$omicdata$dose), facetby = "trend", 
                      shapeby = "model", line.size = 2)

# Step 5: calculation of confidence intervals on the BMDs by bootstrap 
#

(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a reasonable precision
plot(b, BMDtype = "zSD") 
plot(b, BMDtype = "zSD", by = "trend") 
b$res

}

# About using the DRomics-shiny app for this workflow
# 

if(interactive()) {
  appDir <- system.file("DRomics-shiny", package = "DRomics")
  shiny::runApp(appDir, display.mode = "normal")
}


#### Help for biological interpretation on an example with metabolomic data ####

# the dataframe with metabolomic results 
# (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE)
str(res)

# the dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", 
                              package="DRomics")
annot <- read.table(annotfilename, header = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
annotres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(annotres)


# BMDplot with gradient splitted by pathway
bmdplotwithgradient(annotres, BMDtype = "zSD",
                      xmax = 8,  facetby = "path_class", 
                       shapeby = "trend",
                       line.size = 2) 
                       
# an ECDFplot of BMD_zSD with confidence intervals splitted by pathway
# with color coding for dose-response trend
ecdfplotwithCI(variable = annotres$BMD.zSD, 
               CI.lower = annotres$BMD.zSD.lower, 
               CI.upper = annotres$BMD.zSD.upper, 
               by = annotres$path_class,
               CI.col = annotres$trend) 


# an ECDFplot of quantiles of BMD-zSD calculated by pathway
ecdfquantileplot(variable = annotres$BMD.zSD, 
               by = annotres$path_class,
               quantile.prob = 0.25) 

# Plot of the dose-response curves for a specific pathway
# in this example the "lipid metabolism" pathclass
LMres <- annotres[annotres$path_class == "Lipid metabolism", ]
curvesplot(LMres, facetby = "id", npoints = 100, line.size = 1,
           colorby = "trend",
           xmin = 0, xmax = 8) 


}