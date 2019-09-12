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


Below is proposed an example including each step or the workflow on microarray data. 
}

\seealso{
    See \code{\link{microarraydata}}, \code{\link{RNAseqdata}},
    \code{\link{metabolomicdata}}, \code{\link{itemselect}},
    \code{\link{drcfit}}, \code{\link{bmdcalc}}, 
    \code{\link{bmdboot}} 
    for details about each function.
}


\author{ 
Marie-Laure Delignette-Muller, Elise Billoir, Floriane Larras and Aurelie Siberchicot.
}

\examples{
  \donttest{


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
plot(r, BMDtype = "xfold", plottype = "hist", by = "trend", hist.bins = 10) 
plot(r, BMDtype = "xfold", plottype = "hist", by = "model", hist.bins = 10) 
plot(r, BMDtype = "xfold", plottype = "hist", by = "typology", hist.bins = 10) 

# Step 5: calculation of confidence intervals on the BMDs by bootstrap 
#

(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a reasonable precision
plot(b, BMDtype = "zSD") 
plot(b, BMDtype = "zSD", by = "trend") 
b$res

}


# About using the DRomics-shiny app
# 

if(interactive()) {
  appDir <- system.file("DRomics-shiny", package = "DRomics")
  shiny::runApp(appDir, display.mode = "normal")
}
}