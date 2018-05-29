\name{DRomics-package}
\alias{DRomics}
\alias{DRomics-package}
\title{Overview of the \pkg{DRomics} package}

\description{
DRomics provides several functions for dose-response (or concentration-response) characterization from omic data. It is especially dedicated to omic data obtained using a typical dose-response design, favoring a great number of tested doses (or concentrations) rather than a great number of replicates. DRomics provides four main functions described as follows:
\itemize{
\item \code{\link{omicdata}} to check and normalize data,
\item \code{\link{itemselect}} to select monotonic or biphasic significant responses,
\item \code{\link{drcfit}} to choose the best-fit model among a predefined family of monotonic and biphasic models to describe each significant response and classify it in a typology of response, 
\item and \code{\link{bmdcal}} to derive a sensitivity value (a benchmark dose or concentration) from each fitted curve.
}

Below is proposed an example including each step or the workflow. 
}

\seealso{
    See \code{\link{omicdata}}, \code{\link{itemselect}},
    \code{\link{drcfit}}, \code{\link{bmdcalc}} 
    for details about each function.
}


\author{ 
Marie-Laure Delignette-Muller, Elise Billoir, Floriane Larras and Aurelie Siberchicot.
}

\examples{

# Step 1: importation, check and normalization of data if need
#
## here cyclicloess normalization of a small transcripto data set
## (sample of a true data set)

datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))
plot(o)

# Step 2: item selection using the quadratic method
#
## the quadratic method is the one we preconize to select both
## monotonic and biphasic curves from
## a typical dose-response design (with few replicates per dose)

(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))


# Step 3: fit of dose-response models, choice of the best fit for each curve
# and definition of the typology of response
#

(f <- drcfit(s_quad, progressbar = TRUE))
f$fitres
plot(f)

# Step 4: calculation of x-fold and z-SD benchmark doses 
#

(r <- bmdcalc(f, z = 1, x = 10))
plot(r, BMDtype = "zSD", plottype = "ecdf", bytypology = FALSE) 
plot(r, BMDtype = "xfold", plottype = "hist", bytypology = TRUE, hist.bins = 10) 

}


