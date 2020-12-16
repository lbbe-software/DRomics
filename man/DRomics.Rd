\name{DRomics-package}
\alias{DRomics}
\alias{DRomics-package}
\title{Overview of the \pkg{DRomics} package}

\description{
DRomics provides several functions for dose-response (or concentration-response) characterization from omics data. It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses (or concentrations, at least 5, and the more the better) rather than a great number of replicates (no need of replicates).
DRomics deals with transcriptomics (microarray and RNAseq) and metabolomics data
(or other continuous omics data such as proteomics data), and with 
apical data (as long as they are continuous and follow a normal distribution for each dose or concentration, with a common standard error) in order to offer the possibility to link the responses across biological levels (from molecular to apical observations) using a common method.

DRomics provides eight functions for the main workflow:
\itemize{
\item \code{\link{microarraydata}}, \code{\link{RNAseqdata}}, 
\code{\link{metabolomicdata}} that is equivalent to 
\code{\link{continuousomicdata}} and \code{\link{continuousanchoringdata}} 
to check the conformity of the dataset, 
normalize and transform data depending of the type of data (see next paragraph for details),
\item \code{\link{itemselect}} to select monotonic and biphasic significant responses,
\item \code{\link{drcfit}} to choose the best-fit model among a predefined family of monotonic and biphasic models to describe each significant response and classify it in a typology of response, 
\item and \code{\link{bmdcalc}} to derive a benchmark dose or concentration from each fitted curve.
\item and \code{\link{bmdboot}} to estimate the uncertainty on benchmark doses using bootstrap.
}
and other functions to help the biological interpretation of the workflow results and especially
the analysis of results by groups, with groups defined from functional annotation:
\itemize{
\item \code{\link{ecdfplotwithCI}} to plot BMD values with confidence intervals as an 
ECDF plot partitionned by groups,
\item \code{\link{bmdplotwithgradient}} to plot BMD values as an ECDF plot, with a horizontal
color gradient coding for the signal, partitioned by groups,
\item \code{\link{ecdfquantileplot}} to plot a defined quantile of the BMD values (e.g.
the median) per group,
\item \code{\link{curvesplot}} to plot fitted curves of significant items splitted and/or colored
by group and
\item \code{\link{targetplot}} to plot dose-response raw data of target items (whether or not their response is considered significant) with fitted curves
if available.
}

The available version supports four types of data :
\itemize{
\item Single-channel microarray data (previously transformed in log2)
that must be imported  using the function \code{\link{microarraydata}}, 
\item RNAseq (in raw counts, so integer values) that must be imported using the function 
\code{\link{RNAseqdata}}, 
\item continuous omics data such as metabolomic data that must be imported using the function 
\code{\link{metabolomicdata}} or \code{\link{continuousomicdata}} and
\item anchoring apical data that must be imported using the function 
\code{\link{continuousanchoringdata}}.

For metabolomics or other continuous omics data, all the pretreatment steps must be done before their importation
and they should be imported in a scale that enables a direct fit by least-square regression
(assuming a normal error model valid). For example the metabolomic data should
imperatively be imported in log scale. 

Anchoring apical data must be continuous and also imported
in a scale that enables the use of least-square regression
(assuming a normal error model valid).

The difference between continuous omics data and continuous anchoring data
is that in omics data the different lines correspond to the measurements of the
same variable (same type of signal) on different items, while in 
anchoring data the different lines may correspond to measurements of different
variables (different continuous endpoints), that in particular may have very 
different standard deviations.

}


Below is proposed an example including each step or the workflow on microarray data 
(sample data from Larras et al. 2018) and 
another example on metabolomic data (results from Larras et al. 2020)
to show how some functions provided in the package can be used to help to link 
the workflow results with the biological meaning of selected items.

}

\seealso{
    See \code{\link{microarraydata}}, \code{\link{RNAseqdata}},
    \code{\link{metabolomicdata}}, \code{\link{continuousomicdata}},
    \code{\link{continuousanchoringdata}}, 
    \code{\link{itemselect}},
    \code{\link{drcfit}}, \code{\link{bmdcalc}}, 
    \code{\link{bmdboot}}, \code{\link{bmdplotwithgradient}},
    \code{\link{ecdfplotwithCI}}, \code{\link{ecdfquantileplot}},
    \code{\link{curvesplot}}, \code{\link{targetplot}}
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

# Step 1: importation, check and normalization of data if needed
#
   # Import and check
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

   # Normalization 
   # here cyclicloess normalization of a small microarray data set
   # (sample of a real data set)
(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
plot(o)

# Step 2: selection of significantly responding items
#
   # The quadratic method is the one we preconize to select both
   # monotonic and biphasic curves from
   # a typical dose-response design (with few replicates per dose)

(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.001))

# Step 3: fit of dose-response models, choice of the best fit for each curve
# and definition of the typology of response
#

(f <- drcfit(s_quad, progressbar = TRUE))

   # Results of the fit per item are stored in the output fitres
f$fitres

   # Default plot provides fitted curves with observed points
plot(f) 

   # Plot of residuals is also recommended to validate the normal error model
plot(f, plot.type = "fitted_residuals")

# Step 4: calculation of x-fold and z-SD benchmark doses 
#

(r <- bmdcalc(f, z = 1, x = 10))

   # Distribution of benchmark doses as an ECDF plot
   # (ECDF = Empirical Cumulative Density Function)
plot(r, BMDtype = "zSD", plottype = "ecdf") 

plot(r, BMDtype = "xfold", plottype = "ecdf") 

   # Histogram of benchmark doses
plot(r, BMDtype = "zSD", plottype = "hist", hist.bins = 10) 

   # Density plot of benchmark doses
plot(r, BMDtype = "zSD", plottype = "density") 

   # Plot of the distribution of benchmark doses grouped by reponse trend
plot(r, BMDtype = "zSD", plottype = "ecdf", by = "trend")

   # Same plot with the add of a color gradient for each item coding for
   # the intensity of the signal (after shift of the control signal at 0)
   # as a function of the dose

bmdplotwithgradient(r$res, BMDtype = "zSD",
                       facetby = "trend", shapeby = "model")

# Step 5: calculation of confidence intervals on the BMDs by bootstrap 
#

(b <- bmdboot(r, niter = 100)) # niter to put at 1000 for a reasonable precision

   # Results of the fit per item and confidence intervals are stored in the output res
b$res

      # Distribution of benchmark doses as an ECDF plot
      # with confidence intervals on each BMD values
plot(b, BMDtype = "zSD") 

   # Same plot with BMD grouped by rsponse trend
plot(b, BMDtype = "zSD", by = "trend") 

}

# About using the DRomics-shiny app for this workflow
# 

if(interactive()) {
  appDir <- system.file("DRomics-shiny", package = "DRomics")
  shiny::runApp(appDir, display.mode = "normal")
}


#### Help for biological interpretation of DRomics outputs ####
#### based on an example with metabolomic data             ####
  \donttest{

# 1. Import the dataframe with metabolomic results to use:
# the output $res of bmdcalc() or bmdboot() functions)
# from step 4 or 5 of the main DRomics workflow

# This step will not be necessary if previous steps are done directly
# in R using the DRomics package as described previously
# but in this example we did it to take a real example
# that took a long time to run but from which results are stored in the package

   # code to import the file for this example in our package
metabresfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
metabres <- read.table(metabresfilename, header = TRUE, stringsAsFactors = TRUE)

   # to see the structure of this file
str(metabres)

# 2. Import the dataframe with functional annotation (or any other
# descriptor/category you want to use, here KEGG metabolic pathway)
# of each item present in the 'res' file. 
# Be cautious, this file must be produced by the user.
# Each item may have more than one annotation (-> more than one line)

   # code to import the file for this example in our package
metabannotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", 
                              package="DRomics")
metabannot <- read.table(metabannotfilename, header = TRUE, stringsAsFactors = TRUE)

   # to see the structure of this file
str(metabannot)

# 3. Merging of both previous dataframes
# in order to obtain a so-called 'extenderes' dataframe
# gathering for each item metrics derived from the DRomics workflow and 
# functional annotation

metabextendedres <- merge(x = metabres, y = metabannot, by.x = "id", by.y = "metab.code")
str(metabextendedres)


# 4. Graphical representations provided in the package

   # BMDplot with gradient splitted here by metabolic pathway
bmdplotwithgradient(metabextendedres, BMDtype = "zSD",
                      facetby = "path_class", shapeby = "trend") 
                       
   # the same representation with labels of items (so without shapeby)
bmdplotwithgradient(metabextendedres, BMDtype = "zSD", facetby = "path_class", 
                       add.label = TRUE) 
                       
   # an ECDFplot of BMD_zSD with confidence intervals splitted 
   # here by metabolic pathway
   # with color coding for dose-response trend
ecdfplotwithCI(variable = metabextendedres$BMD.zSD, 
               CI.lower = metabextendedres$BMD.zSD.lower, 
               CI.upper = metabextendedres$BMD.zSD.upper, 
               by = metabextendedres$path_class,
               CI.col = metabextendedres$trend) 


   # an ECDFplot of quantiles of BMD-zSD calculated 
   # here by metabolic pathway
ecdfquantileplot(variable = metabextendedres$BMD.zSD, 
               by = metabextendedres$path_class,
               quantile.prob = 0.25) 

   # Plot of the dose-response curves for a specific metabolic pathway
   # in this example the "lipid metabolism" pathclass
LMmetabextendedres <- metabextendedres[metabextendedres$path_class == "Lipid metabolism", ]
curvesplot(LMmetabextendedres, facetby = "id", npoints = 100, line.size = 1,
           colorby = "trend",
           xmin = 0, xmax = 8) 

#### Help for multiomics approach                               ####
#### an example with metabolomics and transcriptomics           ####
#### data for Scenedesmus and triclosan (cf. Larras et al. 2020 ####

# 5. following the same steps described before for metabolomics
# import and merge the results for microarray data

contigresfilename <- system.file("extdata", "triclosanSVcontigres.txt", package="DRomics")
contigres <- read.table(contigresfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigres)

contigannotfilename <- system.file("extdata", "triclosanSVcontigannot.txt", 
                              package="DRomics")
contigannot <- read.table(contigannotfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigannot)

contigextendedres <- merge(x = contigres, y = contigannot, by.x = "id", by.y = "contig")
str(contigextendedres)


# 6. Comparison of results obtained at both molecular levels (metabolites and contigs)

   # Binding of dataframes at both levels adding a variable coding
   # for the level
extendedres <- rbind(metabextendedres, contigextendedres)
extendedres$level <- factor(c(rep("metabolites", nrow(metabextendedres)),
                              rep("contigs", nrow(contigextendedres))))
str(extendedres)

   # Frequencies of pathways by molecular levels
(t.pathways <- table(extendedres$path_class, extendedres$level)) 
original.par <- par()
par(las = 2, mar = c(4,13,1,1))
barplot(t(t.pathways), beside = TRUE, horiz = TRUE, 
      cex.names = 0.7, legend.text = TRUE, 
      main = "Frequencies of pathways")
      
   # Proportions of pathways by molecular levels
(t.prop.pathways <- prop.table(t.pathways, margin = 2)) 
barplot(t(t.prop.pathways), beside = TRUE, horiz = TRUE, 
      cex.names = 0.7, legend.text = TRUE, 
      main = "Proportion of pathways")
par(original.par)

if (require(ggplot2))
{
      # an ECDF plot of BMD_zSD by pathways
      # using color
   ggplot(extendedres, aes(x = BMD.zSD, color = level)) +
      stat_ecdf(geom = "step") + ylab("ECDF")
      
      # or using facets
   ggplot(extendedres, aes(x = BMD.zSD)) + facet_wrap(~ level) +
      stat_ecdf(geom = "step") + ylab("ECDF")

}

   # an ECDF plot of BMD_zSD with confidence intervals splitted 
   # here by metabolic pathway 
   # with color coding for molecular level
ecdfplotwithCI(variable = extendedres$BMD.zSD, 
               CI.lower = extendedres$BMD.zSD.lower, 
               CI.upper = extendedres$BMD.zSD.upper, 
               by = extendedres$path_class,
               CI.col = extendedres$level) + labs(col = "Molecular level")

   # an ECDFplot of BMD_zSD with confidence intervals splitted 
   # here by molecular level
ecdfplotwithCI(variable = extendedres$BMD.zSD, 
               CI.lower = extendedres$BMD.zSD.lower, 
               CI.upper = extendedres$BMD.zSD.upper, 
               by = extendedres$level)


   # Plot of the dose-response curves for a specific metabolic pathway
   # in this example the "lipid metabolism" pathclass
LMres <- extendedres[extendedres$path_class == "Lipid metabolism", ]
curvesplot(LMres, facetby = "level", free.y.scales = TRUE, npoints = 100, line.size = 1,
           colorby = "trend",
           xmin = 0, xmax = 8) + labs(col = "DR_trend")
}
}