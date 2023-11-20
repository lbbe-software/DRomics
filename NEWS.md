# DRomics (development version)

NEW FEATURES

- Put the argument dose_log_transfo by default at TRUE in functions plot.drcfit(), plotfit2pdf(), targetplot() and BMD_log_transfo at TRUE in functions bmdplot(), bmdplotwithgradient() and sensitivityplot().
- Add of the argument BMD_log_transfo by default at TRUE in functions plot.bmdcalc() and plot.bmdboot().
- Put the argument scaling by default at TRUE in curvesplot() and bmdplotwithgradient().
- Add of xlab and ylab to plots from curvesplot() (signal or scaled signal for y-axis) and change the color lab in "scaled signal" in plots from bmdplotwithgradient() when the signal is scaled.
- Add the possibility (new argument addBMD of curvesplot()) to add points at BMD-BMR values on curvesplots.
- Add the Peer Community Journal citation.
- Add of the function selectitems() proposing filters to retain
only the items associated to the best estimated BMD values in DRomics workflow output.

BUG FIXES

- Fix a bug that appeared very occasionally in the bootstrap procedure (error in bmdboot() due to fail of the call to uniroot()).
- Define the scale of nb of items in sensitivityplot() and trendplot() to get 4 integer values from min max and rounded 0.5 and 0.75 quartiles.

# DRomics 2.5-0 

NEW FEATURES

- Add of the function selectgroups() to select most represented and/or
most sensitive groups on which to focus for biological interpretation.
- Add of an RNAseq data with batch effect (zebraf) with an example of use of 
ComBat_seq{sva} to correct the batch effect.
- Add of PCAdataplot() a function to visualize omic data.
- Add of a column named maxychange (maximal absolute y change (up or down) 
from the control) in the output of drcfit() (and so of bmdcalc() and bmdboot())
- Add an argument named scaling in curvesplot() and bmdplotwithgradient()
that enables the scaling of the shifted signal (y - y0) by dividing 
it by maxychange (new output of drcfit).
- Add the function formatdata4DRomics() to format data for DRomics from the 
matrix of the signal measurements and the vector of observed/tested doses.
- Add range4boxplot by default fixed at 1e6 to the arguments of plot functions of 
RNAseqdata(), microarraydata() and continuousomicdata() objects, 
to prevent the automatic plot of many outliers as individual points and so
produce lighter plot files.
- Change the default value of transfo.method in RNAseqdata() (put at "vst" 
when the number of samples is larger than 30)

BUG FIXES

- Make sensitivityplot works even when BMDsummary is not given in input 
("first.quartile" defined as default)


# DRomics 2.4-0 

NEW FEATURES

- Forbid the use of the "ANOVA" method to select items when more than half of the doses are without replicates (e.g. for in situ data)
- Add of an example data set named insitu_RNAseq_sample.txt for tests and examples
- Add of arguments BMDoutput and BMDtype to plot.drcfit() and 
plotfit2pdf to make possible the add of BMD values and confidence 
intervals on the plot of fits.
- Add of an argument (enablesfequal0inGP) by default at TRUE in drcfit(), to enable
the simplification of the Gauss-probit model with 5 parameters by its version with f = 0 
(which corresponds to the probit model) to prevent overfitting when the parameter f is close to 0 
(evaluated using the information criterion).
- Add of an argument (enablesfequal0inLGP) by default at TRUE in drcfit(), to enable
the simplification of the log-Gauss-probit model with 5 parameters by its version with f = 0 
(which corresponds to the log-probit model) to prevent overfitting when the parameter f is close 
to 0 (evaluated using the information criterion).
- Add of an argument (preventsfitsoutofrange) by default at TRUE in drcfit() 
to prevent fits of biphasic models giving an extreme value out of the range of 
the observed signal, that could happen in rare cases.
- Add of a defensive code in microarraydata(), continuousomicdata(),
continuousanchoringdata(), RNAseqdata(), and the argument backgrounddose was added
to prevent the use of DRomics on a design with no dose at zero. 
In case of observationnal data, to prevent calculation of BMDs by extrapolation, 
doses considered as corresponding to the background exposition must be fixed at 0, 
for example using this new argument.

# DRomics 2.3-0 

NEW FEATURES

- An argument facetby2 was added to bmdplotwithgradient()
and to curvesplot() to be able
to split those plots in rows AND columns using facet_grid().
- Add of the function trendplot() to plot the repartition of dose-response
trend per group of items.
- Add of the function sensitivityplot() to plot various summaries of BMD
values per group of items.
- Add of the function bmdplot() that takes extendedres as a first argument as
trendplot(), bmdplotwithgradient(), sensitivityplot() and curvesplot().
- Removing of the drcfit() argument sigmoid.model that was confusing for some users and not useful for a common use of the package.

# DRomics 2.2-0 

NEW FEATURES

- The second-order Akaike criterion (AICc) recommended to prevent overfitting
with small number of data poinst in each dose-response crives was implemented
and defined as the default option for the argument information.criterion in drcfit().
- The example file of the package (?DRomics) was replaced by a vignette to help
the use of the package and of the Shiny application.
- Improvement of the computation of low BMD values for designs with a high
ratio between the maximal and minimal (non null) tested doses, with the add
of two arguments to bmdcalc, minBMD and ratio2switchinlog.
- Add of two columns in the output of bmdcalc (BMR.zSD and BMR.xfold)
- Add of a function plotfit2pdf() to plot all fits (or residual plots)
in a pdf file, using the raw scale or the log scale of the dose and 
removing of the option saveplot2pdf of drcfit().
- Replacement of the class 'metabolomicdata' by the class 'continuousomicdata'
and add of a function continuousomicdata() that is called by metabolomicdata()
but could be used on other types of continuous omics data such as proteomics data.
- default color changed for bmdplotwithgradient() (green replaced by blue for 
color blind people)
- Removing of three of the four datasets from Zou et al. 2017 to make the 
package lighter
- Add a test on residuals for heteroscedasticity and an output of drcfit: 
residualtests

BUG FIXES

- handling in RNAseqdata() of all the cases for which vst() may give a stop message.


# DRomics 2.1-3

NEW FEATURES

- Add of the function bmdplotwithgradient()
- Add of the function ecdfquantileplot()
- Add of an argument named information.criterion in drcfit() to choose 
the use of AIC or BIC for the best fit model selection process.
- Add of the possibility to enter data as an R object of class data.frame
- Add of published datasets (Zhou et al. 2017, Larras et al. 2020) and 
corresponding help pages
- Add of function continuousanchoringdata and modification of itemselect()
to enable the selection of significant responses on continuous anchoring data.
- Add an argument dose_log_transfo in plot.drcfit to enable the use of
log tranformation of the x-axis.
- Add an element in the list of drcfit() output : unfitres giving some 
information on selected items for which the modelling step is not successful
- Add of a function to plot raw data on target items optionally with fitted curves 
for items which have be selected in step 2 and for which step 3 was successful
(new function targetplot()).
- Add of the argument transfo.blind in RNAseqdata() 
- Add of the argument free.y.scales in curvesplot() to enable free y scales in facets
and dose_log_transfo to use an x log scale for plot and calculation of signal.
- Add of examples in DRomics.Rd to help a multi-omics approach
- Add of the argument round.counts to enable rounding of read counts that would come
from Kallisto or Salmon.

BUG FIXES

- make the direct use of varianceStabilizingTransformation() automatic
for small RNAseq data sets (low number of items: < 1000) to fix a bug in RNAseqdata() that occured when using vst() with small datasets.


# DRomics 2.0-1 

NEW FEATURES

- Add of a filter in itemselect(), to exclude from the selection items
with a too high proportion of non detected values (assuming they were
imputed to a common minimum value).
- Add of an argument point.type that enables the change of point type
in, ecdfplotwithCI or its coding by a given factor.
- Add of an argument plot.type in function plot.drcfit() to enable
residual plots.

# DRomics 2.0-0 

NEW FEATURES

- Replacement of the function omicdata() by the function 
microarraydata() and add of two new data importation functions,
RNAseqdata() and metabolomicdata().

# DRomics 1.1-3 

NEW FEATURES

- Replacement of the argument named bytypology of plot.bmdcalc
by an argument named by which can taka three values,
"none", "trend", "model" or "typology".
- Add of a function to plot fitted curves (new function curvesplot()).

# DRomics 1.1-2 

NEW FEATURES

- Add of a function to plot the distribution of a variable as an ecdf plot, with confidence 
intervals on this variable (new function ecdfplotwithCI)

# DRomics 1.1-1 

NEW FEATURES

- Add of bootstrap computation of confidence intervals on benchmark doses (new function bmdboot)
- Add of a function to plot the distribution of a variable as an ecdf plot, with confidence 
intervals on this variable (new function ecdfplotwithCI)


# DRomics 1.0-1

NEW FEATURES

- Add of column yextrem in the results of drcfit (y value at the extremum for biphasic curves)

# DRomics 1.0-0

- Initial release.
