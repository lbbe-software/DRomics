[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/DRomics)](http://cran.r-project.org/package=DRomics)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/DRomics)](https://cran.r-project.org/package=DRomics)
[![R-CMD-check](https://github.com/aursiber/DRomics/workflows/R-CMD-check/badge.svg)](https://github.com/aursiber/DRomics/actions)

All informations about DRomics can also be found at <a href="https://lbbe.univ-lyon1.fr/fr/dromics" target="_blank">https://lbbe.univ-lyon1.fr/fr/dromics</a>.


# Overview
`DRomics` is a freely available on-line tool for dose-response (or concentration-response) characterization from omics data. It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses (or concentrations, at least 6, and the more the better) rather than a great number of replicates (no need of three replicates). After a first optional step which consists to import, check and if needed normalize/transform the data (step 1), the aim of the proposed workflow is to select monotonic and/or biphasic significantly responsive items (e.g. probes, metabolites) (step 2), to choose the best-fit model among a predefined family of monotonic and biphasic models to describe the response of each selected item (step 3), and to derive a benchmark dose or concentration from each fitted curve (step 4).

In the available version, `DRomics` supports single-channel microarray data (in log2 scale), RNAseq data (in raw counts), metabolomics data or other continuous
omics data and continuous anchoring data (for continuous omics or anchoring data,
in log scale or a scale that enables the use of a normal error model).


# The package 
The `limma` and `DESeq2` packages from Bioconductor must be installed for the use of `DRomics`:

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
   install.packages("BiocManager")

BiocManager::install(c("limma", "DESeq2"))
```

The stable version of `DRomics` can be installed from CRAN using:
```r
install.packages("DRomics")
```

The development version of `DRomics` can be installed from GitHub (`remotes` needed):
```r
if (!requireNamespace("remotes", quietly = TRUE))
   install.packages("remotes")
   
remotes::install_github("aursiber/DRomics")
``` 

Finally load the package in your current R session with the following R command:
```r
library(DRomics)
```


# The vignette
A vignette is attached to the `DRomics` package.

The development version of the pdf of this vignette can be see 
<a href="https://github.com/aursiber/DRomics/blob/master/DRomics_vignette.pdf" target="_blank">here</a>.
 
This vignette can be reached by:
```r
vignette("DRomics_vignette")
```

Note that, by default, the vignette is not installed when the package is installed through GitHub.
The following command (rather long to execute because of the large size of the vignette) will allow you to access the vignette of the development version of the package you installed from GitHub:
```r
remotes::install_github("aursiber/DRomics", build_vignettes = TRUE)
```


# The shiny app 
`DRomics-shiny` can be run on a R session, doing:
```r
shiny::runApp(system.file("DRomics-shiny", package = "DRomics"))
```

or online at:
<a href="http://lbbe-shiny.univ-lyon1.fr/DRomics/inst/DRomics-shiny/" target="_blank">http://lbbe-shiny.univ-lyon1.fr/DRomics/inst/DRomics-shiny/</a>

This shiny app is runing with the development version of DRomics.

# Authors & Contacts
If you have any need that is not yet covered, any feedback on the package / Shiny app, or any training needs, feel free to email us at dromics@univ-lyon1.fr .

Issues can be reported on https://github.com/aursiber/DRomics/issues .

- Elise Billoir: elise.billoir@univ-lorraine.fr
- Marie-Laure Delignette-Muller: marielaure.delignettemuller@vetagro-sup.fr
- Floriane Larras: floriane.larras@inrae.fr
- Mechthild Schmitt-Jansen: mechthild.schmitt@ufz.de
- Aurélie Siberchicot: aurelie.siberchicot@univ-lyon1.fr


# Citation
If you use Dromics, you should cite: <br />
Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018). 
*DRomics : a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment*. 
Environmental Science & Technology. 
<a href="https://pubs.acs.org/doi/10.1021/acs.est.8b04752" target="_blank">https://pubs.acs.org/doi/10.1021/acs.est.8b04752</a>

You can find this article at: <a href="https://hal.archives-ouvertes.fr/hal-02309919" target="_blank">https://hal.archives-ouvertes.fr/hal-02309919</a>

You can also look at the following citation for a complete example of use: <br />
Larras F, Billoir E, Scholz S, Tarkka M, Wubet T, Delignette-Muller ML, Schmitt-Jansen M (2020). 
*A multi-omics concentration-response framework uncovers novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus*.
Journal of Hazardous Materials. 
<a href="https://doi.org/10.1016/j.jhazmat.2020.122727." target="_blank">https://doi.org/10.1016/j.jhazmat.2020.122727.</a>
