# Overview
`DRomics` is a freely available on-line tool for dose-response (or concentration-response) characterization from omics data. It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses (or concentrations, at least 6, and the more the better) rather than a great number of replicates (no need of three replicates). After a first optional step which consists to import, check and if needed normalize/transform the data (step 1), the aim of the proposed workflow is to select monotonic and/or biphasic significantly responsive items (e.g. probes, metabolites) (step 2), to choose the best-fit model among a predefined family of monotonic and biphasic models to describe the response of each selected item (step 3), and to derive a benchmark dose or concentration from each fitted curve (step 4).

In the available version, `DRomics` supports single-channel microarray data (in log2 scale), RNAseq data (in raw counts) or metabolomics data (in log scale).


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

# The shiny app 
`DRomics-shiny` can be run on a R session, doing:
```r
shiny::runApp(system.file("DRomics-shiny", package = "DRomics"))
```

or online at:
<a href="http://lbbe-shiny.univ-lyon1.fr/DRomics-shiny/" target="_blank">http://lbbe-shiny.univ-lyon1.fr/DRomics-shiny/</a>

# Authors & Contacts
- Elise Billoir: elise.billoir@univ-lorraine.fr
- Marie-Laure Delignette-Muller: marielaure.delignettemuller@vetagro-sup.fr
- Floriane Larras: floriane.larras@ufz.de
- Mechthild Schmitt-Jansen: mechthild.schmitt@ufz.de

About technical issues, you can contact Aurélie Siberchicot: aurelie.siberchicot@univ-lyon1.fr


# Citation
Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018). 
*DRomics : a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment*. 
Environmental Science & Technology. 
<a href="https://pubs.acs.org/doi/10.1021/acs.est.8b04752" target="_blank">https://pubs.acs.org/doi/10.1021/acs.est.8b04752</a>
