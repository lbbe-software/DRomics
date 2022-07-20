library(shiny, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(shinyBS, quietly = TRUE)
library(shinyWidgets, quietly = TRUE)
library(DRomics, quietly = TRUE)
library(sortable, quietly = TRUE)
library(ggplot2, quietly = TRUE)
options(shiny.maxRequestSize=30*1024^2)

texthelpnblevel <- "The maximal number of experimental levels is 10. The experimental levels can be for example different molecular levels (transcriptomics, metabolomis, ...), different experimental time points or different biological models (different species, different experimental settings), ..."
helplabel1step1 <- "Each file for annotation data must have exactly two columns."
helplabel2step1 <- "If there are multiple experimental levels, the labels chosen must be unique. All spaces in the labels are removed."

helplabel1step2 <- "To limit the number of annotation groups, you can use the thresholds on the number of items representing the group and/or the BMDsummary value of the group."

helplabel1step4 <- "For this plot, it is necessary to define the range of the dose (for example corresponding to the range of the tested/observed doses) and when using a log scale for the dose, a strictly positive value must be given for the minimum (a value below the smallest non null tested dose is recommended)."

fnvaluecheckbox <- function(valuecheckbox, pathclasslabel) {
  if(valuecheckbox == "annotation") {
    return(pathclasslabel)
  } else if (valuecheckbox == "explevel") {
    return("experimental_level")
  }
}