require("shiny", quietly = TRUE)
require("shinyjs", quietly = TRUE)
require("shinyBS", quietly = TRUE)
require("shinyWidgets", quietly = TRUE)
require("DRomics", quietly = TRUE)
require("sortable", quietly = TRUE)
require("ggplot2", quietly = TRUE)
require("plotly", quietly = TRUE)
addResourcePath('DRomicspkg', system.file("extdata", package="DRomics"))
options(shiny.maxRequestSize=30*1024^2)

texthelpnblevel <- "The maximal number of experimental levels is 10. The experimental levels can be for example different molecular levels (transcriptomics, metabolomis, ...), different experimental time points or different biological models (different species, different experimental settings), ..."
texthelpmaxdosexscale <- "We recommand you to fix it at the maximal tested dose/concentration. If this value is not the same for all the experimental levels, keep the smallest one to prevent extrapolation of dose response curves in plots."
helplabel1step1 <- "Each file for annotation data must have exactly two columns. Take care to reduce the dimension of your annotation file by keeping only the items which are present in the DRomics output and have at least one biological annotation."
helplabel2step1 <- "If there are multiple experimental levels, the labels chosen must be unique. All spaces in the labels are removed."
helplabel3step1 <- "If at least one annotation stands in more than one word (separated by a space), you should surround each of annotation by quotes, or use tab as a column separator in your annotation file (check the box to this option)."

helplabel1step2 <- "To limit the number of annotation groups, you can use the thresholds on the number of items representing the group and/or the BMDsummary value of the group. You can also choose to keep the results of all the experimental levels as soon as the criteria are met for at least one experimental level."
helplabel2step2 <- "When this option is selected, if a group is selected for at least one experimental level, it will be kept in the selection at all the experimental levels."

helplabel1step4 <- "For this plot, it is necessary to define the range of the dose (for example corresponding to the range of the tested/observed doses) and when using a log scale for the dose, a strictly positive value must be given for the minimum (a value below the smallest non null tested dose is recommended)."

fnvaluecheckbox <- function(valuecheckbox, pathclasslabel) {
  if(valuecheckbox == "annotation") {
    return(pathclasslabel)
  } else if (valuecheckbox == "explevel") {
    return("experimental_level")
  }
}