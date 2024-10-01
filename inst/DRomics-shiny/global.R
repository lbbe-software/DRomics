require("shiny", quietly = TRUE)
require("DRomics", quietly = TRUE)
require("ggplot2", quietly = TRUE)
require("shinyjs", quietly = TRUE)
require("shinyBS", quietly = TRUE)
require("shinycssloaders", quietly = TRUE)
require("tools", quietly = TRUE)
require("svglite", quietly = TRUE)
addResourcePath('DRomicspkg', system.file("extdata", package="DRomics"))
options(shiny.maxRequestSize=30*1024^2)

text_bgdose <- "The value of doses under which doses can be replaced by 0, to be considered as the background dose for BMD calculation, is necessary only if there is no dose at zero in the data. To prevent hazardous calculation of BMDs by extrapolation, DRomics will not run without null doses in the design or specification of this background dose."
