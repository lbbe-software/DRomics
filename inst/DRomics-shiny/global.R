library(shiny, quietly = TRUE)
library(DRomics, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(shinyBS, quietly = TRUE)
library(shinycssloaders, quietly = TRUE)
library(tools, quietly = TRUE)
addResourcePath('DRomicspkg', system.file("extdata", package="DRomics"))
options(shiny.maxRequestSize=30*1024^2)

text_bgdose <- "This argument must be used when there is no dose at zero in the data, to prevent the calculation of the BMD by extrapolation. All doses below or equal to the value given in backgrounddose will be fixed at 0, so as to be considered at the background level of exposition."