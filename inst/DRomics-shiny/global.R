library(shiny, quietly = TRUE)
library(DRomics, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(shinyjs, quietly = TRUE)
library(shinyBS, quietly = TRUE)
library(shinycssloaders, quietly = TRUE)
library(tools, quietly = TRUE)
addResourcePath('DRomicspkg', system.file("extdata", package="DRomics"))
options(shiny.maxRequestSize=30*1024^2)

text_bgdose <- "The value of doses under which doses can be replaced by 0, to be considered as the background dose for BMD calculation, is necessary only if there is no dose at zero in the data. To prevent hazardous calculation of BMDs by extrapolation, DRomics will not run without null doses in the design or specification of this background dose."