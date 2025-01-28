
# Function that could be improved and added in DRomics
# to plot means at each condition 
# (condition = dose taken as an ordered factor)
# for experimental data with replicates
meanplot <- function(omicdata, items, facetby, colorby)
{
  d2plot <- data.frame(item = character(),
                                  dose = numeric(),
                                  meanatcontrol = numeric(),
                                  meansignal = numeric(),
                                  sdsignal = numeric(),
                                  colorby = logical(),
                                  facetby = factor())
  dose <- omicdata$dose
  
  (doseu <- unique(dose))
  (ndose <- length(doseu))
  
  index <- match(items, omicdata$item)
  cbind(items, index)
  # to write more efficiently without loop ? !!!!!!!!!!!!!!!!!!!
  for (i in 1:length(index))
  {
    d2bind <- data.frame(index = rep(index[i], ndose),
                         dose = doseu,
                         meanatcontrol = rep(omicdata$data.mean[index[i], 1], ndose),
                         meansignal = omicdata$data.mean[index[i], ],
                         sdsignal = omicdata$data.sd[index[i], ],
                         colorby = rep(colorby[i], ndose),
                         facetby = rep(facetby[i], ndose))
    
    d2plot <- rbind(d2plot, d2bind)            
  }
  # Define the condition using the dose
  d2plot$condition <- as.factor(d2plot$dose)
  levels(d2plot$condition)[1] <-  "control"
  
  rownames(d2plot) <- NULL
  str(d2plot)
  
  ggplot(d2plot, aes(x = condition, y = meansignal - meanatcontrol, 
                                group = index, col = colorby)) + 
                      facet_wrap(~ facetby) +
                      geom_line(alpha = 0.6, linewidth = 0.2) + 
                      geom_point(alpha = 0.6)
  
}


# Test of the function
#####################################
require(DRomics)
require(ggplot2)

# (1) 
# An example with RNAseq data colored by selected or not
# and groups given by the user
#
datafilename <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
(o <- RNAseqdata(datafilename, check = TRUE, transfo.method = "vst"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))

length(s_quad$omicdata$item)
length(s_quad$selectindex)
items <- s_quad$omicdata$item 
(selected_items <- s_quad$omicdata$item[s_quad$selectindex])
# ce serait plus simple si itemselect sortait aussi la liste des items
# à ajouter peut-être
(fitted_items <- f$fitres$id)
selected <- as.factor(is.element(items, selected_items))
levels(selected) <- c("not selected", "selected")
fitted <- as.factor(is.element(items, fitted_items))
levels(fitted) <- c("not fitted", "fitted")

meanplot(omicdata = o, items = items, facetby = selected, colorby = fitted)                     

# (2) 
# Similar example with metabolomic data colored by selected or not
# and groups given by the user
#
datafilename <- system.file("extdata", "metabolo_sample.txt", package = "DRomics")
o <- continuousomicdata(datafilename)
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))

length(s_quad$omicdata$item)
length(s_quad$selectindex)
items <- s_quad$omicdata$item 
(selected_items <- s_quad$omicdata$item[s_quad$selectindex])
# ce serait plus simple si itemselect sortait aussi la liste des items
# à ajouter peut-être
(fitted_items <- f$fitres$id)
selected <- as.factor(is.element(items, selected_items))
levels(selected) <- c("not selected", "selected")
fitted <- as.factor(is.element(items, fitted_items))
levels(fitted) <- c("not fitted", "fitted")

meanplot(omicdata = o, items = items, facetby = selected, colorby = fitted)                     

# (3) 
# Similar example with microarray data colored by selected or not
# and groups given by the user
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
                            package="DRomics")
o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
(f <- drcfit(s_quad, progressbar = TRUE))

length(s_quad$omicdata$item)
length(s_quad$selectindex)
items <- s_quad$omicdata$item 
(selected_items <- s_quad$omicdata$item[s_quad$selectindex])
# ce serait plus simple si itemselect sortait aussi la liste des items
# à ajouter peut-être
(fitted_items <- f$fitres$id)
selected <- as.factor(is.element(items, selected_items))
levels(selected) <- c("not selected", "selected")
fitted <- as.factor(is.element(items, fitted_items))
levels(fitted) <- c("not fitted", "fitted")

meanplot(omicdata = o, items = items, facetby = selected, colorby = fitted)                     
