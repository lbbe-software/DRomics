# Try a Venn diagram with the three selections (not so convincing)
# To put in the vignette ?

require(VennDiagram)
microarrayfilename <- system.file("extdata", "transcripto_sample.txt", package = "DRomics")
(o.microarray <- microarraydata(microarrayfilename, norm.method = "quantile"))
plot(o.microarray, cex.main = 0.8, col = "green")

s_quad <- itemselect(o.microarray, select.method = "quadratic", FDR = 0.01)
s_lin <- itemselect(o.microarray, select.method = "linear", FDR = 0.01)
s_anova <- itemselect(o.microarray, select.method = "ANOVA", FDR = 0.01)
index_quad <- s_quad$selectindex
index_lin <- s_lin$selectindex
index_anova <- s_anova$selectindex
plot(c(0,0), c(1,1), type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
draw.triple.venn(area1 = length(index_quad), 
                 area2 = length(index_lin), 
                 area3 = length(index_anova),
                 n12 = length(which(index_quad %in% index_lin)), 
                 n23 = length(which(index_anova %in% index_lin)), 
                 n13 = length(which(index_quad %in% index_anova)), 
                 n123 = length(which(index_quad %in% index_anova &
                                       index_quad %in% index_lin)),
                 category = c("quadratic trend test",
                              "linear trend test",
                              "ANOVA type test"),
                 cat.col=c("lightblue", "purple", "orange"), 
                 col= c("black", "black", "black"), 
                 fill = c("lightblue", "purple", "orange"), 
                 lty = "blank", cat.pos = c(0, 0, 0))
