# Plot of fitted curves using columns of on extended dataframe to optionnally code 
# for color and or facet 
similaritycalc <- function(extendedres, xmin = 0, xmax, 
                           facetby, # rename facetby group !!!!!!!!!!!!
                           facetby2, # rename facetby2 explevel !!!!!!!!!!!!
                        npoints = 50, dose_log_transfo = FALSE)
{
  if (missing(extendedres) | !is.data.frame(extendedres))
    stop("The first argument of similaritycalc must be a dataframe 
    (see ?similaritycalc for details).")

  cnames <- colnames(extendedres)
  if (any(!is.element(c("id", "model", "b", "c", "d", "e", "f"), cnames)))
      stop("The first argument of similaritycalc must be a dataframe
    containing at least columns named id, model, b, c, d, e, f.")

  
    if (missing(xmax)) 
    stop("xmax must be given. You can fix it at max(f$omicdata$dose)} 
    with f the output of drcfit().")
  if (dose_log_transfo)
  {
    if (xmin == 0)
      stop("When using a log scale for the dose, a strictly positive value must be given for xmin.")
    x2plot <- 10^seq(log10(xmin), log10(xmax), length.out = npoints)
  } else
  {
    x2plot <- seq(xmin, xmax, length.out = npoints)
  }
  
  ns <- nrow(extendedres)
  N <- ns * npoints
  
  # to reformat in a matrix ? !!!!!!!!!!!!!!!!!!!!!!!
  # to rename in curvescalc ? !!!!!!!!!!!!!!!!!!!!!!!!
  curves2plot <- data.frame(x = rep(x2plot, ns), 
                            id = rep(extendedres$id, each = npoints),
                            y = numeric(length = N))
  
  if (!missing(facetby)) 
  {
    if (!is.character(facetby)) 
      stop("facetby should be a character string for the name of the column used for facetting.")
    if (!is.element(facetby, cnames))
      stop("facetby should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
    curves2plot$facetby <- rep(extendedres[, facetby], each = npoints)
    
    if (!missing(facetby2)) 
    {
      if (!is.character(facetby2)) 
        stop("facetby2 should be a character string for the name of the column used for facetting.")
      if (!is.element(facetby2, cnames))
        stop("facetby2 should be a character string corresponding to the name of a column of
           extendedres, the dataframe given in input.")
      curves2plot$facetby2 <- rep(extendedres[, facetby2], each = npoints)
    }     
  }
    
  for (i in 1:ns)
  {
    modeli <- extendedres$model[i]
    if (modeli == "linear")
    {
      b <- extendedres$b[i]
      d <- extendedres$d[i]
      #### enlver les DRomics::: !!!!!!!!!!!!!!!!!!!!!
      curves2plot$y[(i-1)*npoints + 1:npoints] <- DRomics:::flin(x2plot, b = extendedres$b[i], 
                             d = extendedres$d[i]) 
    } else
      if (modeli == "exponential")
      {
        curves2plot$y[(i-1)*npoints + 1:npoints] <- DRomics:::fExpo(x2plot, b = extendedres$b[i], 
                       d = extendedres$d[i], e = extendedres$e[i]) 
      } else
        if (modeli == "Hill")
        {
          curves2plot$y[(i-1)*npoints + 1:npoints] <- DRomics:::fHill(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i]) 
        } else
          if (modeli == "Gauss-probit")
          {
            curves2plot$y[(i-1)*npoints + 1:npoints] <- DRomics:::fGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                       d = extendedres$d[i], e = extendedres$e[i], 
                       f = extendedres$f[i]) 
          } else
            if (modeli == "log-Gauss-probit")
            {
              curves2plot$y[(i-1)*npoints + 1:npoints] <- DRomics:::fLGauss5p(x2plot, b = extendedres$b[i], c = extendedres$c[i],
                      d = extendedres$d[i], e = extendedres$e[i], 
                      f = extendedres$f[i]) 
            }
  }
  
  # function to calculate the pairwise corr summary per group (facetby)
  # from a dataframe with column x, id, y, facetby
  similaritycalc.1G <- function(d1G)
  {
    m1G <- unstack(d1G, select = y, y ~ facetby)
  }
  
  
  return(curves2plot)
  
  #   gg <- ggplot(data = curves2plot, mapping = aes_(x = quote(x), y = quote(y), group = quote(id))) +
  #     geom_line(size = line.size, alpha = line.alpha) 
  # 
  # if (!missing(facetby))
  # {
  #   if (!missing(facetby2)) 
  #   {
  #     gg <- gg + facet_grid(facetby2 ~ facetby,) 
  #   }
  #   else
  #   {
  #     if (missing(ncol4faceting))
  #     {
  #       gg <- gg + facet_wrap(~ facetby) 
  #     } else
  #     {
  #       gg <- gg + facet_wrap(~ facetby, scales = scales.arg, ncol = ncol4faceting) 
  #     }
  #   }
  # }
  
  
}

#### trial
# (1) An example from data published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# a dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
str(res)

# a dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
# bootstrap results and annotation
extendedres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(extendedres)

s1 <- similaritycalc(extendedres, facetby = "path_class", npoints = 10, 
           xmin = 0, xmax = 8) 
str(s1)
head(s1, 20)

!!!!!!!!!!!!!!!!! J'EN SUIS LA !!!!!!!!!!!!!!!!!!
d1G <- s1[s1$facetby == "Amino acid metabolism", ]
head(d1G)
m1G <- as.matrix(unstack(data.frame(y = d1G$y, id = factor(d1G$id)), y ~ id))
m1G 
(cormatrix <- abs(cor(m1G)))

# (2) 
# An example with two molecular levels
#
### Rename metabolomic results
metabextendedres <- extendedres

# Import the dataframe with transcriptomic results 
contigresfilename <- system.file("extdata", "triclosanSVcontigres.txt", package = "DRomics")
contigres <- read.table(contigresfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigres)

# Import the dataframe with functional annotation (or any other descriptor/category 
# you want to use, here KEGG pathway classes) 
contigannotfilename <- system.file("extdata", "triclosanSVcontigannot.txt", package = "DRomics")
contigannot <- read.table(contigannotfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigannot)

# Merging of both previous dataframes   
contigextendedres <- merge(x = contigres, y = contigannot, by.x = "id", by.y = "contig")
# to see the structure of this dataframe
str(contigextendedres)

### Merge metabolomic and transcriptomic results
extendedres2 <- rbind(metabextendedres, contigextendedres)
extendedres2$molecular.level <- factor(c(rep("metabolites", nrow(metabextendedres)),
                                        rep("contigs", nrow(contigextendedres))))
str(extendedres2)

### Plot of 25th quantiles of BMD-zSD calculated by pathway
### and colored by molecular level
# optional inverse alphabetic ordering of groups for the plot
extendedres2$path_class <- factor(extendedres2$path_class, 
                                 levels = sort(levels(extendedres2$path_class), 
                                               decreasing = TRUE))

s2 <- similaritycalc(extendedres2, facetby = "path_class", 
                     facetby2 = "molecular.level", 
                     npoints = 10, 
                     xmin = 0, xmax = 8) 
str(s2)
nrow(s2)
head(s2, 20)
