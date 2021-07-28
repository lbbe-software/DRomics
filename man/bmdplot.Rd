\name{bmdplot}
\alias{bmdplot}
\title{BMD plot optionally with confidence intervals on BMD}

\description{
Provides an ECDF plot of BMD values optionally with confidence intervals on each BMD value
and/or labels of items.
}

\usage{
bmdplot(extendedres, BMDtype = c("zSD", "xfold"),
                                  add.CI = FALSE, 
                                   facetby, facetby2, 
                                   shapeby,  colorby,
                                   point.size = 1,
                                   ncol4faceting, 
                                   add.label = FALSE, label.size = 2,
                                    BMD_log_transfo = FALSE)
}

\arguments{
\item{extendedres}{the dataframe of results provided by bmdcalc (res) 
 or a subset of this data frame (selected lines). This dataframe can be extended 
 with additional columns coming for example from the functional annotation of items, and some lines 
 can be replicated if their corresponding item has more than one annotation. 
 This extended dataframe
 must at least contain the column giving the BMD values (\code{BMD.zSD} or \code{BMD.xfold}
 depending of chosen BMDtype), identification of each curve (\code{id}),
 and if \code{add.CI} is TRUE,
 the columns \code{BMD.zSD.lower}, \code{BMD.zSD.upper} or
 \code{BMD.xfold.lower}, \code{BMD.xfold.upper} depending of the argument \code{BMDtype}.}

\item{BMDtype}{The type of BMD to plot, \code{"zSD"} (default choice) or \code{"xfold"}.}

\item{add.CI}{If \code{TRUE} (default choice at FALSE) for each item the confidence interval is added.}

\item{facetby}{optional argument naming the column of \code{extendedres} chosen 
  to split the plot in facets using \code{ggplot2::facet_wrap} (no split if omitted).}

\item{facetby2}{optional argument naming the column of \code{extendedres} chosen 
  as an additional argument to split the plot in facets using 
  \code{ggplot2::facet_grid}, with columns defined by \code{facetby}
  and rows defined by \code{facetby2} (no split if omitted).}

\item{shapeby}{optional argument naming the column of \code{extendedres} chosen 
  to shape the BMD points (no difference if shapeby if omitted).}

\item{colorby}{optional argument naming the column of \code{extendedres} chosen 
  to color the BMD points (no difference if colorby if omitted).}
  
\item{point.size}{Size of the BMD points.}

\item{ncol4faceting}{Number of columns for facetting (not used if \code{facetby2} 
  is also provided.}

\item{add.label}{Points are replaced  by labels of items if TRUE.}

\item{label.size}{Size of labels if add.label is TRUE.}

\item{BMD_log_transfo}{If TRUE a log transformation of the BMD is used in the plot. 
  This option cannot be used with a null value of xmin in input.}


}

\details{
BMD values are plotted as an ECDF plot, as with \code{\link{plot.bmdcalc}}
using "ecdf" as plottype with confidence intervals on each BMD value
and/or labels of items if requested. The optional use of columns to code for shape and/or 
facets for each item is particularly intended to give a view of all the dose-response 
per group (e.g. metabolic pathways). Those groups must be coded in a column
of extendedres. In case where one item is allocated to more than one group
during the annotation process, the line of this item must be replicated in 
extendedres as many times as the number of annotation groups in which it was
allocated.

}

\value{ 
   a ggplot object.
}

\seealso{
    See \code{\link{plot.bmdcalc}}, \code{\link{plot.bmdboot}} and 
    \code{\link{ecdfplotwithCI}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1)
# Plot of BMD values with color dose-response gradient
# faceted by metabolic pathway (from annotation of the selected items)
# and shaped by dose-response trend

# An example from the paper published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727
# A example of plot obtained with this function is in Figure 5 in Larras et al. 2020

# the dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
str(res)

# the dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
extendedres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(extendedres)


### (1.a) BMDplot by pathway shaped by trend
bmdplot(extendedres, BMDtype = "zSD",
                     facetby = "path_class", 
                       shapeby = "trend") 

\donttest{

### (1.b) BMDplot by pathway with items labels
bmdplot(extendedres, BMDtype = "zSD",
                     facetby = "path_class", 
                       add.label = TRUE,
                       label.size = 1.5) 

### (1.c) BMDplot by pathway with confidence intervals
bmdplot(extendedres, BMDtype = "zSD",
                     facetby = "path_class", 
                       add.CI = TRUE) 

### (1.d) BMDplot by pathway with confidence intervals 
#         in BMD log scale
bmdplot(extendedres, BMDtype = "zSD",
                     facetby = "path_class",
                     add.CI = TRUE,
                     BMD_log_transfo = TRUE) 

### (1.e) BMDplot by pathway with confidence intervals 
#         colored by trend
bmdplot(extendedres, BMDtype = "zSD",
                     facetby = "path_class",
                     add.CI = TRUE,
                     colorby = "trend",
                     BMD_log_transfo = TRUE) 


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
extendedres <- rbind(metabextendedres, contigextendedres)
extendedres$molecular.level <- factor(c(rep("metabolites", nrow(metabextendedres)),
                              rep("contigs", nrow(contigextendedres))))
str(extendedres)

### BMD plot per pathway with molecular level coding for color
bmdplot(extendedres, BMDtype = "zSD",
                     facetby = "path_class",
                     colorby = "molecular.level") 

### BMD plot per pathway and per molecular level 
# for a selection of pathways
chosen_path_class <- c("Membrane transport", "Lipid metabolism")
ischosen <- is.element(extendedres$path_class, chosen_path_class)
bmdplot(extendedres[ischosen, ], BMDtype = "zSD",
                     facetby = "path_class",
                     facetby2 = "molecular.level",
                     colorby = "trend",
                     point.size = 2,
                     add.CI = TRUE) 


}

}
