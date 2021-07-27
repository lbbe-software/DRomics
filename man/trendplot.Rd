\name{trendplot}
\alias{trendplot}
\title{Plot of the repartition of trends per group}

\description{
Provides a plot of the repartition of dose-response trends per group
of items.

}

\usage{
trendplot(extendedres, group, facetby, ncol4faceting, add.color = TRUE)
}

\arguments{
\item{extendedres}{the dataframe of results provided by drcfit (fitres)
or bmdcalc (res) 
 or a subset of this data frame (selected lines). This dataframe should be extended 
 with additional columns coming for the group (for example from the functional
 annotation of items) and/or for another level (for example the molecular level),
 and some lines 
 can be replicated if their corresponding item has more than one annotation. 
 This extended dataframe
 must at least contain as results of the dose-response modelling
 the column giving the trend (\code{trend}.}

\item{group}{The name of the column of extendedres coding for the groups
on which we want to see the repartition of dose-response trends. 
This column should be a factor ordered as you want the groups to appear
in the plot from bottom up.}

\item{facetby}{optional argument naming the column of \code{extendedres} chosen 
  to split the plot in facets using \code{ggplot2::facet_wrap} (no split if omitted).}

\item{ncol4faceting}{Number of columns for facetting.}

\item{add.color}{A color is added coding for the trend if TRUE.}

}


\value{ 
   a ggplot object.
}

\seealso{
    See \code{\link{bmdplotwithgradient}} and \code{\link{curvesplot}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1)

# An example from the paper published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# the dataframe with metabolomic results 
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


# (1.a) Trendplot by pathway
trendplot(extendedres, group = "path_class") 

\donttest{

# (1.b) Trendplot by pathway without color
trendplot(extendedres, group = "path_class", add.color = FALSE) 

# (1.c) Reordering of the groups before plotting
extendedres$path_class <- factor(extendedres$path_class, 
                levels = sort(levels(extendedres$path_class), decreasing = TRUE))
trendplot(extendedres, group = "path_class", add.color = FALSE) 

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

### trend plot of both molecular levels
# optional inverse alphabetic ordering of groups for the plot
extendedres$path_class <- factor(extendedres$path_class, 
                levels = sort(levels(extendedres$path_class), decreasing = TRUE))
trendplot(extendedres, group = "path_class", facetby = "molecular.level") 


}

}
