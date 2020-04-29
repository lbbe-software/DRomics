\name{ecdfquantileplot}
\alias{ecdfquantileplot}
\title{ECDF plot of quantiles of a variable calculated by group}

\description{
Plots the quantiles of a variable calculated by group as an ECDF plot with points sized by the numbers of items per group.
}

\usage{
ecdfquantileplot(variable, by, quantile.prob = 0.5, title)
}

\arguments{
\item{variable}{A numeric vector of the variable on which we want to calculate quantiles by group.}

\item{by}{A factor of the same length definig the groups.}
  
\item{quantile.prob}{The probability (in ]0, 1[) defining the quantile to calculate on each group.}

\item{title}{An optional title for the plot.}
}

\details{

The quantiles are calculated for each group using function \code{\link{quantile}} and plotted as an ECDF plot. In this ECDF plot of quantiles each point is sized according to the number of items in the corresponding group.
}

\value{ 
   a ggplot object.
}

\seealso{
    See \code{\link{quantile}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) An example from data published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# a dataframe with metabolomic results (output $res of bmdcalc() or bmdboot() functions)
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE)
str(res)

# a dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
# bootstrap results and annotation
annotres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(annotres)

### an ECDFplot of quantiles of BMD-zSD calculated by pathway
ecdfquantileplot(variable = annotres$BMD.zSD, 
               by = annotres$path_class,
               quantile.prob = 0.25) 
               
}


