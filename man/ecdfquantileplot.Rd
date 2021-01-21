\name{ecdfquantileplot}
\alias{ecdfquantileplot}
\title{ECDF plot of a given quantile of a variable calculated by group}

\description{
Plots a given quantile of a variable calculated by group as an ECDF plot with points sized by the numbers of items per group.
In the context of this package this function is intended to be
used with the BMD as the variable and with groups defined by the user
from functional annotation.

}

\usage{
ecdfquantileplot(variable, by, quantile.prob = 0.5, title)
}

\arguments{
\item{variable}{A numeric vector corresponding to the variable on which we want to calculate the given quantile by group. In the context of the package this variable
may be a BMD.}

\item{by}{A factor of the same length defining the groups. In the context of this package this factor may code for groups defined by the user from functional annotation.}
  
\item{quantile.prob}{The probability (in ]0, 1[) defining the quantile to calculate on each group.}

\item{title}{An optional title for the plot.}
}

\details{

The given quantile is calculated for each group (e.g.from all items of a metabolic pathway) 
using function \code{\link{quantile}} and plotted as an ECDF plot. In this ECDF plot of quantiles each point is sized according to the number of items in the corresponding group (e.g. metabolic pathway).
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
annotres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(annotres)

### an ECDFplot of quantiles of BMD-zSD calculated by pathway
ecdfquantileplot(variable = annotres$BMD.zSD, 
               by = annotres$path_class,
               quantile.prob = 0.25) 
 
# same plot in log10 dose scale (not interesting on this example
# but could be on another one) 
if (require(ggplot2))
  ecdfquantileplot(variable = annotres$BMD.zSD, 
               by = annotres$path_class,
               quantile.prob = 0.25) + scale_y_log10() 


}


