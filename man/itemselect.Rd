\name{itemselect}
\alias{itemselect}
\alias{print.itemselect}
\title{Selection of significantly responsive items}

\description{
Significantly responsive items are selected using one of the three proposed methods: 
a quadratic trend test, a linear trend test or an ANOVA-based test.
}

\usage{
itemselect(omicdata, select.method = c("quadratic", "linear", "ANOVA"), FDR = 0.05)   

\method{print}{itemselect}(x, \dots)
}

\arguments{
\item{omicdata}{An object of class \code{"omicdata"} returned by 
the function \code{micdata}.}

\item{select.method}{The chosen method for selecting items using the \code{limma} package : \code{"quadratic"} for a 
quadratic trend test on dose ranks, \code{"linear"} for a linear trend test on dose ranks and \code{"ANOVA"} for 
an ANOVA-type test (see details for further explaination).}

\item{FDR}{The threshold in term of FDR (False Discovery Rate) for selecting responsive items.}

\item{x}{An object of class \code{"itemselect"}.}

\item{\dots}{ further arguments passed to print functions.}
}

\details{
TO WRITE !!!!!!!!!!!!!!!!!!!!
}

\value{ 
   \code{itemselect} returns an object of class \code{"itemselect"}, a list with 5 components:
    
  \item{ adjpvalue }{ the vector of the p-values adjusted by the Benjamini-Hochberg method for selected items 
(adjpvalue inferior to FDR) sorted in ascending order}

  \item{ selectindex }{ the corresponding vector of the raw lines of selected items in the object omicdata }

  \item{omicdata}{The corresponding object of class "omicdata" given in input.}

  \item{select.method}{The selection method given in input.}

  \item{FDR}{The threshold in term of FDR given in input.}

  The print of a \code{"itemselect"} object gives the number of selected items and the 
  identifiers of the 20 most responsive items.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) example on a transcriptomic data (a subsample of a greater data set) 
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))

# 1.a using the quadratic trend test
#
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))

# 1.b using the linear trend test
#
(s_quad <- itemselect(o, select.method = "linear", FDR = 0.05))

# 1.c using the ANOVA-based test
#
(s_quad <- itemselect(o, select.method = "ANOVA", FDR = 0.05))

# (2) example on a metabolomic data (which has been normalized before)
#
datatxt <- system.file("extdata", "metabolo_norm.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "none"))
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))

}
