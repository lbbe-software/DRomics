\name{omicdata}
\alias{omicdata}
\alias{print.omicdata}
\title{Import, check and normalization of omic data}

\description{
Data are imported from a .txt file, checked (see the description 
of argument \code{file} for the required format
of data) and normalized if required.
}

\usage{
omicdata(file, check = TRUE, norm.method = c("none", "cyclicloess", "quantile", "scale"))

\method{print}{omicdata}(x, \dots)
}

\arguments{
\item{file}{The name of the .txt file containing one raw per item, with the the 
first column corresponding to the identifier of each item, in a column named 
\code{"item"}, and the other columns giving the responses
of the item for each replicate at each dose or concentration. 
The names of the corresponding columns must correspond to the tested
doses or concentrations in a numeric format for the corresponding replicate.}

\item{check}{If TRUE the format of the input file is checked.}

\item{norm.method}{If \code{"none"} no normalization is performed, else a 
normalization is performed using the function normalizeBetweenArrays of the 
\code{limma} package using the specified method.}

\item{x}{An object of class \code{"omicdata"}.}

\item{\dots}{ further arguments passed to print functions.}
}

\details{
TO WRITE !
}

\value{ 
   \code{omicdata} returns an object of class "omicdata", a list with 5 components:
    \item{data}{ the numeric matrix of responses of each item in each replicate
  (one line per item, one column per replicate  }

    \item{dose}{ the numeric vector of the tested doses or concentrations corresponding
    to each column of data}

    \item{item}{ the character vector of the identifiers of the items, corresponding to
    each line of data}

    \item{design}{ a table with the experimental design (tested doses and number of 
    replicates for each dose) for control by the user}

    \item{data.mean}{ the numeric matrix of mean responses of each item per dose (mean 
    of the corresponding replicates) (one line per item, one column per unique value 
    of the dose}

    \item{norm.method}{The normalization method specified in input}

  The print of a \code{omicdata} object gives the tested doses (or concentrations) 
  and number of replicates for each dose, the number of items, the identifiers 
  of the first 20 items (for check of good coding of data) and the normalization method.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) import, check and normalization of transcriptomic data 
# (a subsample of a greater data set)
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))

# (2) import of metabolomic data (which has been normalized before)
#
datatxt <- system.file("extdata", "metabolo_norm.txt", package="DRomics")
(o <- omicdata(datatxt, check = TRUE, norm.method = "none"))

}
