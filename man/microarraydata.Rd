\name{microarraydata}
\alias{microarraydata}
\alias{omicdata}
\alias{print.microarraydata}
\alias{plot.microarraydata}
\title{Import, check and normalization of single-channel microarray data}

\description{
Single-channel microarray data in log2 are imported from a .txt file, checked (see the description 
of argument \code{file} for the required format
of data) and normalized (between arrays
normalization).
\code{omicdata} is a deprecated version of \code{microarraydata}.
}

\usage{
microarraydata(file, check = TRUE, 
  norm.method = c("cyclicloess", "quantile", "scale", "none"))
  
omicdata(file, check = TRUE, 
  norm.method = c("cyclicloess", "quantile", "scale", "none"))

\method{print}{microarraydata}(x, \dots)
\method{plot}{microarraydata}(x, \dots)
}

\arguments{
\item{file}{The name of the .txt file containing one row per item, with the first column corresponding to the identifier of each item, in a column named 
\code{"item"}, and the other columns giving the responses
of the item for each replicate at each dose or concentration. 
The names of the corresponding columns must correspond to the tested
doses or concentrations in a numeric format for the corresponding replicate (for example, if there are triplicates for each treatment, column names can be "item", 0, 0, 0, 0.1, 0.1, 0.1, etc.}

\item{check}{If TRUE the format of the input file is checked.}

\item{norm.method}{If \code{"none"} no normalization is performed, else a 
normalization is performed using the function normalizeBetweenArrays of the 
\code{limma} package using the specified method.}

\item{x}{An object of class \code{"microarraydata"}.}

\item{\dots}{ further arguments passed to print or plot functions.}
}

\details{
This function imports the data from a .txt file, then checks the format of 
data (see the description 
of argument \code{file} for the required format
of data) and gives in the \code{print} information
that should help the user to check that the coding of data is correct : the tested doses (or concentrations) 
the number of replicates for each dose, the number of items, the identifiers 
of the first 20 items. If the argument \code{norm.method} is not \code{"none"},
data are normalized using the function \code{\link{normalizeBetweenArrays}} of the 
\code{limma} package using the specified method : \code{"cyclicloess"} (default choice), 
\code{"quantile"} or \code{"scale"}.
}

\value{ 
   \code{microarraydata} returns an object of class "microarraydata", a list with 7 components:
    \item{data}{ the numeric matrix of normalized responses of each item in each replicate
  (one line per item, one column per replicate)  }

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

    \item{data.beforenorm}{ the numeric matrix of responses of each item in each replicate
  (one line per item, one column per replicate) before normalization  }

  The print of a \code{microarraydata} object gives the tested doses (or concentrations) 
  and number of replicates for each dose, the number of items, the identifiers 
  of the first 20 items (for check of good coding of data) and the normalization method.
   The plot of a \code{microarraydata} object shows the data distribution for each dose or concentration and replicate before and after normalization.
}

\seealso{
    See \code{\link{normalizeBetweenArrays}} for details about the normalization and 
    \code{\link{RNAseqdata}} and \code{\link{metabolomicdata}} for other types of data.}

\references{ 
Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W, and Smyth, GK (2015), \emph{limma powers differential expression analyses for RNA-sequencing and microarray studies}. Nucleic Acids Research 43, e47.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
# (1) import, check and normalization of microarray data 
# (an example on a subsample of a greater data set)
#
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

o <- microarraydata(datatxt, check = TRUE, norm.method = "cyclicloess")
print(o)
plot(o)

# (2) normalization with other methods
(o.2 <- microarraydata(datatxt, check = TRUE, norm.method = "quantile"))
plot(o.2)
(o.3 <- microarraydata(datatxt, check = TRUE, norm.method = "scale"))
plot(o.3)

}
