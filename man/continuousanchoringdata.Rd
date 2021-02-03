\name{continuousanchoringdata}
\alias{continuousanchoringdata}
\alias{print.continuousanchoringdata}
\alias{plot.continuousanchoringdata}
\title{Import and check of continuous anchoring apical data}

\description{
Continuous anchoring apical data are imported from a .txt file 
(internally imported using the function \code{\link{read.table}})
and checked or from a R object of class \code{data.frame}
(see the description 
of argument \code{file} for the required format
of data). 
No transformation is provided in this function.
If needed the pretreatment of data must be done before importation of data, 
so that they can be directly modelled using a normal 
error model. This strong hypothesis is required both for selection of responsive endpoints
and for dose-reponse modelling.

}

\usage{
continuousanchoringdata(file, check = TRUE)

\method{print}{continuousanchoringdata}(x, \dots)
\method{plot}{continuousanchoringdata}(x, \dots)
}

\arguments{
\item{file}{The name of the .txt file (e.g. \code{"mydata.txt"})
containing one row per endpoint, with the first column corresponding to the identifier of each endpoint, 
and the other columns giving the measured values
of the endpoint for each replicate at each dose or concentration. 
In the first line, after a name for the endpoint column, we must have the tested
doses or concentrations in a numeric format for the corresponding replicate 
(for example, if there are triplicates for each treatment, 
the first line could be "endpoint", 0, 0, 0, 0.1, 0.1, 0.1, etc.). 
This file is imported within the function using the function
\code{\link{read.table}} with its default field separator (sep argument).
Alternatively an R object of class \code{data.frame} can be directly given in input, 
corresponding to the output
of \code{read.table(file, header = FALSE)} on a file described as above. The two
alternatives are illustrated below in examples.}

\item{check}{If TRUE the format of the input file is checked.}

\item{x}{An object of class \code{"continuousanchoringdata"}.}

\item{\dots}{ further arguments passed to print or plot functions.}
}

\details{
This function imports the data, checks their format  
(see the description 
of argument \code{file} for the required format
of data) and gives in the \code{print} information
that should help the user to check that the coding of data is correct : the tested doses (or concentrations) 
the number of replicates for each dose, the number of endpoints. 
}

\value{ 
   \code{continuousanchoringdata} returns an object of class "continuousanchoringdata", a list with 5 components:
    \item{data}{ the numeric matrix of responses of each item in each replicate
  (one line per item, one column per replicate)  }

    \item{dose}{ the numeric vector of the tested doses or concentrations corresponding
    to each column of data}

    \item{item}{ the character vector of the identifiers of the endpoints, corresponding to
    each line of data}

    \item{design}{ a table with the experimental design (tested doses and number of 
    replicates for each dose) for control by the user}

    \item{data.mean}{ the numeric matrix of mean responses of each item per dose (mean 
    of the corresponding replicates) (one line per item, one column per unique value 
    of the dose}
    
    \item{containsNA}{ TRUE if the data set contains NA values}

  The print of a \code{continuousanchoringdata} object gives the tested doses (or concentrations) 
  and number of replicates for each dose, the number of items, the identifiers 
  of the first 20 items (for check of good coding of data) and the normalization method.
   The plot of a \code{continuousanchoringdata} object shows the data distribution for each dose or concentration and replicate.
}

\seealso{
    See  \code{\link{read.table}} the function used to import data, and
    \code{\link{microarraydata}}, \code{\link{RNAseqdata}} and \code{\link{continuousomicdata}} for other types of data.}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
# (1) import and check of continuous anchoring data 
# (an example with two apical endpoints of an example given in the package (see ?Scenedesmus))
#
datafilename <- system.file("extdata", "apical_anchoring.txt", package = "DRomics")

o <- continuousanchoringdata(datafilename, check = TRUE)
print(o)
\donttest{
plot(o)
}
# If you want to use your own data set just replace datafilename, 
# the first argument of continuousanchoringdata(), 
# by the name of your data file (e.g. "mydata.txt")
# 
# You should take care that the field separator of this data file is one
# of the default field separators recognised by the read.table() function
# when it is used with its default field separator (sep argument)
# Tabs are recommended.

# Use of an R object of class data.frame
# on the same example (see ?Scenedesmus for details)
data(Scenedesmus_apical)
o <- continuousanchoringdata(Scenedesmus_apical)
print(o)
\donttest{
plot(o)
}


}
