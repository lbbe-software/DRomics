\name{metabolomicdata}
\alias{metabolomicdata}
\alias{print.metabolomicdata}
\alias{plot.metabolomicdata}
\title{Import and check of metabolomic data}

\description{
Metabolomic data are imported from a .txt file 
(internally imported using the function \code{\link{read.table}})
and checked (see the description 
of argument \code{file} for the required format
of data). No normalization nor transformation is provided in this function.
The pretreatment of metabolomic data must be done before importation of data, 
and data must be imported in log scale,
so that they can be directly modelled using a Gaussian 
error model. This strong hypothesis is required both for selection of items and for dose-reponse modelling.
A basic procedure for this pre-treatment of metabolomic data could follow
the three steps described thereafter:
i) removing of metabolites for which the proportion of missing data 
  (non detections) across all the samples is too high 
  (more than 20 to 50 percents according to your tolerance level);
ii) retrieving of missing values data using half minimum method 
  (i.e. half of the minimum value found for a metabolite across all samples);
iii) log-transformation of values.
If a scaling to the total intensity (normalization by sum of signals 
in each sample) or another normalization is necessary and pertinent, 
we recommend to do it before those three previously decribed steps.


}

\usage{
metabolomicdata(file, check = TRUE)

\method{print}{metabolomicdata}(x, \dots)
\method{plot}{metabolomicdata}(x, \dots)
}

\arguments{
\item{file}{The name of the .txt file (e.g. \code{"mydata.txt"})
containing one row per item, with the first column corresponding to the identifier of each item, in a column named 
\code{"item"}, and the other columns giving the responses
of the item for each replicate at each dose or concentration. 
The names of the corresponding columns must correspond to the tested
doses or concentrations in a numeric format for the corresponding replicate (for example, if there are triplicates for each treatment, column names can be "item", 0, 0, 0, 0.1, 0.1, 0.1, etc.}

\item{check}{If TRUE the format of the input file is checked.}

\item{x}{An object of class \code{"metabolomic"}.}

\item{\dots}{ further arguments passed to print or plot functions.}
}

\details{
This function imports the data from a .txt file, using the function
\code{\link{read.table}} with its default field separator (sep argument),
then checks the format of 
data (see the description 
of argument \code{file} for the required format
of data) and gives in the \code{print} information
that should help the user to check that the coding of data is correct : the tested doses (or concentrations) 
the number of replicates for each dose, the number of items, the identifiers 
of the first 20 items. 
}

\value{ 
   \code{metabolomicdata} returns an object of class "metabolomicdata", a list with 5 components:
    \item{data}{ the numeric matrix of responses of each item in each replicate
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

  The print of a \code{metabolomic} object gives the tested doses (or concentrations) 
  and number of replicates for each dose, the number of items, the identifiers 
  of the first 20 items (for check of good coding of data) and the normalization method.
   The plot of a \code{metabolomic} object shows the data distribution for each dose or concentration and replicate.
}

\seealso{
    See  \code{\link{read.table}} the function used to import data, and
    \code{\link{RNAseqdata}} and \code{\link{microarraydata}} for other types of data.}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
# (1) import and check of metabolomic data 
# (an example on a subsample of a greater data set)
#
datafilename <- system.file("extdata", "metabolo_sample.txt", package="DRomics")

o <- metabolomicdata(datafilename, check = TRUE)
print(o)
plot(o)

# If you want to use your own data set just replace datafilename, 
# the first argument of metabolomicdata(), 
# by the name of your data file (e.g. "mydata.txt")
# 
# You should take care that the field separator of this data file is one
# of the default field separators recognised by the read.table() function
# when it is used with its default field separator (sep argument)
# Tabs are recommended.

}
