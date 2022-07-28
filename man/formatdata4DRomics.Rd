\name{formatdata4DRomics}
\alias{formatdata4DRomics}
\title{Build an R object that can be used as data input in DRomics}

\description{
Build an R object that can be used as data input in data importation function from
two inputs: the nitems x nsamples matrix coding for the signal and the 
nsamples vector of doses 
}

\usage{
formatdata4DRomics(signalmatrix, dose, samplenames)   
}

\arguments{
\item{signalmatrix}{the matrix of the data with one row for each item and one column
for each sample. Depending of the type of measured signal, look at the help of the corresponding
importation function espacially to check that you use the good scale of data
\code{\link{RNAseqdata}}, \code{\link{microarraydata}}, \code{\link{continuousomicdata}} and
  \code{\link{continuousanchoringdata}}.}

\item{dose}{a numeric vector giving the dose for each sample.}

\item{samplenames}{a character vector giving the names of the samples 
(optional argument - if not given, the row names of signalmatrix are taken as sample names).}
}

\value{ 
   an R object that corresponds to a dataframe that can be passed as input in the 
   first argument of the data importation functions \code{\link{RNAseqdata}}, 
   \code{\link{microarraydata}}, \code{\link{continuousomicdata}} or
  \code{\link{continuousanchoringdata}}.
}

\seealso{
    See \code{\link{RNAseqdata}}, \code{\link{microarraydata}},
    \code{\link{continuousomicdata}} and
    \code{\link{continuousanchoringdata}} especially for specification of
    the required scale of data in each case.}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
# (1) load of data
#
data(zebraf)
str(zebraf)

# (2) formating of data for use in DRomics
# 
data4DRomics <- formatdata4DRomics(signalmatrix = zebraf$counts, 
                           dose = zebraf$dose)
\donttest{
# (3) Normalization and transformation of data 
#
o <- RNAseqdata(data4DRomics)
plot(o)
}

}
