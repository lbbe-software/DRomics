\name{RNAseqdata}
\alias{RNAseqdata}
\alias{print.RNAseqdata}
\alias{plot.RNAseqdata}
\title{Import, check and normalization and transformation of RNAseq data}

\description{
RNAseq data in raw counts are imported from a .txt file, checked (see the description 
of argument \code{file} for the required format
of data), normalized with respect to library size and tranformed in 
a log2 scale using
variance stabilizing transformation or regularized logarithm.
}

\usage{
RNAseqdata(file, check = TRUE, transfo.method = c("rlog", "vst"))

\method{print}{RNAseqdata}(x, \dots)
\method{plot}{RNAseqdata}(x, \dots)
}

\arguments{
\item{file}{The name of the .txt file containing one row per item, with the first column corresponding to the identifier of each item, in a column named 
\code{"item"}, and the other columns giving the responses
of the item for each replicate at each dose or concentration. 
The names of the corresponding columns must correspond to the tested
doses or concentrations in a numeric format for the corresponding replicate (for example, if there are triplicates for each treatment, column names can be "item", 0, 0, 0, 0.1, 0.1, 0.1, etc.}

\item{check}{If TRUE the format of the input file is checked.}

\item{transfo.method}{The method chosen to transform raw counts in 
pseudocounts using the
  \code{DESeq2}: \code{"rlog"} for regularized logarithm or
  \code{"vst"} for variance stabilizing transformation.}

\item{x}{An object of class \code{"RNAseqdata"}.}

\item{\dots}{ further arguments passed to print or plot functions.}
}

\details{
This function imports the data from a .txt file, then checks the format of 
data (see the description 
of argument \code{file} for the required format
of data) and gives in the \code{print} information
that should help the user to check that the coding of data is correct : the tested doses (or concentrations) 
the number of replicates for each dose, the number of items, the identifiers 
of the first 20 items. Data are normalized with respect to library size
and tranformed using functions \code{\link{rlog}} or \code{\link{vst}} of the 
\code{DESeq2} package depending on the specified method : \code{"rlog"} 
(recommended default choice) or 
\code{"vst"}.
}

\value{ 
   \code{RNAseqdata} returns an object of class "RNAseqdata", a list with 7 components:
    \item{data}{ the numeric matrix of normalized and transformed
    responses of each item in each replicate
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

    \item{transfo.method}{The transformation method specified in input}

    \item{raw.counts}{ the numeric matrix of non transformed responses (raw counts)
    of each item in each replicate
  (one line per item, one column per replicate) before normalization  }

  The print of a \code{RNAseqdata} object gives the tested doses (or concentrations) 
  and number of replicates for each dose, the number of items, the identifiers 
  of the first 20 items (for check of good coding of data) and the tranformation method.
   The plot of a \code{RNAseqdata} object shows the data distribution for each dose or concentration and replicate before and after normalization and tranformation.
}

\seealso{
    See \code{\link{rlog}} and \code{\link{vst}} for details about the 
    transformation methods and 
    \code{\link{microarraydata}} and \code{\link{metabolomicdata}} for other types of data.
}

\references{ 
Love MI, Huber W, and Anders S (2014), \emph{Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2}. Genome biology, 15(12), 550.}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
# (1) import, check, normalization and transformation of RNAseq data 
# An example on a subsample of a data set published by Zhou et al. 2017
# (in Toxicological sciences, 160, 95-110)
# Effect on kidney transcriptomes of tetrachloroethylene
#
datatxt <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
(o <- RNAseqdata(datatxt, check = TRUE, transfo.method = "rlog"))
plot(o)

# (2) transformation with two methods on the whole data set
\donttest{
datatxt <- system.file("extdata", "Zhou_kidney_pce.txt", package="DRomics")

# variance stabilizing tranformation
(o1 <- RNAseqdata(datatxt, check = TRUE, transfo.method = "vst"))
plot(o1)

# regularized logarithm
(o2 <- RNAseqdata(datatxt, check = TRUE, transfo.method = "vst"))
plot(o2)
}

}
