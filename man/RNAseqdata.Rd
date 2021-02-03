\name{RNAseqdata}
\alias{RNAseqdata}
\alias{print.RNAseqdata}
\alias{plot.RNAseqdata}
\title{Import, check and normalization and transformation of RNAseq data}

\description{
RNAseq data in raw counts (integer values) are imported from a .txt file 
(internally imported using the function \code{\link{read.table}}), 
checked or from a R object of class \code{data.frame} 
(see the description 
of argument \code{file} for the required format
of data), 
normalized with respect to library size and tranformed in 
a log2 scale using
variance stabilizing transformation or regularized logarithm.
}

\usage{
RNAseqdata(file, check = TRUE, transfo.method = c("rlog", "vst"), 
          transfo.blind = TRUE, round.counts = FALSE)

\method{print}{RNAseqdata}(x, \dots)
\method{plot}{RNAseqdata}(x, \dots)
}

\arguments{
\item{file}{The name of the .txt file (e.g. \code{"mydata.txt"})
containing one row per item, with the first column corresponding to the identifier of each item, 
and the other columns giving the responses
of the item for each replicate at each dose or concentration. 
In the first line, after a name for the identifier column, we must have the tested
doses or concentrations in a numeric format for the corresponding replicate 
(for example, if there are triplicates for each treatment, 
the first line could be "item", 0, 0, 0, 0.1, 0.1, 0.1, etc.). 
This file is imported within the function using the function
\code{\link{read.table}} with its default field separator (sep argument).
Alternatively an R object of class \code{data.frame} can be directly given in input, 
corresponding to the output
of \code{read.table(file, header = FALSE)} on a file described as above. The two
alternatives are illustrated below in examples.}

\item{check}{If TRUE the format of the input file is checked.}

\item{transfo.method}{The method chosen to transform raw counts in a log2 scale
using the
  \code{DESeq2}: \code{"rlog"} for regularized logarithm or
  \code{"vst"} for variance stabilizing transformation.}

\item{transfo.blind}{Argument given to function \code{\link{rlog}} or \code{\link{vst}},
  see \code{\link{rlog}} and \code{\link{vst}} for an explaination, by default
  at \code{TRUE} as in the \code{DESeq2} package .}

\item{round.counts}{Put it to TRUE if your counts come from Kallisto or Salmon
  in order to round them before treatment with \code{DESeq2}.}


\item{x}{An object of class \code{"RNAseqdata"}.}


\item{\dots}{ further arguments passed to print or plot functions.}
}

\details{
This function imports the data, checks their format (see the description 
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

    \item{containsNA}{always at FALSE as RNAseq data are not allowed to contain 
    NA values}

  The print of a \code{RNAseqdata} object gives the tested doses (or concentrations) 
  and number of replicates for each dose, the number of items, the identifiers 
  of the first 20 items (for check of good coding of data) and the tranformation method.
   The plot of a \code{RNAseqdata} object shows the data distribution for each dose or concentration and replicate before and after normalization and tranformation.
}

\seealso{
    See \code{\link{read.table}} the function used to import data,
    \code{\link{rlog}} and \code{\link{vst}} for details about the 
    transformation methods and 
    \code{\link{microarraydata}}, \code{\link{continuousomicdata}} and
    \code{\link{continuousanchoringdata}} for other types of data.
}

\references{ 
Love MI, Huber W, and Anders S (2014), \emph{Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2}. Genome biology, 15(12), 550.}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
# (1) import, check, normalization and transformation of RNAseq data 
# An example on a subsample of a data set published by Zhou et al. 2017
# Effect on mouse kidney transcriptomes of tetrachloroethylene
# (see ? Zhou for details)
#
datafilename <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
(o <- RNAseqdata(datafilename, check = TRUE, transfo.method = "vst"))
plot(o, range = 1e6)

# If you want to use your own data set just replace datafilename, 
# the first argument of RNAseqdata(), 
# by the name of your data file (e.g. "mydata.txt")
# 
# You should take care that the field separator of this data file is one
# of the default field separators recognised by the read.table() function
# when it is used with its default field separator (sep argument)
# Tabs are recommended.

# Use of an R object of class data.frame
# below the same example taking a subsample of the data set
# Zhou_kidney_pce (see ?Zhou for details)
data(Zhou_kidney_pce)
subsample <- Zhou_kidney_pce[1:1000, ]
(o <- RNAseqdata(subsample, check = TRUE, transfo.method = "vst"))
plot(o, range = 1e6)


# (2) transformation with two methods on the whole data set
\donttest{
data(Zhou_kidney_pce)

# variance stabilizing tranformation
(o1 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst"))
plot(o1)

# regularized logarithm
(o2 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog"))
plot(o2)

# variance stabilizing tranformation (blind to the experimental design)
(o3 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst",
      transfo.blind = TRUE))
plot(o3)

# regularized logarithm
(o4 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog",
      transfo.blind = TRUE))
plot(o4)


}

}
