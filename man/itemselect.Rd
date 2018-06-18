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
the function \code{omicdata}.}

\item{select.method}{The chosen method for selecting items using the \code{limma} package : \code{"quadratic"} for a 
quadratic trend test on dose ranks, \code{"linear"} for a linear trend test on dose ranks and \code{"ANOVA"} for 
an ANOVA-type test (see details for further explaination).}

\item{FDR}{The threshold in term of FDR (False Discovery Rate) for selecting responsive items.}

\item{x}{An object of class \code{"itemselect"}.}

\item{\dots}{ further arguments passed to print function.}
}

\details{
The selection of responsive items is performed using the \code{limma} package. 
Three methods are proposed, all implemented using functions \code{\link{lmFit}},  
\code{\link{eBayes}} and \code{\link{topTable}} with p-values ajusted for multiple 
testing using the Benjamini-Hochberg method, with the false discovery rate given in
input (argument \code{FDR}). 

 \itemize{
\item The ANOVA_based test (\code{"ANOVA"}) is classically used for 
selection of omics data in the general case but it requires many replicates per dose
to be efficient, and is thus not really suited for a dose-response design.

\item The linear trend test (\code{"linear"}) aims at detecting monotonic trends from dose-response designs,
whatever the number of replicates per dose. 
As proposed by Tukey (1985), it tests the global significance of
a linear model describing the response as a function of the dose in rank-scale.

\item The quadratic trend test (\code{"quadratic"})  
tests the global significance of a quadratic model describing the response as a function of the dose in rank-scale.
It is a variant of the linear trend method that aims at detecting monotonic and non monotonic trends from a dose-response designs, whatever the number of replicates per dose (default chosen method).
 }
 
}

\value{ 
   \code{itemselect} returns an object of class \code{"itemselect"}, a list with 5 components:
    
  \item{ adjpvalue }{ the vector of the p-values adjusted by the Benjamini-Hochberg method for selected items 
(adjpvalue inferior to FDR) sorted in ascending order}

  \item{ selectindex }{ the corresponding vector of row indices of selected items in the object omicdata }

  \item{omicdata}{The corresponding object of class "omicdata" given in input.}

  \item{select.method}{The selection method given in input.}

  \item{FDR}{The threshold in term of FDR given in input.}

  The print of a \code{"itemselect"} object gives the number of selected items and the 
  identifiers of the 20 most responsive items.
}

\seealso{
    See \code{\link{lmFit}}, \code{\link{eBayes}} and \code{\link{topTable}}
    for details about the used functions of the \code{limma} package.
}

\references{ 
Tukey JW, Ciminera JL and Heyse JF (1985), \emph{Testing the statistical certainty of a response to increasing doses of a drug}. Biometrics, 295-301.


Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W, and Smyth, GK (2015), \emph{limma powers differential expression analyses for RNA-sequencing and microarray studies}. Nucleic Acids Research 43, e47.

}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{
\dontrun{

# (1) an example on a transcriptomics data (a subsample of a greater data set) 
#     
datatxt <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- omicdata(datatxt, check = TRUE, norm.method = "cyclicloess"))

# 1.a using the quadratic trend test
#
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))

# 1.b using the linear trend test
#
(s_lin <- itemselect(o, select.method = "linear", FDR = 0.05))

# 1.c using the ANOVA-based test
#
(s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.05))

# 1.d using the quadratic trend test with a smaller false discovery rate
#
(s_quad.2 <- itemselect(o, select.method = "quadratic", FDR = 0.001))

}
}
