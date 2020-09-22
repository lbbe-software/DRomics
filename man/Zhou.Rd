\name{Zhou}
\alias{Zhou}
\alias{Zhou_kidney_pce}
\alias{Zhou_kidney_tce}
\alias{Zhou_liver_pce}
\alias{Zhou_liver_tce}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Dose-response liver and kidney transcriptomic effect of Trichloroethylene end Tetrachloroethylene in mouse}

\description{
RNAseq data sets for the effect of Trichloroethylene (TCE) and Tetrachloroethylene (PCE) on mouse liver and kidney. Each of those environmental contaminants was administered by gavage in aqueous vehicle to male B6C3F1/J mice, within a dose-reponse design
including five doses plus the control.
}

\usage{
data(Zhou_kidney_pce)
data(Zhou_kidney_tce)
data(Zhou_liver_pce)
data(Zhou_liver_tce)
}
%- maybe also 'usage' for other objects documented here.

\format{ 
\code{Zhou_kidney_pce},  \code{Zhou_kidney_tce}, \code{Zhou_liver_pce}  nd \code{Zhou_liver_tce} each contains one row per transcript, with the first column corresponding to the identifier of each transcript, and the other columns giving the count of reads for each replicate at each dose. In the first line, after the name for the identifier column, we have the tested doses for each corresponding replicate.
}

\source{ 
Zhou, Y. H., Cichocki, J. A., Soldatow, V. Y., Scholl, E. H., Gallins, P. J., Jima, D., ... & Rusyn, I. 2017. Comparative dose-response analysis of liver and kidney transcriptomic effects of trichloroethylene and tetrachloroethylene in B6C3F1 mouse. \emph{Toxicological sciences}, \bold{160(1)}, 95-110.
}

%\references{  }

\examples{
# (1) load of data
#
data(Zhou_kidney_pce)
head(Zhou_kidney_pce)
str(Zhou_kidney_pce)

data(Zhou_kidney_tce)
head(Zhou_kidney_tce)

data(Zhou_liver_pce)
head(Zhou_liver_pce)

data(Zhou_liver_tce)
head(Zhou_liver_tce)

\donttest{

# (2) import, check, normalization and transformation of a sample
# of one of those datasets
#
d <- Zhou_kidney_pce[1:501, ]
(o <- RNAseqdata(d))
plot(o)


# (3) analysis of the whole dataset (for kidney and PCE)
# (may be long to run)

d <- Zhou_kidney_pce
(o <- RNAseqdata(d))
plot(o)
(s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s, progressbar = TRUE))
head(f$fitres)

plot(f) 
plot(f, dose_log_trans = TRUE)
plot(f, plot.type = "dose_residuals")

r <- bmdcalc(f, z = 1)
plot(r) 
if (require(ggplot2))
  plot(r) + scale_x_log10() # same plot in log scale of BMD
plot(r, by = "trend")
head(r$res)

}

}

\keyword{ datasets }% at least one, from doc/KEYWORDS
