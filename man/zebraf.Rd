\name{zebraf}
\alias{zebraf}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Transcriptomic dose-response to ionizing radiation in zebrafish with batch effect}

\description{
A sample of an RNAseq data set of the dose-response to the chronic exposure to ionizing radiation of zebrafish embryo from fertilization and up to 48 hours post-fertilization with the corresponding batch effect of the experiment.
}

\usage{
data(zebraf)
}
%- maybe also 'usage' for other objects documented here.

\format{ 
\code{zebraf} contains a list of three objects, \code{zebraf$counts} an integer matrix of counts of reads 
(1000 rows for a sample pf 1000 transcripts and 16 columns for the 16 sampels), \code{zebraf$dose}, a numeric vector coding for the dose of each sample 
and \code{zebraf$batch} a factor coding for the batch of each sample.

}

\seealso{
    See \url{https://github.com/zhangyuqing/ComBat-seq} for indication of use 
    of the ComBat_seq function of the sva package 
    for batch effect correction and \code{\link{formatdata4DRomics}} a function 
    that can be used to format those data before use of the DRomics workflow.}

\source{ 
Murat El Houdigui, S., Adam-Guillermin, C., Loro, G., Arcanjo, C., Frelon, S., Floriani, M., ... & Armant, O. 2019. A systems biology approach reveals neuronal and muscle developmental defects after chronic exposure to ionising radiation in zebrafish. \emph{Scientific reports}, \bold{9(1)}, 1-15.
}

\references{ 
Zhang, Y., Parmigiani, G., & Johnson, W. E. (2020). ComBat-seq: batch effect adjustment for RNA-seq count data. NAR genomics and bioinformatics, 2(3), lqaa078.}

\examples{
# (1) load of data
#
data(zebraf)
str(zebraf)

# (2) formating of data for use in DRomics
# 
data4DRomics <- formatdata4DRomics(signalmatrix = zebraf$counts, 
                           dose = zebraf$dose)

# (3) Normalization and transformation of data followed
# by PCA plot with vizualisation of the batch effect
#
o <- RNAseqdata(data4DRomics, transfo.method = "vst")
PCAdataplot(o, batch = zebraf$batch)

\donttest{
PCAdataplot(o, label = TRUE)

# (4) Batch effect correction using ComBat_seq{sva}
#

if(!requireNamespace("sva", quietly = TRUE)) {
BECcounts <- ComBat_seq(as.matrix(o$raw.counts), 
                        batch = as.factor(zebraf$batch), 
                        group = as.factor(o$dose)) 
BECdata4DRomics <- formatdata4DRomics(signalmatrix = BECcounts, 
                                   dose = o$dose)
(o.BEC <- RNAseqdata(BECdata4DRomics, transfo.method = "vst"))
plot(o.BEC)
PCAdataplot(o.BEC, batch = zebraf$batch)
PCAdataplot(o.BEC, label = TRUE)
}


}
}

\keyword{ datasets }% at least one, from doc/KEYWORDS
