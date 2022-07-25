\name{zebraf}
\alias{zebraf}
\alias{zebraf_48hpf}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Transcriptomic dose-response to ionizing radiation in zebrafish}

\description{
A sample of an RNAseq data set and corresponding batch effect of the dose-response to the chronic exposure to ionizing radiation of zebrafish embryo from fertilization and up to 48 hours post-fertilization.
}

\usage{
data(zebraf)
}
%- maybe also 'usage' for other objects documented here.

\format{ 
\code{zebraf} contains a list of two objects, \code{zebraf$RNAseq_sample_48hpf} a sample (first 1000 transcripts) of RNAseq data  corresponding to the exposition of zebrafish embryo from fertilization and up to 48 hours post-fertilization, and \code{zebraf$batch} a factor coding for the batch of each sample.

The first object is presented as needed by the  \code{\link{RNAseqdata}}: one row per transcript, with the first column corresponding to the identifier of each transcript, and the other columns giving the count of reads for each replicate at each dose. In the first line, after the name for the identifier column, we have the tested doses for each corresponding replicate)
}

\source{ 
Murat El Houdigui, S., Adam-Guillermin, C., Loro, G., Arcanjo, C., Frelon, S., Floriani, M., ... & Armant, O. 2019. A systems biology approach reveals neuronal and muscle developmental defects after chronic exposure to ionising radiation in zebrafish. \emph{Scientific reports}, \bold{9(1)}, 1-15.
}

%\references{  }

\examples{
# (1) load of data
#
data(zebraf)
str(zebraf)
data4DRomics <- zebraf$RNAseq_sample_48hpf
batch <- zebraf$batch

# (2) Normization and transformation of data followed
# by PCA plot without and with the batch effect
#
o <- RNAseqdata(data4DRomics)
PCAdataplot(o)
PCAdataplot(o, batch)

\donttest{


}
}

\keyword{ datasets }% at least one, from doc/KEYWORDS
