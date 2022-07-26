\name{PCAdataplot}
\alias{PCAdataplot}
\title{Performs and plots the results of a PCA on omic data}

\description{
Provides a two dimensional plot (two first components) of a principal 
component analysis (PCA) performed on omic data after normalization
and/or transformation, to check the promiximity of samples exposed
to the same dose and optionally the presence/absence of 
a potential batch effect.
}

\usage{
PCAdataplot(omicdata, batch)
}

\arguments{
\item{omicdata}{An object of class \code{"microarraydata"}, \code{"RNAseqdata"} or
\code{"continuousomicdata"} respectively returned by 
functions \code{microarraydata}, \code{RNAseqdata} or
\code{continuousomicdata}.}

\item{batch}{Optionnally a factor coding for a potential batch effect
(factor of length the number of samples in the dataset).}
}


\value{ 
   a ggplot object.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1) an example on an RNAseq dataset with a potential batch effect 
#
data(zebraf)
str(zebraf)
data4DRomics <- zebraf$RNAseq_sample_48hpf
batch <- zebraf$batch
o <- RNAseqdata(data4DRomics, transfo.method = "vst")
PCAdataplot(o)
PCAdataplot(o, batch)

\donttest{
# (2) on a microarray dataset (without any potential batch effect)
# 
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", 
  package="DRomics")
o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess")
print(o)
plot(o)
PCAdataplot(o)
}
}