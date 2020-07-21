\name{Scenedesmus}
\alias{Scenedesmus}
\alias{Scenedesmus_metab}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Concentration-response metabolomic effect of triclosan in Scenedesmus vacuolatus}

\description{
Metabolomics data set for the effect of triclosan in the chlorophyte Scenedesmus vacuolatus.
}

\usage{
data(Scenedesmus_metab)
}
%- maybe also 'usage' for other objects documented here.

\format{ 
\code{Scenedesmus_metab} contains one row per metabolite, with the first column corresponding to the identifier of each metabolite, and the other columns giving the log10 tranformed area under the curve for each 
replicate at each concentration. 
In the first line, after the name for the identifier column, we have the tested concentrations for each corresponding replicate.}

\source{ 
Larras, F., Billoir, E., Scholz, S., Tarkka, M., Wubet, T., Delignette-Muller, M. L., & Schmitt-Jansen, M. (2020). A multi-omics concentration-response framework uncovers novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus. \emph{Journal of Hazardous Materials}, 122727.
}

%\references{  }

\examples{
# (1) load of data
#
data(Scenedesmus_metab)
head(Scenedesmus_metab)
str(Scenedesmus_metab)

# (2) import and check of the data set
#
(o_metab <- metabolomicdata(Scenedesmus_metab))
plot(o_metab)

}

\keyword{ datasets }% at least one, from doc/KEYWORDS
