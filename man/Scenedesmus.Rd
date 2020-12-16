\name{Scenedesmus}
\alias{Scenedesmus}
\alias{Scenedesmus_metab}
\alias{Scenedesmus_apical}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Concentration-response effect of triclosan in Scenedesmus vacuolatus}

\description{
Metabolomic and apical data sets for the effect of triclosan in the chlorophyte Scenedesmus vacuolatus.
}

\usage{
data(Scenedesmus_metab)
data(Scenedesmus_apical)
}
%- maybe also 'usage' for other objects documented here.

\format{ 
\code{Scenedesmus_metab} contains one row per metabolite, with the first column corresponding to the identifier of each metabolite, and the other columns giving the log10 tranformed area under the curve for each 
replicate at each concentration. 
In the first line, after the name for the identifier column, we have the tested concentrations for each corresponding replicate.

\code{Scenedesmus_apical} contains one row per apical endpoint, with the first column corresponding to the identifier of each endpoint, and the other columns giving the measured
value of this each endpoint for each 
replicate at each concentration. 
In the first line, after the name for the identifier column, we have the tested concentrations for each corresponding replicate.
}


\source{ 
Larras, F., Billoir, E., Scholz, S., Tarkka, M., Wubet, T., Delignette-Muller, M. L., & Schmitt-Jansen, M. (2020). A multi-omics concentration-response framework uncovers novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus. \emph{Journal of Hazardous Materials}, 122727.
}

%\references{  }

\examples{
# (1.1) load of metabolomics data
#
data(Scenedesmus_metab)
head(Scenedesmus_metab)
str(Scenedesmus_metab)

\donttest{

# (1.2) import and check of metabolomics data
#
(o_metab <- continuousomicdata(Scenedesmus_metab))
plot(o_metab)

# (2.1) load of apical data
#
data(Scenedesmus_apical)
head(Scenedesmus_apical)
str(Scenedesmus_apical)

# (2.2) import and check of apical data
#
(o_apical <- continuousanchoringdata(Scenedesmus_apical))
plot(o_apical)

# (2.3) selection of responsive endpoints on apical data
#
(s_apical <- itemselect(o_apical, select.method = "quadratic", FDR = 0.05))

# (2.4) fit of dose-response models on apical data
#
(f_apical <- drcfit(s_apical, progressbar = TRUE))
f_apical$fitres
plot(f_apical) 
plot(f_apical, dose_log_trans = TRUE)
plot(f_apical, plot.type = "dose_residuals")

# (2.5) Benchmark dose calculation on apical data
#
r_apical <- bmdcalc(f_apical, z = 1)
r_apical$res


}
}

\keyword{ datasets }% at least one, from doc/KEYWORDS
