\name{selectgroups}
\alias{selectgroups}
\title{Selection of groups on which to focus}

\description{
Selection of groups (e.g. corresponding to different biological annotations)
on which to focus,
based on their sensitivity (BMD summary value) and their representativeness
(number of items in each group).
}

\usage{
selectgroups(extendedres, group, explev,
   BMDmax, BMDtype = c("zSD", "xfold"), 
   BMDsummary = c("first.quartile", "median" ),
   nitemsmin = 3, keepallexplev = FALSE)
   
}

\arguments{
\item{extendedres}{the dataframe of results provided by drcfit (fitres)
or bmdcalc (res) 
 or a subset of this data frame (selected lines). This dataframe should be extended 
 with additional columns coming for the group (for example from the biological
 annotation of items) and optionnally for another experimental level 
 (for example the molecular level),
 and some lines 
 can be replicated if their corresponding item has more than one annotation.}

\item{group}{the name of the column of \code{extendedres} coding for the groups.}

\item{explev}{optional argument naming the column of \code{extendedres} 
coding for the experimental level.}

\item{BMDmax}{maximum for the BMD summary value used to limit the groups to 
the most sensitive 
(optional input : if missing there is no selection based on the BMD).}

\item{BMDtype}{the type of BMD used for the selection on the BMD, 
  \code{"zSD"} (default choice) or \code{"xfold"}.}
  
\item{BMDsummary}{the type of summary used for the selection based on the BMD, 
\code{"first.quartile"} (default choice of the first quartile of BMD values per group)
or \code{"median"} (for choice of median of BMD values per group).}

\item{nitemsmin}{minimum for the number of items per group to limit the groups
to the most represented (can be put at 1 if you do not want to select on this
number: not recommended.}

\item{keepallexplev}{If TRUE (default value at FALSE), if a group is selected
for at least one experimental level, it will be kept in the selection at all 
the experimental levels.}
}

\details{
This function will provide a subset of the input \code{extendedres} corresponding
to groups for which the number of items representing the group is greater 
than or equal to 
\code{nitemsmin} and if \code{BMDmax} is secified, 
for which the BMD summary value is less than or equal to \code{BMDmax}.
When there is more than one experimental level (\code{explev} specified), 
the selection of groups 
is made separately at each experimental level: so a group may be selected
at one experimental level and removed at another one.
This function eliminates rows with NA values for the chosen BMD 
(of \code{BMDtype}) before performing the the selection.
}


\value{ 
   a dataframe corresponding to a subset of extendedres given in input, that 
   can be used for further exploration using for example \code{\link{bmdplot}},
   \code{\link{bmdplotwithgradient}},
   \code{\link{trendplot}} and \code{\link{sensitivityplot}}.
}

\seealso{
    See \code{\link{bmdfilter}}, \code{\link{bmdplot}}, \code{\link{bmdplotwithgradient}}, 
    \code{\link{trendplot}} and \code{\link{sensitivityplot}}.
}

\author{ 
Marie-Laure Delignette-Muller
}

\examples{

# (1)

# An example from the paper published by Larras et al. 2020
# in Journal of Hazardous Materials
# https://doi.org/10.1016/j.jhazmat.2020.122727

# the dataframe with metabolomic results 
resfilename <- system.file("extdata", "triclosanSVmetabres.txt", package="DRomics")
res <- read.table(resfilename, header = TRUE, stringsAsFactors = TRUE)
str(res)

# the dataframe with annotation of each item identified in the previous file
# each item may have more than one annotation (-> more than one line)
annotfilename <- system.file("extdata", "triclosanSVmetabannot.txt", package="DRomics")
annot <- read.table(annotfilename, header = TRUE, stringsAsFactors = TRUE)
str(annot)

# Merging of both previous dataframes
# in order to obtain an extenderes dataframe
extendedres <- merge(x = res, y = annot, by.x = "id", by.y = "metab.code")
head(extendedres)


# (1) Sensitivity by pathway
# (1.a) before selection
sensitivityplot(extendedres, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")
# (1.b) after selection on representativeness
extendedres.b <- selectgroups(extendedres, 
                         group = "path_class", 
                         nitemsmin = 10)
sensitivityplot(extendedres.b, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")
                
\donttest{
# (1.c) after selection on sensitivity
extendedres.c <- selectgroups(extendedres, 
                         group = "path_class", 
                         BMDmax = 1.25, 
                         BMDtype = "zSD", 
                         BMDsummary = "first.quartile",
                         nitemsmin = 1)
sensitivityplot(extendedres.c, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")

# (1.d) after selection on representativeness and sensitivity 
extendedres.d <- selectgroups(extendedres, 
                         group = "path_class", 
                         BMDmax = 1.25, 
                         BMDtype = "zSD", 
                         BMDsummary = "first.quartile",
                         nitemsmin = 10)
sensitivityplot(extendedres.d, BMDtype = "zSD",
                group = "path_class", 
                BMDsummary = "first.quartile")

# (2) 
# An example with two molecular levels
#
### Rename metabolomic results
metabextendedres <- extendedres

# Import the dataframe with transcriptomic results 
contigresfilename <- system.file("extdata", "triclosanSVcontigres.txt", package = "DRomics")
contigres <- read.table(contigresfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigres)

# Import the dataframe with functional annotation (or any other descriptor/category 
# you want to use, here KEGG pathway classes) 
contigannotfilename <- system.file("extdata", "triclosanSVcontigannot.txt", package = "DRomics")
contigannot <- read.table(contigannotfilename, header = TRUE, stringsAsFactors = TRUE)
str(contigannot)

# Merging of both previous dataframes   
contigextendedres <- merge(x = contigres, y = contigannot, by.x = "id", by.y = "contig")
# to see the structure of this dataframe
str(contigextendedres)

### Merge metabolomic and transcriptomic results
extendedres <- rbind(metabextendedres, contigextendedres)
extendedres$molecular.level <- factor(c(rep("metabolites", nrow(metabextendedres)),
                              rep("contigs", nrow(contigextendedres))))
str(extendedres)

# optional inverse alphabetic ordering of groups for the plot
extendedres$path_class <- factor(extendedres$path_class, 
                levels = sort(levels(extendedres$path_class), decreasing = TRUE))
### (2.1) sensitivity plot of both molecular levels before and after selection of 
#   most sensitive groups
sensitivityplot(extendedres, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level",
                BMDsummary = "first.quartile")
extendedres.2 <- selectgroups(extendedres, 
                         group = "path_class",
                         explev = "molecular.level",
                         BMDmax = 1, 
                         BMDtype = "zSD", 
                         BMDsummary = "first.quartile",
                         nitemsmin = 1)
sensitivityplot(extendedres.2, BMDtype = "zSD",
                group = "path_class", , colorby = "molecular.level",
                BMDsummary = "first.quartile")
### (2.2) same selection but keeping all the experimental as soon
# as the selection criterion is met for at least one experimental level
extendedres.3 <- selectgroups(extendedres, 
                         group = "path_class",
                         explev = "molecular.level",
                         BMDmax = 1, 
                         BMDtype = "zSD", 
                         BMDsummary = "first.quartile",
                         nitemsmin = 1,
                         keepallexplev = TRUE)
extendedres.2
extendedres.3
sensitivityplot(extendedres.3, BMDtype = "zSD",
                group = "path_class", colorby = "molecular.level",
                BMDsummary = "first.quartile")


}

}
