# Import, check and normalization and transformation of RNAseq data

RNAseq data in raw counts (integer values) are imported from a .txt file
(internally imported using the function
[`read.table`](https://rdrr.io/r/utils/read.table.html)), checked or
from a R object of class `data.frame` (see the description of argument
`file` for the required format of data), normalized with respect to
library size and tranformed in a log2 scale using variance stabilizing
transformation or regularized logarithm.

## Usage

``` r
RNAseqdata(file, backgrounddose, check = TRUE, transfo.method, 
          transfo.blind = TRUE, round.counts = FALSE)

# S3 method for class 'RNAseqdata'
print(x, ...)
# S3 method for class 'RNAseqdata'
plot(x, range4boxplot = 0, ...)
```

## Arguments

- file:

  The name of the .txt file (e.g. `"mydata.txt"`) containing one row per
  item, with the first column corresponding to the identifier of each
  item, and the other columns giving the responses of the item for each
  replicate at each dose or concentration. In the first line, after a
  name for the identifier column, we must have the tested doses or
  concentrations in a numeric format for the corresponding replicate
  (for example, if there are triplicates for each treatment, the first
  line could be "item", 0, 0, 0, 0.1, 0.1, 0.1, etc.). This file is
  imported within the function using the function
  [`read.table`](https://rdrr.io/r/utils/read.table.html) with its
  default field separator (sep argument) and its default decimal
  separator (dec argument at "."). Alternatively an R object of class
  `data.frame` can be directly given in input, corresponding to the
  output of `read.table(file, header = FALSE)` on a file described as
  above. The two alternatives are illustrated below in examples.

- backgrounddose:

  This argument must be used when there is no dose at zero in the data,
  to prevent the calculation of the BMD by extrapolation. All doses
  below or equal to the value given in backgrounddose will be fixed at
  0, so as to be considered at the background level of exposition.

- check:

  If TRUE the format of the input file is checked.

- transfo.method:

  The method chosen to transform raw counts in a log2 scale using the
  `DESeq2`: `"rlog"` for regularized logarithm or `"vst"` for variance
  stabilizing transformation. If missing, default value defined at
  `"rlog"` for datasets with less than 30 samples and at `"vst"` if not

- transfo.blind:

  Argument given to function
  [`rlog`](https://rdrr.io/pkg/DESeq2/man/rlog.html) or
  [`vst`](https://rdrr.io/pkg/DESeq2/man/vst.html), see
  [`rlog`](https://rdrr.io/pkg/DESeq2/man/rlog.html) and
  [`vst`](https://rdrr.io/pkg/DESeq2/man/vst.html) for an explaination,
  by default at `TRUE` as in the `DESeq2` package .

- round.counts:

  Put it to TRUE if your counts come from Kallisto or Salmon in order to
  round them before treatment with `DESeq2`.

- x:

  An object of class `"RNAseqdata"`.

- range4boxplot:

  An argument passed to boxplot(), fixed by default at 0 to prevent the
  producing of very large plot files due to many outliers. Can be put at
  1.5 to obtain more classical boxplots.

- ...:

  further arguments passed to print or plot functions.

## Details

This function imports the data, checks their format (see the description
of argument `file` for the required format of data) and gives in the
`print` information that should help the user to check that the coding
of data is correct : the tested doses (or concentrations) the number of
replicates for each dose, the number of items, the identifiers of the
first 20 items. Data are normalized with respect to library size and
tranformed using functions
[`rlog`](https://rdrr.io/pkg/DESeq2/man/rlog.html) or
[`vst`](https://rdrr.io/pkg/DESeq2/man/vst.html) of the `DESeq2` package
depending on the specified method : `"rlog"` (recommended default
choice) or `"vst"`.

## Value

`RNAseqdata` returns an object of class "RNAseqdata", a list with 9
components:

- data:

  the numeric matrix of normalized and transformed responses of each
  item in each replicate (one line per item, one column per replicate)

- dose:

  the numeric vector of the tested doses or concentrations corresponding
  to each column of data

- item:

  the character vector of the identifiers of the items, corresponding to
  each line of data

- design:

  a table with the experimental design (tested doses and number of
  replicates for each dose) for control by the user

- data.mean:

  the numeric matrix of mean responses of each item per dose (mean of
  the corresponding replicates) (one line per item, one column per
  unique value of the dose)

- data.sd:

  the numeric matrix of standard deviations of the response of each item
  per dose (sd of the corresponding replicates, NA if no replicate) (one
  line per item, one column per unique value of the dose)

- transfo.method:

  The transformation method specified in input

- raw.counts:

  the numeric matrix of non transformed responses (raw counts) of each
  item in each replicate (one line per item, one column per replicate)
  before normalization

- containsNA:

  always at FALSE as RNAseq data are not allowed to contain NA values

The print of a `RNAseqdata` object gives the tested doses (or
concentrations) and number of replicates for each dose, the number of
items, the identifiers of the first 20 items (for check of good coding
of data) and the tranformation method. The plot of a `RNAseqdata` object
shows the data distribution for each dose or concentration and replicate
before and after normalization and tranformation.

## See also

See [`read.table`](https://rdrr.io/r/utils/read.table.html) the function
used to import data, [`rlog`](https://rdrr.io/pkg/DESeq2/man/rlog.html)
and [`vst`](https://rdrr.io/pkg/DESeq2/man/vst.html) for details about
the transformation methods and
[`microarraydata`](https://drgarden.pages.in2p3.fr/DRomics/reference/microarraydata.md),
[`continuousomicdata`](https://drgarden.pages.in2p3.fr/DRomics/reference/metabolomicdata.md)
and
[`continuousanchoringdata`](https://drgarden.pages.in2p3.fr/DRomics/reference/continuousanchoringdata.md)
for other types of data.

## References

Love MI, Huber W, and Anders S (2014), *Moderated estimation of fold
change and dispersion for RNA-seq data with DESeq2*. Genome biology,
15(12), 550.

## Author

Marie-Laure Delignette-Muller

## Examples

``` r
# (1) import, check, normalization and transformation of RNAseq data 
# An example on a subsample of a data set published by Zhou et al. 2017
# Effect on mouse kidney transcriptomes of tetrachloroethylene
# (see ? Zhou for details)
#
datafilename <- system.file("extdata", "RNAseq_sample.txt", package="DRomics")
(o <- RNAseqdata(datafilename, check = TRUE, transfo.method = "vst"))
#> converting counts to integer mode
#> Warning: 
#> To optimize the dose-response modelling, it is recommended to use a
#> dose-response design with at least six different tested doses.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>    0 0.22 0.67    2    6 
#>    2    3    3    3    3 
#> Number of items: 999 
#> Identifiers of the first 20 items:
#>  [1] "NM_144958"    "NR_102758"    "NM_172405"    "NM_029777"    "NM_001130188"
#>  [6] "NM_207141"    "NM_001162368" "NM_008117"    "NM_001168290" "NM_010910"   
#> [11] "NM_001004147" "NM_001146318" "NM_145597"    "NM_001161797" "NM_021483"   
#> [16] "NR_002862"    "NR_033520"    "NM_134027"    "NM_010381"    "NM_019388"   
#> Data were normalized with respect to library size and tranformed using 
#> the following method: vst
plot(o)


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
#> converting counts to integer mode
#> Warning: 
#> To optimize the dose-response modelling, it is recommended to use a
#> dose-response design with at least six different tested doses.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>    0 0.22 0.67    2    6 
#>    2    3    3    3    3 
#> Number of items: 999 
#> Identifiers of the first 20 items:
#>  [1] "NM_144958"    "NR_102758"    "NM_172405"    "NM_029777"    "NM_001130188"
#>  [6] "NM_207141"    "NM_001162368" "NM_008117"    "NM_001168290" "NM_010910"   
#> [11] "NM_001004147" "NM_001146318" "NM_145597"    "NM_001161797" "NM_021483"   
#> [16] "NR_002862"    "NR_033520"    "NM_134027"    "NM_010381"    "NM_019388"   
#> Data were normalized with respect to library size and tranformed using 
#> the following method: vst
plot(o)

PCAdataplot(o)



# (2) transformation with two methods on the whole data set
# \donttest{
data(Zhou_kidney_pce)

# variance stabilizing tranformation
(o1 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst"))
#> converting counts to integer mode
#> Warning: 
#> To optimize the dose-response modelling, it is recommended to use a
#> dose-response design with at least six different tested doses.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>    0 0.22 0.67    2    6 
#>    2    3    3    3    3 
#> Number of items: 33394 
#> Identifiers of the first 20 items:
#>  [1] "NM_144958"    "NR_102758"    "NM_172405"    "NM_029777"    "NM_001130188"
#>  [6] "NM_207141"    "NM_001162368" "NM_008117"    "NM_001168290" "NM_010910"   
#> [11] "NM_001004147" "NM_001146318" "NM_145597"    "NM_001161797" "NM_021483"   
#> [16] "NR_002862"    "NR_033520"    "NM_134027"    "NM_010381"    "NM_019388"   
#> Data were normalized with respect to library size and tranformed using 
#> the following method: vst
plot(o1)


# regularized logarithm
(o2 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog"))
#> Just wait, the transformation using regularized logarithm (rlog) may 
#> take a few minutes.
#> converting counts to integer mode
#> Warning: 
#> To optimize the dose-response modelling, it is recommended to use a
#> dose-response design with at least six different tested doses.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>    0 0.22 0.67    2    6 
#>    2    3    3    3    3 
#> Number of items: 33394 
#> Identifiers of the first 20 items:
#>  [1] "NM_144958"    "NR_102758"    "NM_172405"    "NM_029777"    "NM_001130188"
#>  [6] "NM_207141"    "NM_001162368" "NM_008117"    "NM_001168290" "NM_010910"   
#> [11] "NM_001004147" "NM_001146318" "NM_145597"    "NM_001161797" "NM_021483"   
#> [16] "NR_002862"    "NR_033520"    "NM_134027"    "NM_010381"    "NM_019388"   
#> Data were normalized with respect to library size and tranformed using 
#> the following method: rlog
plot(o2)


# variance stabilizing tranformation (blind to the experimental design)
(o3 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "vst",
      transfo.blind = TRUE))
#> converting counts to integer mode
#> Warning: 
#> To optimize the dose-response modelling, it is recommended to use a
#> dose-response design with at least six different tested doses.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>    0 0.22 0.67    2    6 
#>    2    3    3    3    3 
#> Number of items: 33394 
#> Identifiers of the first 20 items:
#>  [1] "NM_144958"    "NR_102758"    "NM_172405"    "NM_029777"    "NM_001130188"
#>  [6] "NM_207141"    "NM_001162368" "NM_008117"    "NM_001168290" "NM_010910"   
#> [11] "NM_001004147" "NM_001146318" "NM_145597"    "NM_001161797" "NM_021483"   
#> [16] "NR_002862"    "NR_033520"    "NM_134027"    "NM_010381"    "NM_019388"   
#> Data were normalized with respect to library size and tranformed using 
#> the following method: vst
plot(o3)


# regularized logarithm
(o4 <- RNAseqdata(Zhou_kidney_pce, check = TRUE, transfo.method = "rlog",
      transfo.blind = TRUE))
#> Just wait, the transformation using regularized logarithm (rlog) may 
#> take a few minutes.
#> converting counts to integer mode
#> Warning: 
#> To optimize the dose-response modelling, it is recommended to use a
#> dose-response design with at least six different tested doses.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>    0 0.22 0.67    2    6 
#>    2    3    3    3    3 
#> Number of items: 33394 
#> Identifiers of the first 20 items:
#>  [1] "NM_144958"    "NR_102758"    "NM_172405"    "NM_029777"    "NM_001130188"
#>  [6] "NM_207141"    "NM_001162368" "NM_008117"    "NM_001168290" "NM_010910"   
#> [11] "NM_001004147" "NM_001146318" "NM_145597"    "NM_001161797" "NM_021483"   
#> [16] "NR_002862"    "NR_033520"    "NM_134027"    "NM_010381"    "NM_019388"   
#> Data were normalized with respect to library size and tranformed using 
#> the following method: rlog
plot(o4)



# }
```
