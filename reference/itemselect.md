# Selection of significantly responsive items

Significantly responsive items are selected using one of the three
proposed methods: a quadratic trend test, a linear trend test or an
ANOVA-based test.

## Usage

``` r
itemselect(omicdata, select.method = c("quadratic", "linear", "ANOVA"), 
  FDR = 0.05, max.ties.prop = 0.2)   

# S3 method for class 'itemselect'
print(x, nfirstitems = 20, ...)
```

## Arguments

- omicdata:

  An object of class `"microarraydata"`, `"RNAseqdata"`,
  `"metabolomicdata"` or `"continuousanchoringdata"` respectively
  returned by functions `microarraydata`, `RNAseqdata`,
  `metabolomicdata` or `continuousanchoringdata`.

- select.method:

  `"quadratic"` for a quadratic trend test on dose ranks, `"linear"` for
  a linear trend test on dose ranks and `"ANOVA"` for an ANOVA-type test
  (see details for further explaination).

- FDR:

  The threshold in term of FDR (False Discovery Rate) for selecting
  responsive items.

- max.ties.prop:

  The maximal tolerated proportion of tied values for each item, above
  which the item cannot be selected (must be in \]0, 0.5\], and by
  default fixed at 0.2 - see details for a description of this filtering
  step).

- x:

  An object of class `"itemselect"`.

- nfirstitems:

  The maximum number of selected items to print.

- ...:

  further arguments passed to print function.

## Details

The selection of responsive items is performed using the `limma` package
for microarray and continuous omics data (such as metabolomics), the
`DESeq2` package for RNAseq data and the `lm` function for continuous
anchoring data. Three methods are proposed (as described below). Within
`limma` those methods are implemented using functions
[`lmFit`](https://rdrr.io/pkg/limma/man/lmFit.html),
[`eBayes`](https://rdrr.io/pkg/limma/man/ebayes.html) and
[`topTable`](https://rdrr.io/pkg/limma/man/toptable.html) with p-values
ajusted for multiple testing using the Benjamini-Hochberg method (also
called q-values), with the false discovery rate given in input (argument
`FDR`). Within `DESeq2` those methods are implemented using functions
[`DESeqDataSetFromMatrix`](https://rdrr.io/pkg/DESeq2/man/DESeqDataSet.html),
[`DESeq`](https://rdrr.io/pkg/DESeq2/man/DESeq.html) and
[`results`](https://rdrr.io/pkg/DESeq2/man/results.html) with p-values
ajusted for multiple testing using the Benjamini-Hochberg method (also
called q-values), with the false discovery rate given in input (argument
`FDR`). For continuous anchoring data, the `lm` and `anova` functions
are used to fit the model and compare it to the null model, and the
pvalues are then corrected using the function `p.adjust` with the
Benjamini-Hochberg method.

- The ANOVA_based test (`"ANOVA"`) is classically used for selection of
  omics data in the general case but it requires many replicates per
  dose to be efficient, and is thus not really suited for a
  dose-response design.

- The linear trend test (`"linear"`) aims at detecting monotonic trends
  from dose-response designs, whatever the number of replicates per
  dose. As proposed by Tukey (1985), it tests the global significance of
  a linear model describing the response as a function of the dose in
  rank-scale.

- The quadratic trend test (`"quadratic"`) tests the global significance
  of a quadratic model describing the response as a function of the dose
  in rank-scale. It is a variant of the linear trend method that aims at
  detecting monotonic and non monotonic trends from a dose-response
  designs, whatever the number of replicates per dose (default chosen
  method).

After the use of one this previously described tests, a filter based on
the proportion of tied values is also performed whatever the type of
data, assuming tied values correspond to a minimal common value at which
non detections were imputed. All items having a proportion of such tied
minimal values above the input argument `max.ties.prop` are eliminated
from the selection.

## Value

`itemselect` returns an object of class `"itemselect"`, a list with 5
components:

- adjpvalue :

  the vector of the p-values adjusted by the Benjamini-Hochberg method
  (also called q-values) for selected items (adjpvalue inferior to FDR)
  sorted in ascending order

- selectindex :

  the corresponding vector of row indices of selected items in the
  object omicdata

- omicdata:

  The corresponding object of class `"microarraydata"`, `"RNAseqdata"`,
  `"continuousomicdata"` or `"continuousanchoringdata"` given in input.

- select.method:

  The selection method given in input.

- FDR:

  The threshold in term of FDR given in input.

The print of a `"itemselect"` object gives the number of selected items
and the identifiers of the 20 most responsive items.

## See also

See [`lmFit`](https://rdrr.io/pkg/limma/man/lmFit.html),
[`eBayes`](https://rdrr.io/pkg/limma/man/ebayes.html) and
[`topTable`](https://rdrr.io/pkg/limma/man/toptable.html) for details
about the used functions of the `limma` package and
[`DESeqDataSetFromMatrix`](https://rdrr.io/pkg/DESeq2/man/DESeqDataSet.html),
[`DESeq`](https://rdrr.io/pkg/DESeq2/man/DESeq.html) and
[`results`](https://rdrr.io/pkg/DESeq2/man/results.html) for details
about the used functions of the `DESeq2` package.

## References

Tukey JW, Ciminera JL and Heyse JF (1985), *Testing the statistical
certainty of a response to increasing doses of a drug*. Biometrics,
295-301.

Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W, and Smyth, GK (2015),
*limma powers differential expression analyses for RNA-sequencing and
microarray studies*. Nucleic Acids Research 43, e47.

Love MI, Huber W, and Anders S (2014), *Moderated estimation of fold
change and dispersion for RNA-seq data with DESeq2*. Genome biology,
15(12), 550.

## Author

Marie-Laure Delignette-Muller

## Examples

``` r
# (1) an example on a microarray data set (a subsample of a greater data set) 
#     
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
#> Just wait, the normalization using cyclicloess may take a few minutes.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>     0  0.69 1.223 2.148 3.774 6.631 
#>     5     5     5     5     5     5 
#> Number of items: 1000 
#> Identifiers of the first 20 items:
#>  [1] "1"    "2"    "3"    "4"    "5.1"  "5.2"  "6.1"  "6.2"  "7.1"  "7.2" 
#> [11] "8.1"  "8.2"  "9.1"  "9.2"  "10.1" "10.2" "11.1" "11.2" "12.1" "12.2"
#> Data were normalized between arrays using the following method: cyclicloess 

# 1.a using the quadratic trend test
#
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
#> Removing intercept from test coefficients
#> Number of selected items using a quadratic trend test with an FDR of 0.05: 318
#> Identifiers of the first 20 most responsive items:
#>  [1] "384.2" "383.1" "383.2" "384.1" "301.1" "363.1" "300.2" "364.2" "364.1"
#> [10] "363.2" "301.2" "300.1" "351.1" "350.2" "239.1" "240.1" "240.2" "370"  
#> [19] "15"    "350.1"
print(s_quad, nfirstitems = 30)
#> Number of selected items using a quadratic trend test with an FDR of 0.05: 318
#> Identifiers of the first 30 most responsive items:
#>  [1] "384.2" "383.1" "383.2" "384.1" "301.1" "363.1" "300.2" "364.2" "364.1"
#> [10] "363.2" "301.2" "300.1" "351.1" "350.2" "239.1" "240.1" "240.2" "370"  
#> [19] "15"    "350.1" "351.2" "12.1"  "7.2"   "239.2" "368.1" "263.2" "264.1"
#> [28] "138.1" "233.2" "334.1"

# to get the names of all the selected items
(selecteditems <- s_quad$omicdata$item[s_quad$selectindex]) 
#>   [1] "384.2" "383.1" "383.2" "384.1" "301.1" "363.1" "300.2" "364.2" "364.1"
#>  [10] "363.2" "301.2" "300.1" "351.1" "350.2" "239.1" "240.1" "240.2" "370"  
#>  [19] "15"    "350.1" "351.2" "12.1"  "7.2"   "239.2" "368.1" "263.2" "264.1"
#>  [28] "138.1" "233.2" "334.1" "353.1" "359"   "136.1" "167.1" "138.2" "27.2" 
#>  [37] "312.1" "367.1" "264.2" "334.2" "168.2" "247.2" "336"   "168.1" "25.2" 
#>  [46] "7.1"   "136.2" "167.2" "233.1" "353.2" "360.2" "13.2"  "12.2"  "88.1" 
#>  [55] "263.1" "352.2" "162.2" "70"    "161.1" "275.1" "358.2" "88.2"  "320.1"
#>  [64] "352.1" "311.1" "27.1"  "4"     "358.1" "42.2"  "55.2"  "92"    "249.2"
#>  [73] "360.1" "103"   "162.1" "320.2" "198.1" "229.2" "371.2" "225"   "321.2"
#>  [82] "329.1" "438.2" "229.1" "25.1"  "348"   "228.1" "247.1" "371.1" "512.1"
#>  [91] "81"    "13.1"  "148"   "311.2" "113.2" "83.2"  "268.2" "268.1" "84.1" 
#> [100] "321.1" "467.1" "228.2" "113.1" "489.1" "330.2" "249.1" "83.1"  "439.2"
#> [109] "369.2" "330.1" "295.1" "294.1" "295.2" "490.2" "117.2" "337"   "267.2"
#> [118] "274.1" "367.2" "468.1" "116.1" "368.2" "401.2" "116.2" "137"   "341.1"
#> [127] "195.2" "490.1" "467.2" "404.2" "446.1" "329.2" "85.2"  "118.1" "118.2"
#> [136] "126"   "267.1" "115"   "132.1" "401.1" "402.1" "449.1" "404.1" "199.2"
#> [145] "312.2" "84.2"  "74.1"  "275.2" "446.2" "373.1" "449.2" "465.2" "117.1"
#> [154] "294.2" "74.2"  "194.2" "195.1" "40.2"  "497.2" "169.2" "5.2"   "194.1"
#> [163] "372.2" "274.2" "410.1" "169.1" "170"   "171.1" "202.2" "207.1" "373.2"
#> [172] "402.2" "496.1" "372.1" "298.2" "67.1"  "144.1" "154.1" "199.1" "369.1"
#> [181] "396.2" "232.1" "177.2" "41.2"  "85.1"  "159"   "171.2" "245.2" "396.1"
#> [190] "87.2"  "299.1" "496.2" "161.2" "266.1" "512.2" "464.1" "356.1" "397.1"
#> [199] "8.1"   "39.2"  "464.2" "61.2"  "385.1" "131.2" "176.1" "497.1" "245.1"
#> [208] "528.1" "131.1" "465.1" "54.1"  "204.2" "451.1" "54.2"  "441.1" "260.1"
#> [217] "299.2" "397.2" "400.2" "436"   "177.1" "232.2" "386.2" "260.2" "256.1"
#> [226] "87.1"  "265.1" "204.1" "258.2" "67.2"  "198.2" "439.1" "40.1"  "206.2"
#> [235] "66.1"  "385.2" "356.2" "293.2" "527.1" "258.1" "129.1" "68.1"  "191.1"
#> [244] "511.1" "286.1" "75.2"  "128.2" "428.1" "122.1" "144.2" "282.1" "523.2"
#> [253] "377.2" "386.1" "128.1" "440.1" "523.1" "461.2" "293.1" "342.2" "433.1"
#> [262] "68.2"  "86.2"  "451.2" "298.1" "291.1" "342.1" "43.1"  "499.1" "333.2"
#> [271] "227.1" "121.1" "208.2" "412.2" "438.1" "282.2" "376.2" "75.1"  "155.1"
#> [280] "526.2" "291.2" "482.2" "129.2" "526.1" "121.2" "440.2" "515.2" "65.2" 
#> [289] "341.2" "122.2" "461.1" "224.1" "419.1" "482.1" "466.1" "524.2" "524.1"
#> [298] "112.1" "41.1"  "284.1" "187.1" "513.2" "110.1" "101.2" "419.2" "210.1"
#> [307] "205.1" "498.2" "382.1" "133.2" "390.1" "243.1" "452.1" "110.2" "513.1"
#> [316] "468.2" "511.2" "65.1" 

# \donttest{

# 1.b using the linear trend test
#
(s_lin <- itemselect(o, select.method = "linear", FDR = 0.05))
#> Removing intercept from test coefficients
#> Number of selected items using a linear trend test with an FDR of 0.05: 90
#> Identifiers of the first 20 most responsive items:
#>  [1] "300.2" "301.1" "239.1" "300.1" "240.1" "301.2" "240.2" "239.2" "364.2"
#> [10] "363.1" "364.1" "136.1" "363.2" "27.2"  "138.1" "336"   "233.2" "25.2" 
#> [19] "27.1"  "136.2"

# 1.c using the ANOVA-based test
#
(s_ANOVA <- itemselect(o, select.method = "ANOVA", FDR = 0.05))
#> Removing intercept from test coefficients
#> Number of selected items using an ANOVA type test with an FDR of 0.05: 203
#> Identifiers of the first 20 most responsive items:
#>  [1] "384.2" "363.2" "367.1" "383.1" "383.2" "363.1" "364.1" "364.2" "384.1"
#> [10] "368.1" "300.2" "351.1" "301.1" "320.1" "350.2" "300.1" "351.2" "353.1"
#> [19] "353.2" "350.1"

# 1.d using the quadratic trend test with a smaller false discovery rate
#
(s_quad.2 <- itemselect(o, select.method = "quadratic", FDR = 0.001))
#> Removing intercept from test coefficients
#> Number of selected items using a quadratic trend test with an FDR of 0.001: 78
#> Identifiers of the first 20 most responsive items:
#>  [1] "384.2" "383.1" "383.2" "384.1" "301.1" "363.1" "300.2" "364.2" "364.1"
#> [10] "363.2" "301.2" "300.1" "351.1" "350.2" "239.1" "240.1" "240.2" "370"  
#> [19] "15"    "350.1"

# }
```
