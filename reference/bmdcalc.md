# Computation of benchmark doses for responsive items

Computes x-fold and z-SD benchmark doses for each responsive item using
the best fit dose-reponse model.

## Usage

``` r
bmdcalc(f, z = 1, x = 10, minBMD, ratio2switchinlog = 100)

# S3 method for class 'bmdcalc'
print(x, ...)

# S3 method for class 'bmdcalc'
plot(x, BMDtype = c("zSD", "xfold"), 
            plottype = c("ecdf", "hist", "density"), 
            by = c("none", "trend", "model", "typology"),
            hist.bins = 30, BMD_log_transfo = TRUE, ...)
```

## Arguments

- f:

  An object of class `"drcfit"` returned by the function `drcfit`.

- z:

  Value of z defining the BMD-zSD as the dose at which the response is
  reaching y0 +/- z \* SD, with y0 the level at the control given by the
  dose-response fitted model and SD the residual standard deviation of
  the dose-response fitted model.

- x:

  Value of x given as a percentage and defining the BMD-xfold as the
  dose at which the response is reaching y0 +/- (x/100) \* y0, with y0
  the level at the control given by the dose-response fitted model.

  For `print` and `plot` functions, an object of class `"bmdcalc"`.

- minBMD:

  minimal value for calculated BMDs, so a value considered negligible
  compared to the tested doses. If not given by the user this argument
  is fixed at the minimal non null tested dose divided by 100.

- ratio2switchinlog:

  ratio between maximal and minimal tested doses above which the
  numerical computation (when the use of
  [`uniroot`](https://rdrr.io/r/stats/uniroot.html) is necessary) of the
  BMD is performed on a log scale of dose.

- BMDtype:

  The type of BMD to plot, `"zSD"` (default choice) or `"xfold"`.

- plottype:

  The type plot, `"ecdf"` for an empirical cumulative distribution plot
  (default choice), `"hist"` for a histogram or `"density"` for a
  density plot.

- by:

  If different from `"none"` the plot is split by trend (if `"trend"`),
  by model (if `"model"`) or by typology (if `"typology"`).

- hist.bins:

  The number of bins, only used for histogram(s).

- BMD_log_transfo:

  If TRUE, default option, a log transformation of the BMD is used in
  the plot.

- ...:

  further arguments passed to graphical or print functions.

## Details

The two types of benchmark doses (BMD) proposed by the EFSA (2017) were
computed for each responsive item using the best fit dose-reponse model
previously obtained using the [`drcfit`](drcfit.md) function (see Larras
et al. 2018 for details):

- the BMD-zSD defined as the dose at which the response is reaching y0
  +/- z \* SD, with y0 the level at the control given by the
  dose-response model, SD the residual standard deviation of the dose
  response model fit and z given as an input (`z` fixed to 1 by
  default),

- the BMD-xfold defined as the dose at which the response is reaching y0
  +/- (x/100) \* y0, with y0 the level at the control given by the
  dose-response fitted model and x the percentage given as an input (`x`
  fixed at 10 by default.)

When there is no analytical solution for the BMD, it is numerically
searched along the fitted curve using the
[`uniroot`](https://rdrr.io/r/stats/uniroot.html) function.

In cases where the BMD cannot be reached due to the asymptote at high
doses, `NaN` is returned. In cases where the BMD is not reached at the
highest tested dose, `NA` is returned. Very low BMD values obtained by
extrapolation between 0 and the smallest non null tested dose, that
correspond to very sensitive items (that we do not want to exclude), are
thresholded at minBMD, an argument by default fixed at the smallest non
null tested dose divided by 100, but that can be fixed by the user as
what he considers to be a negligible dose.

## Value

`bmdcalc` returns an object of class `"bmdcalc"`, a list with 4
components:

- res:

  a data frame reporting the results of the fit and BMD computation on
  each selected item sorted in the ascending order of the adjusted
  p-values returned by function `itemselect`. The different columns
  correspond to the identifier of each item (`id`), the row number of
  this item in the initial data set (`irow`), the adjusted p-value of
  the selection step (`adjpvalue`), the name of the best fit model
  (`model`), the number of fitted parameters (`nbpar`), the values of
  the parameters `b`, `c`, `d`, `e` and `f`, (`NA` for non used
  parameters), the residual standard deviation (`SDres`), the typology
  of the curve (`typology`, (16 class typology described in the help of
  the `drcfit` function)), the rough trend of the curve (`trend`)
  defined with four classes (U, bell, increasing or decreasing shape),
  the theoretical y value at the control (`y0`), the theoretical y value
  at the maximal dose `yatdosemax`), the theoretical y range for x
  within the range of tested doses (`yrange`), the maximal absolute y
  change (up or down) from the control(`maxychange`) and for biphasic
  curves the x value at which their extremum is reached (`xextrem`) and
  the corresponding y value (`yextrem`), the BMD-zSD value (`BMD.zSD`)
  with the corresponding BMR-zSD value (reached or not, `BMR.zSD`) and
  the BMD-xfold value (`BMD.xfold`) with the corresponding BMR-xfold
  value (reached or not, `BMR.xfold`).

- z:

  Value of z given in input to define the BMD-zSD.

- x:

  Value of x given in input as a percentage to define the BMD-xfold.

- minBMD:

  minimal value for calculated BMDs given in input or fixed at the
  minimal non null tested dose divided by 100.

- ratio2switchinlog:

  ratio between maximal and minimal tested doses above which the
  numerical computations are performed in a log scale (as given in
  input).

- omicdata:

  The corresponding object given in input (component of itemselect).

## See also

See [`uniroot`](https://rdrr.io/r/stats/uniroot.html) for details about
the function used for the numerical search of the benchmark dose for
cases where there is no analytical solution.

## References

EFSA Scientific Committee, Hardy A, Benford D, Halldorsson T, Jeger MJ,
Knutsen KH, ... & Schlatter JR (2017). Update: use of the benchmark dose
approach in risk assessment. EFSA Journal, 15(1), e04658.

Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T,
Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018). DRomics: a
turnkey tool to support the use of the dose-response framework for omics
data in ecological risk assessment. Environmental science &
technology.[doi:10.1021/acs.est.8b04752](https://doi.org/10.1021/acs.est.8b04752)

## Author

Marie-Laure Delignette-Muller and Elise Billoir

## Examples

``` r
# (1) a toy example (a very small subsample of a microarray data set) 
#
datafilename <- system.file("extdata", "transcripto_very_small_sample.txt", package="DRomics")

# to test the package on a small (for a quick calculation) but not very small data set
# use the following commented line
# datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

(o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
#> Just wait, the normalization using cyclicloess may take a few minutes.
#> Elements of the experimental design in order to check the coding of the data:
#> Tested doses and number of replicates for each dose:
#> 
#>     0  0.69 1.223 2.148 3.774 6.631 
#>     5     5     5     5     5     5 
#> Number of items: 100 
#> Identifiers of the first 20 items:
#>  [1] "1"    "2"    "3"    "4"    "5.1"  "6.1"  "7.1"  "8.1"  "9.1"  "10.1"
#> [11] "11.1" "12.1" "13.1" "14.1" "15"   "16.1" "17.1" "18.1" "19.1" "20.1"
#> Data were normalized between arrays using the following method: cyclicloess 
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
#> Removing intercept from test coefficients
#> Number of selected items using a quadratic trend test with an FDR of 0.01: 17
#> Identifiers of the responsive items:
#>  [1] "15"   "12.1" "27.1" "25.1" "4"    "70"   "7.1"  "88.1" "92"   "81"  
#> [11] "13.1" "74.1" "83.1" "84.1" "54.1" "85.1" "67.1"
(f <- drcfit(s_quad, progressbar = TRUE))
#> The fitting may be long if the number of selected items is high.
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   6%  |                                                                              |========                                                              |  12%  |                                                                              |============                                                          |  18%  |                                                                              |================                                                      |  24%  |                                                                              |=====================                                                 |  29%  |                                                                              |=========================                                             |  35%  |                                                                              |=============================                                         |  41%  |                                                                              |=================================                                     |  47%  |                                                                              |=====================================                                 |  53%  |                                                                              |=========================================                             |  59%  |                                                                              |=============================================                         |  65%  |                                                                              |=================================================                     |  71%  |                                                                              |======================================================                |  76%  |                                                                              |==========================================================            |  82%  |                                                                              |==============================================================        |  88%  |                                                                              |==================================================================    |  94%  |                                                                              |======================================================================| 100%
#> Results of the fitting using the AICc to select the best fit model
#> Distribution of the chosen models among the 17 fitted dose-response curves:
#> 
#>             Hill           linear      exponential     Gauss-probit 
#>                0                3                6                7 
#> log-Gauss-probit 
#>                1 
#> Distribution of the trends (curve shapes) among the 17 fitted dose-response curves:
#> 
#>    U bell  dec  inc 
#>    5    3    6    3 
(r <- bmdcalc(f))
#> 9 BMD-xfold values and 0 BMD-zSD values could not be calculated (coded 
#> NA as the BMR stands within the range of response values defined by the 
#> model but outside the range of tested doses).
plot(r) 


# \donttest{
# same plot in raw scale of BMD (without log transformation of BMD values)
plot(r, BMD_log_transfo = FALSE) 


# changing the values of z and x for BMD calculation
(rb <- bmdcalc(f, z = 2, x = 50))
#> 2 BMD-xfold values and 0 BMD-zSD values are not defined (coded NaN as 
#> the BMR stands outside the range of response values defined by the model).
#> 13 BMD-xfold values and 2 BMD-zSD values could not be calculated (coded 
#> NA as the BMR stands within the range of response values defined by the 
#> model but outside the range of tested doses).
plot(rb)
#> Warning: 
#> 2 BMD coded NA or NaN were removed before plotting.


# }

# Plot of fits with BMD values 
# \donttest{
# example with the BMD-1SD
plot(f, BMDoutput = r, BMDtype = "zSD")
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.


# example with the BMD-2SD
plot(f, BMDoutput = rb, BMDtype = "zSD")
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_vline()`).


# example with the BMD-xfold with x = 10 percent
plot(f, BMDoutput = r, BMDtype = "xfold")
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.
#> Warning: log-10 transformation introduced infinite values.
#> Warning: Removed 9 rows containing missing values or values outside the scale range
#> (`geom_vline()`).

# }

# (2) an example on a microarray data set (a subsample of a greater data set) 
#
# \donttest{
datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

# to test the package on a small (for a quick calculation) but not very small data set
# use the following commented line
# datafilename <- system.file("extdata", "transcripto_sample.txt", package="DRomics")

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
(s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.01))
#> Removing intercept from test coefficients
#> Number of selected items using a quadratic trend test with an FDR of 0.01: 183
#> Identifiers of the first 20 most responsive items:
#>  [1] "384.2" "383.1" "383.2" "384.1" "301.1" "363.1" "300.2" "364.2" "364.1"
#> [10] "363.2" "301.2" "300.1" "351.1" "350.2" "239.1" "240.1" "240.2" "370"  
#> [19] "15"    "350.1"
(f <- drcfit(s_quad, progressbar = TRUE))
#> The fitting may be long if the number of selected items is high.
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%  |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |========================                                              |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |=================================                                     |  48%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |=====================================                                 |  54%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  57%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |==========================================                            |  61%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |==============================================                        |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%  |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |======================================================                |  78%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |===========================================================           |  85%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |===============================================================       |  91%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |====================================================================  |  98%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%
#> Results of the fitting using the AICc to select the best fit model
#> 30 dose-response curves out of 183 previously selected were removed 
#> because no model could be fitted reliably.
#> Distribution of the chosen models among the 153 fitted dose-response curves:
#> 
#>             Hill           linear      exponential     Gauss-probit 
#>                1               44               44               59 
#> log-Gauss-probit 
#>                5 
#> Distribution of the trends (curve shapes) among the 153 fitted dose-response curves:
#> 
#>    U bell  dec  inc 
#>   28   36   40   49 
(r <- bmdcalc(f))
#> 2 BMD-xfold values and 0 BMD-zSD values are not defined (coded NaN as 
#> the BMR stands outside the range of response values defined by the model).
#> 82 BMD-xfold values and 0 BMD-zSD values could not be calculated (coded 
#> NA as the BMR stands within the range of response values defined by the 
#> model but outside the range of tested doses).
plot(r) 


# different plots of BMD-zSD

plot(r, plottype = "hist") 

plot(r, plottype = "density") 

plot(r, plottype = "density", by = "trend") 

plot(r, plottype = "ecdf", by = "trend") 

plot(r, plottype = "ecdf", by = "model") 

plot(r, plottype = "ecdf", by = "typology") 


# a plot of BMD-xfold (by default BMD-zSD is plotted)
plot(r, BMDtype = "xfold", plottype = "hist", by = "typology", hist.bins = 10) 
#> Warning: 
#> 84 BMD coded NA or NaN were removed before plotting.

# }
```
