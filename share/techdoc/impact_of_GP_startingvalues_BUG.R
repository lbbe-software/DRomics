Results before the correction of the BUG in starting values of e for the GP model.extract(_______________________________________________________________________________________)
> datafilename <- system.file("extdata", "transcripto_sample.txt", package = "DRomics")
> 
  > (o <- microarraydata(datafilename, check = TRUE, norm.method = "cyclicloess"))
Just wait, the normalization using cyclicloess may take a few minutes.
Elements of the experimental design in order to check the coding of the data:
  Tested doses and number of replicates for each dose:
  
  0  0.69 1.223 2.148 3.774 6.631 
5     5     5     5     5     5 
Number of items: 1000 
Identifiers of the first 20 items:
  [1] "1"    "2"    "3"    "4"    "5.1"  "5.2"  "6.1"  "6.2"  "7.1"  "7.2"  "8.1"  "8.2"  "9.1" 
[14] "9.2"  "10.1" "10.2" "11.1" "11.2" "12.1" "12.2"
Data were normalized between arrays using the following method: cyclicloess 
> (s_quad <- itemselect(o, select.method = "quadratic", FDR = 0.05))
Removing intercept from test coefficients
Number of selected items using a quadratic trend test with an FDR of 0.05: 318
Identifiers of the first 20 most responsive items:
  [1] "384.2" "383.1" "383.2" "384.1" "301.1" "363.1" "300.2" "364.2" "364.1" "363.2" "301.2" "300.1"
[13] "351.1" "350.2" "239.1" "240.1" "240.2" "370"   "15"    "350.1"
> (f <- drcfit(s_quad, progressbar = TRUE))
The fitting may be long if the number of selected items is high.
|===========================================================================================| 100%
Results of the fitting using the AICc to select the best fit model
60 dose-response curves out of 318 previously selected were removed because no model 
could be fitted reliably.
Distribution of the chosen models among the 258 fitted dose-response curves:
  
  Hill           linear      exponential     Gauss-probit log-Gauss-probit 
2               85               64               89               18 
Distribution of the trends (curve shapes) among the 258 fitted dose-response curves:
  
  bell  dec  inc    U 
52   59   92   55 

Results after the correction of the BUG in starting values of e for the GP model.extract(_______________________________________________________________________________________)
