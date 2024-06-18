# Corresponding data are stored in SeaFile
# Outputs correspond to tests made on an IFB VM with 16 cores
###########################################################
# Test of bootstrap of the version of DRomics in development
# installation des packages
BiocManager::install(c("limma", "DESeq2"))

if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")
remotes::install_github("lbbe-software/DRomics")
# remotes::install_github("lbbe-software/DRomics", force = TRUE)

# version on CRAN
# install.packages("DRomics")


################################
require(DRomics)
require(ggplot2)

setwd("~/mydatalocal/tests_big_data")

###############################
# Example Estelle Dubreil
##############################

data <- read.table("allCompounds_pourDRomics_HELIO_ENDO-POS.txt", header = FALSE)
set.seed(1234)

#step1
(o.metabolo <- continuousomicdata(data))
plot(o.metabolo)
str(o.metabolo)
PCAdataplot(o.metabolo, label = TRUE)

##ML Removing of V30 if there is no mean to normalize before
datainit <- as.matrix(o.metabolo$data)
head(datainit)
dose <- o.metabolo$dose
dose2 <- dose[colnames(datainit) != "V30"]
data2 <- formatdata4DRomics(
  signalmatrix = datainit[, colnames(datainit) != "V30"],
  dose = dose2)
(o.metabolo2 <- continuousomicdata(data2))
plot(o.metabolo2)
PCAdataplot(o.metabolo2, label = TRUE)

#step2
(s_quad <- itemselect(o.metabolo2, select.method = "quadratic",
                      FDR = 0.05))
#step3
system.time(f <- drcfit(s_quad, progressbar = TRUE))
# system.time(f <- drcfit(s_quad, parallel = "snow", ncpus = 15)) # fonctionne mais pas rentable sur fit
f

#step4
(r <- bmdcalc(f, z = 1, x = 10))

# system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(b, file = "bootres_ED.rds")

# > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 4.910   2.723 851.309
# > b
# Bootstrap confidence interval computation failed on 143 items among 1744 due to 
# lack of convergence of the model fit for a fraction of the bootstrapped samples 
# greater than 0.5.
# For 580 BMD.zSD values and 1474 BMD.xfold values among 1744 at least one bound of 
# the 95 percent confidence interval could not be computed due to some bootstrapped 
# BMD values not reachable due to model asymptotes or reached outside the range of 
# tested doses (bounds coded Inf)).

#############################################################
# Exemple proteo Sandrine Frelon Hyla Orientalis filtre 100
#############################################################
set.seed(1234)

data_file_name <- "proteo_data_filtre100.txt"

remove_GH <- TRUE
select.method <- "quadratic"
proteo_data <- read.table(data_file_name, header = FALSE)

if (remove_GH)
{
  proteo_data <- proteo_data[, proteo_data[1,] != 0]
  (o <- continuousomicdata(proteo_data, backgrounddose = 0.003))
}else
{
  (o <- continuousomicdata(proteo_data))
}

plot(o)

(s <- itemselect(o, select.method = select.method))
(f <- drcfit(s))
(r <- bmdcalc(f))
# system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_filtre100.rds")
saveRDS(b, file = "bootres_filtre100.rds")

# Results of the fitting using the AICc to select the best fit model
# 33 dose-response curves out of 397 previously selected were removed because no 
# model could be fitted reliably.
# Distribution of the chosen models among the 364 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 7               57               46              111              143 
# Distribution of the trends (curve shapes) among the 364 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 100   52   72  140 
# > (r <- bmdcalc(f))
# 61 BMD-xfold values and 8 BMD-zSD values are not defined (coded NaN as the BMR 
#                                                           stands outside the range of response values defined by the model).
# 301 BMD-xfold values and 10 BMD-zSD values could not be calculated (coded NA as the 
#                                                                     BMR stands within the range of response values defined by the model but outside the 
#                                                                     range of tested doses).
# > # system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
#   > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 1.951   1.030 316.145 
# > b
# Bootstrap confidence interval computation failed on 20 items among 364 due to lack 
# of convergence of the model fit for a fraction of the bootstrapped samples greater than 0.5.
# For 142 BMD.zSD values and 344 BMD.xfold values among 364 at least one bound of the 
# 95 percent confidence interval could not be computed due to some bootstrapped BMD 
# values not reachable due to model asymptotes or reached outside the range of tested 
# doses (bounds coded Inf)).

#############################################################
# Exemple proteo Sandrine Frelon Hyla Orientalis filtre 70each
#############################################################
set.seed(1234)

data_file_name <- "proteo_data_filtre70each.txt"

remove_GH <- TRUE
select.method <- "quadratic"
proteo_data <- read.table(data_file_name, header = FALSE)

if (remove_GH)
{
  proteo_data <- proteo_data[, proteo_data[1,] != 0]
  (o <- continuousomicdata(proteo_data, backgrounddose = 0.003))
}else
{
  (o <- continuousomicdata(proteo_data))
}

plot(o)

(s <- itemselect(o, select.method = select.method))
(f <- drcfit(s))
(r <- bmdcalc(f))
# system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_filtre70each.rds")
saveRDS(b, file = "bootres_filtre70each.rds")

# Results of the fitting using the AICc to select the best fit model
# 37 dose-response curves out of 436 previously selected were removed because no 
# model could be fitted reliably.
# Distribution of the chosen models among the 399 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 7               71               55              123              143 
# Distribution of the trends (curve shapes) among the 399 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 99   64   85  151 
# > (r <- bmdcalc(f))
# 70 BMD-xfold values and 8 BMD-zSD values are not defined (coded NaN as the BMR 
#                                                           stands outside the range of response values defined by the model).
# 325 BMD-xfold values and 11 BMD-zSD values could not be calculated (coded NA as the 
#                                                                     BMR stands within the range of response values defined by the model but outside the 
#                                                                     range of tested doses).
# > # system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
#   > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 2.127   0.933 311.442 
# > b
# Bootstrap confidence interval computation failed on 22 items among 399 due to lack 
# of convergence of the model fit for a fraction of the bootstrapped samples greater than 0.5.
# For 165 BMD.zSD values and 377 BMD.xfold values among 399 at least one bound of the 
# 95 percent confidence interval could not be computed due to some bootstrapped BMD 
# values not reachable due to model asymptotes or reached outside the range of tested 
# doses (bounds coded Inf)).

#############################################################
# Exemple proteo Sandrine Frelon Hyla Orientalis filtre 70tot
#############################################################
set.seed(1234)

data_file_name <- "proteo_data_filtre70tot.txt"

remove_GH <- TRUE
select.method <- "quadratic"
proteo_data <- read.table(data_file_name, header = FALSE)

if (remove_GH)
{
  proteo_data <- proteo_data[, proteo_data[1,] != 0]
  (o <- continuousomicdata(proteo_data, backgrounddose = 0.003))
}else
{
  (o <- continuousomicdata(proteo_data))
}

plot(o)

(s <- itemselect(o, select.method = select.method))
(f <- drcfit(s))
(r <- bmdcalc(f))
# system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))


b
saveRDS(f, file = "fitres_filtre70tot.rds")
saveRDS(b, file = "bootres_filtre70tot.rds")

# Results of the fitting using the AICc to select the best fit model
# 40 dose-response curves out of 499 previously selected were removed because no 
# model could be fitted reliably.
# Distribution of the chosen models among the 459 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 10               85               62              136              166 
# Distribution of the trends (curve shapes) among the 459 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 116   72  102  169 

# 78 BMD-xfold values and 7 BMD-zSD values are not defined (coded NaN as the BMR 
#                                                           stands outside the range of response values defined by the model).
# 372 BMD-xfold values and 16 BMD-zSD values could not be calculated (coded NA as the 
#                                                                     BMR stands within the range of response values defined by the model but outside the 
#                                                                     range of tested doses).


# Bootstrap confidence interval computation failed on 27 items among 459 due to lack 
# of convergence of the model fit for a fraction of the bootstrapped samples greater than 0.5.
# For 193 BMD.zSD values and 432 BMD.xfold values among 459 at least one bound of the 
# 95 percent confidence interval could not be computed due to some bootstrapped BMD 
# values not reachable due to model asymptotes or reached outside the range of tested 
# doses (bounds coded Inf)).

###################################################
# Data Zhou kidney PCE
###################################################
set.seed(1234)
dname <- "Zhou_kidney_pce.txt"
(o <- RNAseqdata(dname))
plot(o)
PCAdataplot(o)
(s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s, progressbar = TRUE))
plot(f)
plot(f, dose_log_trans = TRUE)
(r <- bmdcalc(f))
plot(r) 
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_Zhou_kidney_pce.rds")
saveRDS(b, file = "bootres_Zhou_kidney_pce.rds")

# Results of the fitting using the AICc to select the best fit model
# 25 dose-response curves out of 930 previously selected were removed because no model could be fitted reliably.
# Distribution of the chosen models among the 905 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 1              768               26               88               22 
# Distribution of the trends (curve shapes) among the 905 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 41  363  433   68 
# 
# 24 BMD-xfold values and 0 BMD-zSD values are not defined (coded NaN as the BMR stands outside the range 
#                                                           of response values defined by the model).
# 837 BMD-xfold values and 0 BMD-zSD values could not be calculated (coded NA as the BMR stands within 
#                                                                    the range of response values defined by the model but outside the range of tested doses).
# > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 2.637   1.074 259.487 
# > b
# Bootstrap confidence interval computation failed on 2 items among 905 due to lack of convergence of the 
# model fit for a fraction of the bootstrapped samples greater than 0.5.
# For 37 BMD.zSD values and 887 BMD.xfold values among 905 at least one bound of the 95 percent 
# confidence interval could not be computed due to some bootstrapped BMD values not reachable due to 
# model asymptotes or reached outside the range of tested doses (bounds coded Inf)).

###################################################
# Data Zhou kidney TCE
###################################################
set.seed(1234)
dname <- "Zhou_kidney_tce.txt"
(o <- RNAseqdata(dname))
plot(o)
PCAdataplot(o)
(s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s, progressbar = TRUE))
plot(f)
plot(f, dose_log_trans = TRUE)
(r <- bmdcalc(f))
plot(r) 
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_Zhou_kidney_tce.rds")
saveRDS(b, file = "bootres_Zhou_kidney_tce.rds")

# Results of the fitting using the AICc to select the best fit model
# 2 dose-response curves out of 70 previously selected were removed because no model could be fitted reliably.
# Distribution of the chosen models among the 68 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 0               62                4                1                1 
# Distribution of the trends (curve shapes) among the 68 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 1   26   40    1 
# 
# 4 BMD-xfold values and 0 BMD-zSD values are not defined (coded NaN as the BMR stands outside the range 
#                                                          of response values defined by the model).
# 56 BMD-xfold values and 0 BMD-zSD values could not be calculated (coded NA as the BMR stands within the 
#                                                                   range of response values defined by the model but outside the range of tested doses).
# > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 2.057   0.743 155.177 
# > b
# Bootstrap confidence interval computation was successful on 68 items among68.
# For 0 BMD.zSD values and 63 BMD.xfold values among 68 at least one bound of the 95 percent confidence 
# interval could not be computed due to some bootstrapped BMD values not reachable due to model 
# asymptotes or reached outside the range of tested doses (bounds coded Inf)).

###################################################
# Data Zhou liver PCE
###################################################
set.seed(1234)
dname <- "Zhou_liver_pce.txt"
(o <- RNAseqdata(dname))
plot(o)
PCAdataplot(o)
(s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s, progressbar = TRUE))
plot(f)
plot(f, dose_log_trans = TRUE)
(r <- bmdcalc(f))
plot(r) 
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_Zhou_liver_pce.rds")
saveRDS(b, file = "bootres_Zhou_liver_pce.rds")

# Results of the fitting using the AICc to select the best fit model
# 63 dose-response curves out of 1151 previously selected were removed because no model could be fitted reliably.
# Distribution of the chosen models among the 1088 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 2              865              153               25               43 
# Distribution of the trends (curve shapes) among the 1088 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 25  639  390   34 
# 
# 140 BMD-xfold values and 0 BMD-zSD values are not defined (coded NaN as the BMR stands outside the 
#                                                            range of response values defined by the model).
# 797 BMD-xfold values and 0 BMD-zSD values could not be calculated (coded NA as the BMR stands within 
#                                                                    the range of response values defined by the model but outside the range of tested doses).
# > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 2.442   1.202 268.700 
# > b
# Bootstrap confidence interval computation failed on 1 items among 1088 due to lack of convergence of 
# the model fit for a fraction of the bootstrapped samples greater than 0.5.
# For 30 BMD.zSD values and 1042 BMD.xfold values among 1088 at least one bound of the 95 percent 
# confidence interval could not be computed due to some bootstrapped BMD values not reachable due to 
# model asymptotes or reached outside the range of tested doses (bounds coded Inf)).

###################################################
# Data Zhou liver TCE
###################################################
set.seed(1234)
dname <- "Zhou_liver_tce.txt"
(o <- RNAseqdata(dname))
plot(o)
PCAdataplot(o)
(s <- itemselect(o, select.method = "quadratic", FDR = 0.01))
(f <- drcfit(s, progressbar = TRUE))
plot(f)
plot(f, dose_log_trans = TRUE)
(r <- bmdcalc(f))
plot(r) 
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_Zhou_liver_tce.rds")
saveRDS(b, file = "bootres_Zhou_liver_tce.rds")

# Results of the fitting using the AICc to select the best fit model
# 4 dose-response curves out of 146 previously selected were removed because no model could be fitted reliably.
# Distribution of the chosen models among the 142 fitted dose-response curves:
#   
#   Hill           linear      exponential     Gauss-probit log-Gauss-probit 
# 0               73               56                5                8 
# Distribution of the trends (curve shapes) among the 142 fitted dose-response curves:
#   
#   bell  dec  inc    U 
# 7   60   70    5 
# 
# 45 BMD-xfold values and 0 BMD-zSD values are not defined (coded NaN as the BMR stands outside the range 
#                                                           of response values defined by the model).
# 57 BMD-xfold values and 0 BMD-zSD values could not be calculated (coded NA as the BMR stands within the 
#                                                                   range of response values defined by the model but outside the range of tested doses).
# > system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
# user  system elapsed 
# 2.130   0.760 177.221 
# > b
# Bootstrap confidence interval computation was successful on 142 items among142.
# For 2 BMD.zSD values and 121 BMD.xfold values among 142 at least one bound of the 95 percent confidence 
# interval could not be computed due to some bootstrapped BMD values not reachable due to model 
# asymptotes or reached outside the range of tested doses (bounds coded Inf)).


####################################
# Data Sophie M2 Ellis Danio rerio
####################################
(o <- RNAseqdata("zebrafish_phtalate.txt", transfo.method = 'rlog', round.counts = TRUE)) 
plot(o)
PCAdataplot(o)
(s <- itemselect(o, select.method = select.method))
(f <- drcfit(s))
(r <- bmdcalc(f))
# system.time(b <- bmdboot(r, niter = 1000, progressbar = TRUE))
system.time(b <- bmdboot(r, niter = 1000, parallel = "snow", ncpus = 15))
b
saveRDS(f, file = "fitres_zebrafish_phtalate.rds")
saveRDS(b, file = "bootres_zebrafish_phtalate.rds")