######################################################################################
#   Copyright (c) 2018 Marie Laure Delignette-Muller, Elise Billoir, Floriane Larras,
#   Aurelie Siberchicot
#                                                                                                                                                                        
#   This program is free software; you can redistribute it and/or modify                                               
#   it under the terms of the GNU General Public License as published by                                         
#   the Free Software Foundation; either version 2 of the License, or                                                   
#   (at your option) any later version.                                                                                                            
#                                                                                                                                                                         
#   This program is distributed in the hope that it will be useful,                                                             
#   but WITHOUT ANY WARRANTY; without even the implied warranty of                                          
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                                 
#   GNU General Public License for more details.                                                                                    
#                                                                                                                                                                         
#   You should have received a copy of the GNU General Public License                                           
#   along with this program; if not, write to the                                                                                           
#   Free Software Foundation, Inc.,                                                                                                              
#   59 Temple Place, Suite 330, Boston, MA 02111-1307, USA                                                             
#                                                                                                                                                                         
#####################################################################################
# internally used function (to place elsewhere)
calcBMD <- function(y0, delta, xext, yext, dosemax, ydosemax, func, b, c, d, e, g){
  BMD <- NA
  if(g > 0){ # Umbrella shape
    threshold <- y0 + delta # we first seek the BMR above d
    if(y0 < threshold & threshold < yext){
      BMD <- uniroot(func, interval=c(0,xext), b=b, c=c, d=d, e=e, g=g, threshold=threshold)$root
    }
    else  {
      threshold <- y0 - delta
      if(c < y0 & threshold > ydosemax){  # then we seek the BMR below y0 (possible only if c<y0) 
        BMD <- uniroot(func, interval=c(xext, dosemax), b=b, c=c, d=d, e=e, g=g, threshold=threshold)$root
      }
    }
  }
  if(g < 0){ # U shape
    threshold <- y0 - delta # we first seek the BMR below y0
    if(y0 > threshold & threshold > yext){
      BMD <- uniroot(func, interval=c(0,xext), b=b, c=c, d=d, e=e, g=g, threshold=threshold)$root
    }
    else  {
      threshold <- y0 + delta
      if(c > y0 & threshold < ydosemax){  # then we seek the BMR above y0 (possible only if c>y0) 
        BMD <- uniroot(func, interval=c(xext, dosemax), b=b, c=c, d=d, e=e, g=g, threshold=threshold)$root
      }
    }
  }
  return(list(threshold=threshold, BMD=BMD))
}

### Calculation of BMD values (x-fold or z-SD) from fitted dose-response curves
###
###         R functions
### 
bmdcalc <- function(f, z = 1, x = 10)
{
  # Checks
  if (!inherits(f, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  dfitall <- f$fitres
  nselect <- length(dfitall$irow)
  dosemax <- max(f$omicdata$dose) 
  dcalc <- data.frame(xextrem = rep(NA,nselect), 
                      yextrem = rep(NA,nselect), 
                      y0 = rep(NA, nselect),
                      ydosemax = rep(NA, nselect),
                      yp = rep(NA,nselect), 
                      ysd = rep(NA,nselect),
                      BMDp = rep(NA,nselect), 
                      BMDsd = rep(NA,nselect)
  )
  
  for(i in 1:nselect)
  {
    b <- dfitall$b[i]
    c <- dfitall$c[i]
    d <- dfitall$d[i]
    e <- dfitall$e[i]
    g <- dfitall$f[i] # f is renamed g
    
    modeli <- dfitall$model[i]
    if(modeli == "linear") {
      #print("lin")
      y0 <- dcalc$y0[i] <- flin(x=0, b=b, d=d)
      ydosemax <- dcalc$ydosemax[i] <- flin(x=dosemax, b=b, d=d)
      dcalc$yp[i] <- y0 * ( 1 + x/100*sign(b))
      dcalc$BMDp[i] <- invlin(dcalc$yp[i], b, d)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(b)
      dcalc$BMDsd[i] <- invlin(dcalc$ysd[i], b, d)
    } 
    
    if(modeli == "exponential") {
      #print("Expo")
      y0 <- dcalc$y0[i] <- fExpo(x=0, b=b, d=d, e=e)
      ydosemax <- dcalc$ydosemax[i] <- fExpo(x=dosemax, b=b, d=d, e=e)
      dcalc$yp[i] <- y0 * ( 1 + x/100*sign(e*b))
      dcalc$BMDp[i] <- invExpo(dcalc$yp[i], b, d, e)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(e*b)
      dcalc$BMDsd[i] <- invExpo(dcalc$ysd[i], b, d, e)
    } else
    if(modeli == "Hill") {
      #print("Hill")
      y0 <- dcalc$y0[i] <- fHill(x=0, b=b, c=c, d=d, e=e)
      ydosemax <- dcalc$ydosemax[i] <- fHill(x=dosemax, b=b, c=c, d=d, e=e)
      dcalc$yp[i] <- y0 * ( 1 + x/100*sign(c - d))
      dcalc$BMDp[i] <- invHill(dcalc$yp[i], b, c, d, e)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(c - d)
      dcalc$BMDsd[i] <- invHill(dcalc$ysd[i], b, c, d, e)
    } else
    if(modeli == "log-probit") {
      #print("Lprobit")
      y0 <- dcalc$y0[i] <- fLprobit(x=0, b=b, c=c, d=d, e=e)
      ydosemax <- dcalc$ydosemax[i] <- fHill(x=dosemax, b=b, c=c, d=d, e=e)
      dcalc$yp[i] <- y0 * ( 1 + x/100*sign(c - d))
      dcalc$BMDp[i] <- invLprobit(dcalc$yp[i], b, c, d, e)
      dcalc$ysd[i] <- y0 + z*dfitall$SDres[i]*sign(c - d)
      dcalc$BMDsd[i] <- invLprobit(dcalc$ysd[i], b, c, d, e)
    } else
    if(modeli == "Gauss-probit") {
      #print("Gauss")
      xext <- dcalc$xextrem[i] <- e + (c - d)*b/(g*sqrt(2*pi)) # g is renamed f
      yext <- dcalc$yextrem[i] <- fGauss5p(xext, b=b, c=c, d=d, e=e, f=g)
      y0 <- dcalc$y0[i] <- fGauss5p(x=0, b=b, c=c, d=d, e=e, f=g)
      ydosemax <- dcalc$ydosemax[i] <- fGauss5p(x=dosemax, b=b, c=c, d=d, e=e, f=g)
      
      deltap <- y0 * x/100
      deltasd <- z * dfitall$SDres[i]
      
      resBMDp <- calcBMD(y0=y0, delta=deltap, xext=xext, yext=yext, dosemax=dosemax, 
                         ydosemax=ydosemax, func=fGauss5pBMR, b=b, c=c, d=d, e=e, g=g)
      dcalc$yp[i] <- resBMDp$threshold
      dcalc$BMDp[i] <- resBMDp$BMD
      
      resBMDsd <- calcBMD(y0=y0, delta=deltasd, xext=xext, yext=yext, dosemax=dosemax, 
                          ydosemax=ydosemax, func=fGauss5pBMR, b=b, c=c, d=d, e=e, g=g)
      dcalc$ysd[i] <- resBMDsd$threshold
      dcalc$BMDsd[i] <- resBMDsd$BMD
    } else
    if(modeli == "log-Gauss-probit") {
      xext <- dcalc$xextrem[i] <- exp(log(e) + (c - d)*b/(g*sqrt(2*pi)))# g is renamed f
      yext <- dcalc$yextrem[i] <- fLGauss5p(xext, b=b, c=c, d=d, e=e, f=g)
      y0 <- dcalc$y0[i] <- fLGauss5p(x=0, b=b, c=c, d=d, e=e, f=g)
      ydosemax <- dcalc$ydosemax[i] <- fLGauss5p(x=dosemax, b=b, c=c, d=d, e=e, f=g)
      deltap <- y0 * x/100
      deltasd <- z * dfitall$SDres[i]
      
      resBMDp <- calcBMD(y0=y0, delta=deltap, xext=xext, yext=yext, dosemax=dosemax, ydosemax=ydosemax, func=fLGauss5pBMR, b=b, c=c, d=d, e=e, g=g)
      dcalc$yp[i] <- resBMDp$threshold
      dcalc$BMDp[i] <- resBMDp$BMD
      
      resBMDsd <- calcBMD(y0=y0, delta=deltasd, xext=xext, yext=yext, dosemax=dosemax, ydosemax=ydosemax, func=fLGauss5pBMR, b=b, c=c, d=d, e=e, g=g)
      dcalc$ysd[i] <- resBMDsd$threshold
      dcalc$BMDsd[i] <- resBMDsd$BMD
    }
  }
  dcalc$BMDp[dcalc$BMDp > dosemax] <- NA
  dcalc$BMDsd[dcalc$BMDsd > dosemax] <- NA
  
  reslist <- list(res = as.data.frame(cbind(dfitall,
                                            data.frame(BMD.zSD = dcalc$BMDsd, BMD.xfold = dcalc$BMDp))), 
                  z = z, x = x, omicdata = f$omicdata) 
  
  return(structure(reslist, class = "bmdcalc"))
  
}


print.bmdcalc <- function(x, ...) 
{
  if (!inherits(x, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects")
  
  # count of cases where BMD cannot be reached 
  # being outside the range of response values defined by the model
  nNaN.BMD.zSD <- sum(is.nan(x$res$BMD.zSD))
  nNaN.BMD.xfold <- sum(is.nan(x$res$BMD.xfold))
  if ((nNaN.BMD.zSD > 0) |  (nNaN.BMD.xfold > 0))
    cat(nNaN.BMD.xfold,"BMD-xfold values and ", nNaN.BMD.zSD,
        " BMD-zSD values are not defined 
        (coded NaN as the BMR stands outside the range of response values 
        defined by the model).\n")
  
  # count of cases where BMD is not yet reached at the highest tested dose
  nNA.BMD.zSD <- sum(is.na(x$res$BMD.zSD) & !is.nan(x$res$BMD.zSD))
  nNA.BMD.xfold <- sum(is.na(x$res$BMD.xfold) & !is.nan(x$res$BMD.xfold))
  if ((nNA.BMD.zSD > 0) |  (nNA.BMD.xfold > 0))
    cat(nNA.BMD.xfold,"BMD-xfold values and ", nNA.BMD.zSD,
        " BMD-zSD values could not be calculated 
        (coded NA as the BMR stands within the range of response values defined by the model 
        but outside the range of tested doses).\n")
  
  if ((nNA.BMD.zSD = 0) &  (nNA.BMD.xfold = 0) & (nNaN.BMD.zSD = 0) &  (nNaN.BMD.xfold = 0))
    cat("BMD-xfold and BMD-SD values could be calculated on all the curves
        (the BMR always stands within the range of response values defined by the model
        and within the range of tested doses).\n")
}

plot.bmdcalc <- function(x, BMDtype = c("zSD", "xfold"), 
                         plottype = c("ecdf", "hist", "density"), bytypology = FALSE, 
                         hist.bins = 30, ...) 
{
  if (!inherits(x, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects")
  BMDtype <- match.arg(BMDtype, c("zSD", "xfold"))
  plottype <- match.arg(plottype, c("ecdf", "hist", "density"))  
  
  # que faire des NA et NaN (enlever, les représenter en données censurées ?)
  
  if (BMDtype == "zSD")
  {
    dwithNANaN <- data.frame(BMD = x$res$BMD.zSD, typology = x$res$typology)
  } else
  {
    dwithNANaN <- data.frame(BMD = x$res$BMD.xfold, typology = x$res$typology)
  }
  
  # Remove NA and NaN values if needed
  d <- dwithNANaN[!is.na(dwithNANaN$BMD) & !is.nan(dwithNANaN$BMD), ]
  nremoved <- nrow(dwithNANaN) - nrow(d)
  if (nremoved > 0)
    warning(nremoved," BMD coded NA or NaN were removed before plotting")
  
  if (bytypology) # distribution of BMDs by typology of curves
  {
    if (plottype == "hist") 
    {
      g <- ggplot(data = d, mapping = aes_(x = quote(BMD), fill = quote(typology))) +
        geom_histogram(bins = hist.bins) + facet_wrap(~ typology)
    } else
      if (plottype == "density") 
      {
        g <- ggplot(data = d, mapping = aes_(x = quote(BMD), fill = quote(typology))) + 
          geom_density() + facet_wrap(~ typology)
      } else
        if (plottype == "ecdf") 
        {
          g <- ggplot(data = d, mapping = aes_(x = quote(BMD), col = quote(typology))) +
            stat_ecdf(geom = "step") + facet_wrap(~ typology) + ylab("ECDF")
        }      
  }  else
  { # global distribution of BMDs
    if (plottype == "hist") 
    {
      g <- ggplot(data = d, mapping = aes_(x = quote(BMD))) +
        geom_histogram(bins = hist.bins) 
    } else
      if (plottype == "density") 
      {
        g <- ggplot(data = d, mapping = aes_(x = quote(BMD))) + geom_density(fill = I("grey"))
      } else
        if (plottype == "ecdf") 
        {
          g <- ggplot(data = d, mapping = aes_(x = quote(BMD))) +
            stat_ecdf(geom = "step") + ylab("ECDF")
        }      
  } 
  return(g)
}

