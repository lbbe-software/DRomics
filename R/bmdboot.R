## Perform bootstrap on a selection of items in order to give
## a confidence interval for the BMD values
bmdboot(r, items, niter = 99)
{
  dose <- r$omicdata$dose
  formlist <- list(Exp = formExp3p, 
                   Gauss4p = formGauss4p,  
                   Gauss5p = formGauss5p, 
                   Hill = formHill,
                   Lin = as.formula(signal ~ d + b * dose),
                   LGauss4p = formLGauss4p,
                   LGauss5p = formLGauss5p, 
                   Lprobit = formLprobit)
  # formlist and modelnbpar MUST BE IN THE SAME ORDER
  modelnbpar <- c("exponential3", "Gauss-probit4", "Gauss-probit5", "Hill4", "linear2", 
                  "log-Gauss-probit4", "log-Gauss-probit5", "log-probit4")
  
  ### First trial on one item (assuming items is one id)
  resitem <- r$res[r$res$id == items, ]
  # parameter estimates as a named list for starting values
  estimpar <- unlist(resitem[c("b","c","d","e","f")])
  lestimpar <- as.list(estimpar[!is.na(estimpar)])
  
  # formula of the model
  modelnamei <- as.character(unlist(resitem["model"]))
  nbpari <- as.character(unlist(resitem["nbpar"]))
  modelnbpari <- paste(modelnamei, nbpari, sep = "")
  formi <- formlist[[match(modelnbpari, modelnbpar)]]
  
  # dataset
  datai <- r$omicdata$data[resitem$irow, ]
  dset <- data.frame(signal = datai, dose = dose)
  # plot(dset$dose, dset$signal)
  # fit with port as in drcfit but without lower and upper bounds
  nlsi <- nls(formula = formi, data = dset, start = lestimpar, algorithm = "port") 
#  nlsi <- try(nls(formula = formi, data = dset, start = lestimpar, algorithm = "port"), silent=TRUE) 

    if (inherits(nlsi, "nls"))
  {
    # code of nlstools
    data2 <- eval(nlsi$data, sys.frame(0))
    fitted1 <- fitted(nlsi)
    resid1 <- resid(nlsi)
    var1 <- all.vars(formula(nlsi)[[2]])
    l1 <- lapply(1:niter, function(i){
      data2[,var1] <- fitted1 + sample(scale(resid1, scale=FALSE), replace=TRUE);
      nls2 <- try(update(nlsi, start=as.list(coef(nlsi)), data=data2), silent=TRUE);
      if(inherits(nls2, "nls"))
        return(list(coef=coef(nls2), rse=summary(nls2)$sigma))
    })
    if(sum(sapply(l1, is.null)) > niter/2) stop(paste("Procedure aborted: the fit only converged in", round(sum(sapply(l1, is.null))/niter), "% during bootstrapping"))
    
    tabbooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$coef)
    rsebooti <- sapply(l1[!sapply(l1, is.null)], function(z) z$rse)
    
    pairs(as.data.frame(t(tabbooti)))
    
  }
  
}

# test
source("util-basicandfitfunc.R")
items <- "384.2" # GP
items <- "363.1" # expo
