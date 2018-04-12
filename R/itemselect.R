######################################################################################
#   Copyright (c) 2018 Marie Laure Delignette-Muller, Elise Billoir, Floriane Larras                                                                                                  
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
### select significantly responsive items 
###
###         R functions
### 
itemselect <- function(omicdata, select.method = c("quadratic", "linear", "ANOVA"), FDR = 0.05)
{
  # Checks
  if (!inherits(omicdata, "omicdata"))
    stop("Use only with 'omicdata' objects, created with the function omicdata")
  select.method <- match.arg(select.method, c("quadratic", "linear", "ANOVA"))
  if (!is.numeric(FDR))
    stop("FDR, the false discovery rate, must a number in ]0; 1[ (generally under 0.1).")
  if ((FDR <=0) | (FDR >=1))
    stop("FDR, the false discovery rate, must in ]0; 1[.")
  
  data <- omicdata$data
  item <- omicdata$item
  dose <- omicdata$dose
  doseranks <- as.numeric(as.factor(dose))
  irow <- 1:length(item)
  
  if (select.method == "quadratic")
  {
    doseranks2 <- doseranks*doseranks
    design4lmFit <- model.matrix(~ doseranks + doseranks2)
  } else
    if (select.method == "linear")
    {
      design4lmFit <- model.matrix(~ doseranks)
    } else
      if (select.method == "ANOVA")
      {
        fdose <- as.factor(dose)
        design4lmFit <- model.matrix(~ fdose)
      } 
  
  # Selection using limma    
  fit <- lmFit(data, design4lmFit)
  fitBayes <- eBayes(fit)
  # all adjusted pvalues without sorting
  res <- topTable(fitBayes, adjust.method = "BH", number = nrow(data), sort.by = "none") 
  
  selectindexnonsorted <- irow[res$adj.P.Val < FDR]
  adjpvaluenonsorted <- res$adj.P.Val[selectindexnonsorted]
  irowsortedbypvalue <- order(adjpvaluenonsorted, decreasing = FALSE)
  selectindex <- selectindexnonsorted[irowsortedbypvalue]
  adjpvalue <- adjpvaluenonsorted[irowsortedbypvalue]
  
  reslist <- list(adjpvalue = adjpvalue, selectindex = selectindex, 
                  omicdata = omicdata, select.method = select.method, FDR = FDR)  
  
  return(structure(reslist, class = "itemselect"))
}

print.itemselect <- function(x, ...)
{
  if (!inherits(x, "itemselect"))
    stop("Use only with 'itemselect' objects")
  
  if (x$select.method == "ANOVA")
  {
    cat("Number of selected items using an ANOVA type test with an FDR of ",x$FDR,": ", length(x$selectindex),"\n")
  } else
    if (x$select.method == "linear")
    {
      cat("Number of selected items using a linear trend test with an FDR of ",x$FDR,": ", length(x$selectindex),"\n")
    } else
      if (x$select.method == "quadratic")
      {
        cat("Number of selected items using a quadratic trend test with an FDR of ",x$FDR,": ", length(x$selectindex),"\n")
      } 
  
  if (length(x$selectindex) > 20) 
  {
    cat("Identifiers of the first 20 most responsive items:\n")
    print(x$omicdata$item[x$selectindex[1:20]])
  } else
  {
    cat("Identifiers of the responsive items:\n")
    print(x$omicdata$item[x$selectindex])
  }
}



