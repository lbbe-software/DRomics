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
### import, check and optionnally normalize omic data
###
###         R functions
### 

omicdata <- function(file, check = TRUE, 
                     norm.method = c("none", "cyclicloess", "quantile", "scale"))
{
  if (check)
  {
    # check argument file
    if (!is.character(file))
      stop("The argument file must be a character string")
    le.file <- nchar(file)
    suffix <- substr(file, le.file - 3, le.file)
    if (suffix != ".txt")
      stop("The argument file must be a character string ending by .txt")
  }
  
  d <- read.table(file, header = FALSE)
  nrowd <- nrow(d)
  ncold <- ncol(d)
  data <- as.matrix(d[2:nrowd, 2:ncold]) 

  if (check)
  {
    # check that doses and responses are numeric
    if (!is.numeric(as.matrix(d[,2:ncold])))
      stop("All the columns except the first one must be numeric with the numeric 
           dose in the firt line and the numeric response of each item in the other
           lines.")
  }

  # Normalization using limma
  norm.method <- match.arg(norm.method, c("none", "cyclicloess", "quantile", "scale"))
  if(norm.method == "cyclicloess")
    cat("Just wait, the normalization using cyclicloess may take a few minutes.\n")
  data <- normalizeBetweenArrays(data, method = norm.method)  

  # definition of doses and item identifiers
  (dose <- as.vector(unlist(d[1, 2:ncold])))
  row.names(data) <- item <- as.character(d[2:nrowd, 1])
  (nitems <- nrow(data))
  
  # control of the design
  design <- table(dose, dnn = "")
  
  calcmean <- function(i)
  {
    tapply(data[i,], as.factor(dose), mean)
  }
  s <- sapply(1:(nrowd - 1), calcmean)
  data.mean <- as.matrix(t(s))
  
  reslist <- list(data = data, dose = dose, item = item, 
                  design = design, data.mean = data.mean, 
                  norm.method = norm.method)  
  
  return(structure(reslist, class = "omicdata"))
}


print.omicdata <- function(x, ...)
{
    if (!inherits(x, "omicdata"))
        stop("Use only with 'omicdata' objects")
  
    cat("Elements of the experimental design in order to check the coding of the data :\n")
    cat("Tested doses and number of replicates for each dose:\n")
    print(x$design)
    ########################################################## comment virer dose en titre ?
    cat("Number of items: ", length(x$item),"\n")
    
    if (length(x$item) > 20)
    {
      cat("Identifiers of the first 20 items:\n")
      print(x$item[1:20])
    } else
    {
      cat("Identifiers of the items:\n")
      print(x$item)
    }
    if (x$norm.method != "none")
      cat("Data were normalized between arrays using the following method: ", x$norm.method," \n")
}



