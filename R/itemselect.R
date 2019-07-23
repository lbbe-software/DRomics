### select significantly responsive items 
itemselect <- function(omicdata, select.method = c("quadratic", "linear", "ANOVA"), FDR = 0.05, 
                       max.ties.prop = 0.2)
{
  # Checks
  if (!(inherits(omicdata, "microarraydata") | 
        inherits(omicdata, "RNAseqdata") |
        inherits(omicdata, "metabolomicdata")))
    stop("Use only with 'microarraydata', 'RNAseqdata' or 'metabolomicdata' objects, respectively
         created with functions 'microarraydata', 'RNAseqdata' or 'metabolomicdata' ")
  select.method <- match.arg(select.method, c("quadratic", "linear", "ANOVA"))
  if (!is.numeric(FDR))
    stop("FDR, the false discovery rate, must a number in ]0; 1[ (generally under 0.1).")
  if ((FDR <=0) | (FDR >=1))
    stop("FDR, the false discovery rate, must in ]0; 1[.")
  if ((max.ties.prop <=0) | (max.ties.prop >0.5))
    stop("max.ties.prop, the maximal tolerated proportion of tied values
         per item, must in ]0; 0.5].")
  
  item <- omicdata$item
  dose <- omicdata$dose
  fdose <- as.factor(dose)
  doseranks <- as.numeric(as.factor(dose))
  doseranks2 <- doseranks*doseranks
  irow <- 1:length(item)
  
  if (inherits(omicdata,"microarraydata") | 
      inherits(omicdata,"metabolomicdata"))
  {
    data <- omicdata$data
    if (select.method == "quadratic")
    {
      design4lmFit <- model.matrix(~ doseranks + doseranks2)
    } else
      if (select.method == "linear")
      {
        design4lmFit <- model.matrix(~ doseranks)
      } else
        if (select.method == "ANOVA")
        {
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
  } else
  if (inherits(omicdata,"RNAseqdata"))
  {
    data <- omicdata$raw.counts
    coldata <- data.frame(fdose = fdose, doseranks = doseranks, doseranks2 = doseranks2)
    rownames(coldata) <- colnames(data)
    
    if (select.method == "quadratic")
    {
      dds <- DESeqDataSetFromMatrix(
        countData = data,
        colData = coldata,
        design = ~ doseranks + doseranks2
      )
    } else
      if (select.method == "linear")
      {
        dds <- DESeqDataSetFromMatrix(
          countData = data,
          colData = coldata,
          design = ~ doseranks
        )
      } else
        if (select.method == "ANOVA")
        {
          dds <- DESeqDataSetFromMatrix(
            countData = data,
            colData = coldata,
            design = ~ fdose
          )
        } 
    
    # Selection using DESeq2    
    dds <- DESeq(dds, test = "LRT", reduced = ~ 1)
    res <- results(dds)
    # all adjusted pvalues without sorting in res$padj

    selectindexnonsorted <- irow[(res$padj < FDR) & !is.na(res$padj)]
    adjpvaluenonsorted <- res$padj[selectindexnonsorted]
    irowsortedbypvalue <- order(adjpvaluenonsorted, 
                                decreasing = FALSE, na.last = TRUE)
    selectindex <- selectindexnonsorted[irowsortedbypvalue]
    adjpvalue <- adjpvaluenonsorted[irowsortedbypvalue]
  }
  
  # elimination of first selected items with a proportion of tied values
  # above max.tied.values.prop (assuming tied values correspond to the min 
  # value, at which non detection are imputed
  nsample <- length(dose)
  max4nties <- nsample * max.ties.prop
  check.ties <- function(index)
  {
    datai <- data[index, ]
    mini <- min(datai)
    nbtiesi <- length(which(datai == mini))
    return(nbtiesi < max4nties)
  }
  tokeep <- sapply(selectindex, check.ties)
  selectindex <- selectindex[tokeep]
  adjpvalue <- adjpvalue[tokeep]
  
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



