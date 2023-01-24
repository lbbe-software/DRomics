# To fix the scale in log by default when appropriate
# from the dose
dosemax <- max(f$omicdata$dose)
dosemin <- min(f$omicdata$dose[f$omicdata$dose != 0])
if ((dosemax / dosemin) > ratio2switchinlog)
{
  
}

# from the BMD
if (BMD_log_transfo)
{
  if (missing(xmin))
  {
    xmin <- min(BMD2plot$x[is.finite(BMD2plot$x) & BMD2plot$x != 0])
  } else
  {
    if (xmin == 0)
    {
      warning(strwrap(prefix = "\n", initial = "\n",
                      "When using a log scale for the BMD plot, it is not possible to fix xmin at 0. 
          If the default value does not suit you, you can define a strictly positive value for xmin."))
      xmin <- min(BMD2plot$x[is.finite(BMD2plot$x) & BMD2plot$x != 0])
    }
  }
} else
{
  if (missing(xmin)) xmin <- 0
}

## Try to make a function that takes in argument the  doses or the BMD
## and returns TRUE or FALSE to put in log and if TRUE xmin
## to be used inside the functions of the package bmdplot, bmdplotwithgradient,
## curvesplot, plot.drcfit, plot2pdf, sensitivityplot, targetplot
## and directly before the call to those functions in the Shiny apps.