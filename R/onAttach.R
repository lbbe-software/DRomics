.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("\nDRomics has been loaded.")
    packageStartupMessage("!!!! IMPORTANT CHANGES IN DEFAULT PLOT ARGUMENTS !!!!")
    packageStartupMessage("Now all the plot functions use by default a log10 scale")
    packageStartupMessage("for dose and BMD values, except curvesplot(),")
    packageStartupMessage("for which the use of a dose log scale requires the specification")
    packageStartupMessage("by the user of a non null minimal value (xmin).")
    packageStartupMessage("We also put the default value of the argument scaling at TRUE")
    packageStartupMessage("in curvesplot() and bmdplotwithgradient(),")
    packageStartupMessage("to focus on shapes of dose-responses rather than on their amplitude.\n")
}
  