server <- function(input, output, session) {
  
  
  ####################################################################################
  ####### STEP 1 #####################################################################
  ####################################################################################
  
  inTypeData <- reactive({input$typeData})
  
  validateFile <- function(filename){
    extFile <- tools::file_ext(filename)
    validate(
      need(extFile == "txt" | extFile == "csv", "Only .txt or .csv files are allowed.")
    )
  }
  
  ## Input: file data
  filedata <- eventReactive(input$buttonRunStep1, {
    tryCatch({
      if(inTypeData() == 'microarraydata') {
        req(input$datafile_microarray)
        validateFile(input$datafile_microarray)
        microarraydata(input$datafile_microarray$datapath, backgrounddose = as.numeric(input$bgdose_microarray), check = TRUE, norm.method = input$normMethod_microarray)
      } else if(inTypeData() == 'rnaseqdata') {
        req(input$datafile_rnaseq)
        validateFile(input$datafile_rnaseq)
        RNAseqdata(input$datafile_rnaseq$datapath, backgrounddose = as.numeric(input$bgdose_rnaseq), check = TRUE, transfo.method = input$transfoMethod_rnaseq, round.counts = TRUE)
      } else if(inTypeData() == 'metabolomicdata') {
        req(input$datafile_metabolomic)
        validateFile(input$datafile_metabolomic)
        metabolomicdata(input$datafile_metabolomic$datapath, backgrounddose = as.numeric(input$bgdose_metabolomic), check = TRUE)
      } else if(inTypeData() == 'continuousanchoringdata') {
        req(input$datafile_anchoring)
        validateFile(input$datafile_anchoring)
        continuousanchoringdata(input$datafile_anchoring$datapath, backgrounddose = as.numeric(input$bgdose_anchoring), check = TRUE)}
    },
    error = function(err) {
      # must contain the error returned when the data does not contain dose at zero (see background dose)
      # useful to catch and redirect errors when the app is deployed on a shiny server
      return(err) 
    }
    )      
  })
  
  ## Output : print and plot omic data
  output$printOmicData <- renderPrint({ 
    ff <- filedata()
    if(!"message"%in%names(ff))
      print(ff)
    else
      cat("Error:\n", ff$message)
  })
  
  output$plotOmicData <- renderPlot({
    ff <- filedata()
    if(!"message"%in%names(ff)) {
      gg <- plot(ff, range = 1e10)
      if(inTypeData() == 'continuousanchoringdata')
        gg <- gg + ggplot2::theme_bw()
    }
    return(gg)
  })

  output$plotPCAData <- renderPlot({
    if(inTypeData() != 'continuousanchoringdata') {
      ff <- filedata()
      PCAdataplot(ff, label = TRUE) +
        ggplot2::ggtitle("Principal Component Analysis plot of omic data") + 
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)) +
        ggplot2::theme_bw()
    }
  })
  
  
  ####################################################################################
  ####### STEP 2 #####################################################################
  ####################################################################################
  
  inFDR <- eventReactive(input$buttonRunStep2, {as.numeric(input$FDR)})
  inSelectMethod <- eventReactive(input$buttonRunStep2, {input$selectMethod})
  observe({shinyjs::disable("buttonDowloadItems")})
  
  output$printItemSelect <- renderPrint({ 
    signifitems <<- itemselect(filedata(), select.method = inSelectMethod(), FDR = inFDR())
    print(signifitems)
    
    shinyjs::enable("buttonDowloadItems")
    output$buttonDowloadItems <- downloadHandler(
      filename = function(){
        paste0("items-", Sys.Date(), ".txt")
      },
      content = function(file) {
        write.table(signifitems$omicdata$item[signifitems$selectindex] , file, sep = "\t", dec = ".")
      }
    )
  })
  
  
  ####################################################################################
  ####### STEP 3 #####################################################################
  ####################################################################################
  
  observe({
    if ((inTypeData() == 'microarraydata' & is.null(input$datafile_microarray)) |
        (inTypeData() == 'rnaseqdata' & is.null(input$datafile_rnaseq)) |
        (inTypeData() == 'metabolomicdata' & is.null(input$datafile_metabolomic)) |
        (inTypeData() == 'continuousanchoringdata' & is.null(input$datafile_anchoring))) {
      shinyjs::disable("buttonDrcfit")
    }else{
      shinyjs::enable("buttonDrcfit")
    }
  })
  
  observe({shinyjs::disable("buttonResBmdcalc")})
  observe({shinyjs::disable("buttonPlotBmdcalc")})
  observe({shinyjs::disable("buttonDownloadDrcfitplotBMD")})
  
  rundrcfit <- eventReactive(input$buttonDrcfit, {
    return(drcfit(signifitems, progressbar = FALSE, parallel = "no"))
  })
  
  
  output$plotDrcfit <- renderPlot({
    mydrcfit <- rundrcfit()
    
    myplotdrcfit <- reactive({
      plot(mydrcfit, plot.type = input$plottypeDrcfit, dose_log_transfo = as.logical(input$logdosescale))
    })
    
    mpd <- myplotdrcfit()
    if(!is.null(mpd))
      plot(mpd)
    
    showElement("buttonDownloadDrcfitplot")
    output$buttonDownloadDrcfitplot <- downloadHandler(
      filename = function(){
        "drcfitplot.pdf"
      },
      content = function(file) {
        plotfit2pdf(mydrcfit, plot.type = input$plottypeDrcfit, dose_log_transfo = as.logical(input$logdosescale), path2figs = tempdir())
        file.copy(paste0(tempdir(), "/drcfitplot.pdf"), file)
      },
      contentType = {"application/pdf"}
    )
    
    output$printDrcfit <- renderPrint({
      print(mydrcfit)
    })
  })
  
  
  ####################################################################################
  ####### STEP 4 #####################################################################
  ####################################################################################
  
  numZbmdcalc <- eventReactive(input$buttonRunStep4, {as.numeric(input$zbmdcalc)})
  numXbmdcalc <- eventReactive(input$buttonRunStep4, {as.numeric(input$xbmdcalc)})
  myplottype <- eventReactive(input$buttonRunStep4, {input$plottype})
  
  output$printBmdcalc <- renderPrint({
    
    # calculate bmdcalc
    input$buttonDrcfit
    mydrcfit <- rundrcfit()
    mybmdcalc <- bmdcalc(mydrcfit, z = numZbmdcalc(), x = numXbmdcalc())
    mybmdcalcdigits <- head(mybmdcalc$res, 10)
    idx <- as.numeric(which(sapply(mybmdcalcdigits, function(X) is.numeric(X))))
    mybmdcalcdigits[, idx] <- signif(mybmdcalcdigits[, idx], digits = 4)
    
    # build the plot
    data_DR <- reactiveValues()
    data_DR$plot1_step4 <- 
      if(myplottype() == 'ecdfcolorgradient') {
        if(input$splitby == 'none') {
          bmdplotwithgradient(mybmdcalc$res, BMDtype = input$BMDtype, 
                              BMD_log_transfo = as.logical(input$logbmd_ecdfgradient),
                              add.label = as.logical(input$label_ecdfgradient))
        } else {
          bmdplotwithgradient(mybmdcalc$res, BMDtype = input$BMDtype, 
                              facetby = input$splitby,
                              BMD_log_transfo = as.logical(input$logbmd_ecdfgradient),
                              add.label = as.logical(input$label_ecdfgradient))
        }
        
        ##### ecdf #####
      } else if(myplottype() == 'ecdf') {
        plot(mybmdcalc, BMDtype = input$BMDtype, 
             plottype = 'ecdf', 
             by = input$splitby, 
             hist.bins = input$histbin, 
             BMD_log_transfo = as.logical(input$logbmd_ecdf))
        
      } else {
        plot(mybmdcalc, BMDtype = input$BMDtype, 
             plottype = myplottype(), 
             by = input$splitby, 
             hist.bins = input$histbin)
      }  + 
      ggplot2::theme_bw()
    
    # activate buttons
    shinyjs::enable("buttonResBmdcalc")
    shinyjs::enable("buttonPlotBmdcalc")
    shinyjs::enable("buttonDownloadDrcfitplotBMD")
    
    # print the result in the interface
    print(mybmdcalc)
    cat("\n")
    cat("\n")
    print(mybmdcalcdigits)
    
    # to download the results file
    output$buttonResBmdcalc <- downloadHandler(
      filename = function(){
        paste0("data-", Sys.Date(), ".txt")
      },
      content = function(file) {
        write.table(mybmdcalc$res, file, sep = "\t", dec = ".")
      }
    )
    
    # to print the first plot (panel 4) in the interface
    output$plotBmdcalc <- renderPlot({
      data_DR$plot1_step4
    })
    
    ## to download the first plot (panel 4)
    output$buttonPlotBmdcalc <- downloadHandler(
      filename = function(){
        paste0("data-", Sys.Date(), ".", input$fileformat_bmdcalc)
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = data_DR$plot1_step4, device = input$fileformat_bmdcalc,
                        height = 8.5, width = 11)
      }
    )
    
    # to print the second plot (panel 4) in the interface
    output$plotDrcfitBMD <- renderPlot({
      plot(mydrcfit, plot.type = "dose_fitted",
           BMDoutput = mybmdcalc, BMDtype = input$BMDtype_plot2pdf,
           dose_log_transfo = as.logical(input$logbmd_plot2pdf))
    })
    
    # to download the second plot (panel 4)
    output$buttonDownloadDrcfitplotBMD <- downloadHandler(
      filename = function(){
        "drcfitplot.pdf"
      },
      content = function(file) {
        plotfit2pdf(mydrcfit, plot.type = "dose_fitted",
                    BMDoutput = mybmdcalc, BMDtype = input$BMDtype_plot2pdf,
                    dose_log_transfo = as.logical(input$logbmd_plot2pdf), path2figs = tempdir())
        file.copy(paste0(tempdir(), "/drcfitplot.pdf"), file)
      },
      contentType = {"application/pdf"}
    )
  })
  
  
  ####################################################################################
  ####### R CODE #####################################################################
  ####################################################################################
  
  output$printRCode <- renderText({
    
    if(inTypeData() == 'microarraydata') {
      req(input$datafile_microarray)
    } else if(inTypeData() == 'rnaseqdata') {
      req(input$datafile_rnaseq)
    } else if(inTypeData() == 'metabolomicdata') {
      req(input$datafile_metabolomic)
    } else if(inTypeData() == 'continuousanchoringdata') {
      req(input$datafile_anchoring)
    }
    
    text <- c("library(DRomics)",
              "",
              "# Step 1",
              paste0("o <- ", ifelse(input$typeData == 'microarraydata', 
                                     paste0("microarraydata('", input$datafile_microarray$name, "', backgrounddose = ", input$bgdose_microarray, ", check = TRUE, norm.method = '", input$normMethod_microarray, "')"), 
                                     ifelse(input$typeData == 'rnaseqdata', 
                                            paste0("RNAseqdata('", input$datafile_rnaseq$name, "', backgrounddose = ", input$bgdose_rnaseq, ", check = TRUE, transfo.method = '", input$transfoMethod_rnaseq, "', round.counts = TRUE)"), 
                                            ifelse(input$typeData == 'metabolomicdata', 
                                                   paste0("continuousomicdata('", input$datafile_metabolomic$name, "', backgrounddose = ", input$bgdose_metabolomic, ", check = TRUE)"),
                                                   paste0("continuousanchoringdata('", input$datafile_anchoring$name, "', backgrounddose = ", input$bgdose_anchoring, ", check = TRUE)")
                                                   )))),
              "print(o)",
              ifelse(input$typeData == 'continuousanchoringdata', "plot(o) + theme_bw()", "plot(o)"),
              if(input$typeData != 'continuousanchoringdata') {
                paste0("PCAdataplot(o, label = TRUE) +
                  ggplot2::ggtitle('Principal Component Analysis plot of omic data') + 
                  ggplot2::theme(plot.title = ggplot2::element_text(face = 'bold', hjust = 0.5)) +
                  ggplot2::theme_bw()")},
              "",
              "# Step 2",
              paste0("s <- itemselect(o, select.method = '", inSelectMethod(), "', FDR = ", inFDR(), ")"),
              "print(s)",
              "",
              "# Step 3",
              paste0("f <- drcfit(s, progressbar = FALSE, parallel = 'no')"),
              "# This computation time can be reduced using parallel computing (see ?drcfit)",
              paste0("plot(f, plot.type = '", input$plottypeDrcfit, "', dose_log_transfo = ", input$logdosescale, ")"),
              "",
              "# Step 4",
              paste0("r <- bmdcalc(f, z = ", numZbmdcalc(), ", x = ", numXbmdcalc(), ")"),
              paste0("head(r$res, 10)"),
              if(input$plottype == 'ecdfcolorgradient') {
                if(input$splitby == 'none') {
                  paste0("bmdplotwithgradient(r$res, BMDtype = '", input$BMDtype, 
                         ", BMD_log_transfo = ", as.logical(input$logbmd_ecdfgradient), ", add.label = ", as.logical(input$label_ecdfgradient), 
                         ") + ggplot2::theme_bw()")
                } else {
                  paste0("bmdplotwithgradient(r$res, BMDtype = '", input$BMDtype, 
                         ", BMD_log_transfo = ", as.logical(input$logbmd_ecdfgradient), ", add.label = ", as.logical(input$label_ecdfgradient), 
                         ", facetby = '", input$splitby, "') + ggplot2::theme_bw()")
                }
              } else if(input$plottype == 'ecdf') {
                  paste0("plot(r, BMDtype = '", input$BMDtype, "', plottype = 'ecdf', by = '", input$splitby, "', hist.bins = ", 
                         input$histbin, ", BMD_log_transfo = ", as.logical(input$logbmd_ecdf), ") + ggplot2::theme_bw()")
              } else {
                paste0("plot(r, BMDtype = '", input$BMDtype, "', plottype = '", input$plottype, "', by = '", input$splitby, "', hist.bins = ", 
                       input$histbin, ") + ggplot2::theme_bw()")
              },
              paste0("plot(f, plot.type = 'dose_fitted', BMDoutput = r, BMDtype = '", input$BMDtype_plot2pdf, 
                     "', dose_log_transfo = ", input$logbmd_plot2pdf, ")")
    )
    
    output$buttonDownRCode <- downloadHandler(
      filename = function(){
        paste0("Rcode-", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(paste(text, collapse = "\n"), file)
      },
      contentType = {"text/plain"}
    )
    
    return(paste(text, collapse = "\n"))
  })
  
  
  output$printRCodeFurther <- renderText({
    
    if(inTypeData() == 'microarraydata') {
      req(input$datafile_microarray)
    } else if(inTypeData() == 'rnaseqdata') {
      req(input$datafile_rnaseq)
    } else if(inTypeData() == 'metabolomicdata') {
      req(input$datafile_metabolomic)
    } else if(inTypeData() == 'continuousanchoringdata') {
      req(input$datafile_anchoring)
    }
    
    text <- c("# Few lines of R script to go further using the package",
              "",
              "# Bootstrap to compute confidence intervals on BMD estimations",
              "# May take few hours !!!!!!!!!!!!!!!!!!!!!",
              "# This computation time can be reduced using parallel computing",
              "# (see ?bmdboot for corresponding code)",
              "(b <- bmdboot(r, niter = 1000, progressbar = TRUE))",
              paste0("plot(b, BMD_log_transfo = ", as.logical(input$logbmd_ecdf), ")"),
              "",
              "# plot the fitted dose-response with BMD",
              paste0("plot(f, plot.type = 'dose_fitted', BMDoutput = b, BMDtype = '", input$BMDtype_plot2pdf, "', dose_log_transfo = ", input$logbmd_plot2pdf, ")"),
              "",
              "# Other functions (bmdplot, bmdplotwithgradient, curvesplot, trendplot and sensitivityplot)",
              "# are available in the DRomics package to explore your results (see the vignette and the",
              "# cheat sheet) and in a second shiny application called DRomicsInterpreter-shiny!",
              "",
              "# Before the biological interpretation of results, one could retain only the items associated",
              "# to the best estimated BMD values, using the function bmdfilter (see ?bmdfilter for a description",
              "# of the three proposed filters)",
              "# subres <- bmdfilter(b$res, BMDtype = 'zSD', BMDfilter = 'definedCI')"
    )
    
    output$buttonDownRCodeFurther <- downloadHandler(
      filename = function(){
        paste0("moreRcode-", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(paste(text, collapse = "\n"), file)
      },
      contentType = {"text/plain"}
    )
    
    return(paste(text, collapse = "\n"))
  })
  
}
