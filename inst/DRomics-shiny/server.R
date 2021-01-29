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
  filedata <- reactive({
    
    if(inTypeData() == 'microarraydata') {
      req(input$datafile_microarray)
      validateFile(input$datafile_microarray)
      microarraydata(input$datafile_microarray$datapath, check = TRUE, norm.method = input$normMethod_microarray)
    } else if(inTypeData() == 'rnaseqdata') {
      req(input$datafile_rnaseq)
      validateFile(input$datafile_rnaseq)
      RNAseqdata(input$datafile_rnaseq$datapath, check = TRUE, transfo.method = input$transfoMethod_rnaseq, round.counts = TRUE)
    } else if(inTypeData() == 'metabolomicdata') {
      req(input$datafile_metabolomic)
      validateFile(input$datafile_metabolomic)
      metabolomicdata(input$datafile_metabolomic$datapath, check = TRUE)
    } else if(inTypeData() == 'continuousanchoringdata') {
      req(input$datafile_anchoring)
      validateFile(input$datafile_anchoring)
      continuousanchoringdata(input$datafile_anchoring$datapath, check = TRUE)
    }
  })
  
  ## Output : print and plot omic data
  output$printOmicData <- renderPrint({ 
    ff <- filedata()
    print(ff)
  })
  
  output$plotOmicData <- renderPlot({
    ff <- filedata()
    plot(ff, range = 1e10)
  })
  
  
  ####################################################################################
  ####### STEP 2 #####################################################################
  ####################################################################################
  
  inFDR <- reactive({as.numeric(input$FDR)})
  inSelectMethod <- reactive({input$selectMethod})
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
  
  rundrcfit <- eventReactive(input$buttonDrcfit, {
    return(drcfit(signifitems, progressbar = FALSE, sigmoid.model = "Hill", parallel = "no"))
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
  
  numZbmdcalc <- reactive({as.numeric(input$zbmdcalc)})
  numXbmdcalc <- reactive({as.numeric(input$xbmdcalc)})
  
  output$printBmdcalc <- renderPrint({
    
    input$buttonDrcfit
    mydrcfit <- rundrcfit()
    mybmdcalc <- bmdcalc(mydrcfit, z = numZbmdcalc(), x = numXbmdcalc())
    print(mybmdcalc)
    cat("\n")
    cat("\n")
    
    mybmdcalcdigits <- head(mybmdcalc$res, 10)
    idx <- as.numeric(which(sapply(mybmdcalcdigits, function(X) is.numeric(X))))
    mybmdcalcdigits[, idx] <- signif(mybmdcalcdigits[, idx], digits = 4)
    print(mybmdcalcdigits)
    
    myplottype <- input$plottype
    output$plotBmdcalc <- renderPlot({
      
      ##### ecdfcolorgradient #####
      if(myplottype == 'ecdfcolorgradient') {
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
      } else if (myplottype == 'ecdf') {
        if(as.logical(input$logbmd_ecdf)) {
          plot(mybmdcalc, BMDtype = input$BMDtype, 
               plottype = 'ecdf', 
               by = input$splitby, 
               hist.bins = input$histbin) + scale_x_log10()
        } else {
          plot(mybmdcalc, BMDtype = input$BMDtype, 
               plottype = 'ecdf', 
               by = input$splitby, 
               hist.bins = input$histbin)
        }
        
      } else {
        plot(mybmdcalc, BMDtype = input$BMDtype, 
             plottype = myplottype, 
             by = input$splitby, 
             hist.bins = input$histbin)
      }
    })
    
    # activate the button
    shinyjs::enable("buttonResBmdcalc")
    
    output$buttonResBmdcalc <- downloadHandler(
      filename = function(){
        paste0("data-", Sys.Date(), ".txt")
      },
      content = function(file) {
        write.table(mybmdcalc$res, file, sep = "\t", dec = ".")
      }
    )
    
    # activate the button
    shinyjs::enable("buttonPlotBmdcalc")
    
    ## Output: plots downloading
    output$buttonPlotBmdcalc <- downloadHandler(
      filename = function(){
        paste0("data-", Sys.Date(), ".", input$fileformat_bmdcalc)
      },
      content = function(file) {
        
        switch(input$fileformat_bmdcalc,
               "pdf" = pdf(file),
               "png" = png(file),
               "jpeg" = jpeg(file),
               "tiff" = tiff(file, compression = "lzw"),
               "svg" = svg(file))
        
        
        myplottype <- input$plottype
        print(
          
          if(myplottype == 'ecdfcolorgradient') {
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
            
          } else if(myplottype == 'ecdf') {
            if(as.logical(input$logbmd_ecdf)) {
              plot(mybmdcalc, BMDtype = input$BMDtype, 
                   plottype = 'ecdf', 
                   by = input$splitby, 
                   hist.bins = input$histbin) + scale_x_log10()
            } else {
              plot(mybmdcalc, BMDtype = input$BMDtype, 
                   plottype = 'ecdf', 
                   by = input$splitby, 
                   hist.bins = input$histbin)
            }
            
          } else {
            plot(mybmdcalc, BMDtype = input$BMDtype, 
                 plottype = myplottype, 
                 by = input$splitby, 
                 hist.bins = input$histbin)
            
          })
        dev.off()
      }
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
                                     paste0("microarraydata('", input$datafile_microarray$name, "', check = TRUE, norm.method = '", input$normMethod_microarray, "')"), 
                                     ifelse(input$typeData == 'rnaseqdata', 
                                            paste0("RNAseqdata('", input$datafile_rnaseq$name, "', check = TRUE, transfo.method = '", input$transfoMethod_rnaseq, "', round.counts = TRUE)"), 
                                            ifelse(input$typeData == 'metabolomicdata', 
                                                   paste0("metabolomicdata('", input$datafile_metabolomic$name, "', check = TRUE)"),
                                                   paste0("continuousanchoringdata('", input$datafile_anchoring$name, "', check = TRUE)")
                                                   )))),
              "print(o)",
              "plot(o)",
              "",
              "# Step 2",
              paste0("s <- itemselect(o, select.method = '", inSelectMethod(), "', FDR = ", inFDR(), ")"),
              "print(s)",
              "",
              "# Step 3",
              paste0("f <- drcfit(s, progressbar = FALSE, sigmoid.model = 'Hill', parallel = 'no')"),
              "# This computation time can be reduced using parallel computing (see ?drcfit)",
              paste0("plot(f, plot.type = '", input$plottypeDrcfit, "', dose_log_transfo = ", input$logdosescale, ")"),
              "",
              "# Step 4",
              paste0("r <- bmdcalc(f, z = ", numZbmdcalc(), ", x = ", numXbmdcalc(), ")"),
              if(input$plottype == 'ecdfcolorgradient') {
                if(input$splitby == 'none') {
                  paste0("bmdplotwithgradient(r$res, BMDtype = '", input$BMDtype, 
                         ", BMD_log_transfo = ", as.logical(input$logbmd_ecdfgradient), ", add.label = ", as.logical(input$label_ecdfgradient), ")")
                } else {
                  paste0("bmdplotwithgradient(r$res, BMDtype = '", input$BMDtype, 
                         ", BMD_log_transfo = ", as.logical(input$logbmd_ecdfgradient), ", add.label = ", as.logical(input$label_ecdfgradient), 
                         ", facetby = '", input$splitby, "')")
                }
              } else if(input$plottype == 'ecdf') {
                if(as.logical(input$logbmd_ecdf)) {
                  paste0("plot(r, BMDtype = '", input$BMDtype, "', plottype = 'ecdf', by = '", input$splitby, "', hist.bins = ", input$histbin, ") + scale_x_log10()")
                } else {
                  paste0("plot(r, BMDtype = '", input$BMDtype, "', plottype = 'ecdf', by = '", input$splitby, "', hist.bins = ", input$histbin, ")")
                }
              } else {
                paste0("plot(r, BMDtype = '", input$BMDtype, "', plottype = '", input$plottype, "', by = '", input$splitby, "', hist.bins = ", input$histbin, ")")
              }
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
              "# Plot of fitted curves",
              "curvesplot(f$fitres, xmax = max(f$omicdata$dose), facetby = 'trend', colorby = 'model') + 
  xlab('dose') + ylab('theoretical signal')",
              "# The first argument can be an extended dataframe, which could be used to split",
              "# the plot of color the curves (using facetby or colorby)",
              "",
              "# Bootstrap to compute confidence intervals on BMD estimations",
              "# May take few hours !!!!!!!!!!!!!!!!!!!!!",
              "# This computation time can be reduced using parallel computing",
              "# (see ?bmdboot for corresponding code)",
              "(b <- bmdboot(r, niter = 1000, progressbar = TRUE))",
              "plot(b)",
              "",
              "# Representation of cumulative distribution of BMD values",
              "# with their confidence 95% intervals",
              "# Argument by and CI.col can be used with additional columns",
              "# built from annotation of items for example",
              "a <- b$res[is.finite(b$res$BMD.zSD.upper), ] # removing of CI with infinite bounds",
              "ecdfplotwithCI(variable = a$BMD.zSD, CI.lower = a$BMD.zSD.lower, 
  CI.upper = a$BMD.zSD.upper, by = a$model, CI.col = a$trend)"
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
