server <- function(input, output, session) {
  
  
  ####################################################################################
  ####### STEP 1 #####################################################################
  ####################################################################################
  
  ## Input: file data
  filedata <- reactive({
    req(input$datafile)
    omicdata(input$datafile$datapath, check = TRUE, norm.method = input$normMethod)
  })
  
  ## Output : print and plot omic data
  output$printOmicData <- renderPrint({ 
    print(filedata())
  })
  
  output$plotOmicData <- renderPlot({
    plot(filedata())
  })
  
  
  ####################################################################################
  ####### STEP 2 #####################################################################
  ####################################################################################
  
  inFDR <- reactive({as.numeric(input$FDR)})
  inSelectMethod <- reactive({input$selectMethod})
  
  output$printItemSelect <- renderPrint({ 
    signifitems <<- itemselect(filedata(), select.method = inSelectMethod(), FDR = inFDR())
    print(signifitems)
  })
  
  
  ####################################################################################
  ####### STEP 3 #####################################################################
  ####################################################################################
  
  observe({
    if (is.null(input$datafile)) {
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
    plotdrcfit <- plot(mydrcfit)
    plot(plotdrcfit)
    
    output$okfordowload <- reactive({length(mydrcfit)})
    outputOptions(output, "okfordowload", suspendWhenHidden = FALSE)
    
    output$buttonDownloadDrcfitplot <- downloadHandler(
      filename = function(){
        "drcfitplot.pdf"
      },
      content = function(file) {
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
    
    output$plotBmdcalc <- renderPlot({
      plot(mybmdcalc, BMDtype = input$BMDtype, 
           plottype = input$plottype, 
           by = input$splitby, 
           hist.bins = input$histbin)
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
        paste0("data-", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pdf(file, width = 8, height = 8)
        print(plot(mybmdcalc, BMDtype = input$BMDtype, plottype = input$plottype, by = input$splitby))
        dev.off()
      },
      contentType = {"application/pdf"}
    )
  })
  
  
  ####################################################################################
  ####### R CODE #####################################################################
  ####################################################################################

  output$printRCode <- renderText({ 
    req(input$datafile)
    
    text <- c("library(DRomics)",
              "# Step 1",
              paste0("o <- omicdata('", input$datafile$name, "', check = TRUE, norm.method = '", input$normMethod, "')"),
              "print(o)",
              "plot(o)",
              "# Step 2",
              paste0("s <- itemselect(o, select.method = '", inSelectMethod(), "', FDR = ", inFDR(), ")"),
              "print(s)",
              "# Step 3",
              paste0("f <- drcfit(s, progressbar = FALSE, sigmoid.model = 'Hill', parallel = 'no')"),
              "plot(f)",
              "# Step 4",
              paste0("r <- bmdcalc(f, z = ", numZbmdcalc(), ", x = ", numXbmdcalc(), ")"),
              paste0("plot(r, BMDtype = '", input$BMDtype, "', plottype = '", input$plottype, "', by = '", input$splitby, "', hist.bins = ", input$histbin, ")"))
    
    output$buttonDownRCode <- downloadHandler(
      filename = function(){
        paste0("rcode-", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(paste(text, collapse = "\n"), file)
      },
      contentType = {"text/plain"}
    )
    
    return(paste(text, collapse = "\n"))
    
  })
  
}
