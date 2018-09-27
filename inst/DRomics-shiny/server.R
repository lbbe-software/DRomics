server <- function(input, output, session) {
#pour tester update  
  ## Input: file data
  filedata <- reactive({
    inFile <- NULL
    inFile <- input$datafile
    if(is.null(inFile)) {return(NULL)}
    omicdata(inFile$datapath, check = TRUE, norm.method = input$normMethod)
  })
  
  output$printOmicData <- renderPrint({ 
    oo <<- filedata()
    if (!is.null(oo)) {
      print(oo)
    }
  })
  
  output$plotOmicData <- renderPlot({ 
    oo <<- filedata()
    if (!is.null(oo)) {
      plot(oo)
    }
  })
  
  numFDR <- reactive({as.numeric(input$FDR)})
  
  runitemselect <- reactive({
    oo <- filedata()
    if (!is.null(oo)) {
      itemselect(oo, select.method = input$selectMethod, FDR = numFDR())
    } else {
      NULL
    }
  })
  
  output$printItemSelect <- renderPrint({ 
    ss <<- runitemselect()
    oo <- filedata()
    if (!is.null(oo) & !is.null(ss)) {
      print(ss)
    }
  })
  
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
    drcfit(ss, progressbar = FALSE, sigmoid.model = "Hill", parallel = "no")
  })
  
  output$plotDrcfit <- renderPlot({
    
    if(exists("ss") & !is.null(ss)){
      observe(input$buttonDrcfit) # Re-run when button is clicked
      n <- length(ss$selectindex)
      withProgress(message = 'These ongoing calculations can take from minutes to few hours.
                   Your patience should be proportional to the size of your data and the chosen FDR.', 
                   min = 1, max = 1, value = 1, {
                     mydrcfit <- rundrcfit()
                     plotdrcfit <- plot(mydrcfit)
                     plot(plotdrcfit)
                   })
      
      
      
      output$testdowload <- reactive({length(mydrcfit)})
      outputOptions(output, "testdowload", suspendWhenHidden = FALSE)
      
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
    }
  })
  
  output$printBmdcalc <- renderPrint({
    
    numZbmdcalc <- reactive({as.numeric(input$zbmdcalc)})
    numXbmdcalc <- reactive({as.numeric(input$xbmdcalc)})
    
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
           bytypology = input$bytypology, 
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
        myplot <- function() {plot(mybmdcalc, BMDtype = input$BMDtype, plottype = input$plottype, bytypology = input$bytypology)}
        ggsave(file, width = 8, height = 8, plot = myplot(), device = "pdf")
      },
      contentType = {"application/pdf"}
    )
  })
}
