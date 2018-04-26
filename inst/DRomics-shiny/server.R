server <- function(input, output, session) {
    
    ## Input: file data
    filedata <- reactive({
        inFile <- NULL
        inFile <- input$datafile
        if(is.null(inFile)) {return(NULL)}
        omicdata(inFile$datapath, check = TRUE, norm.method = input$normMethod)
    })
    
    
    runitemselect <- reactive({
        oo <- filedata()
        if (!is.null(oo)) {
            itemselect(oo, select.method = input$selectMethod, FDR = input$FDR)
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
        input$buttonDrcfit # Re-run when button is clicked
        if(exists("ss") & !is.null(ss)){
            n <- length(ss$selectindex)
            withProgress(message = 'Fitting in progress', 
                         detail = 'The fitting may be long if the number of selected items is high.', value = 0, {
                for (i in 1:n) {
                    incProgress(1 / n)
                    zz <- rundrcfit()
                }
            })
            plot(zz)
            
            output$testdowload <- reactive({length(zz)})
            outputOptions(output, "testdowload", suspendWhenHidden = FALSE)
            
            output$buttonDownloadDrcfitplot <- downloadHandler(
                filename = function(){
                    paste("drcfitplot.pdf", sep="")
                },
                content = function(file) {
                    file.copy("drcfitplot.pdf", file)
                }
            )
        }
    })
    
    output$printBmdcalc <- renderPrint({
        input$buttonDrcfit
        mydrcfit <- rundrcfit()
        mybmdcalc <- bmdcalc(mydrcfit)
        print(head(mybmdcalc$res, n = 10))

                
        output$plotBmdcalc <- renderPlot({
            plot(mybmdcalc, BMDtype = input$BMDtype, 
                 plottype = input$plottype, 
                 bytypology = input$bytypology)
        })
        
        # activate the button
        shinyjs::enable("buttonResBmdcalc")
        
        output$buttonResBmdcalc <- downloadHandler(
            filename = function(){
                paste("data-", Sys.Date(), ".txt", sep="")
            },
            content = function(file) {
                write.table(mybmdcalc$res, file)
            }
        )
        
        # activate the button
        shinyjs::enable("buttonPlotBmdcalc")
        
        ## Output: plots downloading
        output$buttonPlotBmdcalc <- downloadHandler(
            filename = function(){
                paste("data-", Sys.Date(), ".pdf", sep="")
            },
            content = function(file) {
                plot(mybmdcalc, BMDtype = input$BMDtype, plottype = input$plottype, bytypology = input$bytypology)
                ggsave(file, width = 8, height = 8, plot = last_plot())
            }
        )
        
    })
}
