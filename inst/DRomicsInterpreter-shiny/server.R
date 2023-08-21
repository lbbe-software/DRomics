server <- function(input, output, session) {
    
    ################################################################################################
    ################################################################################################
    ############ STEP 1 ############
    ################################################################################################
    ################################################################################################
    
    ############
    ############ Manage interface
    ############
    
    # create dynamically a number of input panel specified by user in the 'numericInput'
    output[["inputstep1"]] <- renderUI({
        n <- input[["nbLevel"]]
        
        inputstep1 <- lapply(1:n, function(i){
            fixedRow(
                hr(style = "border-top: 1px solid #ffffff;"),
                fixedRow(
                    column(1, align = "center", HTML("<font face=verdana size=3 color=#9c5c16><b>Level ", i, "</b></font>")),
                    column(4,
                           fileInput(paste0("DRomicsData", i), 'DRomics output', width = '100%', accept = c('.csv', '.txt')),
                           selectizeInput(paste0("id_DRomicsData", i), "ID to merge", "")),
                    column(4,
                           fileInput(paste0('annotationData', i), 'Annotation data', width = '100%', accept = c('.csv', '.txt')),
                           selectInput(paste0("id_annotationData", i), "ID to merge", "")),
                    column(2,
                           textInput(paste0("label", i), "Label of level", paste0("explevel", i )))
                )
            )
        })
        
        do.call(function(...){
            wellPanel(
                fixedRow(
                    column(1),
                    column(4, align = "center", HTML("<font face=verdana size=3 color=#9c5c16><b>(1) Data frame from the DRomics workflow</b></font>")),
                    column(4, align = "center", 
                           HTML("<font face=verdana size=3 color=#9c5c16><b>(2) Annotation data frame &nbsp;</b></font>"),
                           shinyBS::bsButton("helplabel1step1", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                           shinyBS::bsPopover("helplabel1step1", "", helplabel1step1, placement = "top", trigger = "hover", options = list(container = "body")),
                           
                           splitLayout(cellWidths = c(277, 100), style = "margin-left:50px;padding-top:20px;",
                                       checkboxInput("sep_annotationData", label = "Only tab accepted as a column separator.", value = FALSE),
                                       shinyBS::bsButton("helplabel3step1", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                                       shinyBS::bsPopover("helplabel3step1", "", helplabel3step1, placement = "bottom", trigger = "hover", options = list(container = "body"))
                           )
                    ),
                    
                    column(3, align = "center", 
                           HTML("<font face=verdana size=3 color=#9c5c16><b>(3) Name of the experimental level &nbsp;</b></font>"),
                           shinyBS::bsButton("helplabel2step1", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                           shinyBS::bsPopover("helplabel2step1", "", helplabel2step1, placement = "bottom", trigger = "hover", options = NULL)
                    )
                ),
                ..., width = 2, status = "primary")
        }, inputstep1)
    })
    
    # Disable all tab panel except the first one
    # js$disableTab("step2")
    # js$disableTab("step3")
    # js$disableTab("step4")
    # js$disableTab("step5")
    
    # function to check the extension of input files
    validateFile <- function(filename) {
        extFile <- tools::file_ext(filename)
        validate(
            need(extFile == "txt" | extFile == "csv", "Only .txt or .csv files are allowed.")
        )
    }
    
    
    ######
    ###### DRomics data input
    ######
    
    DRData <- function(idlev) {
        eval(parse(text=paste0("req(input$DRomicsData", idlev, ")")))
        eval(parse(text=paste0("validateFile(input$DRomicsData", idlev, ")")))
        myexpr <- paste0("input$DRomicsData", idlev, "$datapath")
        return(eval(parse(text = paste0("read.table(", myexpr, ", header = TRUE, stringsAsFactors = TRUE)"))))
    }
    
    # update selectizeInput according with DRomicsData
    lapply(
        X = 1:10,
        FUN = function(i) {
            observeEvent(input[[paste0("DRomicsData", i)]], {
                updateSelectizeInput(session, 
                                     inputId = paste0("id_DRomicsData", i),
                                     label = "ID to merge",
                                     server = TRUE,
                                     choices = names(DRData(i)),
                                     selected = names(DRData(i))[1])
                shinyjs::disable(paste0('id_DRomicsData', i))
            })
        }
    )
    
    
    ######
    ###### Annotation data
    ######
    
    annotationData <- function(idlev) {
        eval(parse(text=paste0("req(input$annotationData", idlev, ")")))
        eval(parse(text=paste0("validateFile(input$annotationData", idlev, ")")))
        myexpr <- paste0("input$annotationData", idlev, "$datapath")
        sep <- if(input$sep_annotationData) "'\t'" else "''"
        validate(
            need(try(annotationDF <- eval(parse(text = paste0("read.table(", myexpr, ", header = TRUE, stringsAsFactors = TRUE, sep = ", sep, ")")))), "Error reading the file")
        )
        validate(
            need(length(colnames(annotationDF)) == 2, paste0("Your annotation data at level ", idlev," set must have exactly two columns.\nPlease update your imported data."))
        )
        return(annotationDF)
    }
    
    # update selectizeInput according with annotationsData
    lapply(
        X = 1:10,
        FUN = function(i) {
            observeEvent(input[[paste0("annotationData", i)]], {
                updateSelectizeInput(session, 
                                     inputId = paste0("id_annotationData", i),
                                     label = "ID to merge",
                                     server = TRUE,
                                     choices = names(annotationData(i)),
                                     selected = names(annotationData(i))[1])
            })
        }
    )
    
    
    
    ######
    ###### Merge and combine DRomics and annotation data
    ######
    
    mergeddata <- eventReactive(input$buttonRunStep1, {
        # input check
        validate(need(input$maxDoseXScale, "Please fill in the maximal dose/concentration for definition of x-scale of plots."))
        for (i in 1:input$nbLevel) {annotationData(i)}
        myextendedmergeddata <- list()
        alllabels <- rep(NA, input$nbLevel)
        for (i in 1:input$nbLevel) {
            eval(parse(text=paste0("req(input$id_DRomicsData", i, ")")))
            eval(parse(text=paste0("req(input$id_annotationData", i, ")")))
            eval(parse(text=paste0("req(input$label", i, ")")))
            myDRData <- DRData(i)
            myannotationData <- annotationData(i)
            mymergeddata <- merge(x = myDRData, y = myannotationData, by.x = input[[paste0("id_DRomicsData", i)]], by.y = input[[paste0("id_annotationData", i)]])
            alllabels[i] <- gsub("\\s", "", input[[paste0("label", i)]])
            # add a new column containing the 'experimental level' defined by user
            mymergeddata <- cbind("experimental_level" = alllabels[i], mymergeddata)
            mymergeddata$experimental_level <- as.factor(mymergeddata$experimental_level)
            myextendedmergeddata <- rbind(myextendedmergeddata, mymergeddata)
        }
        shinyjs::showElement('text1_step1', time = 0)
        shinyjs::showElement('text2_step1', time = 0)
        if(!all(table(alllabels) == 1)) {
            return("The labels chosen must be unique. Please update duplicate labels.")
        } else {
            # remove duplicated rows
            myextendedmergeddata <- unique(myextendedmergeddata)
            return(myextendedmergeddata)
        }
    })
    
    output$txtcolumnidannot <- renderText({
        namesFirstAnnotData <- names(annotationData(1))
        idnamesFirstAnnotData <- which(namesFirstAnnotData != input$id_annotationData1)
        paste0("The column name '", namesFirstAnnotData[idnamesFirstAnnotData], "' in the first annotation data frame will be used for the annotation at the other levels. The annotation at the other levels should always have this name.")
    })
    
    output$strmergeddata <- renderPrint({
        # js$enableTab("step2")
        # js$enableTab("step3")
        # js$enableTab("step4")
        # js$enableTab("step5")
        mymergeddata <- mergeddata()
        
        # create dynamically as many outputs as number of experimental level
        lapply(1:nlevels(mymergeddata$experimental_level), function(i) {
            output[[paste0("headmergeddata", i)]] <-
                renderPrint({
                    return(head(mymergeddata[mymergeddata$experimental_level == levels(mymergeddata$experimental_level)[i], ], 3))
                })
        })
        
        output$headmergeddata <- renderUI({
            lapply(1:nlevels(mymergeddata$experimental_level), function(i){
                tagList(verbatimTextOutput(paste0("headmergeddata",i)))
            })
        })
        
        return(str(mymergeddata))
    })
    
    observe({
        mydata <- mergeddata()
        mycolnames <- colnames(mydata)
        
        if(!length(grep("zSD", mycolnames))){
            updateRadioButtons(session, "BMDtype",
                               label = "BMD type", 
                               choices = "xfold",
                               selected = "xfold"
            )
        }
        
        if(!length(grep("xfold", mycolnames))){
            updateRadioButtons(session, "BMDtype",
                               label = "BMD type", 
                               choices = "zSD",
                               selected = "zSD"
            )
        }
    })
    
    observe({
        mydata <- mergeddata()
        if(input$BMDtype == "zSD") {
            updateNumericInput(session, "BMDmax", 
                               label = "Maximum for the BMD summary value", 
                               value = ceiling(max(mydata$BMD.zSD, na.rm = TRUE)), 
                               min = 0, step = 0.1)
        }
        else if(input$BMDtype == "xfold") {
            updateNumericInput(session, "BMDmax", 
                               label = "Maximum for the BMD summary value", 
                               value = ceiling(max(mydata$BMD.xfold, na.rm = TRUE)), 
                               min = 0, step = 0.1)
        }
    })
    
    maxDoseXScale <- eventReactive(input$buttonRunStep1, {as.numeric(input$maxDoseXScale)})
    
    ################################################################################################
    ############ STEP 2 ############
    ################################################################################################
    
    # create dynamically the drag and drop widget to sort annotations
    output$specificorder <- renderUI({
        # see https://rstudio.github.io/sortable/articles/updating_rank_list.html
        myextendedmergeddata <- mergeddata()
        myfirstannotationData <- annotationData(1)
        mypathclasslabel <-  names(myfirstannotationData)[which(names(myfirstannotationData) != input$id_annotationData1)]
        
        namestosort <- sort(levels(myextendedmergeddata[, mypathclasslabel]))
        sortable::rank_list(text = HTML("<b>Drag and drop the labels to sort them as you wish</b>"),
                            labels = namestosort, 
                            input_id = "labelssorted",
                            options = sortable_options(multiDrag = TRUE))
    })
    
    
    keepAllExplev <- eventReactive(input$buttonRunStep2, {
        if(input$nbLevel == 1)
            return(FALSE)
        else
            return(input$keepAllExplev)
    })
    minNbItem <- eventReactive(input$buttonRunStep2, {input$minNbItem})
    BMDmax <- eventReactive(input$buttonRunStep2, {
        validate(
            need(input$BMDmax >= 0, "BMDmax must be a positive number.")
        )
        return(input$BMDmax)
    })
    BMDlogtransfoSensitivityplot <- eventReactive(input$buttonRunStep2, {input$BMDlogtransfoSensitivityplot})
    BMDtype <- eventReactive(input$buttonRunStep2, {input$BMDtype})
    BMDsummarysensitivityPlot <- eventReactive(input$buttonRunStep2, {input$BMDsummarysensitivityPlot})
    orderingOnelev <- eventReactive(input$buttonRunStep2, {input$ordering_onelev})
    orderingMoreonelev <- eventReactive(input$buttonRunStep2, {input$ordering_moreonelev})
    labelssorted <- eventReactive(input$buttonRunStep2, {input$labelssorted})
    
    sortextendedres <- eventReactive(input$buttonRunStep2, {
        
        myextendedmergeddata <- mergeddata()
        myfirstannotationData <- annotationData(1)
        mypathclasslabel <-  names(myfirstannotationData)[which(names(myfirstannotationData) != input$id_annotationData1)]
        
        myextendedmergeddata <- selectgroups(extendedres = myextendedmergeddata, 
                                             BMDmax = BMDmax(),
                                             group = mypathclasslabel, 
                                             explev = "experimental_level", 
                                             BMDtype = BMDtype(),
                                             BMDsummary = substr(BMDsummarysensitivityPlot(), 1, 5), 
                                             nitemsmin = minNbItem(),
                                             keepallexplev = keepAllExplev())
        
        # alphabetic ordering of mypathclasslabel groups
        if((orderingOnelev() == "alphaorder_onelev" & input$nbLevel == 1) | (orderingMoreonelev() == "alphaorder_moreonelev" & input$nbLevel > 1)) {
            levelorder <- sort(levels(myextendedmergeddata[, mypathclasslabel]), decreasing = FALSE)
            
            # ordered by number of items
        } else if((orderingOnelev() == "numbitemorder_onelev" & input$nbLevel == 1) | (orderingMoreonelev() == "numbitemorder_moreonelev" & input$nbLevel > 1)) {
            levelorder <- names(sort(table(myextendedmergeddata[, mypathclasslabel]), decreasing = FALSE))
            
        } else if((orderingOnelev() == "specificorder_onelev" & input$nbLevel == 1) | (orderingMoreonelev() == "specificorder_moreonelev" & input$nbLevel > 1)) {
            levelorder <- labelssorted()
        } else {
            levelorder <- levels(myextendedmergeddata[, mypathclasslabel])
        }
        
        myextendedmergeddata[, mypathclasslabel] <- factor(myextendedmergeddata[, mypathclasslabel], levels = levelorder)
        
        return(list("myextendedmergeddata" = myextendedmergeddata,
                    "mypathclasslabel" = mypathclasslabel))
    })
    
    # function to sort the levels. only used for the ggplot display.
    sortlevels4ggplot <- function(df, v) {
        if(input$nbLevel > 1) {
            if(orderingMoreonelev() == "alphaorder_moreonelev" | orderingMoreonelev() == "specificorder_moreonelev"){
                df[, v] <- factor(df[, v], levels = rev(levels(df[, v])))
            }
        } else {
            if(orderingOnelev() == "alphaorder_onelev" | orderingOnelev() == "specificorder_onelev"){
                df[, v] <- factor(df[, v], levels = rev(levels(df[, v])))
            }
        }
        return(df)
    }
    
    
    output$filteredsorteddata <- renderPrint({
        
        sortextendedres <- sortextendedres()
        myextendedmergeddata <- sortextendedres$myextendedmergeddata
        mypathclasslabel <- sortextendedres$mypathclasslabel
        
        ############ sensitivity plot ############
        output$sensitivityplot <- renderPlot({
            myextendedmergeddata <- sortlevels4ggplot(myextendedmergeddata, mypathclasslabel)
            if(input$nbLevel > 1) {
                mysensitivityplot <- DRomics::sensitivityplot(myextendedmergeddata, 
                                                              BMDtype = BMDtype(), 
                                                              group = mypathclasslabel,
                                                              colorby = "experimental_level",
                                                              ECDF_plot = FALSE,
                                                              BMDsummary = BMDsummarysensitivityPlot(), 
                                                              BMD_log_transfo = BMDlogtransfoSensitivityplot())
            } else {
                myECDFplot <- if(orderingOnelev() == "BMDorder_onelev") {TRUE} else {FALSE}
                mysensitivityplot <- DRomics::sensitivityplot(myextendedmergeddata, 
                                                              BMDtype = BMDtype(), 
                                                              group = mypathclasslabel,
                                                              ECDF_plot = myECDFplot,
                                                              BMDsummary = BMDsummarysensitivityPlot(), 
                                                              BMD_log_transfo = BMDlogtransfoSensitivityplot())
            }
            
            shinyjs::showElement('text2_step2', time = 0)
            output$buttonDownloadSensitivityplot <- downloadHandler(
                filename = function(){
                    "sensitivityplot.pdf"
                },
                content = function(file) {
                    pdf(file)
                    plot(mysensitivityplot)
                    dev.off()
                },
                contentType = {"application/pdf"}
            )
            
            return(mysensitivityplot)
        })
        
        ############ trend plot ############
        output$trendplot <- renderPlot({
            myextendedmergeddata <- sortlevels4ggplot(myextendedmergeddata, mypathclasslabel)
            if(input$nbLevel > 1) {
                mytrendplot <- DRomics::trendplot(myextendedmergeddata, 
                                                  group = mypathclasslabel, 
                                                  facetby = "experimental_level", 
                                                  add.color = TRUE)
            } else {
                mytrendplot <- DRomics::trendplot(myextendedmergeddata, 
                                                  group = mypathclasslabel, 
                                                  add.color = TRUE)
            }
            shinyjs::showElement('text1_step2', time = 0)
            output$buttonDownloadTrendplot <- downloadHandler(
                filename = function(){
                    "trendplot.pdf"
                },
                content = function(file) {
                    pdf(file)
                    plot(mytrendplot)
                    dev.off()
                },
                contentType = {"application/pdf"}
            )
            
            return(mytrendplot)
        })
        
        
        ############ structure of data ############
        shinyjs::showElement('text3_step2', time = 0)
        
        output$downloadData <- downloadHandler(
            filename = function() {
                "extendedmergeddata.txt"
            },
            content = function(file) {
                write.table(myextendedmergeddata, file, row.names = FALSE, sep = "\t")
            }
        )
        
        return(str(myextendedmergeddata))
    })
    
    
    ################################################################################################
    ############ STEP 3 ############
    ################################################################################################
    
    # Create the checkboxGroupInput for BMD plots
    observe({
        sortextendedres <- sortextendedres()
        myextendedmergeddata <- sortextendedres$myextendedmergeddata
        mypathclasslabel <- sortextendedres$mypathclasslabel
        mylevels <- names(table(as.vector(myextendedmergeddata[, mypathclasslabel])))
        
        updateCheckboxGroupInput(session, "annotcheckboxBMDplot",
                                 label = "Choose at least one annotation",
                                 choices = mylevels, 
                                 selected = mylevels[1] 
        )
        
        # check all boxes if the button 'selectallBMDplot' is clicked
        observeEvent(input$selectallBMDplot, {
            if(input$selectallBMDplot == 0) return(NULL) 
            else if (input$selectallBMDplot > 0)
            {
                updateCheckboxGroupInput(session, "annotcheckboxBMDplot",
                                         label = "Choose at least one annotation",
                                         choices = mylevels,
                                         selected = unlist(mylevels))
            }
        })
        
        # uncheck all boxes if the button 'unselectallBMDplot' is clicked
        observeEvent(input$unselectallBMDplot, {
            if(input$unselectallBMDplot == 0) return(NULL) 
            else if (input$unselectallBMDplot > 0)
            {
                updateCheckboxGroupInput(session, "annotcheckboxBMDplot",
                                         label = "Choose at least one annotation",
                                         choices = mylevels,
                                         selected = "")
            }
        })
    })
    
    observe({
        updateCheckboxInput(session, "BMDlogtransfoBMDplot", value = input$BMDlogtransfoSensitivityplot)
    })
    
    # Deactivate 'shapeby' if 'addlabel' is checked
    observeEvent(input$addlabelBMDplot, {
        if(!isTRUE(input$addlabelBMDplot))
            shinyjs::enable("shapebyBMDplot")
        else
            shinyjs::disable("shapebyBMDplot")
    })
    
    # Update the choices available in 'facetby' when there is only one level
    observeEvent(input$nbLevel, {
        if(input$nbLevel == 1)
            updateRadioButtons(session, "facetbycolumnsBMDplot", choices = list("Annotation" = "annotation"), selected = "annotation")
        else
            updateRadioButtons(session, "facetbycolumnsBMDplot", choices = list("Annotation" = "annotation", "Experimental level" = "explevel"), selected = "annotation")
    })
    
    # Update the choices available in 'facetby2' according to 'facetby'
    observeEvent(input$facetbycolumnsBMDplot, {
        if(input$facetbycolumnsBMDplot == "annotation")
            updateRadioButtons(session, "facetbyrowsBMDplot", choices = list("Experimental level" = "explevel"), selected = "explevel")
        else if(input$facetbycolumnsBMDplot == "explevel")
            updateRadioButtons(session, "facetbyrowsBMDplot", choices = list("Annotation" = "annotation"), selected = "annotation")
    })
    
    addciBMDplot <- eventReactive(input$buttonRunStep3, {input$addciBMDplot})
    BMDlogtransfoBMDplot <- eventReactive(input$buttonRunStep3, {input$BMDlogtransfoBMDplot})
    addlabelBMDplot <- eventReactive(input$buttonRunStep3, {input$addlabelBMDplot})
    facetbycolumnsBMDplot <- eventReactive(input$buttonRunStep3, {input$facetbycolumnsBMDplot})
    facetbyrowsBMDplot <- eventReactive(input$buttonRunStep3, {input$facetbyrowsBMDplot})
    shapebyBMDplot <- eventReactive(input$buttonRunStep3, {input$shapebyBMDplot})
    colorbyBMDplot <- eventReactive(input$buttonRunStep3, {input$colorbyBMDplot})
    
    # min doses by default according to the log transformation
    xmin_bmdplotwotgradient <- eventReactive(input$BMDlogtransfoBMDplot, {
        if(input$doselogtransfoCurvesplot) {
            return(round(min(BMD) / 2, 2))
        } else {
            return(0)
        }
    })
    
    extendedresforBMD <- eventReactive(input$buttonRunStep3, {
        validate(
            need(input$annotcheckboxBMDplot, "Please choose at least one annotation")
        )
        sortextendedres <- sortextendedres()
        myextendedmergeddata <- sortextendedres$myextendedmergeddata
        mypathclasslabel <- sortextendedres$mypathclasslabel
        myextendedresforBMD <- myextendedmergeddata[myextendedmergeddata[, mypathclasslabel] %in% input$annotcheckboxBMDplot, ]
        return(list("myextendedresforBMD" = myextendedresforBMD,
                    "mypathclasslabel" = mypathclasslabel))
    })
    
    ############ BMD plot ############
    output$bmdplot <- renderPlot({
        
        myextendedresforBMD <- extendedresforBMD()
        myfacetbycolumnsBMDplot <- fnvaluecheckbox(facetbycolumnsBMDplot(), myextendedresforBMD$mypathclasslabel)
        myfacetbyrowsBMDplot <- fnvaluecheckbox(facetbyrowsBMDplot(), myextendedresforBMD$mypathclasslabel)
        
        if(isTRUE(shapebyBMDplot())) {
            if(input$nbLevel > 1) {
                mybmdplotwithgradient <- DRomics::bmdplotwithgradient(myextendedresforBMD$myextendedresforBMD,
                                                                      xmin = xmin_bmdplotwotgradient(),
                                                                      xmax = maxDoseXScale(),
                                                                      scaling = TRUE,
                                                                      facetby = myfacetbycolumnsBMDplot,
                                                                      facetby2 = myfacetbyrowsBMDplot,
                                                                      shapeby = "trend",
                                                                      add.label = addlabelBMDplot(),
                                                                      BMD_log_transfo = BMDlogtransfoBMDplot())
            } else {
                mybmdplotwithgradient <- DRomics::bmdplotwithgradient(myextendedresforBMD$myextendedresforBMD,
                                                                      xmin = xmin_bmdplotwotgradient(),
                                                                      xmax = maxDoseXScale(),
                                                                      scaling = TRUE,
                                                                      facetby = myfacetbycolumnsBMDplot,
                                                                      shapeby = "trend",
                                                                      add.label = addlabelBMDplot(),
                                                                      BMD_log_transfo = BMDlogtransfoBMDplot())
            }
            
            if(isTRUE(colorbyBMDplot())) {
                if(input$nbLevel > 1) {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  facetby2 = myfacetbyrowsBMDplot,
                                                  shapeby = "trend",
                                                  colorby = "trend",
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                } else {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  shapeby = "trend",
                                                  colorby = "trend",
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                }
            } else {
                if(input$nbLevel > 1) {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  facetby2 = myfacetbyrowsBMDplot,
                                                  shapeby = "trend",
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                } else {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  shapeby = "trend",
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                }
            }
        } else {
            if(input$nbLevel > 1) {
                mybmdplotwithgradient <- DRomics::bmdplotwithgradient(myextendedresforBMD$myextendedresforBMD,
                                                                      xmin = xmin_bmdplotwotgradient(),
                                                                      xmax = maxDoseXScale(),
                                                                      scaling = TRUE,
                                                                      facetby = myfacetbycolumnsBMDplot,
                                                                      facetby2 = myfacetbyrowsBMDplot,
                                                                      add.label = addlabelBMDplot(),
                                                                      BMD_log_transfo = BMDlogtransfoBMDplot())
            } else {
                mybmdplotwithgradient <- DRomics::bmdplotwithgradient(myextendedresforBMD$myextendedresforBMD,
                                                                      xmin = xmin_bmdplotwotgradient(),
                                                                      xmax = maxDoseXScale(),
                                                                      scaling = TRUE,
                                                                      facetby = myfacetbycolumnsBMDplot,
                                                                      add.label = addlabelBMDplot(),
                                                                      BMD_log_transfo = BMDlogtransfoBMDplot())
            }
            if(isTRUE(colorbyBMDplot())) {
                if(input$nbLevel > 1) {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  facetby2 = myfacetbyrowsBMDplot,
                                                  colorby = "trend",
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                } else {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  colorby = "trend",
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                }
            } else {
                if(input$nbLevel > 1) {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  facetby2 = myfacetbyrowsBMDplot,
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                } else {
                    mybmdplot <- DRomics::bmdplot(myextendedresforBMD$myextendedresforBMD,
                                                  facetby = myfacetbycolumnsBMDplot,
                                                  add.CI = addciBMDplot(),
                                                  add.label = addlabelBMDplot(),
                                                  BMD_log_transfo = BMDlogtransfoBMDplot())
                }
            }
        }
        
        
        
        
        
        shinyjs::showElement('text1_step3', time = 0)
        output$buttonDownloadBMDplot <- downloadHandler(
            filename = function(){
                "bmdplot.pdf"
            },
            content = function(file) {
                pdf(file)
                plot(mybmdplot)
                dev.off()
            },
            contentType = {"application/pdf"}
        )
        
        ############ BMD plot with gradient ############
        output$bmdplotwithgradient <- renderPlot({
            
            shinyjs::showElement('text2_step3', time = 0)
            output$buttonDownloadBMDplotwithgradient <- downloadHandler(
                filename = function(){
                    "bmdplotwithgradient.pdf"
                },
                content = function(file) {
                    pdf(file)
                    plot(mybmdplotwithgradient)
                    dev.off()
                },
                contentType = {"application/pdf"}
            )
            
            return(mybmdplotwithgradient)
        })
        
        return(mybmdplot)
    })
    
    
    
    ################################################################################################
    ############ STEP 4 ############
    ################################################################################################
    
    # get the selected annotation levels when the 'Run' button in step3 in clicked to update the checkbox group in step4
    annotcheckboxBMDplotforCurvesplot <- eventReactive(input$buttonRunStep3, {input$annotcheckboxBMDplot})
    
    # Create the checkboxGroupInput for Curves plots
    observe({
        sortextendedres <- sortextendedres()
        myextendedmergeddata <- sortextendedres$myextendedmergeddata
        mypathclasslabel <- sortextendedres$mypathclasslabel
        mylevels <- names(table(as.vector(myextendedmergeddata[, mypathclasslabel])))
        
        updateCheckboxGroupInput(session, "annotcheckboxCurvesplot",
                                 label = "Choose at least one annotation",
                                 choices = mylevels, 
                                 selected = annotcheckboxBMDplotforCurvesplot()
        )
        
        # check all boxes if the button 'selectallCurvesplot' is clicked
        observeEvent(input$selectallCurvesplot, {
            if(input$selectallCurvesplot == 0) return(NULL) 
            else if (input$selectallCurvesplot > 0)
            {
                updateCheckboxGroupInput(session, "annotcheckboxCurvesplot",
                                         label = "Choose at least one annotation",
                                         choices = mylevels,
                                         selected = unlist(mylevels))
            }
        })
        
        # uncheck all boxes if the button 'unselectallCurvesplot' is clicked
        observeEvent(input$unselectallCurvesplot, {
            if(input$unselectallCurvesplot == 0) return(NULL) 
            else if (input$unselectallCurvesplot > 0)
            {
                updateCheckboxGroupInput(session, "annotcheckboxCurvesplot",
                                         label = "Choose at least one annotation",
                                         choices = mylevels,
                                         selected = "")
            }
        })
    })
    
    # Update the choice selected in 'facetby' in step4 according to 'facetby' in step3
    observe({
        updateRadioButtons(session, "facetbycolumnsCurvesplot", 
                           choices = list("Annotation" = "annotation", "Experimental level" = "explevel"), 
                           selected = facetbycolumnsBMDplot())
    })
    
    # Update the choices available in 'facetby' when there is only one level
    observeEvent(input$nbLevel, {
        if(input$nbLevel == 1)
            updateRadioButtons(session, "facetbycolumnsCurvesplot", choices = list("Annotation" = "annotation"), selected = "annotation")
        else
            updateRadioButtons(session, "facetbycolumnsCurvesplot", choices = list("Annotation" = "annotation", "Experimental level" = "explevel"), selected = "annotation")
    })
    
    # Update the choices available in 'facetby2' according to 'facetby'
    observeEvent(input$facetbycolumnsCurvesplot, {
        if(input$facetbycolumnsCurvesplot == "annotation")
            updateRadioButtons(session, "facetbyrowsCurvesplot", choices = list("Experimental level" = "explevel"), selected = "explevel")
        else if(input$facetbycolumnsCurvesplot == "explevel")
            updateRadioButtons(session, "facetbyrowsCurvesplot", choices = list("Annotation" = "annotation"), selected = "annotation")
    })
    
    
    # Update the min and max doses by default according to the log transformation and to the annotation levels selected
    observeEvent(input$annotcheckboxCurvesplot, {
        sortextendedres <- sortextendedres()
        myextendedmergeddata <- sortextendedres$myextendedmergeddata
        mypathclasslabel <- sortextendedres$mypathclasslabel
        myextendedresforCurvesplot <- myextendedmergeddata[myextendedmergeddata[, mypathclasslabel] %in% input$annotcheckboxCurvesplot, ]
        
        # get the BMD values in the combined, merged sorted and selected data frame
        BMD <- myextendedresforCurvesplot[, paste0("BMD.", input$BMDtype)]
        
        # Update the min and max doses by default according to the log transformation
        observeEvent(input$doselogtransfoCurvesplot, {
            if(input$doselogtransfoCurvesplot) {
                updateNumericInput(session, "mindoseCurvesplot", value = round(min(BMD) / 2, 2))
            } else {
                updateNumericInput(session, "mindoseCurvesplot", value = 0)
            }
        })
    })
    
    mindoseCurvesplot <- eventReactive(input$buttonRunStep4, {input$mindoseCurvesplot})
    doselogtransfoCurvesplot <- eventReactive(input$buttonRunStep4, {input$doselogtransfoCurvesplot})
    colorbyCurvesplot <- eventReactive(input$buttonRunStep4, {input$colorbyCurvesplot})
    addBMDCurvesplot <- eventReactive(input$buttonRunStep4, {input$addBMDCurvesplot})
    facetbycolumnsCurvesplot <- eventReactive(input$buttonRunStep4, {input$facetbycolumnsCurvesplot})
    facetbyrowsCurvesplot <- eventReactive(input$buttonRunStep4, {input$facetbyrowsCurvesplot})
    
    extendedresforCurvesplot <- eventReactive(input$buttonRunStep4, {
        validate(
            need(input$annotcheckboxCurvesplot, "Please choose at least one annotation")
        )
        sortextendedres <- sortextendedres()
        myextendedmergeddata <- sortextendedres$myextendedmergeddata
        mypathclasslabel <- sortextendedres$mypathclasslabel
        myextendedresforCurvesplot <- myextendedmergeddata[myextendedmergeddata[, mypathclasslabel] %in% input$annotcheckboxCurvesplot, ]
        return(list("myextendedresforCurvesplot" = myextendedresforCurvesplot,
                    "mypathclasslabel" = mypathclasslabel))
    })
    
    
    ############ curves plot ############
    output$curvesplot <- renderPlot({
        
        myextendedresforCurvesplot <- extendedresforCurvesplot()
        myfacetbycolumnsCurvesplot <- fnvaluecheckbox(facetbycolumnsCurvesplot(), myextendedresforCurvesplot$mypathclasslabel)
        myfacetbyrowsCurvesplot <- fnvaluecheckbox(facetbyrowsCurvesplot(), myextendedresforCurvesplot$mypathclasslabel)
        
        if(isTRUE(colorbyCurvesplot())) {
            if(input$nbLevel > 1) {
                mycurvesplot <- DRomics::curvesplot(myextendedresforCurvesplot$myextendedresforCurvesplot, 
                                                    free.y.scales = TRUE, scaling = TRUE,
                                                    xmin = mindoseCurvesplot(),
                                                    xmax = maxDoseXScale(),
                                                    dose_log_transfo = doselogtransfoCurvesplot(),
                                                    addBMD = addBMDCurvesplot(),
                                                    facetby = myfacetbycolumnsCurvesplot,
                                                    facetby2 = myfacetbyrowsCurvesplot,
                                                    colorby = "trend") + 
                    ggplot2::labs(col = "trend")
            } else {
                mycurvesplot <- DRomics::curvesplot(myextendedresforCurvesplot$myextendedresforCurvesplot,
                                                    free.y.scales = TRUE, scaling = TRUE,
                                                    xmin = mindoseCurvesplot(),
                                                    xmax = maxDoseXScale(),
                                                    dose_log_transfo = doselogtransfoCurvesplot(),
                                                    addBMD = addBMDCurvesplot(),
                                                    facetby = myfacetbycolumnsCurvesplot,
                                                    colorby = "trend") + 
                    ggplot2::labs(col = "trend")
            }
        } else {
            if(input$nbLevel > 1) {
                mycurvesplot <- DRomics::curvesplot(myextendedresforCurvesplot$myextendedresforCurvesplot,
                                                    free.y.scales = TRUE, scaling = TRUE,
                                                    xmin = mindoseCurvesplot(),
                                                    xmax = maxDoseXScale(),
                                                    dose_log_transfo = doselogtransfoCurvesplot(),
                                                    addBMD = addBMDCurvesplot(),
                                                    facetby = myfacetbycolumnsCurvesplot,
                                                    facetby2 = myfacetbyrowsCurvesplot)
            } else {
                mycurvesplot <- DRomics::curvesplot(myextendedresforCurvesplot$myextendedresforCurvesplot,
                                                    free.y.scales = TRUE, scaling = TRUE,
                                                    xmin = mindoseCurvesplot(),
                                                    xmax = maxDoseXScale(),
                                                    dose_log_transfo = doselogtransfoCurvesplot(),
                                                    addBMD = addBMDCurvesplot(),
                                                    facetby = myfacetbycolumnsCurvesplot)
            }
        }
        output$buttonDownloadCurvesplot <- downloadHandler(
            filename = function(){
                "curvesplot.pdf"
            },
            content = function(file) {
                pdf(file)
                plot(mycurvesplot)
                dev.off()
            },
            contentType = {"application/pdf"}
        )
        
        return(mycurvesplot)
    })
    
    
    ################################################################################################
    ############ STEP 5 ############
    ################################################################################################
    
    output$printRCode <- renderText({
        
        req(input$DRomicsData1)
        req(input$annotationData1)
        
        ##### STEP 1 #####
        text <- c("library(DRomics)",
                  "",
                  "# Step 1",
                  "extendedres <- list()",
                  "")
        
        for (i in 1:input$nbLevel) {
            text <- c(text, 
                      paste0("dromicsdata", i, " <- read.table('", eval(parse(text = paste0("input$DRomicsData", i, "$name"))), "', header = TRUE, stringsAsFactors = TRUE)"),
                      paste0("annotdata", i, " <- read.table('", eval(parse(text = paste0("input$annotationData", i, "$name"))), "', header = TRUE, stringsAsFactors = TRUE, sep = ", ifelse(input$sep_annotationData, "'\t'", "''"), ")"),
                      paste0("mergeddata", i, " <- merge(x = dromicsdata", i, ", y = annotdata", i, ", by.x = '", input[[paste0("id_DRomicsData", i)]], "', by.y = '", input[[paste0("id_annotationData", i)]], "')"),
                      paste0("mergeddata", i, " <- cbind('experimental_level' = '", gsub("\\s", "", input[[paste0("label", i)]]), "', mergeddata", i, ")"),
                      paste0("mergeddata", i, "$experimental_level <- as.factor(mergeddata", i, "$experimental_level)"),
                      paste0("extendedres <- rbind(extendedres, mergeddata", i, ")"),
                      ""
            )
        }
        
        text <- c(text, paste0("str(extendedres)"))
        for (i in 1:input$nbLevel) {
            text <- c(text, 
                      paste0("head(extendedres[extendedres$experimental_level == levels(extendedres$experimental_level)[", i, "], ], 3)")
            )
        }
        
        text <- c(text, "", "", "# For the other steps, 'extendedres' can be used as input of functions trendplot(), sensitivityplot(), bmdplot(),")
        text <- c(text, "# bmdplotwithgradient() and curvesplot(). You can find several examples of how to use these functions in the DRomics vignette.")
        
        
        ##### STEP 2 #####
        # text <- c(text, "", "", "# Step 2",
        #           paste0("mypathclasslabel <-  names(annotdata1)[which(names(annotdata1) != '", input$id_annotationData1, "')]"),
        #           paste0("extendedres <- selectgroups(extendedres = extendedres, BMDmax = ", 
        #                  input$BMDmax, ", group = mypathclasslabel, "),
        #           paste0("  explev = 'experimental_level', BMDtype = '", input$BMDtype, 
        #                  "', BMDsummary = '", match.arg(substr(input$BMDsummarysensitivityPlot, 1, 5), c('first.quartile', 'median')), 
        #                  "', nitemsmin = ", input$minNbItem, 
        #                  ", keepallexplev = ", input$keepAllExplev, ")")
        # )
        # 
        # if((input$ordering_onelev == "alphaorder_onelev" & input$nbLevel == 1) | (input$ordering_moreonelev == "alphaorder_moreonelev" & input$nbLevel > 1)) {
        #   text <- c(text, paste0("levelorder <- sort(levels(extendedres[, mypathclasslabel]), decreasing = FALSE) # ordered by alphabetic order"))
        # } else if((input$ordering_onelev == "numbitemorder_onelev" & input$nbLevel == 1) | (input$ordering_moreonelev == "numbitemorder_moreonelev" & input$nbLevel > 1)) {
        #   text <- c(text, paste0("levelorder <- names(sort(table(extendedres[, mypathclasslabel]), decreasing = FALSE)) # ordered by number of item"))
        #   
        # } else if((input$ordering_onelev == "specificorder_onelev" & input$nbLevel == 1) | (input$ordering_moreonelev == "specificorder_moreonelev" & input$nbLevel > 1)) {
        #   text <- c(text, paste0("levelorder <- ", input$labelssorted, " # ordered by specific order"))
        # } else {
        #   text <- c(text, paste0("levelorder <- levels(extendedres[, mypathclasslabel]) # ordered by levels order"))
        # }
        # 
        # text <- c(text, paste0("extendedres[, mypathclasslabel] <- factor(extendedres[, mypathclasslabel], levels = levelorder)"))
        # 
        # if(input$nbLevel > 1) {
        #   if(input$ordering_moreonelev == 'alphaorder_moreonelev' | input$ordering_moreonelev == 'specificorder_moreonelev'){
        #     text <- c(text, paste0("extendedres[, mypathclasslabel] <- factor(extendedres[, mypathclasslabel], levels = rev(levels(extendedres[, mypathclasslabel])))"))
        #   }
        # } else {
        #   if(input$ordering_onelev == 'alphaorder_onelev' | input$ordering_onelev == 'specificorder_onelev'){
        #     text <- c(text, paste0("extendedres[, mypathclasslabel] <- factor(extendedres[, mypathclasslabel], levels = rev(levels(extendedres[, mypathclasslabel])))"))
        #   }
        # }
        # 
        # text <- c(text, "")
        # if(input$nbLevel > 1) {
        #   text <- c(text, paste0("(mysensitivityplot <- DRomics::sensitivityplot(extendedres, 
        #                                               BMDtype = '", input$BMDtype, "', 
        #                                               group = mypathclasslabel,
        #                                               colorby = 'experimental_level',
        #                                               ECDF_plot = FALSE,
        #                                               BMDsummary = '", input$BMDsummarysensitivityPlot, "', 
        #                                               BMD_log_transfo = ", input$BMDlogtransfoSensitivityplot, "))"))
        #   text <- c(text, paste0("(mytrendplot <- DRomics::trendplot(extendedres, 
        #                                     group = mypathclasslabel, 
        #                                     facetby = 'experimental_level', 
        #                                     add.color = TRUE))"))
        # } else {
        #   myECDFplot <- if(orderingOnelev() == "BMDorder_onelev") {TRUE} else {FALSE}
        #   text <- c(text, paste0("(mysensitivityplot <- DRomics::sensitivityplot(extendedres, 
        #                                               BMDtype = '", input$BMDtype, "', 
        #                                               group = mypathclasslabel,
        #                                               ECDF_plot = ", myECDFplot, ",
        #                                               BMDsummary = '", input$BMDsummarysensitivityPlot, "', 
        #                                               BMD_log_transfo = ", input$BMDlogtransfoSensitivityplot, "))"))
        #   text <- c(text, paste0("(mytrendplot <- DRomics::trendplot(extendedres, 
        #                                     group = mypathclasslabel, 
        #                                     add.color = TRUE))"))
        # }
        
        
        
        # ##### STEP 3 #####
        # text <- c(text, "", "", "# Step 3")
        # 
        # 
        # ##### STEP 4 #####
        # text <- c(text, "", "", "# Step 4")
        # 
        
        # download button
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
    
}
