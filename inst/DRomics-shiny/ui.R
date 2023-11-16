ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 500px;
              position:fixed;
              top: calc(50%);
              left: calc(50% - 250px);
              color: #9c5c16;
              font-size: 16px;
              font-style: italic;
              background-color: #c6c6c6;
            }
            .progress-bar {
              background-color: #9c5c16;
              value: #eee;
            }
            progress::-webkit-progress-value { 
  background-color: #4CAF50; 
            } 
progress::-moz-progress-bar { 
  background-color: #4CAF50; 
} 
           ",
           lang = "en"
      )
    )),
  
  titlePanel(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logodromics-dr.png"), tags$title(" DRomics-shiny"))
  ),
  br(),
  
  navbarPage(title = "", 
             tabPanel(img(src = "logodromics.png", title = "DRomics", width = 180),
                      
                      fluidRow(
                        br(), br(), 
                        HTML("<center><font face=verdana size=6 color=#9c5c16>Welcome to the DRomics-shiny application</font></center>"),
                        HTML("<center><font face=verdana size=5 color=#9c5c16>A first workflow for dose-response modelling</font></center>"),
                        br(), br(),
                        fixedRow(column(10, offset = 1,
                                        tags$blockquote("DRomics-shiny is a freely available tool for dose-response (or concentration-response) characterization from omics data.
                                             It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses (or concentrations)
                                             rather than a great number of replicates (no need of three replicates).", br(), 
                                                        "After a first optional step which consists to import, check and if needed normalize/transform the data (step 1), the aim of the proposed workflow 
                                             is to select monotonic and/or biphasic significantly responsive items (e.g. probes, metabolites) (step 2), to choose the best-fit model among a 
                                             predefined family of monotonic and biphasic models to describe the response of each selected item (step 3), and to derive a benchmark dose or 
                                             concentration from each fitted curve (step 4).", br(), 
                                                        "In the available version, DRomics supports single-channel microarray data (in log2 scale), RNAseq data (in raw counts) or metabolomics data 
                                             (in log scale). In order to link responses across biological levels based on a common method, DRomics also handles apical data as long as they are continuous and
                                             follow a normal distribution for each dose or concentration, with a common standard error.
                                             DRomics should not be used on other types of data.", br(), br(),
                                                        "Next, for interpretation of results in light of a biological annotation, you can use the ",
                                                        a("DRomicsInterpreter-shiny application", title = "DRomicsInterpreter-shiny application", href = "https://lbbe-shiny.univ-lyon1.fr/DRomics/inst/DRomicsInterpreter-shiny/", TARGET="_blank", style="color:#9c5c16;"), ".",
                                                        style="text-align:justify;")
                                        
                        )),
                        fixedRow(column(10, offset = 1,
                                        br(),
                                        p(strong("Links and resources")),
                                        p("The DRomics-shiny application runs on the ", 
                                           a("shiny server of the LBBE", title = "shiny server of the LBBE", href = "http://lbbe-shiny.univ-lyon1.fr/", TARGET="_blank", style="color:#9c5c16;"), 
                                           "with the develoment version of the DRomics package (available on ", 
                                           a("Github", title = "Github", href = "https://github.com/aursiber/DRomics", TARGET="_blank", style="color:#9c5c16;"),")."),
                                        p("DRomics is also an R package, available on ", 
                                           a("CRAN", title = "CRAN", href = "https://cran.r-project.org/package=DRomics", TARGET="_blank", style="color:#9c5c16;"), 
                                           " and on ",
                                           a("this web page", title = "this web page", href = "https://lbbe.univ-lyon1.fr/fr/dromics", TARGET="_blank", style="color:#9c5c16;"), 
                                           ", where you can find also a vignette and a cheat sheet."),
                                        p(helpText("If there seems to be a problem with the application, please send an explanatory e-mail at aurelie.siberchicot - at - univ-lyon1.fr.")),
                                        
                                        br(),
                                        p(strong("Citation")),
                                        p("If you use Dromics Shiny App, you should cite:"),
                                        p(em("DRomics, a workflow to exploit dose-response omics data in ecotoxicology "), br(),
                                          "Delignette-Muller ML, Siberchicot A, Larras F, Billoir E (2023).", 
                                          "Peer Community Journal.",
                                          a("https://peercommunityjournal.org/articles/10.24072/pcjournal.325/", title = "https://peercommunityjournal.org/articles/10.24072/pcjournal.325/", href = "https://peercommunityjournal.org/articles/10.24072/pcjournal.325/", TARGET = "_blank", style="color:#9c5c16;")),
                                        p(em("DRomics: a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment."), br(),
                                          "Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018).", 
                                          "Environmental Science & Technology.",
                                          a("https://doi.org/10.1021/acs.est.8b04752", title = "https://doi.org/10.1021/acs.est.8b04752", href = "https://pubs.acs.org/doi/10.1021/acs.est.8b04752", TARGET = "_blank", style="color:#9c5c16;"), br(),
                                          "You can freely find this article at: ", 
                                          a("https://hal.science/hal-02309919", title = "https://hal.science/hal-02309919", href = "https://hal.science/hal-02309919", TARGET = "_blank", style="color:#9c5c16;")),
                                        
                                        br(),
                                        p(strong("Contact")),
                                        p("If you have any need that is not yet covered, any feedback on the package / Shiny app, or any training needs, feel free to email us at ", strong("dromics@univ-lyon1.fr"), "."),
                                        p("Issues can be reported on",
                                          a("https://github.com/aursiber/DRomics/issues", title = "https://github.com/aursiber/DRomics/issues", href = "https://github.com/aursiber/DRomics/issues", TARGET = "_blank", style="color:#9c5c16;"), ".")
                                        
                                        
                        )),
                        hr(style='width: 80%;'),
                        br(),
                        fixedRow(column(10, offset = 2,
                                        fillRow(flex = NA,
                                          a(img(src = "https://lbbe.univ-lyon1.fr/sites/default/files/icons/logo_1.svg", title = "LBBE", width = 220), title = "LBBE", href="https://lbbe.univ-lyon1.fr/", TARGET="_blank"),
                                          a(img(src = "logoVetAgroSup.jpg", title = "VetAgroSup", height = 100, width = 120), title = "VetAgroSup", href = "http://www.vetagro-sup.fr/", TARGET="_blank"),
                                          a(img(src = "logoLyon1.png", title = "Université Claude Bernard Lyon 1", height = 80, width = 380), title = "Université Claude Bernard Lyon 1", href = "https://www.univ-lyon1.fr/", TARGET="_blank"),
                                          a(img(src = "logoLiec.png", title = "LIEC", height = 70, width = 100), title = "LIEC", href = "http://liec.univ-lorraine.fr/", TARGET="_blank"),
                                          a(img(src = "LogoUniversiteLorraine.png", title = "Université de Lorraine", height = 80, width = 180), title = "Université de Lorraine", href = "http://www.univ-lorraine.fr/", TARGET="_blank"),
                                          a(img(src = "LogoHelmholtz.jpg", title = "Helmholts", height = 80, width = 180), title = "Helmholts", href = "https://www.ufz.de/index.php?en=33573", TARGET="_blank"),
                                          img(src = "flag_yellow_high.jpg", title = "European Union", height = 70, width = 110),
                                          style="text-align: center;"
                                        )
                                        
                        ))
                        
                      )
             ),
             
             ####################################################################################
             ####### STEP 1 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 1</font>"),
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>IMPORT, CHECK AND PRETREATMENT OF OMICS DATA</b></font>"), br(), br(), br(),
                      fixedRow(
                        
                        ###### Select type of data                                
                        sidebarPanel(
                          style = "background-color: #F5aa4c;",
                          width = 3,
                          radioButtons('typeData', 
                                       "What kind of data do you use?",
                                       choices = c('microarray data (in log scale)' = 'microarraydata', 
                                                   'RNAseq data (in raw counts)' = 'rnaseqdata',
                                                   'metabolomics data (in log scale)' = 'metabolomicdata',
                                                   'anchoring continuous data (in a scale that enables the use of a normal error model)' = 'continuousanchoringdata'),
                                       selected = 'microarraydata'),
                          
                          ###### For micro-array data (default)
                          conditionalPanel(
                            condition = "input.typeData == 'microarraydata'",
                            tags$style(HTML('#bgdose_help1 {margin-top: 26px}')),
                            fileInput('datafile_microarray',
                                      'Select an input file',
                                      accept = c('.csv', '.txt')),
                            h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                            h5("See ", a("here", href = "DRomicspkg/transcripto_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'transcripto_sample.txt'), " an example file"),
                            br(),
                            splitLayout(cellWidths = c("40%", "60%"),
                                        textInput('bgdose_microarray', "Background dose", 0),
                                        bsButton("bgdose_help1", label = "", icon = icon("question"), size = "small"),
                                        bsPopover("bgdose_help1", "", text_bgdose, placement = "right", trigger = "hover", options = NULL)
                            ),
                            radioButtons('normMethod_microarray',
                                         'Select a method to normalize the data',
                                         choices = c('cyclic loess' = 'cyclicloess',
                                                     'quantile' = 'quantile',
                                                     'scale' = 'scale',
                                                     'no normalization' = 'none'),
                                         selected = 'cyclicloess'),
                            h5("See ", a("here", href = "informations_norm_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the normalization methods")
                          ),
                          
                          ###### For RNA-seq data
                          conditionalPanel(
                            condition = "input.typeData == 'rnaseqdata'",
                            tags$style(HTML('#bgdose_help2 {margin-top: 26px}')),
                            fileInput('datafile_rnaseq',
                                      'Select an input file',
                                      accept = c('.csv', '.txt')),
                            h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                            h5("See ", a("here", href = "DRomicspkg/RNAseq_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'RNAseq_sample.txt'), " an example file"),
                            icon("triangle-exclamation"), "Be aware that counts are automatically rounded to ensure compatibility of counts from Kallisto or Salmon with the tool.",
                            br(), br(),
                            splitLayout(cellWidths = c("40%", "60%"),
                                        textInput('bgdose_rnaseq', "Background dose", 0),
                                        bsButton("bgdose_help2", label = "", icon = icon("question"), size = "small"),
                                        bsPopover("bgdose_help2", "", text_bgdose, placement = "right", trigger = "hover", options = NULL)
                            ),
                            radioButtons('transfoMethod_rnaseq',
                                         'Select a method to transform the data',
                                         choices = c('regularized logarithm (rlog, may take a few minutes)' = 'rlog',
                                                     'variance stabilizing transformation (vst)' = 'vst'),
                                         selected = 'rlog'),
                            h5("See ", a("here", href = "informations_transfo_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the transformation methods")
                          ),
                          
                          ###### For metabolomic data
                          conditionalPanel(
                            condition = "input.typeData == 'metabolomicdata'",
                            tags$style(HTML('#bgdose_help3 {margin-top: 26px}')),
                            fileInput('datafile_metabolomic',
                                      'Select an input file',
                                      accept = c('.csv', '.txt')),
                            h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                            h5("See ", a("here", href = "DRomicspkg/metabolo_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'metabolo_norm.txt'), " an example file"),
                            br(),
                            splitLayout(cellWidths = c("40%", "60%"),
                                        textInput('bgdose_metabolomic', "Background dose", 0),
                                        bsButton("bgdose_help3", label = "", icon = icon("question"), size = "small"),
                                        bsPopover("bgdose_help3", "", text_bgdose, placement = "right", trigger = "hover", options = NULL)
                            ),
                            icon("triangle-exclamation"), "We recommend you to check that your metabolomics data were correctly pretreated before importation. In particular data (metabolomic signal) should have been log-transformed, without replacing 0 values by NA values (consider using the half minimum method instead for example).",
                            h5("See ", a("here", href = "informations_metabolo_pretreatment.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " more information about metabolomics data pretreatment")
                          ),
                          
                          ###### For continuous anchoring data
                          conditionalPanel(
                            condition = "input.typeData == 'continuousanchoringdata'",
                            tags$style(HTML('#bgdose_help4 {margin-top: 26px}')),
                            fileInput('datafile_anchoring',
                                      'Select an input file',
                                      accept = c('.csv', '.txt')),
                            h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                            h5("See ", a("here", href = "DRomicspkg/apical_anchoring.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'apical_anchoring.txt'), " an example file"),
                            br(), 
                            splitLayout(cellWidths = c("40%", "60%"),
                                        textInput('bgdose_anchoring', "Background dose", 0),
                                        bsButton("bgdose_help4", label = "", icon = icon("question"), size = "small"),
                                        bsPopover("bgdose_help4", "", text_bgdose, placement = "right", trigger = "hover", options = NULL)
                            ),
                            icon("triangle-exclamation"),
                            "We recommend you to check that your anchoring data are continuous and expressed in a scale that enables the use of a normal error model (a transformation of data may be needed for some endpoints). If this assumption is not respected, results of selection and further steps may be inaccurate."
                          ),
                          
                          fixedRow(
                            column(12, align="center",
                                   actionButton("buttonRunStep1", "Run", icon = icon("file-import"), style='font-size:200%')
                            )
                          )
                        ),
                        
                        mainPanel(
                          width = 9,
                          verbatimTextOutput('printOmicData'),
                          br(),
                          withSpinner(plotOutput("plotOmicData", width = "100%", height = "900px"), type = 4, color = '#9c5c16'),
                          br(),
                          conditionalPanel(
                            condition = "input.typeData != 'continuousanchoringdata'",
                            withSpinner(plotOutput("plotPCAData", width = "100%", height = "900px"), type = 4, color = '#9c5c16')
                          ),
                          br()
                        )
                      )
             ),
             
             
             ####################################################################################
             ####### STEP 2 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 2</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>SELECTION OF SIGNIFICANTLY RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 3,
                                   radioButtons('selectMethod', 
                                                'Select a method',
                                                choices = c('quadratic trend test' = 'quadratic', 
                                                            'linear trend test' = 'linear',
                                                            'ANOVA test' = 'ANOVA')),
                                   h5("See ", a("here", href = "informations_select_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the selection methods"),
                                   br(),
                                   textInput('FDR', label = 'False Discovery Rate (FDR) for the Benjamini-Hochberg correction of p-values', value = "0.05"),
                                   h5("See ", a("here", href = "informations_FDR_choice.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the choice of FDR"),
                                   fixedRow(
                                     column(12, align="center",
                                            actionButton("buttonRunStep2", "Run", icon = icon("fas fa-gear"), style='font-size:200%')
                                     )
                                   ), br(), 
                                   fixedRow(
                                     column(12, align="center",
                                            downloadButton("buttonDowloadItems", "Download all items", icon = icon("fas fa-download"), style='font-size:110%')
                                     ))
                                 ),
                                 mainPanel(
                                   width = 9,
                                   withSpinner(verbatimTextOutput('printItemSelect'), type = 4, color = '#9c5c16')
                                 )
                               )
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 3 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 3</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>DOSE RESPONSE MODELLING FOR RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 3,
                                   "Click this button each time you update a setting in previous steps:",
                                   br(), br(),
                                   fixedRow(
                                     column(12, align="center",
                                            actionButton("buttonDrcfit", "Fit", icon = icon("chart-bar"), style='font-size:250%')
                                     )),
                                   br(),
                                   h5("See ", a("here", href = "informations_modelling_procedure.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the dose reponse modelling procedure"),
                                   icon("triangle-exclamation"),
                                   "These ongoing calculations can take from minutes to about an hour. Your patience should be proportional to the size of your data and the chosen FDR.",
                                   hr(), 
                                   radioButtons('plottypeDrcfit', 
                                                'Plot type', inline = TRUE, 
                                                choices = c('dose / fitted' = 'dose_fitted',
                                                            'dose / residuals' = 'dose_residuals',
                                                            'fitted / residuals' = 'fitted_residuals'),
                                                selected = "dose_fitted"),
                                   radioButtons('logdosescale',
                                                'Log dose-scale', inline = TRUE,
                                                choices = c('yes' = 'TRUE',
                                                            'no' = 'FALSE'),
                                                selected = "TRUE"),
                                   hr(), 
                                   useShinyjs(),
                                   fixedRow(
                                     column(12, align="center",
                                            hidden(
                                              downloadButton("buttonDownloadDrcfitplot", 
                                                             HTML("Download all the fitted<br/>dose-response plots"), 
                                                             style = 'font-size:110%;', 
                                                             icon = icon("fas fa-download"))
                                            )))
                                 ),
                                 
                                 mainPanel(
                                   width = 9,
                                   verbatimTextOutput('printDrcfit'),
                                   br(), br(),
                                   withSpinner(plotOutput("plotDrcfit", width = "100%", height = "900px"), type = 4, color = '#9c5c16'),
                                   br(), br()
                                 )
                               )
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 4 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 4</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>COMPUTATION OF BENCHMARK DOSES FOR RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 3,
                                   textInput('zbmdcalc', label = 'z value for BMD-zSD', value = "1"),
                                   textInput('xbmdcalc', label = 'x value for BMD-xfold (in %)', value = "10"),
                                   h5("See ", a("here", href = "informations_z_x.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the BMD-zSD and the BMD-xfold"),
                                   hr(),
                                   fixedRow(
                                     column(12, align="center",
                                            actionButton("buttonRunStep4", "Calculate", icon = icon("fas fa-calculator"), style='font-size:200%')
                                     )
                                   ), br(), 
                                   fluidRow(
                                     column(12, align="center",
                                            downloadButton("buttonResBmdcalc", "Download results", icon = icon("fas fa-download"))
                                     )
                                   ), br(),
                                   h5("See ", a("here", href = "informations_bmdcalc_results.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the provided results")
                                 ),
                                 mainPanel(
                                   width = 9,
                                   withSpinner(verbatimTextOutput('printBmdcalc'), type = 4, color = '#9c5c16')
                                 )),
                               
                               br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 3,
                                   radioButtons('BMDtype', 
                                                'BMD type', inline = TRUE,
                                                choices = c('zSD' = 'zSD',
                                                            'xfold' = 'xfold')),
                                   br(),
                                   radioButtons('plottype', 
                                                'Plot type',
                                                choices = c('ECDF (Empirical Cumulative Distribution)' = 'ecdf',
                                                            'ECDF plot with color gradient' = 'ecdfcolorgradient',
                                                            'histogram' = 'hist' ,
                                                            'density' = 'density')),
                                   br(),
                                   conditionalPanel(
                                     condition = "input.plottype == 'hist'",
                                     numericInput('histbin', label = 'Histogram bins',
                                                  min = 1, max = 100, value = 30, step = 1, width = "100%"),
                                     br()
                                   ),
                                   conditionalPanel(
                                     condition = "input.plottype == 'ecdf'",
                                     radioButtons('logbmd_ecdf',
                                                  'Log transformation', inline = TRUE,
                                                  choices = c('yes' = 'TRUE',
                                                              'no' = 'FALSE'),
                                                  selected = "TRUE"),
                                     br()
                                   ),
                                   conditionalPanel(
                                     condition = "input.plottype == 'ecdfcolorgradient'",
                                     radioButtons('logbmd_ecdfgradient',
                                                  'Log transformation', inline = TRUE,
                                                  choices = c('yes' = 'TRUE',
                                                              'no' = 'FALSE'),
                                                  selected = "TRUE"),
                                     radioButtons('label_ecdfgradient',
                                                  'Labels', inline = TRUE,
                                                  choices = c('yes' = 'TRUE',
                                                              'no' = 'FALSE'),
                                                  selected = "TRUE"),
                                     br()
                                   ),
                                   radioButtons('splitby', 
                                                'Split by',
                                                choices = c('none' = 'none',
                                                            'trend' = 'trend',
                                                            'model' = 'model',
                                                            'typology' = 'typology'),
                                                selected = 'none'),
                                   h5("See ", a("here", href = "informations_modelling_procedure.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about models and trends"),
                                   hr(),
                                   radioButtons('fileformat_bmdcalc', 
                                                'File format',
                                                choices = c('pdf' = 'pdf',
                                                            'png' = 'png',
                                                            'jpeg' = 'jpeg',
                                                            'tiff' = 'tiff',
                                                            'svg' = 'svg'),
                                                selected = 'pdf'),
                                   fluidRow(
                                     column(12, align="center",
                                            downloadButton("buttonPlotBmdcalc", "Download figure", icon = icon("fas fa-download"))
                                     )
                                   )
                                 ),
                                 mainPanel(
                                   width = 9,
                                   plotOutput("plotBmdcalc", width = "100%", height = "900px")
                                 )
                               ),
                               
                               br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 3,
                                   radioButtons('BMDtype_plot2pdf', 
                                                'BMD type', inline = TRUE,
                                                choices = c('zSD' = 'zSD',
                                                            'xfold' = 'xfold')),
                                   radioButtons('logbmd_plot2pdf',
                                                'Log-dose scale', inline = TRUE,
                                                choices = c('yes' = 'TRUE',
                                                            'no' = 'FALSE'),
                                                selected = "TRUE"),
                                   hr(),
                                   fluidRow(
                                     column(12, align="center", 
                                            downloadButton("buttonDownloadDrcfitplotBMD", 
                                                           HTML("Download all the<br/>fitted dose-response<br/>plots with BMD"), 
                                                           icon = icon("fas fa-download"))
                                     )
                                   )
                                 ),
                                 mainPanel(
                                   width = 9,
                                   plotOutput("plotDrcfitBMD", width = "100%", height = "900px"),
                                   br(), br()
                                 )
                               )
                               
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 5 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>R code to go further</font>"),
                      fixedRow(
                        column(8, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>R CODE TO GO FURTHER</b></font>"), br(), br(), br(),
                               tags$blockquote("To see what more you can do using the R package, we recommend you to consult the vignette and the cheat sheet", 
                                               "(links to all resources ", a("here", href = "https://lbbe.univ-lyon1.fr/fr/dromics", TARGET="_blank", style="color:#9c5c16;"),")."), 
                               br(), 
                               downloadButton("buttonDownRCode", "Download R Code", icon = icon("fas fa-download"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'), br(), br(),
                               verbatimTextOutput('printRCode'), br(), br(),
                               downloadButton("buttonDownRCodeFurther", "Download R Code to go further", icon = icon("fas fa-download"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'), br(), br(),
                               verbatimTextOutput('printRCodeFurther'), br(), br()
                        ))
             )
  ), 

hr(),
helpText("If there seems to be a problem with the application, please send an explanatory e-mail at aurelie.siberchicot - at - univ-lyon1.fr.")
)