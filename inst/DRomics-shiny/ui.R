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
            }
           "
      )
    )),
  
  titlePanel(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logodromics-dr.png"), tags$title(" DRomics Shiny App"))
  ),
  br(),
  
  navbarPage(title = "", 
             tabPanel(img(src = "logodromics.png", width = 180),
                      
                      fluidRow(
                        br(), br(), 
                        HTML("<center><font face=verdana size=6 color=#9c5c16>Welcome to the DRomics Shiny Application</font></center>"),
                        br(), br(),
                        fixedRow(column(8, offset = 2,
                                        tags$blockquote("DRomics is a freely available on-line tool for dose-response (or concentration-response) characterization from omics data.
                                             It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses 
                                             (or concentrations, at least 6, and the more the better) rather than a great number of replicates (no need of three replicates).", br(), 
                                             "After a first optional step which consists to import, check and if needed normalize/transform the data (step 1), the aim of the proposed workflow 
                                             is to select monotonic and/or biphasic significantly responsive items (e.g. probes, metabolites) (step 2), to choose the best-fit model among a 
                                             predefined family of monotonic and biphasic models to describe the response of each selected item (step 3), and to derive a benchmark dose or 
                                             concentration from each fitted curve (step 4).", br(), 
                                             "In the available version, DRomics supports single-channel microarray data (in log2 scale), RNAseq data (in raw counts) or metabolomics data 
                                             (in log scale). It should not be used on other types of data.", 
                                                        style="text-align:justify;"),
                                        br(),
                                        h4("DRomics Shiny App runs on the ", 
                                           a("shiny server of the LBBE", href = "http://lbbe-shiny.univ-lyon1.fr/", TARGET="_blank", style="color:#f28d0f;"), "(see here the ",
                                           a("DRomics tutorial", href = "Dromics_tutorial.pdf", TARGET="_blank", style="color:#f28d0f;"), "),
                                           with the develoment version of the DRomics package (available on ", 
                                           a("Github", href = "https://github.com/aursiber/DRomics", TARGET="_blank", style="color:#f28d0f;"),")."),
                                        h4("DRomics is also an R package, available on ", 
                                           a("CRAN", href = "https://cran.r-project.org/package=DRomics", TARGET="_blank", style="color:#f28d0f;"), 
                                           " and on ",
                                           a("this web page", href = "https://lbbe.univ-lyon1.fr/-DRomics-.html", TARGET="_blank", style="color:#f28d0f;"), ".")
                        )),
                        hr(style='width: 70%;'),
                        fixedRow(column(8, offset = 2,
                                        p(strong("If you use Dromics Shiny App, you should cite:")),
                                        p("Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018).", br(),
                                          em("DRomics: a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment."), br(),
                                          "Environmental Science & Technology.",
                                          a("https://doi.org/10.1021/acs.est.8b04752", href = "https://pubs.acs.org/doi/10.1021/acs.est.8b04752", TARGET = "_blank", style="color:#f28d0f;")),
                                        
                                        br(),
                                        p("You can find this article at: ", a("https://hal.archives-ouvertes.fr/hal-02309919", href = "https://hal.archives-ouvertes.fr/hal-02309919", TARGET = "_blank", style="color:#f28d0f;")),
                                        
                                        br(),
                                        p("You can also look at the following citation for a complete example of use:"),
                                        p("Larras F, Billoir E, Scholz S, Tarkka M, Wubet T, Delignette-Muller ML, Schmitt-Jansen M (2020).", br(),
                                          em("A multi-omics concentration-response framework uncovers novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus."), br(),
                                          "Journal of Hazardous Materials.",
                                          a("https://doi.org/10.1016/j.jhazmat.2020.122727", href = "https://doi.org/10.1016/j.jhazmat.2020.122727", TARGET = "_blank", style="color:#f28d0f;")),
                                          
                                        br(),
                                        p(strong("Authors & Contacts")),
                                        p(a("Elise Billoir", href = "http://bddc.liec.univ-lorraine.fr/cv/BILLOIR%20E.htm", TARGET = "_blank", style="color:#f28d0f;"), 
                                          "- elise.billoir@univ-lorraine.fr - Laboratoire Interdisciplinaire des Environnements Continentaux - Université de Lorraine - Metz - France"),
                                        p(a("Marie-Laure Delignette-Muller", href = "http://lbbe.univ-lyon1.fr/-Delignette-Muller-Marie-Laure-.html", TARGET = "_blank", style="color:#f28d0f;"), 
                                          "- marielaure.delignettemuller@vetagro-sup.fr - Laboratoire de Biométrie et Biologie Evolutive - VetAgro Sup - Lyon - France"),
                                        p(a("Floriane Larras", href = "http://www.ufz.de/index.php?en=42332&nopagecache", TARGET = "_blank", style="color:#f28d0f;"),
                                          "- floriane.larras@ufz.de - Department of Bioanalytical Ecotoxicology - Helmholtz Center for Environmental Research GmbH - Leipzig - Germany"),
                                        p(a("Mechthild Schmitt-Jansen", href = "https://www.ufz.de/index.php?en=38467", TARGET = "_blank", style="color:#f28d0f;"),
                                          "- mechthild.schmitt@ufz.de - Department of Bioanalytical Ecotoxicology - Helmholtz Center for Environmental Research GmbH - Leipzig - Germany"),
                                        br(),
                                        p(strong("Technical maintainer")),
                                        p(a("Aurélie Siberchicot", href = "https://lbbe.univ-lyon1.fr/-Siberchicot-Aurelie-.html", TARGET = "_blank", style="color:#f28d0f;"), 
                                          "- aurelie.siberchicot@univ-lyon1.fr - Laboratoire de Biométrie et Biologie Evolutive - Université Lyon 1 - Lyon - France"),
                                        p("Issues can be reported on", 
                                          a("https://github.com/aursiber/DRomics/issues", href = "https://github.com/aursiber/DRomics/issues", TARGET = "_blank", style="color:#f28d0f;"), ".")
                        )),
                        hr(style='width: 70%;'),
                        fixedRow(column(10, offset = 2,
                                        p("Grant Agreement number: 705149 - MicroERA - H2020-MSCA-IF-2015"),
                                        p("Horizon 2020"),
                                        br(), br()
                        )),
                        div(a(img(src = "logoLbbe.png", height = 70, width = 92), href="https://lbbe.univ-lyon1.fr/", TARGET="_blank"),
                            a(img(src = "logoVetAgroSup.jpg", height = 100, width = 120), href="http://www.vetagro-sup.fr/", TARGET="_blank"),
                            a(img(src = "logoLyon1.png", height = 80, width = 380), href="https://www.univ-lyon1.fr/", TARGET="_blank"),
                            a(img(src = "logoLiec.png", height = 70, width = 100), href="http://liec.univ-lorraine.fr/", TARGET="_blank"),
                            a(img(src = "LogoUniversiteLorraine.png", height = 80, width = 180), href="http://www.univ-lorraine.fr/", TARGET="_blank"),
                            a(img(src = "LogoHelmholtz.jpg", height = 80, width = 180), href="https://www.ufz.de/index.php?en=33573", TARGET="_blank"),
                            img(src = "flag_yellow_high.jpg", height = 70, width = 110),
                            style="text-align: center;")
                      )
             ),
             
             ####################################################################################
             ####### STEP 1 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 1</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>IMPORT, CHECK AND PRETREATMENT OF OMICS DATA</b></font>"), br(), br(), br(),
                               
                               fixedRow(
                                 
                                 ###### Select type of data 
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 4,
                                   radioButtons('typeData', 
                                                "What kind of data do you use?",
                                                choices = c('microarray data (in log scale)' = 'microarraydata', 
                                                            'RNAseq data (in raw counts)' = 'rnaseqdata',
                                                            'metabolomics data (in log scale)' = 'metabolomicdata'),
                                                selected = 'microarraydata'), 
                                   br(), br()),
                                 
                                 ###### For micro-array data (default)
                                 conditionalPanel(
                                   condition = "input.typeData == 'microarraydata'",
                                   
                                   sidebarPanel(
                                     style = "background-color: #F5aa4c;",
                                     width = 4,
                                     fileInput('datafile_microarray', 
                                               'Select an input file',
                                               accept = c('text/csv', 'text/plain')),
                                     h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                                     h5("See ", a("here", href = "DRomicspkg/transcripto_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'transcripto_sample.txt'), " an example file")
                                   ),
                                   sidebarPanel(
                                     style = "background-color: #F5aa4c;",
                                     width = 4,
                                     radioButtons('normMethod_microarray', 
                                                  'Select a method to normalize the data',
                                                  choices = c('cyclic loess' = 'cyclicloess',
                                                              'quantile' = 'quantile',
                                                              'scale' = 'scale',
                                                              'no normalization' = 'none'),
                                                  selected = 'cyclicloess'),
                                     
                                     h5("See ", a("here", href = "informations_norm_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the normalization methods")
                                   )
                                 ),
                                 
                                 ###### For RNA-seq data
                                 conditionalPanel(
                                   condition = "input.typeData == 'rnaseqdata'",
                                   sidebarPanel(
                                     style = "background-color: #F5aa4c;",
                                     width = 4,
                                     fileInput('datafile_rnaseq', 
                                               'Select an input file',
                                               accept = c('text/csv', 'text/plain')),
                                     h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                                     h5("See ", a("here", href = "DRomicspkg/RNAseq_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'RNAseq_sample.txt'), " an example file")
                                   ),
                                   sidebarPanel(
                                     style = "background-color: #F5aa4c;",
                                     width = 4,
                                     radioButtons('transfoMethod_rnaseq', 
                                                  'Select a method to transform the data',
                                                  choices = c('regularized logarithm (rlog)' = 'rlog',
                                                              'variance stabilizing transformation (vst)' = 'vst'),
                                                  selected = 'rlog'),
                                     
                                     h5("See ", a("here", href = "informations_transfo_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the transformation methods")
                                   )
                                 ),
                                 
                                 ###### For metabolomic data
                                 conditionalPanel(
                                   condition = "input.typeData == 'metabolomicdata'",
                                   sidebarPanel(
                                     style = "background-color: #F5aa4c;",
                                     width = 4,
                                     fileInput('datafile_metabolomic', 
                                               'Select an input file',
                                               accept = c('text/csv', 'text/plain')),
                                     h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the format required"),
                                     h5("See ", a("here", href = "DRomicspkg/metabolo_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;", download = 'metabolo_norm.txt'), " an example file")
                                   ),
                                   sidebarPanel(
                                     style = "background-color: #F5aa4c;",
                                     width = 4,
                                     icon("exclamation-triangle"), "We recommend you to check that your metabolomics data were correctly pretreated before importation. In particular data (metabolomic signal) should have been log-transformed, without replacing 0 values by NA values (consider using the half minimum method instead for example).",
                                     h5("See ", a("here", href = "informations_metabolo_pretreatment.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " more information about metabolomics data pretreatment")
                                   )
                                 )
                                 
                               ),
                               
                               
                               fixedRow(
                                 mainPanel(
                                   width = 12,
                                   verbatimTextOutput('printOmicData'),
                                   br(),
                                   withSpinner(plotOutput("plotOmicData", width = "100%", height = "900px"), type = 4, color = '#9c5c16'),
                                   br()
                                 )
                               )
                        )
                      )),
             
             
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
                                   textInput('FDR', label = 'False Discovery Rate (FDR) for the Benjamini-Hochberg correction of p-values', value = "0.05"))),
                               fixedRow(
                                 mainPanel(
                                   width = 12,
                                   withSpinner(verbatimTextOutput('printItemSelect'), type = 4, color = '#9c5c16')
                                 )
                               )
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 3 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 3</font>"),
                      fluidRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>DOSE RESPONSE MODELLING FOR RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fluidRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 8,
                                   fluidRow(
                                     column(width = 6, 
                                            "Click this button each time you update a setting in previous steps:",
                                            br(), 
                                            h5("See ", a("here", href = "informations_modelling_procedure.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the dose reponse modelling procedure")
                                     ),
                                     column(width = 2, actionButton("buttonDrcfit", "Fit", icon = icon("bar-chart-o"), style='font-size:200%')),
                                     column(width = 3,
                                            useShinyjs(),
                                            hidden(
                                              downloadButton("buttonDownloadDrcfitplot", HTML("Download all the fitted<br/>dose-response plots"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d; font-size:110%;', icon = icon("fas fa-download"))
                                            )
                                     )
                                   )
                                 ),
                                 
                                 sidebarPanel(
                                   style = "background-color: #FFFFFF;",
                                   width = 4,
                                   icon("exclamation-triangle"),
                                   "These ongoing calculations can take from minutes to about an hour. Your patience should be proportional to the size of your data and the chosen FDR."
                                 )
                               ),
                               
                               fluidRow(
                                 mainPanel(
                                   width = 12,
                                   verbatimTextOutput('printDrcfit'),
                                   br(),
                                   radioButtons('plottypeDrcfit', 
                                                'Plot type', inline = TRUE, 
                                                choices = c('dose / fitted' = 'dose_fitted',
                                                            'dose / residuals' = 'dose_residuals',
                                                            'fitted / residuals' = 'fitted_residuals'),
                                                selected = "dose_fitted"),
                                   br(),
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
                                   width = 2,
                                   textInput('zbmdcalc', label = 'z value for BMD-zSD', value = "1"),
                                   textInput('xbmdcalc', label = 'x value for BMD-xfold (in %)', value = "10"),
                                   h5("See ", a("here", href = "informations_z_x.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the BMD-zSD and the BMD-xfold"),
                                   hr(),
                                   downloadButton("buttonResBmdcalc", "Download results", icon = icon("fas fa-download")),
                                   h5("See ", a("here", href = "informations_bmdcalc_results.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about the provided results")
                                 ),
                                 mainPanel(
                                   width = 10,
                                   verbatimTextOutput('printBmdcalc')
                                 )),
                               
                               br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #F5aa4c;",
                                   width = 2,
                                   radioButtons('BMDtype', 
                                                'BMD type',
                                                choices = c('zSD' = 'zSD',
                                                            'xfold' = 'xfold')),
                                   br(),
                                   radioButtons('plottype', 
                                                'plot type',
                                                choices = c('empirical cumulative distribution' = 'ecdf',
                                                            'histogram' = 'hist' ,
                                                            'density' = 'density', 
                                                            'ECDF plot with color gradient' = 'ecdfcolorgradient')),
                                   br(),
                                   conditionalPanel(
                                     condition = "input.plottype == 'hist'",
                                     numericInput('histbin', label = 'Histogram bins',
                                                  min = 1, max = 100, value = 30, step = 1, width = "100%"),
                                     br()
                                   ),
                                   radioButtons('splitby', 
                                                'split by',
                                                choices = c('none' = 'none',
                                                            'trend' = 'trend',
                                                            'model' = 'model',
                                                            'typology' = 'typology'),
                                                selected = 'none'),
                                   h5("See ", a("here", href = "informations_modelling_procedure.txt", TARGET = "_blank", style="text-decoration:underline; color:#9c5c16;"), " information about typologies"),
                                   hr(),
                                   downloadButton("buttonPlotBmdcalc", "Download figure", icon = icon("fas fa-download"))
                                 ),
                                 mainPanel(
                                   width = 10,
                                   plotOutput("plotBmdcalc", width = "100%", height = "900px"),
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
                               downloadButton("buttonDownRCode", "Download R Code", icon = icon("fas fa-download"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'), br(), br(),
                               verbatimTextOutput('printRCode'), br(), br(),
                               downloadButton("buttonDownRCodeFurther", "Download R Code to go further", icon = icon("fas fa-download"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'), br(), br(),
                               verbatimTextOutput('printRCodeFurther'), br(), br()
                      ))
             )
  )
)