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
              color: #155450;
              font-size: 16px;
              font-style: italic;
              background-color: #c6c6c6;
            }
           "
      )
    )),
  
  titlePanel(
    # title = HTML("<font face=verdana size=6 color=#155450><b>DRomics Shiny Application</b></font>"),
    tags$head(tags$title("DRomics Shiny App"))
    # tags$head(tags$link(rel = "icon", type = "image/png", href = "logo-guts-shinnyapp.png"), tags$title(" GUTS-shinyapp"))
  ),
  br(),
  navbarPage(title = "", 
             
             tabPanel(HTML("<font face=verdana size=3 color=#155450>DRomics Shiny App</font>"),
                      fluidRow(
                        br(), br(), br(), 
                        HTML("<center><font face=verdana size=6 color=#155450>Welcome to the DRomics Shiny Application</font></center>"),
                        br(), br(),
                        fixedRow(column(8, offset = 2,
                                        tags$blockquote("DRomics is a freely available on-line tool for dose-response (or concentration-response) characterization from omics data.
                                             It is especially dedicated to omics data obtained using a typical dose-response design, favoring a great number of tested doses 
                                             (or concentrations, at least 6, and the more the better) rather than a great number of replicates (no need of three replicates).
                                             After a first optional step which consists to normalize the data (step 1), the aim of the proposed workflow is to select monotonic
                                             and/or biphasic significantly responsive items (e.g. probes, metabolites) (step 2), to choose the best-fit model among a predefined
                                             family of monotonic and biphasic models to describe the response of each selected item (step 3), and to derive 
                                             a benchmark dose or concentration and a typology of response from each fitted curve.
                                             In the available version data are supposed to be single-channel microarray data transformed in log2, or another type of data that can
                                             be directly fitted by least-square regression without any normalization step. In the future this tool will also be able to process RNA-seq data.", 
                                                        style="text-align:justify;"),
                                        br(),
                                        h4("DRomics Shiny App runs on the ", 
                                           a("shiny server of the LBBE", href = "http://lbbe-shiny.univ-lyon1.fr/", TARGET="_blank", style="color:#34837e;"), "(see here the ",
                                           a("DRomics tutorial", href = "Dromics_tutorial.pdf", TARGET="_blank", style="color:#34837e;"), ")."),
                                        h4("DRomics is also available on CRAN as an R package and on ",
                                           a("this web page", href = "https://lbbe.univ-lyon1.fr/-DRomics-.html", TARGET="_blank", style="color:#34837e;"), ".")
                        )),
                        hr(style='width: 70%;'),
                        fixedRow(column(8, offset = 2,
                                        p(strong("Authors & Contacts")),
                                        p(a("Elise Billoir", href = "http://bddc.liec.univ-lorraine.fr/cv/BILLOIR%20E.htm", TARGET = "_blank", style="color:#34837e;"), 
                                          "- elise.billoir@univ-lorraine.fr - Laboratoire Interdisciplinaire des Environnements Continentaux - Université de Lorraine - Metz - France"),
                                        p(a("Marie-Laure Delignette-Muller", href = "http://lbbe.univ-lyon1.fr/-Delignette-Muller-Marie-Laure-.html", TARGET = "_blank", style="color:#34837e;"), 
                                          "- marielaure.delignettemuller@vetagro-sup.fr - Laboratoire de Biométrie et Biologie Evolutive - VetAgro Sup - Lyon - France"),
                                        p(a("Floriane Larras", href = "http://www.ufz.de/index.php?en=42332&nopagecache", TARGET = "_blank", style="color:#34837e;"),
                                          "- floriane.larras@ufz.de - Department of Bioanalytical Ecotoxicology - Helmholtz Center for Environmental Research GmbH - Leipzig - Germany"),
                                        p(a("Mechthild Schmitt-Jansen", href = "https://www.ufz.de/index.php?en=38467", TARGET = "_blank", style="color:#34837e;"),
                                          "- mechthild.schmitt@ufz.de - Department of Bioanalytical Ecotoxicology - Helmholtz Center for Environmental Research GmbH - Leipzig - Germany"),
                                        br(),
                                        p(strong("Technical maintainer")),
                                        p(a("Aurélie Siberchicot", href = "https://lbbe.univ-lyon1.fr/-Siberchicot-Aurelie-.html", TARGET = "_blank", style="color:#34837e;"), 
                                          "- aurelie.siberchicot@univ-lyon1.fr - Laboratoire de Biométrie et Biologie Evolutive - Université Lyon 1 - Lyon - France")
                        )),
                        hr(style='width: 70%;'),
                        fixedRow(column(10, offset = 2,
                                        p("Grant Agreement number: 705149 - MicroERA - H2020-MSCA-IF-2015"),
                                        br(), br()
                        )),
                        div(a(img(src = "logoLbbe.png", height = 70, width = 92), href="https://lbbe.univ-lyon1.fr/", TARGET="_blank"),
                            a(img(src = "logoVetAgroSup.jpg", height = 100, width = 120), href="http://www.vetagro-sup.fr/", TARGET="_blank"),
                            a(img(src = "logoLyon1.png", height = 80, width = 380), href="https://www.univ-lyon1.fr/", TARGET="_blank"),
                            a(img(src = "logoLiec.png", height = 70, width = 100), href="http://liec.univ-lorraine.fr/", TARGET="_blank"),
                            a(img(src = "LogoUniversiteLorraine.png", height = 80, width = 180), href="http://www.univ-lorraine.fr/", TARGET="_blank"),
                            a(img(src = "LogoHelmholtz.jpg", height = 80, width = 180), href="https://www.ufz.de/index.php?en=33573", TARGET="_blank"),
                            style="text-align: center;")
                      )
             ),
             
             ####################################################################################
             ####### STEP 1 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#155450>Step 1</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#155450><b>IMPORT, CHECK AND NORMALIZATION OF OMICS DATA</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 5,
                                   fileInput('datafile', 
                                             'Select an input file',
                                             accept = c('text/csv', 'text/plain')),
                                   h5("See ", a("here", href = "informations_datafile_input.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the format required"),
                                   h5("See ", a("here", href = "DRomicspkg/transcripto_sample.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;", download = 'transcripto_sample.txt'), " an example file (choose the 'cyclic loess' method to normalize these data)")
                                 ),
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 4,
                                   radioButtons('normMethod', 
                                                'Select a method to normalize the data',
                                                choices = c('no normalization' = 'none', 
                                                            'cyclic loess' = 'cyclicloess',
                                                            quantile = 'quantile',
                                                            scale = 'scale'),
                                                selected = 'cyclicloess'),
                                   
                                   # tags$style(".popover{max-width: 30%;vertical-align: middle}"),
                                   # h5("See ", bsButton("Help_norm_methods", label = "here", style = "info", size = "extra-small"), " information about the normalization methods"),
                                   # bsPopover("Help_norm_methods", "help", HTML(help1), placement = "bottom", trigger = "click", options=list(container="body"))
                                   h5("See ", a("here", href = "informations_norm_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the normalization methods")
                                 )
                               ),
                               fixedRow(
                                 mainPanel(
                                   width = 12,
                                   verbatimTextOutput('printOmicData'),
                                   br(),
                                   plotOutput("plotOmicData", width = "100%", height = "900px"),
                                   br()
                                 )
                               )
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 2 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#155450>Step 2</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#155450><b>SELECTION OF SIGNIFICANTLY RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 3,
                                   radioButtons('selectMethod', 
                                                'Select a method',
                                                choices = c('quadratic trend test' = 'quadratic', 
                                                            'linear trend test' = 'linear',
                                                            'ANOVA test' = 'ANOVA')),
                                   h5("See ", a("here", href = "informations_select_methods.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the selection methods"),
                                   br(),
                                   textInput('FDR', label = 'False Discovery Rate (FDR) for the Benjamini-Hochberg correction of p-values', value = "0.05"))),
                               fixedRow(
                                 mainPanel(
                                   width = 12,
                                   verbatimTextOutput('printItemSelect')
                                 )
                               )
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 3 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#155450>Step 3</font>"),
                      fluidRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#155450><b>DOSE RESPONSE MODELLING FOR RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fluidRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 5,
                                   fluidRow(
                                     column(width = 9, "Click this button each time you update a setting in previous steps"),
                                     column(width = 2, actionButton("buttonDrcfit", "Fit", icon = icon("bar-chart-o")))),
                                   br(), 
                                   h5("See ", a("here", href = "informations_modelling_procedure.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the dose reponse modelling procedure")),
                                 conditionalPanel(
                                   condition = "output.testdowload",
                                   sidebarPanel(style = "background-color: #a7dbd8;",
                                                width = 3,
                                                fluidRow(
                                                  style="text-align: center;", 
                                                  downloadButton("buttonDownloadDrcfitplot", "Download all the fitted dose-response plots", icon = icon("fas fa-download")))))
                               ),
                               
                               fluidRow(
                                 mainPanel(
                                   width = 12,
                                   verbatimTextOutput('printDrcfit'),
                                   br(),
                                   plotOutput("plotDrcfit", width = "100%", height = "900px"),
                                   br(), br()
                                 )
                               )
                        )
                      )),
             
             
             ####################################################################################
             ####### STEP 4 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#155450>Step 4</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#155450><b>COMPUTATION OF BENCHMARK DOSES FOR RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 2,
                                   textInput('zbmdcalc', label = 'z value for BMD-zSD', value = "1"),
                                   textInput('xbmdcalc', label = 'x value for BMD-xfold (in %)', value = "10"),
                                   h5("See ", a("here", href = "informations_z_x.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the BMD-zSD and the BMD-xfold"),
                                   hr(),
                                   downloadButton("buttonResBmdcalc", "Download results", icon = icon("fas fa-download")),
                                   h5("See ", a("here", href = "informations_bmdcalc_results.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the provided results")
                                 ),
                                 mainPanel(
                                   width = 10,
                                   verbatimTextOutput('printBmdcalc')
                                 )),
                               
                               br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
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
                                                            'density' = 'density')),
                                   br(),
                                   conditionalPanel(
                                     condition = "input.plottype == 'hist'",
                                     numericInput('histbin', label = 'Histogram bins',
                                                  min = 1, max = 100, value = 30, step = 1, width = "100%"),
                                     br()
                                   ),
                                   radioButtons('bytypology', 
                                                'by typology ?',
                                                choices = c('TRUE' = 'TRUE',
                                                            'FALSE' = 'FALSE'),
                                                selected = 'FALSE'),
                                   h5("See ", a("here", href = "informations_modelling_procedure.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about typologies"),
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
                      ))
  )
)