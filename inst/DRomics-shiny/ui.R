ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 500px;
              position:fixed;
              top: calc(50%);;
              left: calc(50% - 250px);;
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
                        fixedRow(column(10, offset = 2,
                                        h4("DRomics Shiny App is an interactive tool to fit dose-response curves to omic data and estimates corresponding benchmark doses."),
                                        h4("It runs on the ", a("shiny server of the LBBE", href = "http://lbbe.univ-lyon1.fr/", TARGET="_blank", style="color:#34837e;"),"."),
                                        h4("DRomics is also available on CRAN as an R package and on this web page.")
                        )),
                        hr(style='width: 70%;'),
                        fixedRow(column(10, offset = 2,
                                        p(strong("Authors & Contacts")),
                                        p(a("Marie-Laure DELIGNETTE-MULLER", href = "http://lbbe.univ-lyon1.fr/-Delignette-Muller-Marie-Laure-.html", TARGET = "_blank", style="color:#34837e;"), 
                                          "- marielaure.delignettemuller@vetagro-sup.fr - VetAgro Sup Campus Vétérinaire de Lyon - Laboratoire de Biométrie et Biologie Evolutive "),
                                        p("Floriane Larras - floriane.larras@ufz.de - Department of Bioanalytical Ecotoxicology - Helmholtz Center for Environmental Research GmbH - UFZ - Leipzig, Germany"),
                                        p("Elise Billoir - elise.billoir@univ-lorraine.fr"),
                                        p(a("Aurélie SIBERCHICOT", href = "https://lbbe.univ-lyon1.fr/-Siberchicot-Aurelie-.html", TARGET = "_blank", style="color:#34837e;"), 
                                          "- aurelie.siberchicot@univ-lyon1.fr - Laboratoire de Biométrie et Biologie Evolutive")
                        )),
                        hr(style='width: 70%;'),
                        # HTML("<hr style='height: 2px; color: #FF4000; background-color: #FF4000; width: 50%;>"),
                        fixedRow(column(10, offset = 2,
                                        p("Grant Agreement number: 705149 - MicroERA - H2020-MSCA-IF-2015"),
                                        br(), br()
                        )),
                        div(a(img(src = "logoLbbe.png", height = 68, width = 92), href="https://lbbe.univ-lyon1.fr/", TARGET="_blank"),
                            "   ",
                            a(img(src = "logoVetAgroSup.jpg", height = 100, width = 120), href="http://www.vetagro-sup.fr/", TARGET="_blank"),
                            "   ",
                            a(img(src = "logoLyon1.png", height = 80, width = 350), href="https://www.univ-lyon1.fr/", TARGET="_blank"),
                            style="text-align: center;")
                      )
             ),
             
             ####################################################################################
             ####### STEP 1 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#155450>Step 1</font>"),
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#155450><b>IMPORT, CHECK AND NORMALIZATION OF OMIC DATA</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 5,
                                   fileInput('datafile', 
                                             'Select an input file',
                                             accept = c('text/csv',
                                                        'text/comma-separated-values,
                                                      text/plain',
                                                        '.csv')),
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
                                   numericInput('FDR', label = 'False Discovery Rate (FDR) for the Benjamini-Hochberg correction of p-values',
                                                min = 0, max = 1, value = 0.05, step = 1e-03, width = "100%"))),
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
                      fixedRow(
                        column(12, 
                               br(), HTML("<font face=verdana size=5 color=#155450><b>DOSE RESPONSE MODELLING FOR RESPONSIVE ITEMS</b></font>"), br(), br(), br(),
                               fixedRow(
                                 sidebarPanel(
                                   style = "background-color: #a7dbd8;",
                                   width = 5,
                                   fixedRow(
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
                               
                               fixedRow(
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
                                   numericInput('zbmdcalc', label = 'z value for BMD_zSD', min = 0, max = 100, value = 1, step = 1e-02),
                                   numericInput('xbmdcalc', label = 'x value for BMD_xfold (in %)', min = 0, max = 100, value = 10, step = 1e-02),
                                   h5("See ", a("here", href = "informations_z_x.txt", TARGET = "_blank", style="text-decoration:underline; color:#155450;"), " information about the BMD_zSD and the BMD_xfold"),
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