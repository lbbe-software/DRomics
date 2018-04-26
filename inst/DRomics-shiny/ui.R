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
        title = ""
        # tags$head(tags$link(rel = "icon", type = "image/png", href = "logo-guts-shinnyapp.png"), tags$title(" GUTS-shinyapp"))
    ),
    theme = "styles.css", 
    style = "background-color: #d2d2d2;",
    
    wellPanel(style = "background-color: #d2d2d2;",
              fixedRow(
                  column(width = 7,
                         h1("DRomics Shiny Application")
                  ))
    ),
    
    ####################################################################################
    ####### STEP 1 #####################################################################
    ####################################################################################
    fixedRow(
        column(12, 
               wellPanel(style = "background-color: #ffffff;",
                         h3("Step 1: Import, check and normalization of omic data"), br(),
                         fixedRow(
                             sidebarPanel(
                                 width = 5,
                                 fileInput('datafile', 
                                           'Select an input file',
                                           accept = c('text/csv',
                                                      'text/comma-separated-values,
                                                      text/plain',
                                                      '.csv')),
                                 "You can test the DRomics shiny app with this ",
                                 tags$a(href='DRomicspkg/transcripto_sample.txt', target='blank', 'test file', download = 'transcripto_sample.txt'),
                                 " (choose the 'cyclic loess' method to normalize the data)."
                                 
                             ),
                             sidebarPanel(
                                 width = 4,
                                 radioButtons('normMethod', 
                                              'Select a method to normalize the data',
                                              choices = c('no normalization' = 'none', 
                                                          'cyclic loess' = 'cyclicloess',
                                                          quantile = 'quantile',
                                                          scale = 'scale'),
                                              selected = 'cyclicloess')
                             )
                             )
               )
        )
    ),
    
    
    ####################################################################################
    ####### STEP 2 #####################################################################
    ####################################################################################
    fixedRow(
        column(12, 
               wellPanel(style = "background-color: #ffffff;",
                         h3("Step 2: Selection of significantly responsive items"), br(),
                         fixedRow(
                             sidebarPanel(
                                 width = 3,
                                 radioButtons('selectMethod', 
                                              'Select a method',
                                              choices = c('quadratic trend test' = 'quadratic', 
                                                          'linear trend test' = 'linear',
                                                          'ANOVA test' = 'ANOVA')),
                                 br(),
                                 numericInput('FDR', label = 'False Discovery Rate for the Benjamini-Hochberg correction of p-values',
                                              min = 0, max = 1, value = 0.05, step = 1e-03, width = "100%")),
                             mainPanel(
                                 width = 9,
                                 verbatimTextOutput('printItemSelect')
                             )
                         )
               )
        )
    ),
    # "input.selectMethod == quadratic",
    ####################################################################################
    ####### STEP 3 #####################################################################
    ####################################################################################
    fixedRow(
        column(12, 
               wellPanel(style = "background-color: #ffffff;",
                         h3("Step 3: Dose response modelling for responsive items"), br(),
                         fixedRow(
                             sidebarPanel(
                                 width = 4,
                                 fixedRow(
                                     column(width = 9, "Click this button each time you update a setting above"),
                                     column(width = 2, actionButton("buttonDrcfit", "Fit", icon = icon("bar-chart-o"))))),
                             conditionalPanel(
                                 condition = "output.testdowload",
                                 sidebarPanel(width = 3,
                                              fluidRow(
                                                  style="text-align: center;", 
                                                  downloadButton("buttonDownloadDrcfitplot", "Download all the fitted dose-response plots", icon = icon("fas fa-download")))))
                             ),
                         
                         fixedRow(
                             mainPanel(
                                 width = 12,
                                 plotOutput("plotDrcfit", width = "100%", height = "900px")
                             )
                         )
               )
        )
    ),
    
    
    ####################################################################################
    ####### STEP 4 #####################################################################
    ####################################################################################
    fixedRow(
        column(12, 
               wellPanel(style = "background-color: #ffffff;",
                         h3("Step 4: Computation of benchmark doses for responsive items"), br(),
                         fixedRow(
                             sidebarPanel(
                                 width = 2,
                                 numericInput('zbmdcalc', label = 'z', min = 0, max = 100, value = 1, step = 1e-02),
                                 numericInput('xbmdcalc', label = 'x (in %)', min = 0, max = 100, value = 10, step = 1e-02),
                                 hr(),
                                 downloadButton("buttonResBmdcalc", "Download results", icon = icon("fas fa-download"))
                             ),
                             mainPanel(
                                 width = 10,
                                 verbatimTextOutput('printBmdcalc')
                             )),
                         
                         br(),
                         fixedRow(
                             sidebarPanel(
                                 width = 2,
                                 radioButtons('BMDtype', 
                                              'BMDtype',
                                              choices = c('zSD' = 'zSD',
                                                          'xfold' = 'xfold')),
                                 br(),
                                 radioButtons('plottype', 
                                              'plottype',
                                              choices = c('ecdf' = 'ecdf',
                                                          'hist' = 'hist',
                                                          'density' = 'density')),
                                 br(),
                                 conditionalPanel(
                                   condition = "input.plottype == 'hist'",
                                   numericInput('histbin', label = 'Hist bins',
                                                min = 1, max = 100, value = 30, step = 1, width = "100%"),
                                   br()
                                 ),
                                 radioButtons('bytypology', 
                                              'bytypology',
                                              choices = c('TRUE' = 'TRUE',
                                                          'FALSE' = 'FALSE'),
                                              selected = 'FALSE'),
                                 hr(),
                                 downloadButton("buttonPlotBmdcalc", "Download figure", icon = icon("fas fa-download"))
                             ),
                             mainPanel(
                                 width = 10,
                                 plotOutput("plotBmdcalc", width = "60%", height = "600px")
                             )
                         )
               )
        )
    )
)