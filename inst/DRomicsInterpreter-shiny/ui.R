## JavaScript that dis/enables the ABILITY to click the tab (without changing aesthetics)
# app_jscode <-
#   "shinyjs.disableTab = function(name) {
#     var tab = $('.nav li a[data-value=' + name + ']');
#     tab.bind('click.tab', function(e) {
#       e.preventDefault();
#       return false;
#     });
#     tab.addClass('disabled');
#   }
#   shinyjs.enableTab = function(name) {
#     var tab = $('.nav li a[data-value=' + name + ']');
#     tab.unbind('click.tab');
#     tab.removeClass('disabled');
#   }"
## css snipit that makes it LOOK like we are/n't able click the tab (with outchanging functionality)
app_css <-
  ".nav li a.disabled {
    background-color: #aaa !important;
    color: #333 !important;
    cursor: not-allowed !important;
    border-color: #aaa !important;
  }
  .progress-bar {
    background-color: #9c5c16;
  }
  .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
    background: #9c5c16;
    border-top: 1px #9c5c16 ;
    border-bottom: 1px #9c5c16 ;
  }
  label > input[type='radio'] + *::before {
    content: '';
    margin: 4px 0 0;
    width: 13px;
    height: 13px;
    position: absolute;
    margin-left: -20px;
    border-radius: 50%;
    border-style: solid;
    border-width: 0.1rem;
    border-color: #9c5c16;
  }
  label > input[type='radio']:checked + *::before {
    background: radial-gradient(white 0%, white 30%, #9c5c16 30%, #9c5c16);
    border-color: #9c5c16;
  }
  label > input[type='checkbox'] {
    opacity: 0;
    position: absolute;
  }
  label > input[type='checkbox'] + *::before {
    content: '';
    position: absolute;
    margin: 4px 0 0;
    margin-left: -20px;
    align: center;
    width: 13px;
    height: 13px;
    margin-right: 1rem;
    border-radius: 0%;
    border-style: solid;
    border-width: 0.1rem;
    border-color: #9c5c16;
  }
  label > input[type='checkbox']:checked + *::before {
    content: '';
    width: 13px;
    height: 13px;
    background-color: #9c5c16;
  }
"


ui <- fluidPage(
  shinyjs::useShinyjs(),
  # shinyjs::extendShinyjs(text = app_jscode, functions = c('disableTab','enableTab')),
  shinyjs::inlineCSS(app_css),
  
  titlePanel(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logodromics-dr.png"), tags$title(" DRomicsInterpreter-shiny"))
  ),
  br(),
  
  navbarPage(title = "", 
             tabPanel(img(src = "logodromics.png", width = 180),

                      fluidRow(
                        br(), br(),
                        HTML("<center><font face=verdana size=6 color=#9c5c16>Welcome to the DRomicsInterpreter-shiny application</font></center>"),
                        HTML("<center><font face=verdana size=5 color=#9c5c16>A second workflow for interpretation in light of a biological annotation</font></center>"),
                        br(), br(),
                        fixedRow(column(10, offset = 1,
                                        br(),
                                        p(strong("Links and resources")),
                                        p("The DRomicsInterpreter-shiny application runs on the ",
                                           a("shiny server of the LBBE", href = "http://lbbe-shiny.univ-lyon1.fr/", TARGET="_blank", style="color:#f28d0f;"),
                                           "with the develoment version of the DRomics package (available on ",
                                           a("Github", href = "https://github.com/aursiber/DRomics", TARGET="_blank", style="color:#f28d0f;"),")."),
                                        p("DRomics is also an R package, available on ",
                                           a("CRAN", href = "https://cran.r-project.org/package=DRomics", TARGET="_blank", style="color:#f28d0f;"), ".", 
                                          " You can find more information and help about the DRomicsInterpreter-shiny application and the DRomics package on ",
                                           a("this web page", href = "https://lbbe.univ-lyon1.fr/fr/dromics", TARGET="_blank", style="color:#f28d0f;"), "."),
                                        p(" Reading the vignette first and using the cheat sheet (both are available on this ", 
                                          a("this page", href = "https://lbbe.univ-lyon1.fr/fr/dromics", TARGET="_blank", style="color:#f28d0f;"), 
                                          ") are recommended. "),
                                        
                                        br(),
                                        p(strong("Citation and publications")),
                                        p("If you use Dromics Shiny App, you should cite:"),
                                        p(em("DRomics: a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment."), br(),
                                          "Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018).", 
                                          "Environmental Science & Technology.",
                                          a("https://doi.org/10.1021/acs.est.8b04752", href = "https://pubs.acs.org/doi/10.1021/acs.est.8b04752", TARGET = "_blank", style="color:#f28d0f;")),
                                        p("You can freely find this article at: ", a("https://hal.archives-ouvertes.fr/hal-02309919", href = "https://hal.archives-ouvertes.fr/hal-02309919", TARGET = "_blank", style="color:#f28d0f;")),

                                        br(),
                                        p("You can also look at the following citation for a complete example of use:"),
                                        p(em("A multi-omics concentration-response framework uncovers novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus."), br(),
                                          "Larras F, Billoir E, Scholz S, Tarkka M, Wubet T, Delignette-Muller ML, Schmitt-Jansen M (2020).",
                                          "Journal of Hazardous Materials.",
                                          a("https://doi.org/10.1016/j.jhazmat.2020.122727", href = "https://doi.org/10.1016/j.jhazmat.2020.122727", TARGET = "_blank", style="color:#f28d0f;")),

                                        br(),
                                        p(strong("Contact")),
                                        p("If you have any need that is not yet covered, any feedback on the package / Shiny app, or any training needs, feel free to email us at ", strong("dromics@univ-lyon1.fr"), "."),
                                        p("Issues can be reported on",
                                          a("https://github.com/aursiber/DRomics/issues", href = "https://github.com/aursiber/DRomics/issues", TARGET = "_blank", style="color:#f28d0f;"), ".")

                                        
                        )),
                        
                        hr(style='width: 80%;'),
                        fixedRow(column(10, offset = 2,
                                        fillRow(flex = 0.11,
                                                a(img(src = "https://lbbe.univ-lyon1.fr/sites/default/files/icons/logo_1.svg", width = 120), href="https://lbbe.univ-lyon1.fr/", TARGET="_blank"),
                                                a(img(src = "logoVetAgroSup.jpg", height = 100, width = 120), href="http://www.vetagro-sup.fr/", TARGET="_blank"),
                                                a(img(src = "logoLyon1.png", height = 80, width = 380), href="https://www.univ-lyon1.fr/", TARGET="_blank"),
                                                a(img(src = "LogoUniversiteLorraine.png", height = 80, width = 180), href="http://www.univ-lorraine.fr/", TARGET="_blank"),
                                                style="text-align: center;"
                                        )
                                        
                        ))
                        
                      )
             ),
             
             ####################################################################################
             ####### STEP 1 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 1</font>"),
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>Import and merge of DRomics results and annotation data</b></font>"), br(), br(), br(),
                      
                      fluidRow(
                        column(1,
                               numericInput("nbLevel", "Number of experimental levels", value = 1, min = 1, max = 10, step = 1)
                        ),
                        column(2, style = "margin-top: 25px;padding:20px;", 
                               shinyBS::bsButton("nblevel_help", label = "", icon = icon("info"), size = "small"),
                               shinyBS::bsPopover("nblevel_help", "", texthelpnblevel, placement = "right", trigger = "hover", options = NULL)
                        ),
                        column(2,
                               textInput("maxDoseXScale", "Maximal dose/concentration for definition of x-scale of plots", width = "100%")
                        ), 
                        column(3, style = "margin-top: 25px;padding:20px;", 
                               shinyBS::bsButton("maxdosexscale_help", label = "", icon = icon("info"), size = "small"),
                               shinyBS::bsPopover("maxdosexscale_help", "", texthelpmaxdosexscale, placement = "right", trigger = "hover", options = NULL)
                        )
                      ),
                      
                      br(),
                      uiOutput("inputstep1"),
                      fixedRow(
                        div(align = "center", actionButton("buttonRunStep1", "Merge and Combine", icon = icon("object-group"), style='font-size:150%')), br(), br(),
                        conditionalPanel(
                          condition = "input.nbLevel > 1",
                          span(textOutput("txtcolumnidannot"), style = 'color:#9c5c16;font-size:large;'), br(), br()
                        ),
                        shinyjs::hidden(div(id = 'text1_step1',
                                            style = 'color:#9c5c16; font-size:large;line-height: 50px;',
                                            "Structure of the data frame merged and combined")),
                        verbatimTextOutput("strmergeddata"), br(), br(),
                        shinyjs::hidden(div(id = 'text2_step1',
                                            style = 'color:#9c5c16; font-size:large;line-height: 50px;',
                                            "First 3 lines of each experimental level in the data frame merged and combined")),
                        uiOutput("headmergeddata"),
                        br(), br()
                      )
                      
             ),
             
             
             ####################################################################################
             ####### STEP 2 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 2</font>"),
                      value = "step2",
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>Trend and sensitivity plots</b></font>"), br(), br(), br(),
                      
                      wellPanel(
                        fixedRow(
                          column(3, style='border-right:1px solid #9c5c16;',
                                 HTML("<font face=verdana size=3 color=#9c5c16><b>Selection of annotation groups to plot &nbsp;</b></font>"), 
                                 shinyBS::bsButton("helplabel1step2", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                                 shinyBS::bsPopover("helplabel1step2", "", helplabel1step2, placement = "right", trigger = "hover", options = list(container = "body")),
                                 br(), 
                                 br(), 
                                 checkboxInput("keepAllExplev", label = HTML("<b>Keep all experimental levels</b>"), value = FALSE),
                                 br(), 
                                 sliderInput("minNbItem", "Minimum for the number of items",
                                             width = "90%", 
                                             min = 1, max = 10, step = 1, ticks = TRUE, 
                                             value = 3),
                                 numericInput("BMDmax", label = "Maximum for the BMD summary value", value = 0, min = 0, step = 0.1, width = "70%")
                          ),
                          column(1, 
                                 checkboxInput("BMDlogtransfoSensitivityplot", label = HTML("<b>Log transformation of the BMD</b>"), value = FALSE),
                          ),
                          column(1, 
                                 radioButtons("BMDtype", label = "BMD type", 
                                              choices = list("zSD" = "zSD", 
                                                             "xfold" = "xfold")
                                 )
                          ),
                          column(1,
                                 radioButtons("BMDsummarysensitivityPlot", label = "BMD summary", 
                                              choices = list("First quartile" = "first.quartile", 
                                                             "Median" = "median", 
                                                             "Median and IQR" = "median.and.IQR")
                                 )),
                          column(2, 
                                 # if several experimental level
                                 conditionalPanel(condition = "input.nbLevel > 1",
                                                  radioButtons("ordering_moreonelev", label = "Ordering of the annotations",
                                                               choices = list("alphabetic order" = "alphaorder_moreonelev",
                                                                              "ordered by total number of items in all the experimental levels" = "numbitemorder_moreonelev",
                                                                              "specific order" = "specificorder_moreonelev"))
                                 ),
                                 # if only one experimental level
                                 conditionalPanel(condition = "input.nbLevel == 1",
                                                  radioButtons("ordering_onelev", label = "Ordering of the annotations",
                                                               choices = list("alphabetic order" = "alphaorder_onelev",
                                                                              "ordered by number of items" = "numbitemorder_onelev",
                                                                              "ordered by BMD summary value" = "BMDorder_onelev",
                                                                              "specific order" = "specificorder_onelev"))
                                 ),
                          ), 
                          column(3,
                                 conditionalPanel(condition = "input.ordering_moreonelev == 'specificorder_moreonelev' | input.ordering_onelev == 'specificorder_onelev'",
                                                  uiOutput("specificorder", style="font-size:85%;")
                                 )
                          ),
                          column(1,
                                 br(),
                                 div(align="right", actionButton("buttonRunStep2", "Run", icon = icon("fas fa-gear"), style='font-size:200%'))
                          )
                        )),
                      fixedRow(
                        column(6,
                               shinyjs::hidden(div(id = 'text1_step2',
                                                   style = 'color:#9c5c16; font-size:large;text-align: center;line-height: 150px;',
                                                   "Trend plot")),
                               plotOutput("trendplot", width = "100%", height = "900px"),
                               br(), br(),
                               div(align = "center", downloadButton("buttonDownloadTrendplot", "Download Trend Plot", icon = icon("fas fa-download")))),
                        column(6,
                               fixedRow(
                                 shinyjs::hidden(div(id = 'text2_step2',
                                                     style = 'color:#9c5c16; font-size:large;text-align: center;line-height: 150px;',
                                                     "Sensitivity plot")),
                                 plotOutput("sensitivityplot", width = "100%", height = "900px"),
                                 br(), br(),
                                 div(align = "center", downloadButton("buttonDownloadSensitivityplot", "Download Sensitivity Plot", icon = icon("fas fa-download")))
                               )
                        )
                      ),
                      br(), br(),                      
                      fixedRow(
                        shinyjs::hidden(div(id = 'text3_step2',
                                            style = 'color:#9c5c16; font-size:large;line-height: 50px;',
                                            "Structure of the data frame merged and combined")),
                        verbatimTextOutput("filteredsorteddata"),
                        downloadButton('downloadData', 'Download Data')
                      ),
                      br(), br(), br(), br()
             ),
             
             ####################################################################################
             ####### STEP 3 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 3</font>"),
                      value = "step3",
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>BMD plots (with and without gradient)</b></font>"), br(), br(), br(),
                      
                      wellPanel(
                        fixedRow(
                          column(12,
                                 checkboxGroupInput("annotcheckboxBMDplot", label = "Choose at least on annotation"),
                                 actionButton("selectallBMDplot", "Select All"),
                                 actionButton("unselectallBMDplot", "Unselect All")
                          ))),
                      wellPanel(
                        fixedRow(
                          column(2, 
                                 checkboxInput("addciBMDplot", label = HTML("<b>Add CI</b> (only for the BMD plot without gradient)"), value = FALSE),
                                 checkboxInput("BMDlogtransfoBMDplot", label = HTML("<b>Log transformation of the BMD</b>"), value = FALSE),
                                 checkboxInput("addlabelBMDplot", label = HTML("<b>Add labels</b>"), value = FALSE)
                          ),
                          column(2,
                                 radioButtons("facetbycolumnsBMDplot", label = "Facet by (for columns)", 
                                              choices = list("Annotation" = "annotation",
                                                             "Experimental level" = "explevel")
                                 )),
                          conditionalPanel(condition = "input.nbLevel > 1",
                                           column(2,
                                                  radioButtons("facetbyrowsBMDplot", label = "Facet by (for rows)", 
                                                               choices = list("Annotation" = "annotation",
                                                                              "Experimental level" = "explevel")
                                                  ))
                          ),
                          column(2,
                                 fixedRow(
                                   checkboxInput("shapebyBMDplot", label = HTML("<b>Shape by trend</b>"), value = FALSE),
                                   checkboxInput("colorbyBMDplot", label = HTML("<b>Color by trend</b> (only for the BMD plot without gradient)"), value = FALSE)
                                 )
                          ),
                          column(1,
                                 br(),
                                 div(align="right", actionButton("buttonRunStep3", "Run", icon = icon("fas fa-gear"), style='font-size:200%'))
                          )
                        )
                      ),
                      fixedRow(
                        column(6,
                               shinyjs::hidden(div(id = 'text1_step3',
                                                   style = 'color:#9c5c16; font-size:large;text-align: center;line-height: 150px;',
                                                   "BMD plot")),
                               plotOutput("bmdplot", width = "100%", height = "900px"),
                               br(), br(),
                               div(align = "center", downloadButton("buttonDownloadBMDplot", "Download BMD Plot", icon = icon("fas fa-download")))),
                        column(6,
                               shinyjs::hidden(div(id = 'text2_step3',
                                                   style = 'color:#9c5c16; font-size:large;text-align: center;line-height: 150px;',
                                                   "BMD plot with gradient")),
                               plotOutput("bmdplotwithgradient", width = "100%", height = "900px"),
                               br(), br(),
                               div(align = "center", downloadButton("buttonDownloadBMDplotwithgradient", "Download BMD Plot with gradient", icon = icon("fas fa-download")))
                        )
                      ),
                      br(), br(), br(), br()
             ),
             
             ####################################################################################
             ####### STEP 4 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 4</font>"),
                      value = "step4",
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>Curves plot</b></font>"), br(), br(), br(),
                      
                      wellPanel(
                        fixedRow(
                          column(12,
                                 checkboxGroupInput("annotcheckboxCurvesplot", label = "Choose at least on annotation"),
                                 actionButton("selectallCurvesplot", "Select All"),
                                 actionButton("unselectallCurvesplot", "Unselect All")
                          ))),
                      wellPanel(
                        fixedRow(
                          column(3, 
                                 splitLayout(cellWidths = c("60%", "40%"),
                                             checkboxInput("doselogtransfoCurvesplot", label = HTML("<b>Dose log transformation</b>"), value = FALSE),
                                             shinyBS::bsButton("helplabel1step4", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                                             shinyBS::bsPopover("helplabel1step4", "", helplabel1step4, placement = "right", trigger = "hover", options = list(container = "body"))
                                 ),
                                 numericInput("mindoseCurvesplot", label = "Minimal dose for the x range", value = 0, width = "60%")
                          ),
                          column(2,
                                 radioButtons("facetbycolumnsCurvesplot", label = "Facet by (for columns)", 
                                              choices = list("Annotation" = "annotation",
                                                             "Experimental level" = "explevel")
                                 )),
                          conditionalPanel(condition = "input.nbLevel > 1",
                                           column(2,
                                                  radioButtons("facetbyrowsCurvesplot", label = "Facet by (for rows)", 
                                                               choices = list("Annotation" = "annotation",
                                                                              "Experimental level" = "explevel")
                                                  ))
                          ),
                          column(2,
                                 fixedRow(
                                   checkboxInput("colorbyCurvesplot", label = HTML("<b>Color by trend</b>"), value = TRUE)
                                 )
                          ),
                          column(1,
                                 br(),
                                 div(align="right", actionButton("buttonRunStep4", "Run", icon = icon("fas fa-gear"), style='font-size:200%'))
                          )
                        )
                      ),
                      fixedRow(
                        plotOutput("curvesplot", width = "100%", height = "900px"),
                        br(), br(),
                        div(align = "center", downloadButton("buttonDownloadCurvesplot", "Download Curves Plot", icon = icon("fas fa-download"))),
                      ),
                      br(), br(), br(), br()
             ),
             
             ####################################################################################
             ####### STEP 5 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>R code to go further</font>"),
                      value = "step5",
                      fixedRow(
                        column(8, 
                               br(), HTML("<font face=verdana size=5 color=#9c5c16><b>R CODE TO GO FURTHER</b></font>"), br(), br(), br(),
                               tags$blockquote("To see what more you can do using the R package, we recommend you to consult the vignette and the cheat sheet", 
                                               "(links to all resources ", a("here", href = "https://lbbe.univ-lyon1.fr/fr/dromics", TARGET="_blank", style="color:#f28d0f;"),")."), 
                               br(), 
                               downloadButton("buttonDownRCode", "Download R Code", icon = icon("fas fa-download"), style = 'background-color:#e6e6e6; color:#000000; border-color:#9d9d9d;'), br(), br(),
                               verbatimTextOutput('printRCode'), br(), br()
                        ))
             )
             
  )
)
