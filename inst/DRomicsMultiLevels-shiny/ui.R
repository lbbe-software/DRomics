## JavaScript that dis/enables the ABILITY to click the tab (without changing aesthetics)
app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
  }"
## css snipit that makes it LOOK like we are/n't able click the tab (with outchanging functionality)
app_css <-
  ".nav li a.disabled {
    background-color: #aaa !important;
    color: #333 !important;
    cursor: not-allowed !important;
    border-color: #aaa !important;
  }"



ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = app_jscode, functions = c('disableTab','enableTab')),
  shinyjs::inlineCSS(app_css),
  
  titlePanel(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "logodromics-dr.png"), tags$title(" DRomicsMultiLevels Shiny App"))
  ),
  br(),
  
  navbarPage(title = "", 
             # tabPanel(HTML("<center><font face=verdana size=3 color=#9c5c16>DRomics WF 2</font></center>"),
             #          
             #          fluidRow(
             #            br(), br(), 
             #            HTML("<center><font face=verdana size=6 color=#9c5c16>DRomics Shiny Application - workflow 2</font></center>"),
             #            br(), br(),
             #            fixedRow(column(8, offset = 2,
             #                            h4("DRomics Shiny App runs on the ", 
             #                               a("shiny server of the LBBE", href = "http://lbbe-shiny.univ-lyon1.fr/", TARGET="_blank", style="color:#f28d0f;"), 
             #                               "with the develoment version of the DRomics package (available on ", 
             #                               a("Github", href = "https://github.com/aursiber/DRomics", TARGET="_blank", style="color:#f28d0f;"),")."),
             #                            h4("DRomics is also an R package, available on ", 
             #                               a("CRAN", href = "https://cran.r-project.org/package=DRomics", TARGET="_blank", style="color:#f28d0f;"), 
             #                               " and on ",
             #                               a("this web page", href = "https://lbbe.univ-lyon1.fr/fr/dromics", TARGET="_blank", style="color:#f28d0f;"), "."),
             #                            h4("You can find help about the DRomics Shiny App and the DRomics package in a ",
             #                               a("vignette", href = "https://lbbe.univ-lyon1.fr/sites/default/files/media/downloads/dromics_vignette_0.pdf", TARGET="_blank", style="color:#f28d0f;"), " and a ",
             #                               a("cheat sheet", href = "https://lbbe.univ-lyon1.fr/sites/default/files/media/downloads/dromics_cheat_sheet_0.pdf", TARGET="_blank", style="color:#f28d0f;"), "."
             #                            )
             #            )),
             #            hr(style='width: 70%;'),
             #            fixedRow(column(8, offset = 2,
             #                            p(strong("Citation and publications")),
             #                            p("If you use Dromics Shiny App, you should cite:"),
             #                            p("Larras F, Billoir E, Baillard V, Siberchicot A, Scholz S, Wubet T, Tarkka M, Schmitt-Jansen M and Delignette-Muller ML (2018).", br(),
             #                              em("DRomics: a turnkey tool to support the use of the dose-response framework for omics data in ecological risk assessment."), br(),
             #                              "Environmental Science & Technology.",
             #                              a("https://doi.org/10.1021/acs.est.8b04752", href = "https://pubs.acs.org/doi/10.1021/acs.est.8b04752", TARGET = "_blank", style="color:#f28d0f;")),
             #                            
             #                            br(),
             #                            p("You can freely find this article at: ", a("https://hal.archives-ouvertes.fr/hal-02309919", href = "https://hal.archives-ouvertes.fr/hal-02309919", TARGET = "_blank", style="color:#f28d0f;")),
             #                            
             #                            br(),
             #                            p("You can also look at the following citation for a complete example of use:"),
             #                            p("Larras F, Billoir E, Scholz S, Tarkka M, Wubet T, Delignette-Muller ML, Schmitt-Jansen M (2020).", br(),
             #                              em("A multi-omics concentration-response framework uncovers novel understanding of triclosan effects in the chlorophyte Scenedesmus vacuolatus."), br(),
             #                              "Journal of Hazardous Materials.",
             #                              a("https://doi.org/10.1016/j.jhazmat.2020.122727", href = "https://doi.org/10.1016/j.jhazmat.2020.122727", TARGET = "_blank", style="color:#f28d0f;")),
             #                            
             #                            br(),
             #                            p(strong("Contacts and authors")),
             #                            p("If you have any need that is not yet covered, any feedback on the package / Shiny app, or any training needs, feel free to email us at ", strong("dromics@univ-lyon1.fr"), "."),
             #                            p("Issues can be reported on",
             #                              a("https://github.com/aursiber/DRomics/issues", href = "https://github.com/aursiber/DRomics/issues", TARGET = "_blank", style="color:#f28d0f;"), "."),
             #                            
             #                            br(),
             #                            p(a("Elise Billoir", href = "http://bddc.liec.univ-lorraine.fr/cv/BILLOIR%20E.htm", TARGET = "_blank", style="color:#f28d0f;"),
             #                              "- elise.billoir@univ-lorraine.fr - Laboratoire Interdisciplinaire des Environnements Continentaux - Université de Lorraine - Metz - France"),
             #                            p(a("Marie-Laure Delignette-Muller", href = "https://lbbe.univ-lyon1.fr/fr/annuaires-des-membres/delignette-muller-marie-laure", TARGET = "_blank", style="color:#f28d0f;"),
             #                              "- marielaure.delignettemuller@vetagro-sup.fr - Laboratoire de Biométrie et Biologie Evolutive - VetAgro Sup - Lyon - France"),
             #                            p(a("Floriane Larras", href = "https://www.researchgate.net/profile/Floriane_Larras", TARGET = "_blank", style="color:#f28d0f;"),
             #                              "- floriane.larras@inrae.fr - Delegation for scientific expertise, foresight and advanced studies (DEPE) - INRAE - Paris - France"),
             #                            p(a("Mechthild Schmitt-Jansen", href = "https://www.ufz.de/index.php?en=38467", TARGET = "_blank", style="color:#f28d0f;"),
             #                              "- mechthild.schmitt@ufz.de - Department of Bioanalytical Ecotoxicology - Helmholtz Center for Environmental Research GmbH - Leipzig - Germany"),
             #                            p(a("Aurélie Siberchicot", href = "https://lbbe.univ-lyon1.fr/fr/annuaires-des-membres/siberchicot-aurelie", TARGET = "_blank", style="color:#f28d0f;"),
             #                              "- aurelie.siberchicot@univ-lyon1.fr - Laboratoire de Biométrie et Biologie Evolutive - Université Lyon 1 - Lyon - France")
             #                            
             #            ))
             #            
             #          )
             # ),
             
             ####################################################################################
             ####### STEP 1 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 1</font>"),
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>Import and merge of DRomics results and annotation data</b></font>"), br(), br(), br(),
                      
                      fluidRow(
                        column(2,
                               numericInput("nbLevel", "Number of experimental levels", value = 1, min = 1, max = 10, step = 1)
                        ),
                        column(3, style = "margin-top: 25px;", 
                               shinyBS::bsButton("nblevel_help", label = "", icon = icon("info"), size = "small"),
                               shinyBS::bsPopover("nblevel_help", "", texthelpnblevel, placement = "right", trigger = "hover", options = NULL)
                        )),
                      
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
                          column(2, style='border-right:1px solid #9c5c16;',
                                 HTML("<font face=verdana size=3 color=#9c5c16><b>Selection of items to plot &nbsp;</b></font>"), 
                                 shinyBS::bsButton("helplabel1step2", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                                 shinyBS::bsPopover("helplabel1step2", "", helplabel1step2, placement = "right", trigger = "hover", options = NULL),
                                 br(), 
                                 br(), 
                                 sliderInput("minNbItem", "Minimum number of items",
                                             width = "90%", 
                                             min = 1, max = 10, step = 1, ticks = TRUE, 
                                             value = 3),
                                 numericInput("BMDmax", label = "BMDmax", value = 0, min = 0, step = 0.1, width = "50%")
                          ),
                          column(1, 
                                 radioButtons("BMDtypesensitivityPlot", label = "BMD type", 
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
                                 div(align="right", actionButton("buttonRunStep2", "Run", icon = icon("fas fa-cog"), style='font-size:200%'))
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
                        verbatimTextOutput("filteredsorteddata")
                      ),
                      br(), br(), br(), br()
             ),
             
             ####################################################################################
             ####### STEP 3 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>Step 3</font>"),
                      value = "step3",
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>BMD plots</b></font>"), br(), br(), br(),
                      
                      wellPanel(
                        fixedRow(
                          column(12,
                                 checkboxGroupInput("annotcheckboxBMDplot", label = "Choose at least on annotation"),
                                 actionButton("selectallBMDplot", "Select All"),
                                 actionButton("unselectallBMDplot", "Unselect All")
                          ))),
                      wellPanel(
                        fixedRow(
                          column(1, 
                                 radioButtons("BMDtypeBMDPlot", label = "BMD type", 
                                              choices = list("zSD" = "zSD", 
                                                             "xfold" = "xfold")
                                 )),
                          column(2, 
                                 checkboxInput("addciBMDplot", label = HTML("<b>Add CI</b> (only for the BMD plot)"), value = FALSE),
                                 checkboxInput("BMDlogtransfoBMDplot", label = HTML("<b>log transformation of the BMD</b>"), value = FALSE),
                                 checkboxInput("addlabelBMDplot", label = HTML("<b>Add labels</b>"), value = FALSE)
                                 ),
                          column(2,
                                 radioButtons("facetbycolumnsBMDplot", label = "Facet by (for columns)", 
                                              choices = list("Annotation" = "annotation",
                                                             "Experimental level" = "explevel")
                                 )),
                          column(2,
                                 radioButtons("facetbyrowsBMDplot", label = "Facet by (for rows)", 
                                              choices = list("Annotation" = "annotation",
                                                             "Experimental level" = "explevel")
                                 )),
                          column(2,
                                 fixedRow(
                                   checkboxInput("shapebyBMDplot", label = HTML("<b>Shape by trend</b>"), value = FALSE),
                                   checkboxInput("colorbyBMDplot", label = HTML("<b>Color by trend</b> (only for the BMD plot)"), value = FALSE)
                                 )
                          ),
                          column(1,
                                 br(),
                                 div(align="right", actionButton("buttonRunStep3", "Run", icon = icon("fas fa-cog"), style='font-size:200%'))
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
                          column(2, 
                                 numericInput("mindoseCurvesplot", label = "Minimal dose for the x range", value = 0),
                                 numericInput("mindoseCurvesplot", label = "Maximal dose for the x range", value = 1)
                          ),
                          column(2, 
                                 splitLayout(cellWidths = c("70%", "30%"),
                                             checkboxInput("doselogtransfoCurvesplot", label = HTML("<b>Dose log transformation</b>"), value = FALSE),
                                             checkboxInput("doselogtransfoCurvesplot", label = HTML("<b>Dose log transformation</b>"), value = FALSE),
                                             # shinyBS::bsButton("helplabel1step4", label = "", icon = icon("info"), size = "small", style="color:#9c5c16"),
                                             # shinyBS::bsPopover("helplabel1step4", "", helplabel1step4, placement = "right", trigger = "hover", options = NULL)
                                 )
                          ),
                          column(2,
                                 radioButtons("facetbycolumnsCurvesplot", label = "Facet by (for columns)", 
                                              choices = list("Annotation" = "annotation",
                                                             "Experimental level" = "explevel")
                                 )),
                          column(2,
                                 radioButtons("facetbyrowsCurvesplot", label = "Facet by (for rows)", 
                                              choices = list("Annotation" = "annotation",
                                                             "Experimental level" = "explevel")
                                 )),
                          column(2,
                                 fixedRow(
                                   checkboxInput("colorbyCurvesplot", label = HTML("<b>Color by trend</b>"), value = FALSE)
                                 )
                          ),
                          column(1,
                                 br(),
                                 div(align="right", actionButton("buttonRunStep4", "Run", icon = icon("fas fa-cog"), style='font-size:200%'))
                          )
                        )
                      ),
                      fixedRow(
                        plotOutput("curvesplot", width = "100%", height = "900px"),
                        br(), br(),
                        div(align = "center", downloadButton("buttonDownloadCurvesplot", "Download Curves Plot", icon = icon("fas fa-download"))),
                      ),
             ),
             
             ####################################################################################
             ####### STEP 5 #####################################################################
             ####################################################################################
             tabPanel(HTML("<font face=verdana size=3 color=#9c5c16>R code to go further</font>"),
                      value = "step5",
                      br(), HTML("<font face=verdana size=5 color=#9c5c16><b>R code to go further</b></font>"), br(), br(), br(),
             )
             
  )
)
