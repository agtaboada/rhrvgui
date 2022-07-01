library(RHRV)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(tkrplot)
library(stringr)
library(DT)

ui<-fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "download.js"),
    tags$script(src = "html2canvas.min.js")
  ),
  navbarPage("RHRV GUI", id="mainTabSelect",
    tabPanel("Main menu",
             sidebarPanel(id="mainSidebar",
                          "",
                          fluidRow(
                            h3("Options")
                          ),
                          fluidRow(
                            h4("Heart rate data"),
                            shinyFilesButton("loadHrButton",
                                             "Load data",
                                             "Select a file",
                                             FALSE,
                                             buttonType = "default",
                                             class = NULL,
                                             icon = NULL,
                                             style = NULL,
                                             viewtype = "detail",
                            ),
                            actionButton("filterHrButton", "Filter"),
                            actionButton("editHrButton", "Edit")
                          ),
                          fluidRow(
                          h4("Episodes"),
                          shinyFilesButton("loadEpButton", "Load episodes",
                                           "Select a file",
                                           FALSE,
                                           buttonType = "default",
                                           class = NULL,
                                           icon = NULL,
                                           style = NULL,
                                           viewtype = "detail"),
                          actionButton("clearEpButton", "Clear")
                          ),
                          fluidRow(
                            h4("Interpolation"),
                            actionButton("interpolateButton", "Interpolate")
                          )
             ),
             mainPanel(id="mainMainPanel",
                       "",
                         plotOutput("mainGraph",
                                    width = "100%",
                                    height = "800px",
                                    click = NULL,
                                    dblclick = NULL,
                                    hover = NULL,
                                    brush = NULL,
                                    inline = FALSE
                       )
             )         
    ),
    tabPanel(id="panelFrame", value="frameTab", "Frame-based evolution",
             fluidPage(
               useShinyjs(),
               sidebarPanel(id="frameSidebar",
                            fluidRow(
                              h3("Visible Bands")
                              ),
                            fluidRow(
                                checkboxInput("lfhf", "LF/HF", TRUE),
                                checkboxInput("ulf", "ULF", TRUE),
                                checkboxInput("vlf", "VLF", TRUE),
                                checkboxInput("hf", "HF", TRUE),
                                checkboxInput("lf", "LF", TRUE),
                                checkboxInput("hr", "HR", TRUE)
                            ),fluidRow(id="significanceOptions",
                                selectInput("significanceEpisodes", "Episodes", "", width="180px"),
                                selectInput("significanceComparing", "CompareTo", "", width="180px"),
                                radioButtons("radioSigBands", "Parameter", choices = c("ULF","VLF","HF","LF")),
                                textOutput("significanceText")
                            ),fluidRow(
                              actionButton("sigAnBt", "Significance Analysis")
                            )
               ),
               mainPanel(
                 fluidRow(id="mainFrameRow",
                   plotOutput("lfhfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("ulfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("vlfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("hfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("lfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("hrPlot", width = "1000px", height = "180px", inline = F)
                 ),
                fluidRow(id="significanceRow",
                  plotOutput("frameHistogram", width="1000px", height="700px", inline = FALSE)
                )
             )
          )
    ),
    tabPanel(id="panelReport", value="reportTab", "Report",
             fluidPage(
               mainPanel( id="panelReportMainPanel",
                 tags$div(id="printDiv",
                   fluidRow(id="fileReportRow",
                     h4("File details"),
                     actionButton(inputId="downloadButton", "Save", width="7.5%", inline=T)
                   ),
                   fluidRow(id="fileDetailsHeader",
                     textOutput("fileName", inline=TRUE),
                     textOutput("signalLength", inline=TRUE)
                   ),
                   fluidRow(
                     plotOutput("reportFilePlot", width = "100%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE)
                   ),
                   fluidRow(
                     fluidRow(id="globalReportRow",
                       h4("Global Analysis (time-domain parameters)")
                     ),
                     column(8, id="reportHistogram",
                             plotOutput("reportHistogramPlot", width="100%", height="400px", inline=F)
                     ),
                     column(2, id="reportGlobalData1",
                            textOutput("beatNumber", inline=F),
                            textOutput("meanHr", inline=F),
                            textOutput("stdHr", inline=F),
                            textOutput("avnn", inline=F),
                            textOutput("sdnn", inline=F),
                            textOutput("sdann", inline=F),
                            textOutput("sdnnidx", inline=F)
                     ),
                     column(2, id="reportGlobalData2",
                            textOutput("pnn50", inline=F),
                            textOutput("rmssd", inline=F),
                            textOutput("irrr", inline=F),
                            textOutput("madrr", inline=F),
                            textOutput("tinn", inline=F),
                            textOutput("hrvIndex", inline=F)
                     )
                   ),
                   fluidRow(
                     fluidRow(id="nonLinearReportRow",
                       h4("Non-linear analysis")
                     ),
                     fluidRow(
                       column(8, id="reportPoincare",
                            plotOutput("reportPoincarePlot", width="100%", height="400px", inline=F)
                       ),
                       column(2, id="reportPoinData",
                            textOutput("reportSd1", inline=F),
                            textOutput("reportSd2", inline=F)
                       )
                     )
                   ),
                   fluidRow(
                     fluidRow(id="frameReportRow",
                       h4("Frame-based analysis")
                     ),
                     fluidRow( id="reportFramePlot",
                       column(8,
                         plotOutput("lfhfPlotReport", width = "105%",height = "200px",inline = FALSE),
                         plotOutput("ulfPlotReport", width = "105%",height = "200px",inline = FALSE),
                         plotOutput("vlfPlotReport", width = "105%",height = "200px",inline = FALSE),
                         plotOutput("hfPlotReport", width = "105%",height = "200px",inline = FALSE),
                         plotOutput("lfPlotReport", width = "105%",height = "200px",inline = FALSE),
                         plotOutput("hrPlotReport", width = "105%", height = "200px",inline = F)
                       ),
                       column(4, id="frameReportData",
                         textOutput("interpolationValueRep", inline = F),
                         textOutput("frameLength", inline = F),
                         textOutput("frameShift", inline = F),
                         textOutput("frameNumber", inline = F),
                         textOutput("ULFmin", inline = F),
                         textOutput("ULFmax", inline = F),
                         textOutput("VLFmin", inline = F),
                         textOutput("VLFmax", inline = F),
                         textOutput("LFmin", inline = F),
                         textOutput("LFmax", inline = F),
                         textOutput("HFmin", inline = F),
                         textOutput("HFmax", inline = F)
                       )
                     )
                   )
                 )
               )
             )
    ),
    tabPanel(id="panelPoincare", value="poinTab", "Poincare plot",
             fluidPage(
               useShinyjs(),
               sidebarPanel(id="poincareSidebar",
                 h4("Poincare Plot"),
                 selectInput("poincareEpisodes", "Episodes","Global"),
                 selectInput("poincareComparing", "Compare to","Global"),
                 h5(tags$b("SDs main plot")),
                 textOutput("sd1"),
                 textOutput("sd2"),
                 h5(tags$b("SDs secondary plot")),
                 textOutput("sd1sec"),
                 textOutput("sd2sec")
               ),
               mainPanel(id="mainPoinPanel",
                   fluidRow(id="mainPoinRow",
                     plotOutput("mainPoinPlot",
                                width = "800px",
                                height = "400px",
                                click = NULL,
                                dblclick = NULL,
                                hover = NULL,
                                brush = NULL,
                                inline = FALSE)
                    ),
                   fluidRow(
                     plotOutput("secondaryPoinPlot",
                                width = "800px",
                                height = "400px",
                                click = NULL,
                                dblclick = NULL,
                                hover = NULL,
                                brush = NULL,
                                inline = FALSE)
                   )
               )
    )),
    tabPanel(id="panelBatch", value="batchTab", "Batch Mode",
             sidebarPanel(id="batchSidebar",
               fluidRow(
                 h4("Beats and episodes"),
                 tags$div(id = "batchActionsWrapper", 
                   shinyFilesButton("loadMultipleData", "Load data", "Select a file",multiple=T, buttonType = "default", viewtype = "detail"),
                   selectInput("batchEpisodes","",choices="",multiple=FALSE, width="150px"),
                   shinyFilesButton("batchEpisodesBt", "Add episode", "", multiple=F)
                 )
               ),
               fluidRow(
                   DT::dataTableOutput("batchTable")
               ),
               fluidRow(
                 h4("Parameters to calculate"),
                  column(6,
                         checkboxGroupInput("bcol1", "", c("Signal length" = "signalLen", "Number of beats" = "NumBeats","Mean HR" = "MeanHr",
                                                           "SDNN" = "SDNN", "SDANN" = "SDANN","SD1" = "SD1"))
                  ),
                  column(6,
                         checkboxGroupInput("bcol2", "", c("SDNNIDX" = "SDNNIDX", "pNN50" = "pNN50", "rMSSD" = "rMSSD", "IRRR" = "IRRR", "TINN" = "TINN",
                                                           "HRV Index" = "HRVIndex", "SD2" = "SD2"))
                  ),
               ),
               h4("Options"),
               fluidRow(
                 column(4, numericInput("bWiSize", "Window size", 120, width="100px")),
                 column(4, numericInput("bWinShift","Window shift", 10, width="100px")),
                 column(4, numericInput("bInterVal", "Interp. freq.", 4, width="100px"))
               ),
               fluidRow(
                 column(4),
                 column(4),
                 column(4, actionButton("runBatch", "Run", width="90px"))
               )
             ),
             mainPanel(id="mainBatchPanel",
                DT::dataTableOutput("batchMainTable")
             )
    ),
    tabPanel(id="panelOptions", value="optionsTab", "Options",
             tabPanel("Frame-based evolution",
                      mainPanel(id="mainOptPanel",
                                column(6,
                                  fluidRow(
                                    h4("Interpolate")
                                  ),
                                  fluidRow(
                                    sliderInput("sliderInterp", "Frequency", min=1, max=25, value=4, step = 0.1)
                                  ),
                                  fluidRow(
                                    h4("Poincare")
                                  ),
                                  fluidRow(
                                    checkboxInput("poinCustomPlot", "Use custom axis values", FALSE)
                                  ),
                                  fluidRow(
                                    column(2,numericInput("poincarexMin", "Min. X", -800, width="100px")),
                                    column(2,numericInput("poincarexMax", "Max. X", 800, width="100px"))
                                  ),
                                  fluidRow(
                                    column(2, numericInput("poincareyMin", "Min. Y", -800, width="100px")),
                                    column(2, numericInput("poincareyMax", "Max. Y", 800,  width="100px"))
                                  ),
                                  fluidRow(
                                    h4("Window options")
                                  ),
                                  fluidRow(
                                    column(2, numericInput("windowSize", "Window size", 120, width="100px")),
                                    column(2, numericInput("windowShift","Window shift", 10, width="100px"))
                                  )
                                ),
                                column(6,
                                  fluidRow(
                                    h4("Freq. Band Limits")
                                  ),
                                  fluidRow(
                                    column(3, numericInput("ulfMin", "ULF min", 0.0, width = "100px")),
                                    column(3, numericInput("ulfMax", "ULF max", 0.03, width = "100px"))
                                  ),
                                  fluidRow(
                                    column(3, numericInput("vlfMin", "VLF min", 0.03, width = "100px")),
                                    column(3, numericInput("vlfMax", "VLF max", 0.05, width = "100px"))
                                  ),
                                  fluidRow(
                                    column(3, numericInput("lfMin", "LF min", 0.05, width = "100px")),
                                    column(3, numericInput("lfMax", "LF max", 0.15, width = "100px"))
                                  ),
                                  fluidRow(
                                    column(3, numericInput("hfMin", "HF min", 0.15, width = "100px")),
                                    column(3, numericInput("hfMax", "HF max", 0.4, width = "100px"))
                                  )
                                )
                      )
             )
    )

))
