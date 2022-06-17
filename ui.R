library(RHRV)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(spsComps)
library(tkrplot)
library(stringr)

ui<-fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
               mainPanel(
                 fluidRow(
                   h4("File details")
                 ),
                 fluidRow(
                   textOutput("fileName", inline=TRUE),
                   textOutput("signalLength", inline=TRUE)
                 ),
                 fluidRow(
                   plotOutput("reportFilePlot", width = "80%", height = "400px", click = NULL, dblclick = NULL, hover = NULL, brush = NULL, inline = FALSE)
                 ),
                 fluidRow(
                   fluidRow(
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
                   fluidRow(
                     h4("Non-linear analysis")
                   ),
                   fluidRow(
                     column(8, id="reportPoincare",
                          plotOutput("reportPoincarePlot", width="100%", height="400px", inline=F)
                     ),
                     column(2,
                          textOutput("reportSd1", inline=F),
                          textOutput("reportSd2", inline=F)
                     )
                   )
                 ),
                 fluidRow(
                   fluidRow(
                     h4("Frame-based analysis")
                   ),
                   fluidRow(
                     column(8,
                       plotOutput("lfhfPlotReport", width = "100%",height = "200px",inline = FALSE),
                       plotOutput("ulfPlotReport", width = "100%",height = "200px",inline = FALSE),
                       plotOutput("vlfPlotReport", width = "100%",height = "200px",inline = FALSE),
                       plotOutput("hfPlotReport", width = "100%",height = "200px",inline = FALSE),
                       plotOutput("lfPlotReport", width = "100%",height = "200px",inline = FALSE),
                       plotOutput("hrPlotReport", width = "100%", height = "200px",inline = F)
                     ),
                     column(4,
                       textOutput("interpolationValueRep", inline = F),
                       textOutput("frameLength", inline = F),
                       textOutput("frameShift", inline = F),
                       textOutput("frameNumber", inline = F)
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
    tabPanel(id="panelOptions", value="optionsTab", "Options",
             tabPanel("Frame-based evolution",
                      mainPanel(id="mainOptPanel",
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
                      )
             )
    )

))
