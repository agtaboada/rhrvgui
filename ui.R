library(RHRV)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(spsComps)
library(tkrplot)
library(stringr)

ui<-fluidPage(
  shinyjs::useShinyjs(),
  tags$link(rel = "stylesheet", type="text/css", href="css/style.css"),
  navbarPage("RHRV GUI", id="mainTabSelect",
    tabPanel("Main menu",
             sidebarPanel(id="mainSidebar",
                          "",
                          fluidRow(
                            h3("Options")
                          ),
                          fluidRow(
                            h4("Heart rate data")
                          ),
                          fluidRow(
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
                            h4("Episodes")
                          ),
                          fluidRow(
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
                       fluidRow(
                         plotOutput("mainGraph",
                                    width = "100%",
                                    height = "400px",
                                    click = NULL,
                                    dblclick = NULL,
                                    hover = NULL,
                                    brush = NULL,
                                    inline = FALSE
                         )
                       )
             )         
    ),
    tabPanel(id="panelFrame", value="frameTab", "Frame-based evolution",
             fluidPage(
               useShinyjs(),
               sidebarPanel(id="frameSidebar",
                            h5("Visible Bands"),
                              checkboxInput("lfhf", "LF/HF", TRUE),
                              checkboxInput("ulf", "ULF", TRUE),
                              checkboxInput("vlf", "VLF", TRUE),
                              checkboxInput("hf", "HF", TRUE),
                              checkboxInput("lf", "LF", TRUE),
                              actionButton("sigAnBt", "Significance Analysis")
               ),
               mainPanel(
                 fluidRow(id="mainFrameRow",
                   plotOutput("lfhfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("ulfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("vlfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("hfPlot", width = "1000px",height = "180px",inline = FALSE),
                   plotOutput("lfPlot", width = "1000px",height = "180px",inline = FALSE)
                 ),
                fluidRow(id="significanceRow",
                  plotOutput("frameHistogram", width="1000px", height="700px", inline = FALSE)
                )
             )
          )
    ),
    tabPanel("Report"),
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
               mainPanel(
                 tabsetPanel(
                   tabPanel(
                     plotOutput("mainPoinPlot",
                                width = "800px",
                                height = "400px",
                                click = NULL,
                                dblclick = NULL,
                                hover = NULL,
                                brush = NULL,
                                inline = FALSE)
                    ),
                   tabPanel(
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
               )
    )),
    tabPanel(id="panelOptions", value="optionsTab", "Options",
             tabPanel("Frame-based evolution",
                      mainPanel("",
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
                                )
                      )
             ))

))
