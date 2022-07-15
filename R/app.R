library(RHRV)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(tkrplot)
library(stringr)
library(DT)
library(ggplot2)

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
                            fluidRow(id="significanceOptions",
                                selectInput("significanceEpisodes", "Episodes", "", width="180px"),
                                selectInput("significanceComparing", "CompareTo", "", width="180px"),
                                radioButtons("radioSigBands", "Parameter", choices = c("ULF","VLF","HF","LF"))
                            ),
                            fluidRow(
                              textOutput("significanceText1"),
                              textOutput("significanceText2"),
                              textOutput("significanceText3")
                            ),
                            fluidRow(
                              actionButton("sigAnBt", "Significance Analysis")
                            )
               ),
               mainPanel(
                 fluidRow(id="mainFrameRow",
                  plotOutput("mainFramePlot", width = "1100px",height = "900px",inline = FALSE),
                  plotOutput("frameHistogram", width="1100px", height="900px", inline = FALSE)
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
                   shinyFilesButton("loadMultipleData", "Load data", "Select a file",multiple=T, buttonType = "default", viewtype = "detail")
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
                                    column(4,numericInput("poincarexMin", "Min. X", -800, width="400px")),
                                    column(4,numericInput("poincarexMax", "Max. X", 800, width="400px"))
                                  ),
                                  fluidRow(
                                    column(4, numericInput("poincareyMin", "Min. Y", -800, width="400px")),
                                    column(4, numericInput("poincareyMax", "Max. Y", 800,  width="400px"))
                                  ),
                                  fluidRow(
                                    h4("Window options")
                                  ),
                                  fluidRow(
                                    column(4, numericInput("windowSize", "Window size", 120, width="400px")),
                                    column(4, numericInput("windowShift","Window shift", 10, width="400px"))
                                  )
                                ),
                                column(6,
                                  fluidRow(
                                    h4("Freq. Band Limits")
                                  ),
                                  fluidRow(
                                    column(4, numericInput("ulfMin", "ULF min", 0.0, width = "200px")),
                                    column(4, numericInput("ulfMax", "ULF max", 0.03, width = "200px"))
                                  ),
                                  fluidRow(
                                    column(4, numericInput("vlfMin", "VLF min", 0.03, width = "200px")),
                                    column(4, numericInput("vlfMax", "VLF max", 0.05, width = "200px"))
                                  ),
                                  fluidRow(
                                    column(4, numericInput("lfMin", "LF min", 0.05, width = "200px")),
                                    column(4, numericInput("lfMax", "LF max", 0.15, width = "200px"))
                                  ),
                                  fluidRow(
                                    column(4, numericInput("hfMin", "HF min", 0.15, width = "200px")),
                                    column(4, numericInput("hfMax", "HF max", 0.4, width = "200px"))
                                  )
                                )
                      )
             )
    )

))

server <- function(input, output, session){
    hrv.data <- NULL
    beatSelected <<- FALSE
    episodesSelected <<- FALSE
    beatInterpolated <<- FALSE
    interpolationValue <<- 4
    timeLineX <<- c(-800, 800)
    timeLineY <<- c(-800, 800)
    poincarexMin <<- -800
    poincareyMin <<- -800
    poincarexMax <<- 800
    poincareyMax <<- 800
    customPlotAxis <<- FALSE
    significanceAnalysis <<- FALSE
    significanceEpisodeList <<- NULL
    hrv.episode <<- NULL
    fileName <<- NULL
    windowSize <<- 120
    windowShift <<- 10
    loadingFileErrorStr <<- "Error loading the file: make sure you are using the proper file extension and data format."
    batchFileList <<- list()
    batchRouteList <<- list()
    batchHrvObjects <<- list()
    batchFileNum <<- 1
    ulfMin <<- 0
    ulfMax <<- 0.03
    vlfMin <<- 0.03
    vlfMax <<- 0.05
    lfMin <<- 0.05
    lfMax <<- 0.15
    hfMin <<- 0.15
    hfMax <<- 0.4
    significanceMain <<- NULL
    significanceComparing <<- NULL
    
    shinyjs::disable("sigAnBt")
    shinyjs::disable("filterHrButton")
    shinyjs::disable("editHrButton")
    shinyjs::disable("loadEpButton")
    shinyjs::disable("clearEpButton")
    shinyjs::disable("interpolateButton")
    shinyjs::disable("runBatch")
    
    hideElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    hideElement(id="panelReportMainPanel", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    hideElement(id="significanceText1", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    hideElement(id="significanceText2", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    hideElement(id="significanceText3", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    hideElement(id="frameHistogram",anim = F, selector=NULL, asis = FALSE)
    
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    shinyFileChoose(input, "loadHrButton", roots = volumes, session = session)
    shinyFileChoose(input, "loadEpButton", roots = volumes, session = session)
    shinyFileChoose(input, "loadMultipleData", roots = volumes, session = session)
    
    hrv.data = reactiveVal()
    hrv.data = CreateHRVData()
    hrv.data = SetVerbose(hrv.data, TRUE)
    
    deleteBeat <- function(selectedRow){
      print(selectedRow)
      batchFileNum <<- batchFileNum - 1
      batchFileList[[selectedRow]] <<- NULL
      batchRouteList[[selectedRow]] <<- NULL
      batchHrvObjects[[selectedRow]] <<- NULL
      reloadBatchDatatable()
      if(batchFileNum == 1){
        shinyjs::disable("runBatch")
      }
    }
    
    shinyInput <- function(FUN, len, id, ...) {
      if(len > 0){
        inputs <- character(len)
        for (i in seq_len(len)) {
          inputs[i] <- as.character(FUN(paste0(id, i), ...))
        }
        inputs
      }else{
        NULL
      }
    }
    
    reloadBatchDatatable <- function(){
      df <- data.frame(
        Name = unlist(batchFileList),
        Route = unlist(batchRouteList),
        Delete = shinyInput(actionButton, batchFileNum-1, 'button_', label = "Delete"),
        stringsAsFactors = FALSE
      )
      print("data frame created successfully")
      onclick(paste0("button_",1), deleteBeat(1))
      onclick(paste0("button_",2), deleteBeat(2))
      onclick(paste0("button_",3), deleteBeat(3))
      onclick(paste0("button_",4), deleteBeat(4))
      onclick(paste0("button_",5), deleteBeat(5))
      output$batchTable <- renderDataTable(df, options=list(searching=FALSE,dom=""), escape = F)
      print("exiting reload func")
    }
    
    observeEvent(input$loadHrButton, {
      file <- reactive(input$loadHrButton)
      hrv.data <<- CreateHRVData()
      if(length(file()) > 0 & is.numeric(file())){
        return(NULL)
      }else{
        beatSelected <<- TRUE
        beatInterpolated <<- FALSE
        episodesSelected <<- FALSE
        datapath <- parseFilePaths(volumes, file())$datapath
        datapath <- gsub("/",.Platform$file.sep, datapath)
        datapath <- gsub(basename(datapath), "", datapath)
        fileName <<- parseFilePaths(volumes, file())$name
        tryCatch({
                  records <- readLines(paste0(datapath,fileName), n=5)
                  record1 <- as.integer(records[1])
                  record2 <- as.integer(records[2])
                  record5 <- as.integer(records[5])
                  print((record5 - record1)/(record2 - record1))
                  if(((record5 - record1)/(record2 - record1)) >= 3){
                    print("Loading ASCII...")
                    hrv.data = LoadBeatAscii(hrv.data, parseFilePaths(volumes, file())$name, datapath)
                  }else{
                    print("Loading RR...")
                    hrv.data = LoadBeatRR(hrv.data, parseFilePaths(volumes, file())$name, datapath, scale=0.001)
                  }
                  hrv.data = BuildNIHR(hrv.data)
                  hrv.data <<- hrv.data
                  shinyjs::enable("filterHrButton")
                  shinyjs::enable("editHrButton")
                  shinyjs::enable("loadEpButton")
                  shinyjs::enable("interpolateButton")
                  output$mainGraph<-renderPlot({
                    PlotNIHR(hrv.data)
                  })
                  shinyjs::show("mainPoinPanel")
        },error = function(e){
          showNotification(loadingFileErrorStr, type = 'err')
          print(e)
        })
        updateSelectInput(session, "poincareEpisodes", choices = c(""))
        updateSelectInput(session, "poincareComparing", choices = c(""))
      }
    })
    
    observeEvent(input$filterHrButton,{
      hrv.data = FilterNIHR(hrv.data)
      hrv.data<<- hrv.data
      output$mainGraph<-renderPlot({
        PlotNIHR(hrv.data, Indexes="all", main="Filtered non-interpolated heart rate")
        })
    })
    
    observeEvent(input$editHrButton, {
        hrv.data = EditNIHR(hrv.data)
        hrv.data <<- hrv.data
        output$mainGraph<-renderPlot({
          PlotNIHR(hrv.data, Indexes="all", main="Edited data")})
    })
    
    observeEvent(input$loadEpButton, {
      file <- reactive(input$loadEpButton)
      if(length(file()) > 0 & is.numeric(file())){
        return(NULL)
      }else{
        datapath <- parseFilePaths(volumes, file())$datapath
        datapath <- gsub("/",.Platform$file.sep, datapath)
        datapath <- gsub(basename(datapath), "", datapath)
        tryCatch({hrv.data <<- LoadEpisodesAscii(hrv.data, parseFilePaths(volumes, file())$name, datapath)
                  hrv.data <<- hrv.data
                  listOfEpisodeOptions <- unique(ListEpisodes(hrv.data)["Tag"])
                  listOfEpisodeOptions <- append(listOfEpisodeOptions, "GLOBAL")
                  updateSelectInput(session, "poincareEpisodes", choices = listOfEpisodeOptions, selected="GLOBAL")
                  updateSelectInput(session, "poincareComparing", choices = unique(ListEpisodes(hrv.data)["Tag"]))
                  episodesSelected <<- TRUE
                  shinyjs::enable("clearEpButton")
                  if(beatInterpolated){
                    hrv.data <<- InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
                  }
                  output$mainGraph<-renderPlot({
                    PlotNIHR(hrv.data, Indexes="all", main="Data with episodes")
                  })
        },error = function(e){
          showNotification(loadingFileErrorStr, type = 'err')
        })
          
      }
    })
    
    observeEvent(input$clearEpButton, {
        shinyjs::disable("sigAnBt")
        hrv.data <<- RemoveEpisodes(hrv.data, Tags = "all", Indexes = "all")
        episodesSelected <<- FALSE
        updateSelectInput(session, "poincareEpisodes", choices = c("GLOBAL"), selected="GLOBAL")
        updateSelectInput(session, "poincareComparing", choices = c(""), selected="GLOBAL")
        shinyjs::disable("clearEpButton")
        output$mainGraph<-renderPlot({
          PlotNIHR(hrv.data, main="Data")
        })
    })
    
    observeEvent(input$mainTabSelect, {
        if(input$mainTabSelect == "poinTab"){
          output$mainPoinPlot <- {
            if(beatSelected){
              renderPlot({
                hrv.data = CreateNonLinearAnalysis(hrv.data)
                if(customPlotAxis){
                  pointcareData = PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, verbose=NULL,
                                            xlim=timeLineX, ylim=timeLineY)
                }else{
                  pointcareData = PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, verbose=NULL)
                }
                refreshSd1(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
                refreshSd2(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
              })
            }
          }
        }
      if(input$mainTabSelect == "frameTab"){
        initializeFrameInputs()
        if(beatSelected && beatInterpolated){
          hrv.data <<- CreateFreqAnalysis(hrv.data)
          hrv.data <<- CalculatePowerBand(hrv.data,length(hrv.data$FreqAnalysis), size = windowSize, shift = windowShift, sizesp = 1024, ULFmin = ulfMin, 
                                         ULFmax = ulfMax, VLFmin = vlfMin, VLFmax = vlfMax, LFmin = lfMin, LFmax = lfMax, HFmin = hfMin, HFmax = hfMax)
          hrv.data <<- CreateTimeAnalysis(hrv.data, size=windowSize, numofbins=NULL, interval=7.8125, verbose=NULL )
          output$mainFramePlot <- renderPlot(PlotPowerBand(hrv.data, length(hrv.data$FreqAnalysis), hr=T, Indexes="all", Tags="all"))
          if(episodesSelected){
            shinyjs::enable("sigAnBt")
            listOfEpisodeOptions <- unique(ListEpisodes(hrv.data)["Tag"])
            names(listOfEpisodeOptions) <- NULL
            aux <- c()
            for(i in 1:length(listOfEpisodeOptions)){
              ep <- listOfEpisodeOptions[[i]]
              aux <-append(aux, gsub(" ", "", ep))
            }
            significanceEpisodeList <<- aux
            updateSelectInput(session, "significanceEpisodes", choices = aux, selected=listOfEpisodeOptions[[1]])
            updateSelectInput(session, "significanceComparing", choices = aux, selected=listOfEpisodeOptions[[1]])
            print(ListEpisodes(hrv.data)["Tag"])
          }
        }else{
          showNotification("Please, select a beat and interpolate it to use this menu.", type='warning')
        }
      }
      if(input$mainTabSelect == "reportTab"){
          if(beatSelected){
            episodesValue <- NULL
            if(episodesSelected == TRUE){
              episodesValue <- "all"
            }
            
            hrv.data <- InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
            hrv.data <- CreateTimeAnalysis(hrv.data, size=windowSize, numofbins=NULL, interval=7.8125, verbose=NULL )
            hrv.data <- CreateNonLinearAnalysis(hrv.data)
            if(length(hrv.data$FreqAnalysis) == 0){
              hrv.data <- CreateFreqAnalysis(hrv.data)
              hrv.data <- CalculateCorrDim(hrv.data,indexNonLinearAnalysis=1, minEmbeddingDim=2, maxEmbeddingDim=8,timeLag=1,minRadius=1, maxRadius=15, pointsRadius=20,
                                           theilerWindow=10, corrOrder=2,doPlot=FALSE)
              hrv.data <- CalculateSampleEntropy(hrv.data,indexNonLinearAnalysis=1,doPlot=FALSE)
              hrv.data <- EstimateSampleEntropy(hrv.data,indexNonLinearAnalysis=1,regressionRange=c(6,10))
              hrv.data <- CalculatePowerBand(hrv.data, indexFreqAnalysis = 1, size = windowSize, shift = windowShift, sizesp = 1024, ULFmin = ulfMin, 
                                             ULFmax = ulfMax, VLFmin = vlfMin, VLFmax = vlfMax, LFmin = lfMin, LFmax = lfMax, HFmin = hfMin, HFmax = hfMax)
            }
            
            poincareData = PoincarePlot(hrv.data, doPlot = F, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, xlim=timeLineX, ylim=timeLineY,
                                        verbose=NULL)
            timeAnalysis = hrv.data$TimeAnalysis[[1]]
            frameNumber = tail(hrv.data$Beat[["Time"]], n=1) / windowShift
            
            output$fileName <- renderText({ paste("Name: ", fileName)})
            output$signalLength <- renderText({ paste("Signal length: ", max(hrv.data$Beat[["Time"]]))})
            output$reportFilePlot <- renderPlot({PlotHR(hrv.data, Indexes=episodesValue, main=paste(fileName, " - Interpolated HR"))})
            
            output$reportHistogramPlot <-renderPlot(hist(hrv.data$Beat[["niHR"]], main="HR histogram", xlab="HR"))
            output$beatNumber <- renderText({paste("No. of beats: ", length(hrv.data$Beat[["Time"]]))})
            output$meanHr <- renderText({paste("Mean HR: ", round(mean(hrv.data$Beat[["niHR"]]),4))})
            output$stdHr <- renderText({paste("STD HR: ", round(sd(hrv.data$Beat[["niHR"]]),4))})
            output$avnn <- renderText({paste("Mean RR (AVNN): ","")})
            output$sdnn <- renderText({paste("STD RR (SDNN): ", round(timeAnalysis$SDNN, 4))})
            output$sdann <- renderText({paste("SDANN: ", round(timeAnalysis$SDANN, 4))})
            output$sdnnidx <- renderText({paste("SDNNIDX: ", round(timeAnalysis$SDNNIDX, 4))})
            output$pnn50 <- renderText({paste("pNN50: ", round(timeAnalysis$pNN50, 4))})
            output$rmssd <- renderText({paste("rMSSD: ", round(timeAnalysis$rMSSD, 4))})
            output$irrr <- renderText({paste("IRRR: ", round(timeAnalysis$IRRR, 4))})
            output$madrr <- renderText({paste("MADRR: ", round(timeAnalysis$MADRR, 4))})
            output$tinn <- renderText({paste("TINN: ", round(timeAnalysis$TINN, 4))})
            output$hrvIndex <- renderText({paste("HRV Index: ", round(timeAnalysis$HRVi, 4))})
            
            if(customPlotAxis){
              output$reportPoincarePlot <- renderPlot(PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE,
                                                                 xlim=timeLineX, ylim=timeLineY, verbose=NULL))
            }else{
              output$reportPoincarePlot <- renderPlot(PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE,
                                                                   verbose=NULL))
            }
            output$reportSd1 <- renderText(paste("SD1: ", round(poincareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1, 4)))
            output$reportSd2 <- renderText(paste("SD2: ", round(poincareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2, 4)))
            
            output$lfhfPlotReport <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "LF/HF",
                                                               epColorPalette = "red", ylab = "LF/HF",xlab = "", main = "")})
            output$ulfPlotReport <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "ULF", epColorPalette = "red",
                                                              epLegendCoords = c(2000,7500), ylab = "ULF",xlab = "", main = "")})
            output$vlfPlotReport <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "VLF", epColorPalette = "red",
                                                              epLegendCoords = c(2000,7500), ylab = "VLF",xlab = "", main = "")})
            output$hfPlotReport <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "HF", epColorPalette = "red",
                                                             epLegendCoords = c(2000,7500), ylab = "HF",xlab = "", main = "")})
            output$lfPlotReport <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "LF", epColorPalette = "red",
                                                             epLegendCoords = c(2000,7500), ylab = "LF", main = "", xlab="")})
            output$hrPlotReport <- renderPlot({PlotHR(hrv.data, Tags = "all", xlab = "time (sec.)", ylab = "Heart Rate", main = "", type = "l")})
            output$interpolationValueRep <- renderText(paste("Interpolation value: ", interpolationValue))
            output$frameLength <- renderText(paste("Frame length: ", windowSize))
            output$frameShift <- renderText(paste("Frame shift: ", windowShift))
            output$frameNumber <- renderText(paste("Number of frames: ", round(frameNumber, 0)))
            output$ULFmin <- renderText(paste("ULF min: ", ulfMin))
            output$ULFmax <- renderText(paste("ULF max: ", ulfMax))
            output$VLFmin <- renderText(paste("VLF min: ", vlfMin))
            output$VLFmax <- renderText(paste("VLF min: ", vlfMax))
            output$LFmin <- renderText(paste("LF min: ", lfMin))
            output$LFmax <- renderText(paste("LF max: ", lfMax))
            output$HFmin <- renderText(paste("HF min: ", hfMin))
            output$HFmax <- renderText(paste("HF max: ", hfMin))
            
            showElement(id="panelReportMainPanel", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
          }else{
            showNotification("Please, load some beat data in order to use this menu.", type='warning')
          }
        }
    })
    
    observeEvent(input$poincareEpisodes, {
      if(beatSelected){
        hrv.data = CreateNonLinearAnalysis(hrv.data)
        if(input$poincareEpisodes == "GLOBAL"){
          shinyjs::showElement("secondaryPoinPlot")
          updateSelectInput(session, "poincareComparing", choices = ListEpisodes(hrv.data)["Tag"])
          output$mainPoinPlot <- {
            renderPlot({
              if(customPlotAxis){
                pointcareData = PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE,
                                             xlim=timeLineX, ylim=timeLineY, verbose=NULL)
              }else{
                pointcareData = PoincarePlot(hrv.episode, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, verbose=NULL)
              }
              refreshSd1(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
              refreshSd2(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
            })
          }
        }else{
          updateSelectInput(session, "poincareComparing", choices="")
          shinyjs::hideElement("secondaryPoinPlot")
          output$mainPoinPlot <- {
            if(episodesSelected){
              hrv.episode = CreateHRVData(Verbose = TRUE)
              hrv.data = InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
              episodesVector = SplitHRbyEpisodes(hrv.data, T=str_replace_all(input$poincareEpisodes, fixed(" "), ""), verbose=NULL)
              hrv.episode = LoadBeatVector(hrv.episode, episodesVector$InEpisodes)
              hrv.episode = BuildNIHR(hrv.episode, verbose=NULL)
              hrv.episode = CreateNonLinearAnalysis(hrv.episode)
              renderPlot({
                if(customPlotAxis){
                  pointcareData = PoincarePlot(hrv.episode, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, xlim=timeLineX,
                                               ylim=timeLineY, verbose=NULL)
                }else{
                  pointcareData = PoincarePlot(hrv.episode, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, verbose=NULL)
                }
                refreshSd1(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
                refreshSd2(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
              })
            }
          }
        }
    }})
    
    
    observeEvent(input$poincareComparing, {
      if(episodesSelected){
        output$secondaryPoinPlot <- {
          hrv.episode <<- CreateHRVData(Verbose = TRUE)
          hrv.data = InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
          episodesVector = SplitHRbyEpisodes(hrv.data, T=str_replace_all(input$poincareComparing, fixed(" "), ""), verbose=NULL)
          hrv.episode <<- LoadBeatVector(hrv.episode, episodesVector$InEpisodes)
          hrv.episode <<- BuildNIHR(hrv.episode)
          hrv.episode <<- CreateNonLinearAnalysis(hrv.episode)
          renderPlot({
            if(customPlotAxis){
              pointcareData = PoincarePlot(hrv.episode, doPlot = T, main="Secondary Plot", indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE,
                                         xlim=timeLineX, ylim=timeLineY)
            }else{
              pointcareData = PoincarePlot(hrv.episode, doPlot = T, main="Secondary Plot", indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE)
            }
            refreshSd1sec(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
            refreshSd2sec(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
          })
        }
      }
    })
    
    observeEvent(input$interpolateButton, {
      if(beatSelected){
        hrv.data <<- InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=FALSE)
        beatInterpolated <<- TRUE
        output$mainGraph<-renderPlot({
          PlotHR(hrv.data, Indexes="all", main="Interpolated data")
        })
      }
    })
    
    observeEvent(input$sliderInterp, {
      interpolationValue <<- input$sliderInterp
    })
    
    observeEvent(input$poincarexMax, {
      poincarexMax <<- input$poincarexMax
      timeLineX <<- c(poincarexMin, poincarexMax)
    })
    
    observeEvent(input$poincarexMin, {
      poincarexMin <<- input$poincarexMin
      timeLineX <<- c(poincarexMin, poincarexMax)
    })
    
    observeEvent(input$poincareyMax, {
      poincareyMax <<- input$poincareyMax
      timeLineY <<- c(poincareyMin, poincareyMax)
    })
    
    observeEvent(input$poincareyMin, {
      poincareyMin <<- input$poincareyMin
      timeLineY <<- c(poincareyMin, poincareyMax)
    })
    
    observeEvent(input$poinCustomPlot, {
      if(input$poinCustomPlot == TRUE){
        enablePlotOptions()
      }else{
        disablePlotOptions()
      }
      repaintPoincareCompare()
    })
    
    refreshSd1 <- function(data){
      output$sd1 <- renderText({
        paste("SD1: ",data)
      })
    }
    
    refreshSd2 <- function(data){
      output$sd2 <- renderText({
        paste("SD2: ",data)
      })
    }
    
    refreshSd1sec <- function(data){
      output$sd1sec <- renderText({
        paste("SD1: ",data)
      })
    }
    
    refreshSd2sec <- function(data){
      output$sd2sec <- renderText({
        paste("SD2: ",data)
      })
    }
    
    disablePlotOptions <- function(){
      customPlotAxis <<- FALSE
      shinyjs::disable("poincarexMin")
      shinyjs::disable("poincarexMax")
      shinyjs::disable("poincareyMin")
      shinyjs::disable("poincareyMax")
    }
    
    enablePlotOptions <- function(){
      customPlotAxis <<- TRUE
      shinyjs::enable("poincarexMin")
      shinyjs::enable("poincarexMax")
      shinyjs::enable("poincareyMin")
      shinyjs::enable("poincareyMax")
    }
    
    observeEvent(input$lf, {
      if(input$lf == FALSE){
        hideElement(id = "lfPlot", anim = TRUE, animType = "slide", time = 0.2, selector = NULL, asis = FALSE)
      }else{
        showElement(id = "lfPlot", anim = TRUE, animType = "fade", time = 0.4, selector = NULL, asis = FALSE)
      }
    })
    
    observeEvent(input$hf, {
      if(input$hf == FALSE){
        hideElement(id = "hfPlot", anim = TRUE, animType = "slide", time = 0.2, selector = NULL, asis = FALSE)
      }else{
        showElement(id = "hfPlot", anim = TRUE, animType = "fade", time = 0.4, selector = NULL, asis = FALSE)
      }
    })
    
    observeEvent(input$vlf, {
      if(input$vlf == FALSE){
        hideElement(id = "vlfPlot", anim = TRUE, animType = "slide", time = 0.2, selector = NULL, asis = FALSE)
      }else{
        showElement(id = "vlfPlot", anim = TRUE, animType = "fade", time = 0.4, selector = NULL, asis = FALSE)
      }
    })
    
    observeEvent(input$ulf, {
      if(input$ulf == FALSE){
        hideElement(id = "ulfPlot", anim = TRUE, animType = "slide", time = 0.2, selector = NULL, asis = FALSE)
      }else{
        showElement(id = "ulfPlot", anim = TRUE, animType = "fade", time = 0.4, selector = NULL, asis = FALSE)
      }
    })
    
    observeEvent(input$lfhf, {
      if(input$lfhf == FALSE){
        hideElement(id = "lfhfPlot", anim = TRUE, animType = "slide", time = 0.2, selector = NULL, asis = FALSE)
      }else{
        showElement(id = "lfhfPlot", anim = TRUE, animType = "fade", time = 0.4, selector = NULL, asis = FALSE)
      }
    })
    
    observeEvent(input$hr, {
      if(input$hr == FALSE){
        hideElement(id = "hrPlot", anim = TRUE, animType = "slide", time = 0.2, selector = NULL, asis = FALSE)
      }else{
        showElement(id = "hrPlot", anim = TRUE, animType = "fade", time = 0.4, selector = NULL, asis = FALSE)
      }
    })
    
    observeEvent(input$sigAnBt, {
      significanceAnalysis <<- !significanceAnalysis
      if(significanceAnalysis == TRUE){
        showElement(id="significanceRow", anim = TRUE, animType="fade", time=0.4, selector=NULL, asis = FALSE)
        showElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        showElement(id="significanceText1", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        showElement(id="significanceText2", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        showElement(id="significanceText3", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        showElement(id="frameHistogram",anim = F, selector=NULL, asis = FALSE)
        hideElement(id="mainFramePlot", anim=F, selector="NULL", asis=FALSE)
        updateActionButton(session, "sigAnBt",label = "Back")
      }else{
        hideElement(id="significanceRow", anim=TRUE, animType="fade", time=0.4, selector="NULL", asis=FALSE)
        hideElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        hideElement(id="significanceText1", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        hideElement(id="significanceText2", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        hideElement(id="significanceText3", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        hideElement(id="frameHistogram",anim = F, selector=NULL, asis = FALSE)
        showElement(id="mainFramePlot", anim = F, selector=NULL, asis = FALSE)
        updateActionButton(session, "sigAnBt",label = "Significance Analysis")
      }
    })
    
    observeEvent(input$windowSize, {
      windowSize <<- input$windowSize
    })
    
    observeEvent(input$windowShift, {
      windowShift <<- input$windowShift
    })
    
    observeEvent(input$downloadButton, {
      shinyjs::reset("panelReportMainPanel")
    })
    
    observeEvent(input$loadMultipleData, {
        file <- reactive(input$loadMultipleData)
        if(length(file()) > 0 & is.numeric(file())){
          return(NULL)
        }else{
          datapath <- parseFilePaths(volumes, file())$datapath
          for(d in datapath){
            if(batchFileNum < 6){
              tryCatch({
                currentDatapath <- gsub("/",.Platform$file.sep, d)
                currentDatapath <- gsub(basename(d), "", d)
                fileName <- basename(d)
                
                hrv.batchData <- CreateHRVData()
                hrv.batchData <- SetVerbose(hrv.data, TRUE)
                hrv.batchData <- LoadBeatAscii(hrv.batchData, fileName, currentDatapath)
                
                batchHrvObjects[batchFileNum] <<- list(hrv.batchData)
                batchFileList[batchFileNum] <<- fileName
                batchRouteList[batchFileNum] <<- currentDatapath
                batchFileNum <<- batchFileNum + 1
                
                hrv.batchData <- NULL
              },error = function(e){
                  showNotification("There was an error while loading at least one of the files. You can check the files actually loaded on the sidebar menu.", 
                                   type = 'err')
              })
            }else{
              showNotification("You have already reached the maximum number of files allowed on this mode.", type='warning')
            }
          }
          shinyjs::enable("runBatch")
        }
        reloadBatchDatatable()
   })
    
    observeEvent(input$runBatch, {
      winSize <- input$bWiSize
      winShift <- input$bWinShift
      intepolation <- input$bInterVal
      selectedParams <- c(input$bcol1, input$bcol2)
      
      data <- vector("list", batchFileNum)
      for(i in 1:length(selectedParams)){
          data[[i]] <- list()
      }
      
      for(j in seq_len(batchFileNum - 1)){
        
        k = 0
        
        hrv.batch.data <- batchHrvObjects[[j]]
        hrv.batch.data <- BuildNIHR(hrv.batch.data)
        hrv.batch.data <- InterpolateNIHR(hrv.batch.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
        hrv.batch.data <- CreateTimeAnalysis(hrv.batch.data, size=winSize, numofbins=NULL, interval=7.8125, verbose=NULL )
        hrv.batch.data <- CreateNonLinearAnalysis(hrv.batch.data)
        hrv.batch.data <- CreateFreqAnalysis(hrv.batch.data)
        poincareData = PoincarePlot(hrv.batch.data, doPlot = F, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, xlim=timeLineX, ylim=timeLineY,
                                    verbose=NULL)
        hrv.batch.data <- CalculateCorrDim(hrv.batch.data,indexNonLinearAnalysis=1, minEmbeddingDim=2, maxEmbeddingDim=8,timeLag=1,minRadius=1, maxRadius=15, pointsRadius=20,
                                     theilerWindow=10, corrOrder=2,doPlot=FALSE)
        hrv.batch.data <- CalculateSampleEntropy(hrv.batch.data,indexNonLinearAnalysis=1,doPlot=FALSE)
        hrv.batch.data <- EstimateSampleEntropy(hrv.batch.data,indexNonLinearAnalysis=1,regressionRange=c(6,10),doPlot=FALSE)
        hrv.batch.data <- CalculatePowerBand(hrv.batch.data, indexFreqAnalysis = 1, size = winSize, shift = winShift, sizesp = 1024)
        
        timeAnalysis = hrv.batch.data$TimeAnalysis[[1]]
        
        for(k in 1:length(selectedParams)){
          switchObject = selectedParams[[k]]
          switch(switchObject,
                 signalLen = {
                   data[[j]][[k]] <- max(hrv.batch.data$Beat[["Time"]])
                 },
                 NumBeats = {
                   data[[j]][[k]] <- length(hrv.batch.data$Beat[["Time"]])
                 },
                 MeanHr = {
                   data[[j]][[k]] <- round(mean(hrv.batch.data$Beat[["niHR"]]),4)
                 },
                 SDNN = {
                   data[[j]][[k]] <- round(timeAnalysis$SDNN, 4)
                 },
                 SDANN = {
                   data[[j]][[k]] <- round(timeAnalysis$SDANN, 4)
                 },
                 SD1 = {
                   data[[j]][[k]] <- round(poincareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1, 4)
                 },
                 SDNNIDX = {
                   data[[j]][[k]] <- round(timeAnalysis$SDNNIDX, 4)
                 },
                 pNN50 = {
                   data[[j]][[k]] <- round(timeAnalysis$pNN50, 4)
                 },
                 rMSSD = {
                   data[[j]][[k]] <- round(timeAnalysis$rMSSD, 4)
                 },
                 IRRR = {
                   data[[j]][[k]] <- round(timeAnalysis$IRRR, 4)
                 },
                 TINN = {
                   data[[j]][[k]] <- round(timeAnalysis$TINN, 4)
                 },
                 HRVIndex = {
                   data[[j]][[k]] <- round(timeAnalysis$HRVi, 4)
                 },
                 SD2 = {
                   data[[j]][[k]] <- round(poincareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2, 4)
                 },
                 stop("Error loading data...")
          )
        }
      }
      df <- as.data.frame(do.call(cbind, data))
      rownames(df) <- unlist(selectedParams)
      colnames(df) <- seq_len(batchFileNum-1)
      output$batchMainTable <- renderDataTable(df, options=list(searching=FALSE,dom="",pageLength = 15), escape = F)
    })
    
    repaintPoincareCompare <- function(){
      if(episodesSelected){
        output$secondaryPoinPlot<-renderPlot({
          if(customPlotAxis){
            pointcareData = PoincarePlot(hrv.episode, doPlot = T, main="Secondary Plot", indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE,
                                         xlim=timeLineX, ylim=timeLineY)
          }else{
            pointcareData = PoincarePlot(hrv.episode, doPlot = T, main="Secondary Plot", indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE)
          }
          refreshSd1sec(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
          refreshSd2sec(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
        })
      }
    }
    
    renderKsTest <- function(){
      if(!is.null(significanceMain) && !is.null(significanceComparing)){
        tryCatch({
          text <- capture.output(print(ks.test(significanceMain[[input$radioSigBands]], significanceComparing[[input$radioSigBands]])))
          testRes <- ks.test(significanceMain[[input$radioSigBands]], significanceComparing[[input$radioSigBands]])
          testRes <- gsub('data:  significanceMain[[input$radioSigBands]] and significanceComparing[[input$radioSigBands]]','',testRes)
          dValue <- paste("D = ", testRes[[1]][[1]])
          pValue <- paste("p-value = ", testRes[2])
          hypothesis <- paste("Alternative hypothesis: ",testRes[3])
          output$significanceText1 <- renderText(dValue)
          output$significanceText2 <- renderText(pValue)
          output$significanceText3 <- renderText(hypothesis)
          output$frameHistogram <- renderPlot({
            hist(significanceMain[[input$radioSigBands]],col=rgb(1,0,0,0.5), ylab="", xlab="", main="Histogram")
            hist(significanceComparing[[input$radioSigBands]], col=rgb(0,0,1,0.5), add=T)
            legend("topright",legend=c(input$significanceEpisodes, input$significanceComparing),col=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)), pt.cex=2, pch=15 )
          })
        },error = function(e){
          output$significanceText1 <- renderText("Not enough data to perform KS test.")
        })
      }
    }
    
    observeEvent(input$significanceEpisodes, {
      chosenEpisode <- input$significanceEpisodes
      auxEpisodesList <- significanceEpisodeList
      if(length(auxEpisodesList) > 1){
        for(i in 1:length(auxEpisodesList)){
          if(auxEpisodesList[[i]] == chosenEpisode){
            auxEpisodesList[[i]] <- paste0("OUTSIDE_", auxEpisodesList[[i]])
          }
        }
        updateSelectInput(session, "significanceComparing", choices = auxEpisodesList, selected=auxEpisodesList[1])
        
        epSigMain <- SplitPowerBandByEpisodes(hrv.data,length(hrv.data$FreqAnalysis), Tag=c(input$significanceEpisodes))
        significanceMain <<- epSigMain[["InEpisodes"]]
        renderKsTest()
      }
    })
    
    observeEvent(input$significanceComparing, {
      chosenEpisode <- input$significanceComparing
      if(length(significanceEpisodeList) > 1){
        if(grepl("OUTSIDE_", chosenEpisode, fixed = T )){
          chosenEpisode <- gsub("OUTSIDE_","",chosenEpisode)
          episodios <- SplitPowerBandByEpisodes(hrv.data, length(hrv.data$FreqAnalysis), Tag=c(chosenEpisode))
          significanceComparing <<- episodios[["OutEpisodes"]]
        }else{
          episodios <- SplitPowerBandByEpisodes(hrv.data, length(hrv.data$FreqAnalysis), Tag=c(chosenEpisode))
          significanceComparing <<- episodios[["InEpisodes"]]
        }
        renderKsTest()
      }
    })
    
    observeEvent(input$ulfMin, {
      ulfMin <<- input$ulfMin
    })
    
    observeEvent(input$ulfMax, {
      ulfMax <<- input$ulfMax
    })
    
    observeEvent(input$vlfMin, {
      vlfMin <<- input$vlfMin
    })
    
    observeEvent(input$vlfMax, {
      vlfMax <<- input$vlfMax
    })
    
    observeEvent(input$lfMin, {
      lfMin <<- input$lfMin
    })
    
    observeEvent(input$lfMax, {
      lfMax <<- input$lfMax
    })
    
    observeEvent(input$hfMin, {
      hfMin <<- input$hfMin
    })
    
    observeEvent(input$hfMax, {
      hfMax <<- input$hfMax
    })
    
    initializeFrameInputs <- function(){
      hideElement(id="significanceRow", anim=TRUE, animType="fade", time=0.4, selector="NULL", asis=FALSE)
      hideElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    }
  }


shinyApp(ui = ui, server = server)