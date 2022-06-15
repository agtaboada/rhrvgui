shinyServer(function(input, output, session){
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
    poincareYMax <<- 800
    customPlotAxis <<- FALSE
    significanceAnalysis <<- FALSE
    hrv.episode <<- NULL
    loadingFileErrorStr <<- "Error loading the file: make sure you are using the proper file extension and data format."
    
    shinyjs::disable("sigAnBt")
    shinyjs::disable("filterHrButton")
    shinyjs::disable("editHrButton")
    shinyjs::disable("loadEpButton")
    shinyjs::disable("clearEpButton")
    shinyjs::disable("interpolateButton")
    
    hideElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    shinyFileChoose(input, "loadHrButton", roots = volumes, session = session)
    shinyFileChoose(input, "loadEpButton", roots = volumes, session = session)
    
    hrv.data = reactiveVal()
    hrv.data = CreateHRVData()
    hrv.data = SetVerbose(hrv.data, TRUE)
    
    observeEvent(input$loadHrButton, {
      file <- reactive(input$loadHrButton)
      if(length(file()) > 0 & is.numeric(file())){
        return(NULL)
      }else{
        beatSelected <<- TRUE
        beatInterpolated <<- FALSE
        episodesSelected <<- FALSE
        datapath <- parseFilePaths(volumes, file())$datapath
        datapath <- gsub("/",.Platform$file.sep, datapath)
        datapath <- gsub(basename(datapath), "", datapath)
        tryCatch({hrv.data = LoadBeatAscii(hrv.data, parseFilePaths(volumes, file())$name, datapath)
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
        })
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
                  episodesSelected <<- TRUE
                  shinyjs::enable("clearEpButton")
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
          if(episodesSelected){
            shinyjs::enable("sigAnBt")
            listOfEpisodeOptions <- unique(ListEpisodes(hrv.data)["Tag"])
            updateSelectInput(session, "significanceEpisodes", choices = listOfEpisodeOptions, selected=listOfEpisodeOptions[0])
            updateSelectInput(session, "significanceComparing", choices = listOfEpisodeOptions, selected=listOfEpisodeOptions[1])
          }
          hrv.data = CreateFreqAnalysis(hrv.data, verbose = F)
          hrv.data = CalculatePowerBand(hrv.data,length(hrv.data$FreqAnalysis), size = 300, shift = 60, sizesp = 1024)
          if(input$lfhf){#todo: checkear por que indexes no funcionan
              output$lfhfPlot <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "LF/HF",
                                                                 epColorPalette = "red")})
              output$ulfPlot <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "ULF", epColorPalette = "red",
                                                                 epLegendCoords = c(2000,7500))})
              output$vlfPlot <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "VLF", epColorPalette = "red",
                                                                epLegendCoords = c(2000,7500))})
              output$hfPlot <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "HF", epColorPalette = "red",
                                                                epLegendCoords = c(2000,7500))})
              output$lfPlot <- renderPlot({PlotSinglePowerBand(hrv.data, length(hrv.data$FreqAnalysis), "LF", epColorPalette = "red",
                                                               epLegendCoords = c(2000,7500))})
          }
        }else{
          showNotification("Please, select a beat and interpolate it to use this menu.",type='warning')
        }
      }
    })
    
    observeEvent(input$poincareEpisodes, {
      if(beatSelected){
        hrv.data = CreateNonLinearAnalysis(hrv.data)
        if(input$poincareEpisodes == "GLOBAL"){
          updateSelectInput(session, "poincareComparing", choices = ListEpisodes(hrv.data)["Tag"])
          output$mainPoinPlot <- {
            renderPlot({
              if(customPlotAxis){
                pointcareData = PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE,
                                             xlim=timeLineX, ylim=timeLineY, verbose=NULL)
              }else{
                PoincarePlot(hrv.episode, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, verbose=NULL)
              }
              refreshSd1(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
              refreshSd2(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
            })
          }
        }else{
          updateSelectInput(session, "poincareComparing", choices="")
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
                  poincareData = PoincarePlot(hrv.episode, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE, verbose=NULL)
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
    
    observeEvent(input$sigAnBt, {
      significanceAnalysis <<- !significanceAnalysis
      if(significanceAnalysis == TRUE){
        showElement(id="significanceRow", anim = TRUE, animType="fade", time=0.4, selector=NULL, asis = FALSE)
        showElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        hideElement(id="mainFrameRow", anim=TRUE, animType="fade", time=0.4, selector="NULL", asis=FALSE)
        updateActionButton(session, "sigAnBt",label = "Back")
      }else{
        hideElement(id="significanceRow", anim=TRUE, animType="fade", time=0.4, selector="NULL", asis=FALSE)
        hideElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
        showElement(id="mainFrameRow", anim = TRUE, animType="fade", time=0.4, selector=NULL, asis = FALSE)
        updateActionButton(session, "sigAnBt",label = "Significance Analysis")
      }
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
    
    initializeFrameInputs <- function(){
      hideElement(id="significanceRow", anim=TRUE, animType="fade", time=0.4, selector="NULL", asis=FALSE)
      hideElement(id="significanceOptions", anim = TRUE, animType="slide", time=0.1, selector=NULL, asis = FALSE)
    }
  })

