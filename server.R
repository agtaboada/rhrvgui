shinyServer(function(input, output, session){
    hrv.data <- NULL
    beatSelected <<- FALSE
    episodesSelected <<- FALSE
    interpolationValue <<- 4
    
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
        datapath <- parseFilePaths(volumes, file())$datapath
        datapath <- gsub("/",.Platform$file.sep, datapath)
        datapath <- gsub(basename(datapath), "", datapath)
        hrv.data = LoadBeatAscii(hrv.data, parseFilePaths(volumes, file())$name, datapath)
        hrv.data = BuildNIHR(hrv.data)
        hrv.data <<- hrv.data
        output$mainGraph<-renderPlot({
          PlotNIHR(hrv.data)
          })
      }
    })
    
    observeEvent(input$filterHrButton,{
      hrv.data = FilterNIHR(hrv.data)
      hrv.data<<- hrv.data
      output$mainGraph<-renderPlot({
        PlotNIHR(hrv.data, main="Filtered non-interpolated heart rate")
        })
    })
    
    observeEvent(input$editHrButton, {
        hrv.data = EditNIHR(hrv.data)
        hrv.data <<- hrv.data
        output$mainGraph<-renderPlot({
          PlotNIHR(hrv.data, main="Edited data")})
    })
    
    observeEvent(input$loadEpButton, {
      file <- reactive(input$loadEpButton)
      if(length(file()) > 0 & is.numeric(file())){
        return(NULL)
      }else{
        datapath <- parseFilePaths(volumes, file())$datapath
        datapath <- gsub("/",.Platform$file.sep, datapath)
        datapath <- gsub(basename(datapath), "", datapath)
        hrv.data <- LoadEpisodesAscii(hrv.data, parseFilePaths(volumes, file())$name, datapath)
        hrv.data = BuildNIHR(hrv.data)
        hrv.data <<- hrv.data
        listOfEpisodeOptions <- unique(ListEpisodes(hrv.data)["Tag"])
        listOfEpisodeOptions <- append(listOfEpisodeOptions, "GLOBAL")
        updateSelectInput(session, "poincareEpisodes", choices = listOfEpisodeOptions, selected="GLOBAL")
        episodesSelected <<- TRUE
        output$mainGraph<-renderPlot({
          PlotNIHR(hrv.data, Indexes="all", main="Data with episodes")
        })
      }
    })
    
    observeEvent(input$clearEpButton, {
        hrv.data = BuildNIHR(hrv.data)
        hrv.data <<- hrv.data
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
                pointcareData = PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE)
                refreshSd1(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
                refreshSd2(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
              })
            }
          }
        }
    })
    
    observeEvent(input$poincareEpisodes, {
      hrv.data = CreateNonLinearAnalysis(hrv.data)
      if(input$poincareEpisodes == "GLOBAL"){
        updateSelectInput(session, "poincareComparing", choices = ListEpisodes(hrv.data)["Tag"])
        output$mainPoinPlot <- {
          renderPlot({
            pointcareData = PoincarePlot(hrv.data, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE)
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
            hrv.episode = BuildNIHR(hrv.episode)
            hrv.episode = CreateNonLinearAnalysis(hrv.episode)
            renderPlot({
              pointcareData = PoincarePlot(hrv.episode, doPlot = T, indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE)
              refreshSd1(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
              refreshSd2(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
            })
          }
        }}
    })
    
    
    observeEvent(input$poincareComparing, {
      output$secondaryPoinPlot <- {
        if(episodesSelected){
          hrv.episode = CreateHRVData(Verbose = TRUE)
          hrv.data = InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
          episodesVector = SplitHRbyEpisodes(hrv.data, T=str_replace_all(input$poincareComparing, fixed(" "), ""), verbose=NULL)
          hrv.episode = LoadBeatVector(hrv.episode, episodesVector$InEpisodes)
          hrv.episode = BuildNIHR(hrv.episode)
          hrv.episode = CreateNonLinearAnalysis(hrv.episode)
          renderPlot({
            pointcareData = PoincarePlot(hrv.episode, doPlot = T, main="Secondary Plot", indexNonLinearAnalysis=1,timeLag=1,confidenceEstimation = TRUE)
            refreshSd1sec(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD1)
            refreshSd2sec(pointcareData$NonLinearAnalysis[[1]]$PoincarePlot$SD2)
          })
        }
      }
    })
    
    observeEvent(input$sliderInterp, {
      interpolationValue <<- input$sliderInterp
    })
    
    observeEvent(input$interpolateButton, {
      if(beatSelected){
        hrv.data <<- InterpolateNIHR(hrv.data, freqhr = interpolationValue, method = c("linear", "spline"), verbose=NULL)
        output$mainGraph<-renderPlot({
          PlotHR(hrv.data, Indexes="all", main="Interpolated data")
        })
      }
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
  })

