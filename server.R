shinyServer(function(input, output, session){
    hrv.data <- NULL
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
  })