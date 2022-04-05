shinyServer(function(input, output, session){
    print("Nueva ejecucion")
    hrv.data <- NULL
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyFileChoose(input, "loadHrButton", roots = volumes, session = session)
    hrv.data = reactiveVal()
    hrv.data = CreateHRVData()
    hrv.data = SetVerbose(hrv.data, TRUE)
    observeEvent(input$loadHrButton, { 
      file <- reactive(input$loadHrButton)
      if(length(file()) > 0 & is.numeric(file())){
        return(NULL)
      }else{
        datapath <- parseFilePaths(volumes, file())$datapath
        datapath <- gsub("/","\\\\", datapath)
        datapath <- gsub(basename(datapath), "", datapath)
        hrv.data = LoadBeatAscii(hrv.data, parseFilePaths(volumes, file())$name, datapath)
        hrv.data = BuildNIHR(hrv.data)
        hrv.data <<- hrv.data
        output$mainGraph<-renderPlot({
          PlotNIHR(hrv.data)
          })
        print(hrv.data)
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
  })