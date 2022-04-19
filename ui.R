library(RHRV)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(spsComps)
library(tkrplot)
ui<-fluidPage(
  tags$link(rel = "stylesheet", type="text/css", href="css/style.css"),
  navbarPage("RHRV GUI",
    tabPanel("Main menu"),
    tabPanel("Interpolate"),
    tabPanel("Frame-based evolution"),
    tabPanel("Report"),
    tabPanel("Pointcare plot")
  ),
  sidebarPanel("",
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
  mainPanel("",
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
              ),
              useShinyjs()
  )
)
