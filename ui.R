library(RHRV)
library(shiny)
library(shinyFiles)
library(shinyjs)
library(spsComps)
library(tkrplot)
ui<-fluidPage(
  useShinyjs(),
  tags$link(rel = "stylesheet", type="text/css", href="css/style.css"),
  navbarPage("RHRV GUI",
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
    tabPanel("Interpolate"),
    tabPanel("Frame-based evolution"),
    tabPanel("Report"),
    tabPanel(id="panelPoincare","Poincare plot",
             fluidPage(
               sidebarPanel(id="poincareSidebar",
               h4("Poincare Plot")
                            )
             )
  )
))
