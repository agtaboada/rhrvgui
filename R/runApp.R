#' runApplication
#' @import shiny
#' @import RHRV
#' @import shinyjs
#' @import shinyFiles
#' @import tkrplot
#' @import DT
#' @import ggplot2
#' Starts the application.
#' @return Opens the GUI on a new window.
#' @examples 
#' runApplication()
#' @export
runApplication <- function(){
  fileSep = .Platform$file.sep
  print(.libPaths())
  for(path in .libPaths()){
    path <- paste(path,fileSep,sep = "")
    path <- paste(path,"rhrvgui",sep = "")
    path <- paste(path,fileSep,sep = "")
    path <- paste(path,"web",sep = "")
    shiny::runApp(path)
  }
} 