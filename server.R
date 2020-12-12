#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(shinyDirectoryInput)
library(fs)
library(knitr)
library(rmarkdown)

server <-  function(input, output, session) {

  # unlink("Green/*.jpg")
  # unlink("Dried/*.jpg")
  
  observeEvent(input$myFileG, {
    inFile <- input$myFileG
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path("./Green", inFile$name) )
  })
  
  observeEvent(input$myFileD, {
    inFile <- input$myFileD
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path("./Dried", inFile$name) )
  })
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()(), wd='.')
  
  shinyDirChoose(input, "directoryG",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = FALSE,
                 defaultRoot = "wd",
                 defaultPath=''
                 )

  shinyDirChoose(input, "directoryD",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = FALSE,
                 defaultRoot = "wd",
                 defaultPath=''
                 
                )
  
  output$report <- downloadHandler(
    
    # For  output
    filename = "report001.html",
    
    content = function(file) {
      
      withProgress(message = 'Rendering, please wait!', {

      tempReport <- "rmarkdown1.Rmd"
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider,
                     pathG = parseDirPath(volumes, input$directoryG),
                     pathD = parseDirPath(volumes, input$directoryD))
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  

    })
  
  
  }
  



