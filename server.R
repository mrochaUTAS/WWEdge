###
###

library(shiny)
library(shinyFiles)
library(fs)
library(rmarkdown)

server <-  function(input, output, session) {

  #unlink("./Green/*.jpg")
  #unlink("./Dried/*.jpg")
  # dir.create("Green")
  # dir.create("Dried")
  #unlink("./report01.html")
  
  observeEvent(input$myFileG, {
    inFile <- input$myFileG
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path("/Green/", inFile$name) )
  })
  
  observeEvent(input$myFileD, {
    inFile <- input$myFileD
    if (is.null(inFile))
      return()
    file.copy(inFile$datapath, file.path("/Dried/", inFile$name) )
  })
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()(), wd='.')
  
  shinyDirChoose(input, "directoryG",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = TRUE,
                 defaultRoot = "wd",
                 defaultPath=''
                 )

  shinyDirChoose(input, "directoryD",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = TRUE,
                 defaultRoot = "wd",
                 defaultPath=''
                 
                )
  
  shinyDirChoose(input, "directoryR",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = TRUE,
                 defaultRoot = "wd",
                 defaultPath=''
                 
  )
  
  observeEvent(input$do, {
    
     withProgress(message = 'Rendering, please wait!', {
      tempReport <- "rmarkdown1.Rmd"
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider,
                     pathG = parseDirPath(volumes, input$directoryG),
                     pathD = parseDirPath(volumes, input$directoryD))
      
      rmarkdown::render(tempReport, output_file = "report01.html",
                        params = params,
                        envir = new.env(parent = globalenv())
      )})
     
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  output$report <- downloadHandler(

    filename = function() {
          paste("report01.html", sep = "")
        },
        content = function(con) {
          file.copy("report01.html", con)
        }
      )
  
  
  
  }
  



