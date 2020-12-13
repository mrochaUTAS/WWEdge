###
###

library(shiny)
library(shinyFiles)
library(fs)

server <-  function(input, output, session) {

  # unlink("Green/*.jpg")
  # unlink("Dried/*.jpg")
  # dir.create("Green")
  # dir.create("Dried")
  
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
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()(), wd='srv/shiny-server/WWEdge/')
  
  shinyDirChoose(input, "directoryG",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = TRUE,
                 defaultRoot = "wd"
                 
                 )

  shinyDirChoose(input, "directoryD",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = TRUE,
                 defaultRoot = "wd"
                
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
          paste("report01.html")
        },
        content = function(con) {
          file.copy("report01.html", con)
        }
      )
  
  
  
  }
  



