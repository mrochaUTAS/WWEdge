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
 
  
  output$report <- downloadHandler(

    filename = function() {
      paste("report01.html")
        },
    
        content = function(con) {
          
          out <- rmarkdown::render("rmarkdown1.Rmd" , output_file = "report01.html",
                            params = params,
                            envir = new.env(parent = globalenv()))
                            
          file.copy("report01.html", con)
        }
      )
  
  
  
  }
  



