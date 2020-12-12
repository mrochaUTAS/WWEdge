############################
### WWEdge shiny app ######
###########################

library(shiny)
library(shinyFiles)
library(shinyDirectoryInput)
library(fs)
library(knitr)

ui = fluidPage(
  sliderInput("slider", "Slider", 1, 100, 50),
  

  fileInput("myFileG", "Upload green wedge images", accept = c('image/png', 'image/jpeg'), multiple = TRUE),
  
  fileInput("myFileD", "Upload dried wedge images", accept = c('image/png', 'image/jpeg'), multiple = TRUE),

  shinyDirButton("directoryG", "Green wedges", "Please select a folder of green wedges"),
  tags$p(),
  tags$p(),
  
  shinyDirButton("directoryD", "Dried wedges", "Please select a folder of dried wedges"),
  tags$p(),
  tags$p(),
  
  downloadButton("report", "Generate report"),
  
  textOutput("checkrender")

)
