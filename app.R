# rm(list=ls())
# options(shiny.reactlog=TRUE)

library(shiny)

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)