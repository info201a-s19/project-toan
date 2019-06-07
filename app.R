# Load in packages
library(shiny)
source("app_ui.R")
source("app_server.R")
library(rsconnect)


# Load shiny application
shinyApp(ui = ui, server = server)
