# Load in packages
library("ggplot2")
library("rsconnect")
library(plotly)
library(shiny)

volcano_data_set <- read.csv("data/volcano_1980.csv", stringsAsFactors = FALSE)
volcano_data_set <- volcano_data_set[-1, ]
volcano_select <- colnames(volcano_data_set[, c(8, 12, 13, 17:36)])
year_select <- unique(volcano_data_set[, 1])

# Application UI
var_select <- selectInput(
  "pie_var",
  label = "Select a variable to investigate",
  choices = volcano_select,
  selected = "Country"
)

yr_select <- selectInput(
  "year_var",
  label = "Select a year",
  choices = year_select,
  selected = "1980"
)

pie_sidebar <- sidebarPanel(
  var_select,
  yr_select
)

pie_main <- mainPanel(
  plotlyOutput("pie")
)

pie_page <- tabPanel(
  "Pie Chart Exploration",
  titlePanel("Pie Chart Exploration"),
  sidebarLayout(
    pie_sidebar,
    pie_main
  )
)


ui <- navbarPage(
  "Volcano Data",
  pie_page
)

