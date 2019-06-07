# Application server
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

server <- function(input, output) {
  output$pie <- renderPlotly({
    volcano_data_set <- read.csv("data/volcano_1980.csv", stringsAsFactors = FALSE)
    if(input$pie_var == "Country") {
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        group_by(Country) %>%
        summarize(Incidents = n())
      p <- plot_ly(volcano_data, labels = ~Country, values = ~Incidents, type = 'pie', 
                   showlegend = FALSE) %>%
        layout(title = paste0("Proportion of ", input$pie_var, "of volcanos in ", input$year_var),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p
    } 
    else if(input$pie_var == "Type"){
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        group_by(Type) %>%
        summarize(Incidents = n())
      p <- plot_ly(volcano_data, labels = ~Type, values = ~Incidents, type = 'pie', 
                   showlegend = FALSE) %>%
        layout(title = paste0("Proportion of ", input$pie_var, "of volcanos in ", input$year_var),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
      p
    }
    else if(input$pie_var == "Status") {
      volcano_data <- volcano_data_set %>%
        filter(Year == Status) %>%
        group_by(input$pie_var) %>%
        summarize(Incidents = n())
      p <- plot_ly(volcano_data, labels = ~Status, values = ~Incidents, type = 'pie', 
                   showlegend = FALSE) %>%
        layout(title = paste0("Proportion of ", input$pie_var, " of volcanos in ", input$year_var),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) 
      p
    }
    else {
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        drop_na(input$pie_var)
      volcano_data <- volcano_data %>%
        mutate(string = paste(
          paste0(Name, ", ", Country),
          paste0("Date: ", Month, "/", Day, "/", Year),
          sep = "<br>"
        ))
      p <- plot_ly(volcano_data, labels = ~string, values = ~volcano_data[, input$pie_var], type = 'pie', 
                   showlegend = FALSE) %>%
        layout(title = paste0("Proportion of ", input$pie_var, " per incident in ", input$year_var),
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p                
    }
  })
}
