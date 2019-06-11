# Application server
library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(lintr)
library(DT)

server <- function(input, output) {
  volcano_data_set <- read.csv("data/volcano_1980.csv",
                               stringsAsFactors = FALSE)
  #pie chart
  output$pie <- renderPlotly({
    #first 3 branches of if statement used due to syntax issues
    #and different pie chart produced
    if (input$pie_var == "Country") {
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        group_by(Country) %>%
        summarize(Incidents = n())
      p <- plot_ly(volcano_data,
        labels = ~Country, values = ~Incidents, type = "pie",
        showlegend = FALSE
      ) %>%
        layout(
          title = paste0(
            "Proportion of ", input$pie_var,
            " of volcanos in ", input$year_var
          ),
          xaxis = list(
            showgrid = FALSE, zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE, zeroline = FALSE,
            showticklabels = FALSE
          )
        )
      p
    }
    else if (input$pie_var == "Type") {
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        group_by(Type) %>%
        summarize(Incidents = n())
      p <- plot_ly(volcano_data,
        labels = ~Type, values = ~Incidents, type = "pie",
        showlegend = FALSE
      ) %>%
        layout(
          title = paste0(
            "Proportion of ", input$pie_var,
            "of volcanos in ", input$year_var
          ),
          xaxis = list(
            showgrid = FALSE, zeroline = FALSE,
            showticklabels = FALSE
          )
        )
      p
    }
    else if (input$pie_var == "Status") {
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        group_by(Status) %>%
        summarize(Incidents = n())
      p <- plot_ly(volcano_data,
        labels = ~Status, values = ~Incidents, type = "pie",
        showlegend = FALSE
      ) %>%
        layout(
          title = paste0(
            "Proportion of ", input$pie_var,
            " of volcanos in ", input$year_var
          ),
          xaxis = list(
            showgrid = FALSE, zeroline = FALSE,
            showticklabels = FALSE
          )
        )
      p
    }
    #creates pie chart for variable not listed above
    else {
      volcano_data <- volcano_data_set %>%
        filter(Year == input$year_var) %>%
        drop_na(input$pie_var)
      volcano_data_na <- volcano_data_set %>%
        filter(Year == input$year_var)
      num_missing <- nrow(volcano_data_na) - nrow(volcano_data)
      #produces hover text
      volcano_data <- volcano_data %>%
        mutate(string = paste(
          paste0(Name, ", ", Country),
          paste0("Date: ", Month, "/", Day, "/", Year),
          sep = "<br>"
        ))
      #if all incidents in selected year have unrecorded variable, print this
      if (nrow(volcano_data) == 0) {
        p <- plot_ly(volcano_data,
          labels = ~string, values = ~ volcano_data[, input$pie_var],
          type = "pie",
          showlegend = FALSE
        ) %>%
          layout(
            title = paste0(
              "No data available for ", input$pie_var,
              " for Year ", input$year_var
            ),
            xaxis = list(
              showgrid = FALSE, zeroline = FALSE,
              showticklabels = FALSE
            ),
            yaxis = list(
              showgrid = FALSE, zeroline = FALSE,
              showticklabels = FALSE
            )
          )
      }
      #otherwise produce pie chart, saying number of incidents
      #with unrecorded variable
      else {
        p <- plot_ly(volcano_data,
          labels = ~string, values = ~ volcano_data[, input$pie_var],
          type = "pie",
          showlegend = FALSE
        ) %>%
          layout(
            title = paste0(
              "Proportion of ", input$pie_var, " per incident in ",
              input$year_var, "<br>Number of incidents with unrecorded ",
              input$pie_var, ": ", num_missing
            ),
            xaxis = list(
              showgrid = FALSE, zeroline = FALSE,
              showticklabels = FALSE
            ),
            yaxis = list(
              showgrid = FALSE, zeroline = FALSE,
              showticklabels = FALSE
            )
          )
      }
      p
    }
  })
  #produces table for question nuumber 1 in takeaway page
  output$table_one <- DT::renderDataTable({
    vol_df <- volcano_data_set %>%
      drop_na(Year) %>%
      group_by(Country) %>%
      summarise(Incidents = n())
    vol_df_two <- volcano_data_set %>%
      drop_na(TOTAL_DAMAGE_MILLIONS_DOLLARS)
    vol_table <- vol_df_two %>%
      group_by(Country) %>%
      summarize(Incidents = n(), TOTAL_DAMAGE_MILLIONS = 
                  round(sum(TOTAL_DAMAGE_MILLIONS_DOLLARS) / n(), 1)) 
    vol_table <- left_join(vol_df, vol_table, by = "Country")
    vol_table$TOTAL_DAMAGE_MILLIONS[is.na(vol_table$TOTAL_DAMAGE_MILLIONS)] <- 
      "Not Available"
    vol_table$Incidents.y[is.na(vol_table$Incidents.y)] <- 0
    vol_table <- vol_table %>%
      mutate("Missing incidents" = Incidents.x - Incidents.y)
    names(vol_table)[names(vol_table) == "Incidents.y"] <-
      "Number of Incidents with Recorded Damage in Millions of $"
    names(vol_table)[names(vol_table) == "Incidents.x"] <-
      "Total Number of Incidents in the past 19 years"
    names(vol_table)[names(vol_table) == "TOTAL_DAMAGE_MILLIONS"] <-
      "Total damage within the past 19 years, in millions of dollars"
    vol_table <- vol_table[c(1, 2, 3, 5, 4)]
    return(vol_table)
  })
  #table for insight 2
  output$table_two <- DT::renderDataTable({
    vol_df <- volcano_data_set %>%
      drop_na(DEATHS) %>% 
      filter(Year <= 1990) %>%
      filter(DEATHS < 100)
      vol_df <- vol_df[c(1, 2, 3, 6, 8, 17)]
  })
  
  output$table_three <- DT::renderDataTable({
   vol_df <- volcano_data_set %>%
     mutate(Volcano = paste0(Name, ", ", Country)) %>%
     filter(Year >= 1989) %>%
     group_by(Volcano) %>%
     summarize(Eruptions = n()) %>%
     arrange(-Eruptions)
  })
}
