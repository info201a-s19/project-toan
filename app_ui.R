# Load in packages
library("rsconnect")
library(plotly)
library(shiny)
library(DT)

# reads in data set
volcano_data_set <- read.csv("data/volcano_1980.csv", stringsAsFactors = FALSE)
# gets rid of a row with no elements filled
volcano_data_set <- volcano_data_set[-1, ]
# creates set of variables as options to explore
volcano_select <- colnames(volcano_data_set[, c(8, 12, 13, 17:36)])
# creates list of years for user to explore further
year_select <- unique(volcano_data_set[, 1])

# variable used in pie chart to investigate
var_select <- selectInput(
  "pie_var",
  label = 
  HTML('<p style=
       "color:black;font-size:16px;">Select a variable to investigate:'),
  choices = volcano_select,
  selected = "Country"
)

# year used to investigate variable
yr_select <- selectInput(
  "year_var",
  label = HTML('<p style="color:black;font-size:16px;">Select a year:'),
  choices = year_select,
  selected = "1980"
)

# combines user input menus into panel
pie_sidebar <- sidebarPanel(
  var_select,
  yr_select
)

# description of chart and plots chart
pie_main <- mainPanel(
  p("The pie chart attempts to provide an insight to the proportion
    of a certain variable for a certain year. For instance, if you selected
    the variable \"Type\",  then the chart would show the proportion of
    volcanos of a certain type (Stratovolcano, etc.) that erupted in that
    year. However, if picking a variable such as number of deaths, the pie 
    char would show the number of deaths per incident for that year in
    proportion to the rest of the year's incidents, in addition to 
    the number of incidents that had an unrecorded number of deaths to
    show that there are incidents with unrecorded numbers."),
  plotlyOutput("pie")
)

# creates tab panel for pie chart
pie_page <- tabPanel(
  "Pie Chart Exploration",
  titlePanel("Pie Chart Exploration"),
  sidebarLayout(
    pie_sidebar,
    pie_main
  )
)


# page used for project insights
insight_panel <- mainPanel(
#insight 1
  h4("Question 1: What countries had the most amount of damage in 
      millions of dollars out of all the incidents in the 21st 
      century?"),
  br(),
  p("This question provides an insight to the countries most recently 
      affected and the amount of money lost in trying to fix what has been
      lost. This data could also be used to help better allocate resources
      to countries who might not have resources readily available. Due to the
      number of missing values in the dataset, a lot of the countries do not
      appear in the table below. However, the countries that have a number 
      on the list give an estimate as to how much relief aid might be needed.
      An interesting observation is the Indonesia having the same number of 
      incidents with recorded damage as the US, but those eruptions caused 
      less damage in terms of money. That could mean those volcanos were in 
      less populated areas.
    "),
  DT::dataTableOutput("table_one"),
#insight 2
  h4("Question 2: How many eruptions before, and including, 1990 had less 
     than 100 people killed?"),
  br(),
  p("Given how many protocols and safety measures were facilitated by 
    technology, limiting the scope to 1990 and before can give a sense 
    as to how well people were being evacuated before the rise of a lot
    of alarm systems, etc. The incident out of the ones listed with 
    the highest number of deaths is Semeru in Indonesia at 70 deaths. 
    Most of them had between 1-5 deaths, which could signify that 
    evacuations were successful. This could be due to either a small 
    population near the volcano, which is easier than a city, or large 
    cities had intricate plans in case of emergencies such as this."),
  DT::dataTableOutput("table_two"),
#insight 3
  h4("Question 3: Within the past 30 years, which volcanoes erupted?"),
  br("It sort of goes without saying, but past eruptions can help 
     predict if another will go off. I do not know the exact science for 
     predicting these events, but the table below lists a number of of 
     volcanos that had erupted in the past 30 years. Kilauea had 10 eruptions
     in the past 30 years. That is on average 1 every three. This is a good 
     indicator of the possibilty of it erupting again, but some volcanos that 
     have laid dormant for decades can also erupt without a moment's notice."),
  DT::dataTableOutput("table_three")
)

# main panel for intro page
intro_panel <- mainPanel(
  p("As a group, we were interested in the various natural phenomena that 
     occurs around us and decided to delve deeper into an environmental 
     structure that currently has tremendous impact on us in Seattle, 
     Washington, and that would be Mount Rainier. Some of us have taken/are 
     currently taking a volcanos course and we believe this would be 
     another way to learn more about this topic, as it can greatly affect 
     our awareness of volcanic activity. We source our volcano 
     data (from 1980 to 2019) from the ", 
    a("Significant Volcano Eruption Database.",
    href =
      "https://www.ngdc.noaa.gov/nndc/servlet/ShowDatasets?dataset=102557&search_look=50&display_look=50"
  )),
  img(src = "ad_233677945.jpg", width = 750, height = 500)
)

# tab for intro page
intro_tab <- tabPanel(
  "Introduction",
  titlePanel("Introduction"),
  sidebarLayout(
    intro_panel,
    sidebarPanel(
      h3(HTML('<p style="color:black;">image from'), a("metro.co.uk",
        href = "https://metro.co.uk/2017/02/03/incredible-image-captures-the-exact-second-lightning-struck-an-erupting-volcano-6425301/"
      ))
    )
  )
)

questions_page <- tabPanel(
  "Insights",
  titlePanel("Insights"),
  insight_panel
)


ui <- navbarPage(
  "Volcano Data",
  fluid = TRUE,
  intro_tab,
  pie_page,
  questions_page
)
