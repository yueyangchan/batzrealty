
# install.packages("shiny")
# install.packages("dplyr")

library("shiny")
library("dplyr")

all_city <- read.csv("data/City_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)
state_names <- all_city %>% select(State) %>% unique() %>% arrange(State)
state_names <- state_names[,1]

years <- c(1996:2018)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

ui <- navbarPage(
  title = "Zillow",

  tabPanel(
    title = "Rate of Growth",
    sidebarPanel(
      p("EARLIEST DATE AVAILABLE: ", strong("1996-04")),
      p("MOST RECENT DATE AVAILABLE: ", strong("2018-04")),
      selectInput("start_year",
                  label = "Starting Year",
                  choices = years),
      selectInput("start_month",
                  label = "Starting Month",
                  choices = months),
      selectInput("end_year",
                  label = "Ending Year",
                  choices = years),
      selectInput("end_month",
                  label = "Ending Month",
                  choices = months),
      selectInput("states",
                  label = "States (for Cities tab)",
                  choices = state_names),
      actionButton("submit", 
                   label = "Submit")
    ),
    
    mainPanel(
      navbarPage(
        title = "Rate of Growth",
        
        tabPanel(
          title = "States",
          plotOutput("state_rate_plot")
        ),
      
        tabPanel(
          title = "Cities",
          tableOutput("city_rate_table")
        )
      )
    )
  )
)

shinyUI(ui)
