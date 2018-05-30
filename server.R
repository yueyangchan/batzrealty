
# install.packages("shiny")
# install.packages("XML")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("ggplot2")

library("shiny")
library("XML")
library("jsonlite")
library("dplyr")
library("ggplot2")

# File as of 5-29-18.
all_state <- read.csv("data/State_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)
all_city <- read.csv("data/City_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)

# QUESTION
# What state/city is growing the fastest? In terms of increasing housing prices.
# Increasing housing prices can be interpreted as a fast growing state/city. 
# This information can help new/young professionals identify which city they can 
# potentially move to, or avoid, when they first start their career.

state_map_data <- map_data("state")

server <- function(input, output) {
  
  # Dataframe to be used to plot a map of the United States.
  state_data <- reactive({
    per_inc_state <- all_state %>%
      select(start_date(), end_date()) %>%
      summarize(per_inc = ((all_state[end_date()] - all_state[start_date()]) / all_state[start_date()] * 100))
    
    new_state <- all_state %>%
      select(RegionName)
    new_state$per_inc <- per_inc_state$per_inc
    new_state$RegionName <- tolower(new_state$RegionName)
    
    combined_state <- left_join(state_map_data, new_state, by = c("region" = "RegionName"))
    combined_state  
  })
  
  # Dataframe for state data.
  state_data_table <- reactive({
    per_inc_state <- all_state %>%
      select(start_date(), end_date()) %>%
      summarize(per_inc = ((all_state[end_date()] - all_state[start_date()]) / all_state[start_date()] * 100))
    
    new_state <- all_state %>%
      select(RegionName, start_date(), end_date())
    new_state$per_inc <- per_inc_state$per_inc
    
    colnames(new_state)[1] <- "State"
    colnames(new_state)[2] <- paste0(rog_vals$start_year, "-", rog_vals$start_month)
    colnames(new_state)[3] <- paste0(rog_vals$end_year, "-", rog_vals$end_month)
    colnames(new_state)[4] <- "Percent Increase"
    
    new_state
  })
  
  # Dataframe for cities in a given state.
  city_data <- reactive({
    new_city <- all_city %>%
      filter(State == rog_vals$states) %>%
      select(RegionName, State, Metro, CountyName, start_date(), end_date())
  
    colnames(new_city)[1] <- "City"
    colnames(new_city)[2] <- "State"
    colnames(new_city)[3] <- "Metro"
    colnames(new_city)[4] <- "County"
    colnames(new_city)[5] <- paste0(rog_vals$start_year, "-", rog_vals$start_month)
    colnames(new_city)[6] <- paste0(rog_vals$end_year, "-", rog_vals$end_month)
  
    new_city
  })
  
  start_date <- reactive({
    rog_vals$start_date <- paste0("X", rog_vals$start_year, ".", rog_vals$start_month)
  })
  
  end_date <- reactive({
    rog_vals$end_date <- paste0("X", rog_vals$end_year, ".", rog_vals$end_month)
  })
  
  rog_vals <- reactiveValues()
  rog_vals$start_year <- "1996"
  rog_vals$start_month <- "04"
  rog_vals$end_year <- "2018"
  rog_vals$end_month <- "04"
  rog_vals$states <- "WA"
  rog_vals$start_date <- "X1996.04"
  rog_vals$end_date <- "X2018.04"
  
  observeEvent(
    input$submit, {
    rog_vals$start_year <- input$start_year
    rog_vals$start_month <- input$start_month
    rog_vals$end_year <- input$end_year
    rog_vals$end_month <- input$end_month
    rog_vals$states <- input$states
  })
  
  output$state_rate_plot <- renderPlot(
    ggplot(state_data()) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = per_inc), color = "black") +
    coord_quickmap() +
    labs(title = paste0("zindex Growth Rate in the United States from ", rog_vals$start_year, "-", 
                        rog_vals$start_month, " to ", rog_vals$end_year, "-", rog_vals$end_month), 
         fill = "Percent Change") +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ) 
  
  output$state_rate_table <- renderTable({
    state_data_table()
  }, width = "100%", align = "c")
  
  output$city_rate_table <- renderDataTable({
    city_data()
  })
}

shinyServer(server)
