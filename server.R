
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
# This information can help new/young professionals identify which city they can potentially move to, or avoid, when they first start their career.

# PSEUDOCODE
# Pick from a date range: let user choose year and month, then make that into a string for easy access.
# Pick from past to present dates (ex. X1996.04, X2009.06)
# Calculate the percent increase from past date to present date for each state (make sure to take into account missing dates: North Dakota)
# Display visualization of a map of the UNITED STATES.
# Color each state by rate of increase.
# Add mouse effects for addition data.
# Maybe add a table underneath for indepth data on a single state?
# Do another thing for all cities in a state. Just show a graph, sorted by the city with the highest rate of growth.

state_map_data <- map_data("state")

sdate <- "X1996.04"
edate <- "X2018.04"

# % increase = Increase ÷ Original Number × 100

new_state <- all_state %>%
  select("RegionName", sdate, edate) %>%
  summarize(per_inc = ((all_state[edate] - all_state[sdate]) / all_state[sdate] * 100))

server <- function(input, output) {
  
  rog_vals <- reactiveValues()
  rog_vals$start_year <- ""
  rog_vals$start_month <- ""
  rog_vals$end_year <- ""
  rog_vals$end_month <- ""
  rog_vals$states <- ""
  
  observeEvent(
    input$submit, {
    rog_vals$start_year <- input$start_year
    rog_vals$start_month <- input$start_month
    rog_vals$end_year <- input$end_year
    rog_vals$end_month <- input$end_month
    rog_vals$states <- input$states
  })

  output$state_rate_plot <- renderPlot(
    ggplot(world_map_data) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = region)) +
    coord_quickmap() +
    labs(title = "TITLE", fill = "LEGEND") +
    theme(legend.position = "none",
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ) 
  
  output$city_rate_table
  
  output$test <- renderText({
    paste(rog_vals$start_year,
    rog_vals$start_month,
    rog_vals$end_year,
    rog_vals$end_month,
    rog_vals$states)
  })
  
  
}

shinyServer(server)














# base_uri <- "http://www.zillow.com/webservice/GetRegionChildren.htm"
# z_id_ars3697 <- "X1-ZWz18lzviceeiz_9skq6"
# 
# # state_df (function) - Returns a dataframe of all of a state's cities/counties and their "zindex" in USD.
# # state (string) - State in the United States. Should be writen in two letters (ex. "wa" for Washington, "ca" for California).
# state_df <- function(state) {
#   uri <- paste0(base_uri, "?zws-id=", z_id_ars3697, "&state=", state)
#   # Zillow returns the response back in XML.
#   response <- xmlParse(uri)	
#   # Cleaning up XML and turning it into a cleaner looking dataframe.
#   list <- response %>% xmlToList() %>% toJSON(pretty = TRUE) %>% fromJSON()	
#   # Getting rid of the first index, because it's just a "count" row. It's irrelevant.
#   raw_data <- list$response$list[-1]
#   # Creating a new dataframe with two columns: "name" and "zindex_usd".
#   # Only has one row, the first row from the raw_data dataframe.
#   state_data <- data_frame(name = raw_data[1]$region$name,
#                            zindex_usd = raw_data[1]$region$zindex$text)
#   
#   # Adds in the rest of the rows into the state_data dataframe.
#   # tryCatch is used to prevent error messages in the console.
#   for(i in 2:NROW(raw_data)) {
#     tryCatch({
#       temp_df <- data.frame(name = raw_data[i]$region$name,
#                             zindex_usd = raw_data[i]$region$zindex$text)
#       state_data <- rbind(state_data, temp_df)}, error = function(e){})
#   }
#   
#   state_data
# }