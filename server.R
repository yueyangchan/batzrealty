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

state_map_data <- map_data("state")

# create shiny server function with regular input and output
# parameters along with a session for changing the input slider
# bar based on filtered smallest and largest values
shinyServer(function(input, output, session) {
  
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
  
  # given a city, it will return all the neighborhoods
  # in that city that have a complete set of data. Many
  # cities don't have any neighborhoods that have a completed
  # set of data, so the function may return nothing.
  add_city_df <- function(city){
    uri <- paste0(
      base_uri,
      "?zws-id=",
      z_id, "&state=",
      state,
      "&city=",
      city,
      "&childtype=",
      childtype
      )
    response <- (uri)
  
    list <- xmlToList(response)
    list <- toJSON(list, pretty = TRUE)
    list <- fromJSON(list)
    regions <- list$response$list
    # remove missing values
    regions <- regions[lapply(regions,length)>5]
    df <- data.frame(
      matrix(unlist(regions), nrow=7, byrow=F),
      stringsAsFactors=FALSE
      )
    df <- t(df)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    row.names(df) <- 1:NROW(df)
    colnames(df) <- c("id", "name", "price", "currency", "web", "lat", "long")
    df <- df %>% mutate(city = city)
    return(df)
  }
  
  # calculate distance based on two 
  # set of longitude and latitude points
  earth.dist <- function(long1, lat1, long2, lat2)
  {
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
  }
  
  seattle_lat_long <- c(47.608013, -122.335167)
  
  # combine all the dataframes into a a single dataframe
  realstate_df <- rbind(
    add_city_df("seattle"),
    add_city_df("bellevue"),
    add_city_df("bothell"),
    add_city_df("tacoma"),
    add_city_df("renton"),
    add_city_df("kent"),
    add_city_df("kirkland"),
    add_city_df("monroe"),
    add_city_df("everett"),
    add_city_df("auburn"),
    add_city_df("lynnwood")
  )

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
})
