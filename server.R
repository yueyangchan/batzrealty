# create shiny server function with regular input and output
# parameters along with a session for changing the input slider
# bar based on filtered smallest and largest values
shinyServer(function(input, output, session) {
  library("ggplot2")
  library("dplyr")
  library("XML")
  library("jsonlite")
  
  # paramters for GET request
  base_uri <- "http://www.zillow.com/webservice/GetRegionChildren.htm"
  z_id <- "X1-ZWz1geilg38iyz_9r65p"
  state <- "wa"
  childtype <- "neighborhood"
  
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
  
  # calculate the distance from the center 
  # of seattle for each row in the data table
  for(i in 1:NROW(realstate_df)){
    realstate_df$dist[i] <- earth.dist(
      as.double(realstate_df$long[i]),
      as.double(realstate_df$lat[i]),
      seattle_lat_long[2],
      seattle_lat_long[1]
      )
  }
  realstate_df$price <- as.numeric(realstate_df$price)
  
  realstate_reactive <- reactive({
    return(realstate_df)
  })
  
  # create variable that stores the value that is being clicked on
  clicked_city <- reactiveValues()
  clicked_city$city <- ""
  
  # observe when plot is clicked on to set city 
  # that was clicked on to a new color
  observeEvent(input$plot_click, {
    selected <- nearPoints(realstate_reactive(), input$plot_click)
    clicked_city$city <- unique(selected$city)
  })
  
  # output the city that was clicked on
  output$selected_city <- renderText({
    return(clicked_city$city)
  })
  
  # output the intext correlation text
  output$correlation <- renderText({
    cor(realstate_df$dist, realstate_df$price)
  })
  
  # create interactive/reactive plot of eviction filings versus race
  output$plot <- renderPlot({
    p <- ggplot(
      data = realstate_reactive(),
      mapping = aes(x = dist, y = price)
      ) +
      geom_smooth(mapping = aes(x = dist, y = price, alpha = 1)) +
      # create point plot with points in the same city
      # as the clicked point colored a contrasting color
      geom_point(aes(color = (city %in% clicked_city$city)), size = 3) +
      guides(color = FALSE) +
      labs(
        title = "Price in USD vs Distance from Downtown in miles",
        x = "Distance",
        y = "Price"
      ) 
    return(p)
  })
})
