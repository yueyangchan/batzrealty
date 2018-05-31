
# install.packages("shiny")
# install.packages("XML")
# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("stringr")
# install.packages("plotly")
# install.packages("forecast")
# install.packages("FNN")
# install.packages("caret")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("randomForest")
# install.packages("stringr")
# install.packages("httr")
# install.packages("xml2")
# install.packages("e1071")

library("shiny")
library("XML")
library("jsonlite")
library("dplyr")
library("ggplot2")
library("stringr")
library("plotly")
library("maps")

# create shiny server function with regular input and output
# parameters along with a session for changing the input slider
# bar based on filtered smallest and largest values
server <- function(input, output) {
  library("shiny")
  library("XML")
  library("jsonlite")
  library("dplyr")
  library("ggplot2")
  library("stringr")
  library("plotly")
  library("maps")
  
  ####################
  # Zechariah Cheung #
  ####################
  
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
  hover <- reactiveValues()
  hover$loc <- ""
  
  # observe when plot is clicked on to set city 
  # that was clicked on to a new color
  observeEvent(input$plot_click, {
    selected <- nearPoints(realstate_reactive(), input$plot_click)
    clicked_city$city <- unique(selected$city)
  })
  
  # observe and record when a point is being hovered over
  observeEvent(input$hover, {
    selected <- nearPoints(realstate_reactive(), input$hover)
    hover$loc <- unique(selected$name)
  })
  
  # output the city that was clicked on
  output$selected_city <- renderText({
    city <- clicked_city$city
    capitalized_city<- paste0(toupper(substring(city, 1,1)), substring(city, 2))
    return(capitalized_city)
  })
  
  # output the neighborhood that is being hovered
  output$hover_name <- renderText({
    return(hover$loc)
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
      geom_smooth(mapping = aes(x = dist, y = price, alpha = 1), show.legend = FALSE) +
      # create point plot with points in the same city
      # as the clicked point colored a contrasting color
      geom_point(aes(color = (city %in% clicked_city$city)), size = 4) +
      guides(color = FALSE) +
      labs(
        title = "Price in USD for Neighborhoods vs Distance from Downtown in miles",
        x = "Distance",
        y = "Price"
      ) 
    return(p)
  })
  
  #####################
  # Addison Sengvilay #
  #####################
  
  # File as of 5-29-18.
  all_state <- read.csv("data/State_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)
  all_city <- read.csv("data/City_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)
  
  state_map_data <- map_data("state")
  
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
  
  ####################
  ##### Ted Chan #####
  ####################
  source("factor_analysis.R")
  source("monthly_count.R")
  source("monthly_price.R")
  
  # Intro for Best Month to Sell House
  output$month_intro <- renderText({
    "This tab examines if there is a best time of the year to sell a house. 
    The insights generated can be useful for homesellers to identify the 
    best month of the year to sell their property."
  })
  
  # Renders Seattle monthly sales count
  output$count_plot <- renderPlotly({
    sales_count <- plot_ly(city_avg_df, x = ~Month, y = ~Avg_Value,
                           type = 'bar') %>%
      layout(title = "Seattle Homes Sales Count by Month (2009 - 2017)",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Count"))
    sales_count
  })
  
  # Summary for the count plot
  output$count_summary <- renderText({
    paste0("The above bar chart summarises the average number of 
           home sales per month during 2009-2017 in Seattle. On 
           average, the first two months of the year have the least 
           home sales while the middle of the year (May-July) have 
           the most sales. It can be concluded that demand for 
           homes increase during the summer times. Sellers should 
           capitalize on that demand and try to sell their homes 
           during the months of May and July.")
  })
  
  # Renders Seattle monthly median sales price
  output$price_plot <- renderPlotly({
    sales_price <- plot_ly(city_avg_price_df, x = ~Month, y = ~Avg_Value,
                           type = 'bar') %>%
      layout(title = "Seattle Homes Median Sales Price by Month (2009 - 2017)",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Median Sales Price (thousand $)"))
    sales_price
  })
  
  output$price_summary <- renderText({
    paste0("We further examined if the time of the year of sales have 
           an impact on the sales price. The above bar chart shows that 
           the median sales price have very negligible differences between 
           the different months. The median sales price trends slightly 
           upwards in the last few months of the year but the difference 
           is very small. Therefore, we still recommend sellers to try 
           to sell their houses in mid-year (May-July) since that is 
           when the demand for houses is the strongest.")
  })
  
  output$factor_intro <- renderText({
    paste0("This tab examines the different factors of a home that 
           contribute to its value. We built a number of models 
           to identify the statistically significant factors 
           and attempt to predict home values. Home buyers can use  
           the insights from this section to identify good value
           homes as well as the type of homes that would most likely  
           appreciate in value. Home sellers can identify which factors of 
           their properties to improve on in order to increase 
           the potential sales price.")
  })
  
  output$variables <- renderText({
    paste(colnames(model_df), collapse = ", ")
  })
  
  output$coefficients <- renderText({
    paste0("Our regression model shows that a property's previous sales price, 
           the year it was built in, the number of bedrooms, the size of the 
           finished property in sq.ft and the size of the lot in sq.ft. are 
           all statistically significant factors in predicting the value of 
           a home.")
  })
  
  output$coefficient_screenshot <- renderImage({
    return(list(
      src = "./img/coefficient_pic.png", 
      contentType = "image/png",
      width = 600,
      height = 390
    ))
  }, deleteFile = FALSE)
  
  output$regression_insights <- renderText({
    paste0("Homes that are most likely to retain its value and appreciate 
           in the future are those that 1) have previously been sold at 
           a high price 2) were built relatively recent 3) have a high 
           number of bedrooms and 4) have a large size in both the lot 
           and the finished property. Buyers should also avoid buying 
           a townhouse as it has a negative impact on home valuation. 
           Sellers should be noted that the number of bathrooms have 
           a negative effect on valuation. It is much more rewarding 
           to focus on increasing the number of bedrooms.")
  })
  
  output$classification_intro <- renderText({
    "A random forest model is a supervised machine learning algorithm 
    that generates multiple decision trees, then combine them all to 
    make a more accurate and stable classification model. We divided home 
    value into 4 different levels based on the quantiles 
    of home value. The first class is any values below the 1st quartile, 
    the second class is any values between the 1st quartile and the median, 
    the third class is any values between the median and the 3rd quartile, 
    and the last class is any values above the 3rd quartile."
  })
  
  output$forest_confusion <- renderTable(rownames = TRUE, {
    forest_confusion_matrix <- model.rf$confusion
    forest_confusion_matrix
  })
  
  output$forest_result <- renderText({
    paste0("The random forest model had an accuracy of ", 
           forest_accuracy, "%. It is best at predicting the classification 
           of the top value class (> 75%). The error rate of the model can 
           potentially be a result of an incomplete model since the value 
           of a home should derive from more than just the variables we 
           included in the model.")
  })
  
  output$importance <- renderPlot({
    varImpPlot(model.rf)
  })
  
  output$importance_explain <- renderText({
    "The mean decrease accuracy plot shows the loss in prediction performance 
    when that particular factor is omitted from the dataset while the mean 
    decrease gini plot shows the decrease in GINI when the GINI factor is omitted. 
    GINI is a measure of node impurity. The mean decrease gini plot shows how 
    important a factor is to split the data correctly. The plots reaffirms that 
    the historical sales price of the home is the most important factor in 
    determining a home value."
  })
  
  ####################
  # Brianne Ichiyama #
  ####################
  
  # read in csv data
  studio_data <- read.csv("data/City_MedianRentalPrice_Studio.csv", 
                          stringsAsFactors = FALSE)
  one_data <- read.csv("data/City_MedianRentalPrice_1Bedroom.csv", 
                       stringsAsFactors = FALSE)
  two_data <- read.csv("data/City_MedianRentalPrice_2Bedroom.csv", 
                       stringsAsFactors = FALSE)
  three_data <- read.csv("data/City_MedianRentalPrice_3Bedroom.csv", 
                         stringsAsFactors = FALSE)
  four_data <- read.csv("data/City_MedianRentalPrice_4Bedroom.csv", 
                        stringsAsFactors = FALSE)
  
  # filter for recent date
  alter_data <- function(df) {
    df <- df %>% 
      select(RegionName, contains("18")) %>% 
      arrange(desc(X2018.04)) %>% 
      distinct() %>%
      head(5)
  }
  
  studio_data <- alter_data(studio_data)
  one_data <- alter_data(one_data)
  two_data <- alter_data(two_data)
  three_data <- alter_data(three_data)
  four_data <- alter_data(four_data)
  
  # introduction
  output$intro <- renderText({
    paste0("This tab examines whether the cities with the highest priced 
           living spaces are consistent across the living spaces that vary
           in bedroom availability. By analyzing the results, driving 
           factors can be attributed to the findings. The data being used is 
           the most recent record of median rent listings, which is April 
           of 2018.")
  })
  
  # studio bar graph and summary
  output$studio <- renderPlotly({
    p <- plot_ly(data = studio_data, x = ~RegionName, y = ~X2018.04,
                 type = "bar") %>% 
      layout(title = "5 Cities with Highest Rent for Studios",
             xaxis = list(title = "City"),
             yaxis = list(title = "Rent Amount"))
    p
  })
  
  output$highest_city_studio <- renderText({
    city <- studio_data[1, 1]
    city
  })
  
  output$studio_sum <- renderText({
    paste0("The bar graph above displays the 5 cities with the highest rent
           for studio living spaces. The results showed that Hicksville, 
           Montauk, Naples, Potomac, and Princeton have the highest rent. 
           ")
  })
  
  # one-bedroom bar graph and summary
  output$onebed <- renderPlotly({
    p <- plot_ly(data = one_data, x = ~RegionName, y = ~X2018.04,
                 type = "bar") %>% 
      layout(title = "5 Cities with Highest Rent for 1-Bedrooms",
             xaxis = list(title = "City"),
             yaxis = list(title = "Rent Amount"))
    p
  })
  
  output$highest_city_one <- renderText({
    city <- one_data[1, 1]
    city
  })
  
  output$one_sum <- renderText({
    paste0("The bar graph above displays the 5 cities with the highest rent
           for 1-bedroom living spaces. The results showed that Foster City,
           Marina del Rey, Menlo Park, Redwood City, and San Francisco have   
           the highest rent. The city with the highest rent is San Francisco
           with 3K")
  })
  
  # two-bedroom bar graph and summary
  output$twobed <- renderPlotly({
    p <- plot_ly(data = two_data, x = ~RegionName, y = ~X2018.04,
                 type = "bar") %>% 
      layout(title = "5 Cities with Highest Rent for 2-Bedrooms",
             xaxis = list(title = "City"),
             yaxis = list(title = "Rent Amount"))
    p
  })
  
  output$highest_city_two <- renderText({
    city <- two_data[1, 1]
    city
  })
  
  output$two_sum <- renderText({
    paste0("The bar graph above displays the 5 cities with the highest rent
           for 3-bedroom living spaces. The results showed that Albany,
           Elizabethtown, Jefferson City, and Killeen have the highest rent. 
           Here we see the first repetition of Killeen which was seen in the
           previous bar graph.")
  })
  
  # three-bedroom bar graph and summary
  output$threebed <- renderPlotly({
    p <- plot_ly(data = three_data, x = ~RegionName, y = ~X2018.04,
                 type = "bar") %>% 
      layout(title = "5 Cities with Highest Rent for 3-Bedrooms",
             xaxis = list(title = "City"),
             yaxis = list(title = "Rent Amount"))
    p
  })
  
  output$highest_city_three <- renderText({
    city <- three_data[1, 1]
    city
  })
  
  output$three_sum <- renderText({
    paste0("The bar graph above displays the 5 cities with the highest rent
           for 3-bedroom living spaces. The results showed that Akron, 
           Augusta, Canton, Fort Wayne, and Pocatello have the highest rent. 
           There is significantly a lower price ($568.50) in Augusta, which also
           happens to be the lowest over within the 5 graphs, making Augusta
           an outlier.")
  })
  
  # four-bed bar graph and summary
  output$fourbed <- renderPlotly({
    p <- plot_ly(data = four_data, x = ~RegionName, y = ~X2018.04,
                 type = "bar") %>% 
      layout(title = "5 Cities with Highest Rent for 4-Bedrooms",
             xaxis = list(title = "City"),
             yaxis = list(title = "Rent Amount"))
    p
  })
  
  output$highest_city_four <- renderText({
    city <- four_data[1, 1]
    city
  })
  
  output$four_sum <- renderText({
    paste0("The bar graph above displays the 5 cities with the highest rent
           for 4-bedroom living spaces. The results showed that Buffalo, 
           Cleveland, Shreveport, Toledo, and Wichita have the highest rent.
           Wichita is an outlier with the highest rent amount ($1045), which 
           happens to be the highest amount of rent overall between the 5 
           graphs.")
  })
  
  # concluson
  output$conclusion <- renderText({
    paste0("After analyzing the results, the cities with the highest rent
           values are all big cities with a high volume population. These
           cities are typically heavily traveled to and visited. A few of
           the most common cities between the different graphs were in 
           popular states like California and New York, popular travel
           destinations. Many of these cities are large in size, which means 
           a heavy population and influx on rent prices. This attractions of
           these locations can cause upcharges on rent. There were a few
           cities that were consitent in being one of having the highest
           rent amounts. This was consistent with what we predicted. Knowledge 
           of these factors can be utilized when searching for a place to rent.")
  })
}

shinyServer(server)
