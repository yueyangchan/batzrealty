
# install.packages("shiny")
# install.packages("dplyr")
# install.packages("plotly")

library("shiny")
library("dplyr")
library("plotly")

all_city <- read.csv("data/City_Zhvi_AllHomes.csv", stringsAsFactors = FALSE)
state_names <- all_city %>% select(State) %>% unique() %>% arrange(State)
state_names <- state_names[,1]

years <- c(1996:2018)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

ui <- navbarPage(
  title = "Housing Data",
  
  ####################
  # Zechariah Cheung #
  ####################
  tabPanel(
    title = "Distance from Seattle",
    mainPanel(
      # create tabset panel to display table and plot neatly
      tabsetPanel(
        type = "tabs",
        tabPanel(
          p(
            "The correlation between distance from the neighborhood
            to downtown Seattle and the the average price of the 
            neighborhood is ", textOutput("correlation", inline = TRUE),
            ". Although there isn't strong correlation between distance
            between the center of seattle and the price of the location,
            distance the center of seattle may play a role in the average
            price of the house along with many other factors."),
          "Plot",
          # create interactive click plot
          plotOutput("plot", click = "plot_click", hover = "hover"),
          # display city that is currently selected
          p(
            "Clicked City:",
            strong(textOutput("selected_city", inline = TRUE))
          ),
          # display hovered neighborhood
          p(
            "Hovered Neighborhood:",
            strong(textOutput("hover_name", inline = TRUE))
          )
          )
        )
      )
  ),
  
  #####################
  # Addison Sengvilay #
  #####################
  tabPanel(
    title = "Rate of Growth",
    sidebarPanel(
      p("EARLIEST DATE AVAILABLE: ", strong("1996-04")),
      p("MOST RECENT DATE AVAILABLE: ", strong("2018-04")),
      selectInput("start_year",
                  label = "Starting Year",
                  selected = 1994,
                  choices = years),
      selectInput("start_month",
                  label = "Starting Month",
                  selected = "04",
                  choices = months),
      selectInput("end_year",
                  label = "Ending Year",
                  selected = 2018,
                  choices = years),
      selectInput("end_month",
                  label = "Ending Month",
                  selected = "04",
                  choices = months),
      selectInput("states",
                  label = "States (for Cities tab)",
                  selected = "WA",
                  choices = state_names),
      actionButton("submit", 
                   label = "Submit")
    ),
    
    mainPanel(
      navbarPage(
        title = "Rate of Growth",
        
        tabPanel(
          title = "States",
          titlePanel("Zillow Home Value Index Percentage Change by State"),
          p("Zillow offers a zIndex value, which is the Zillow Home Value Index (ZHVI). 
          It is a measure of the median estimated home value across a given region and 
          housing type. The plot and table below details the percentage change of the 
          zIndex value across an entire state between a given timeframe. This data can 
          help answer the question of which state/city is growing the fastest/slowest 
          throughout time. This data can be useful, because increasing housing prices can be 
          interpreted as a fast growing state/city. This information can help new/young professionals 
          identify which city they can potentially move to, or avoid, when they first start their career."),
          plotOutput("state_rate_plot", width = "100%", height = "600px"),
          p("In the plot above, the darker a state's color, the less percent change they have. The 
          lighter a state's color, the more percent change they have. This plot can be used to see 
          the overall trend in percent change throughout a given date range."),
          p("In the table below, there are four columns of importance. The State column gives the state 
          in question. The second column produces the zIndex under the starting date. The third column 
          produces the zIndex under the ending date. The Percent Change column gives the percent change 
          between the starting and ending date. This table provides an at-a-glance overview of all states 
          and their overall growth in price."),
          tableOutput("state_rate_table")
        ),
        
        tabPanel(
          title = "Cities",
          titlePanel("Zillow Home Value Index Percentage Change by City"),
          p("Zillow offers a zIndex value, which is the Zillow Home Value Index (ZHVI). 
          It is a measure of the median estimated home value across a given region and 
          housing type. The plot and table below details the percentage change of the 
          zIndex value across an entire state between a given timeframe. This data can 
          help answer the question of which state/city is growing the fastest/slowest 
          throughout time. This data can be useful, because increasing housing prices can be 
          interpreted as a fast growing state/city. This information can help new/young professionals 
          identify which city they can potentially move to, or avoid, when they first start their career."),
          p("In the table below, there are six available columns of information about a specific state, and its cities.
          The City column gives the name of a city within a given state. The State column gives the state
          that is selected by the user. The Metro columns gives the name of the metro area in the city. The
          County column gives the county that a given city is in. The fifth column gives information about the
          zIndex of the starting date. The sixth columns gives information about the zIndex of the ending date."),
          dataTableOutput("city_rate_table")
        )
      )
    )
  ),
  
  ####################
  ##### Ted Chan #####
  ####################
  tabPanel(
    title = "Best Month to Sell Home",
    mainPanel(
      # create tabset panel to display table and plot neatly
      tabsetPanel(
        type = "tabs",
        tabPanel(
          textOutput("month_intro"),
          br(),
          plotlyOutput('count_plot'),
          textOutput('count_summary'),
          br(),
          plotlyOutput('price_plot'),
          textOutput('price_summary')
          )
        )
      )
  ),
  
  tabPanel(
    title = "Statistical Analysis",
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          textOutput("factor_intro"),
          br(),
          h3("Multiple Linear Regression"),
          strong("We used the following variables 
                 for our model"), 
          textOutput("variables"), 
          em("* Zestimate is Zillow's own valuation 
             of the property's current value"),
          br(),
          br(),
          strong("Coefficient Results:"),
          imageOutput("coefficient_screenshot"),
          textOutput("coefficients"),
          br(),
          strong("Insights:"),
          textOutput("regression_insights"),
          br(),
          h3("Classification Model (Random Forest)"),
          textOutput("classification_intro"),
          br(),
          strong("Results"),
          tableOutput("forest_confusion"),
          br(),
          textOutput("forest_result"),
          br(),
          strong("Factor Importance:"),
          plotOutput("importance"),
          textOutput("importance_explain")
          )
        )
    )
  ),
  
  ###################
  #Brianne Ichiyama #
  ###################
  tabPanel(
    title = "Rental Listings",
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          h3("Median Rental Listings"),
          br(),
          textOutput("intro"),
          br(),
          strong("Studio"),
          plotlyOutput("studio"),
          br(),
          p("The bar graph above displays the 5 cities with the highest rent
           for studio living spaces. The results showed that ", 
            textOutput("highest_city_studio", inline = TRUE), "is the city 
            with the highest rent."),
          br(),
          strong("1-Bedroom"),
          plotlyOutput("onebed"),
          br(),
          p("The bar graph above displays the 5 cities with the highest rent
           for one-bedroom living spaces. The results showed that ", 
            textOutput("highest_city_one", inline = TRUE), "is the city 
            with the highest rent."),
          br(),
          strong("2-Bedroom"),
          plotlyOutput("twobed"),
          br(),
          p("The bar graph above displays the 5 cities with the highest rent
           for two-bedroom living spaces. The results showed that ", 
            textOutput("highest_city_two", inline = TRUE), "is the city 
            with the highest rent."),
          br(),
          strong("3-Bedroom"),
          plotlyOutput("threebed"),
          br(),
          p("The bar graph above displays the 5 cities with the highest rent
           for three-bedroom living spaces. The results showed that ", 
            textOutput("highest_city_three", inline = TRUE), "is the city 
            with the highest rent."),
          br(),
          strong("4-Bedroom"),
          plotlyOutput("fourbed"),
          br(),
          p("The bar graph above displays the 5 cities with the highest rent
           for four-bedroom living spaces. The results showed that ", 
            textOutput("highest_city_four", inline = TRUE), "is the city 
            with the highest rent."),
          br(),
          strong("Conclusion"),
          textOutput("conclusion")
        )
      )
    )
  )
)

shinyUI(ui)
