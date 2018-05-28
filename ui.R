library("shiny")
shinyUI(
  fluidPage(
    titlePanel("Using Zillow API"),
    sidebarLayout(
      # create widgets
      sidebarPanel(
    
      ),
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
            plotOutput("plot", click = "plot_click"),
            # display city that is currently selected
            p(
              "Selected City:",
              strong(textOutput("selected_city", inline = TRUE))
            )
          )
          )
        )
      )
    )
)