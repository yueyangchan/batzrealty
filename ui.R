
# install.packages("shiny")

library("shiny")

ui <- navbarPage(
  title = "Zillow",
  
  tabPanel(
    title = "1",
    titlePanel("1")
  ),
  
  tabPanel(
    title = "2",
    titlePanel("2")
  ),

  tabPanel(
    title = "3",
    titlePanel("3")
  ),
  
  tabPanel(
    title = "4",
    titlePanel("ars3697")
  )
)

shinyUI(ui)
