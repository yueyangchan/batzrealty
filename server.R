
# install.packages("shiny")
# install.packages("xml2")
# install.packages("jsonlite")
# install.packages("dplyr")

library("shiny")
library("xml2")
library("jsonlite")
library("dplyr")

base_uri <- "http://www.zillow.com/webservice/GetRegionChildren.htm"
z_id_ars3697 <- "X1-ZWz18lzviceeiz_9skq6"

# state_df (function) - Returns a dataframe of all of a state's cities/counties and their "zindex" in USD.
# state (string) - State in the United States. Should be writen in two letters (ex. "wa" for Washington, "ca" for California).
state_df <- function(state) {
  uri <- paste0(base_uri, "?zws-id=", z_id_ars3697, "&state=", state)
  # Zillow returns the response back in XML.
  response <- xmlParse(uri)	
  # Cleaning up XML and turning it into a cleaner looking dataframe.
  list <- response %>% xmlToList() %>% toJSON(pretty = TRUE) %>% fromJSON()	
  # Getting rid of the first index, because it's just a "count" row. It's irrelevant.
  raw_data <- list$response$list[-1]
  # Creating a new dataframe with two columns: "name" and "zindex_usd".
  # Only has one row, the first row from the raw_data dataframe.
  state_data <- data_frame(name = raw_data[1]$region$name,
                           zindex_usd = raw_data[1]$region$zindex$text)
  
  # Adds in the rest of the rows into the state_data dataframe.
  # tryCatch is used to prevent error messages in the console.
  for(i in 2:NROW(raw_data)) {
    tryCatch({
      temp_df <- data.frame(name = raw_data[i]$region$name,
                          zindex_usd = raw_data[i]$region$zindex$text)
      state_data <- rbind(state_data, temp_df)}, error = function(e){})
  }
  
  state_data
}

View(state_df("ca"))

server <- function(input, output) {
  
}

shinyServer(server)


