library(httr)
library(jsonlite)
library(dplyr)
library(xml2)
library(XML)

# API Key
source("apikey.R")
zws_key <- api_key

comp_df <- data.frame(matrix(ncol = 17, nrow = 0))
colnames(comp_df) <- c("ZPID", "Zestimate", "Year Built", "Last Sold Date", 
                       "Last Sold Price", "Floors", "Basement", "Bathrooms", 
                       "Bedrooms", "Finished SQFT", "Lot Size SQFT", 
                       "Home Type", "Roof", "View", "Parking",
                       "Heating Sources", "Heating System")

# uri for principal deep search
address <- "3057+61st+Ave+SE&citystatezip=Mercer+Island%2C+WA"
resource_uri <- "http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&address=", address)

# Parse data into body
response <- read_xml(uri)
body <- as_list(response)

# Address for principal
principal_street <- body$searchresults$response$results$result$address$street[[1]]
principal_city <- body$searchresults$response$results$result$address$city[[1]]
principal_state <- body$searchresults$response$results$result$address$state[[1]]
principal_address <- paste0(street, " ", city, ", ", state)

# Data for principal
principal_zpid <- body$searchresults$response$results$result$zpid[[1]]
principal_zestimate <- body$searchresults$response$results$result$zestimate$amount[[1]]
principal_last_sold_date <- body$searchresults$response$results$result$lastSoldDate[[1]]
principal_last_sold_price <- body$searchresults$response$results$result$lastSoldPrice[[1]]
principal_year_built <- body$searchresults$response$results$result$yearBuilt[[1]]
principal_finished_sqft <- body$searchresults$response$results$result$finishedSqFt[[1]]
principal_lot_size <- body$searchresults$response$results$result$lotSizeSqFt[[1]]
principal_bathrooms <- body$searchresults$response$results$result$bathrooms[[1]]
principal_bedrooms <- body$searchresults$response$results$result$bedrooms[[1]]

#########################################################################################

# Get additional information on property with ZPID of 48749425
resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&zpid=", "48749425")

response <- xmlParse(uri)
response <- xmlToList(response)
response <- toJSON(response, pretty = TRUE)
response <- fromJSON(response)

# Storing information extracted from API into variables
principal_floors <- response$response$editedFacts$numFloors[[1]]
principal_basement <- response$response$editedFacts$basement[[1]]
principal_home_type <- response$response$editedFacts$useCode[[1]]
principal_roof <- response$response$editedFacts$roof[[1]]
principal_view <- response$response$editedFacts$view[[1]]
principal_parking <- response$response$editedFacts$parkingType[[1]]
principal_heating_sources <- response$response$editedFacts$heatingSources[[1]]
principal_heating_system <- response$response$editedFacts$heatingSystem[[1]]

# Storing all information into a comparable data frame
comp_df[1, "ZPID"] <- principal_zpid
comp_df[1, "Zestimate"] <- principal_zestimate
comp_df[1, "Last Sold Date"] <- principal_last_sold_date
comp_df[1, "Last Sold Price"] <- principal_last_sold_price
comp_df[1, "Year Built"] <- principal_year_built
comp_df[1, "Floors"] <- principal_floors
comp_df[1, "Basement"] <- principal_basement
comp_df[1, "Bathrooms"] <- principal_bathrooms
comp_df[1, "Finished SQFT"] <- principal_finished_sqft
comp_df[1, "Lot Size SQFT"] <- principal_lot_size
comp_df[1, "Home Type"] <- principal_home_type
comp_df[1, "Roof"] <- principal_roof
comp_df[1, "View"] <- principal_view
comp_df[1, "Parking"] <- principal_parking
comp_df[1, "Heating Sources"] <- principal_heating_sources
comp_df[1, "Heating System"] <- principal_heating_system


#########################################################################################

# Generate List of Comparable Results
# uri for comparables deep search
comp_count = 25
resource_uri <- "http://www.zillow.com/webservice/GetDeepComps.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&zpid=", "48749425", "&count=", comp_count)

response <- xmlParse(uri)
response <- xmlToList(response)
response <- toJSON(response, pretty = TRUE)
response <- fromJSON(response)

# Identify actual number of comparables
num_comp <- length(response$response$properties$comparables)

# Populate the comparable dataframe with all the comparables from the uri
for (i in 1:num_comp) {
  comp <- response[[3]][[1]][[2]][[i]]
  zpid <- comp$zpid[[1]]
  comp_df[i + 1, "ZPID"] <- zpid
  zestimate <- comp$zestimate$amount[[1]][[1]]
  comp_df[i + 1, "Zestimate"] <- zestimate
  sold_date <- comp$lastSoldDate[[1]]
  comp_df[i + 1, "Last Sold Date"] <- sold_date
  sold_price <- comp$lastSoldPrice$text[[1]]
  comp_df[i + 1, "Last Sold Price"] <- sold_price
  year_built <- comp$yearBuilt[[1]]
  comp_df[i + 1, "Year Built"] <- year_built
  finished_sqft <- comp$finishedSqFt[[1]]
  comp_df[i + 1, "Finished SQFT"] <- finished_sqft
  lot_size <- comp$lotSizeSqFt[[1]]
  comp_df[i + 1, "Lot Size SQFT"] <- lot_size
  bathrooms <- comp$bathrooms[[1]]
  comp_df[i + 1, "Bathrooms"] <- bathrooms
  bedrooms <- comp$bedrooms[[1]]
  comp_df[i + 1, "Bedrooms"] <- bedrooms
}

# Get additional information on the comparables
resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="

# Fill in the additional information for the comparables
for (i in 1:num_comp) {
  uri <- paste0(resource_uri, zws_key, "&zpid=", comp_df[i, "ZPID"])
  response <- xmlParse(uri)
  response <- xmlToList(response)
  response <- toJSON(response, pretty = TRUE)
  response <- fromJSON(response)
  
  floors <- response$response$editedFacts$numFloors[[1]]
  if (is.null(floors) == FALSE) {
    comp_df[i + 1, "Floors"] <- floors
  }
  basement <- response$response$editedFacts$basement[[1]]
  if (is.null(basement) == FALSE) {
    comp_df[i + 1, "Basement"] <- basement
  }
  home_type <- response$response$editedFacts$useCode[[1]]
  if (is.null(home_type) == FALSE) {
    comp_df[i + 1, "Home Type"] <- home_type
  }
  roof <- response$response$editedFacts$roof[[1]]
  if (is.null(roof) == FALSE) {
    comp_df[i + 1, "Roof"] <- roof
  }
  view <- response$response$editedFacts$view[[1]]
  if (is.null(view) == FALSE) {
    comp_df[i + 1, "View"] <- view
  }
  parking <- response$response$editedFacts$parkingType[[1]]
  if (is.null(parking) == FALSE) {
    comp_df[i + 1, "Parking"] <- parking
  }
  heating_sources <- response$response$editedFacts$heatingSources[[1]]
  if (is.null(heating_sources) == FALSE) {
    comp_df[i + 1, "Heating Sources"] <- heating_sources
  }
  heating_system <- response$response$editedFacts$heatingSystem[[1]]
  if (is.null(heating_system) == FALSE) {
    comp_df[i + 1, "Heating System"] <- heating_system
  }
}

#########################################################################################

# Get more observations
# info for ZPID 48775056 (Yale Ave)
address <- "2350+Yale+Ave+E&citystatezip=Seattle%2C+WA"
resource_uri <- "http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&address=", address)

# Parse data into body
response <- read_xml(uri)
body <- as_list(response)

# Data for principal
principal_zpid <- body$searchresults$response$results$result$zpid[[1]]
principal_zestimate <- body$searchresults$response$results$result$zestimate$amount[[1]]
principal_last_sold_date <- body$searchresults$response$results$result$lastSoldDate[[1]]
principal_last_sold_price <- body$searchresults$response$results$result$lastSoldPrice[[1]]
principal_year_built <- body$searchresults$response$results$result$yearBuilt[[1]]
principal_finished_sqft <- body$searchresults$response$results$result$finishedSqFt[[1]]
principal_lot_size <- body$searchresults$response$results$result$lotSizeSqFt[[1]]
principal_bathrooms <- body$searchresults$response$results$result$bathrooms[[1]]
principal_bedrooms <- body$searchresults$response$results$result$bedrooms[[1]]

resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&zpid=", "48775056")

response <- xmlParse(uri)
response <- xmlToList(response)
response <- toJSON(response, pretty = TRUE)
response <- fromJSON(response)

principal_floors <- response$response$editedFacts$numFloors[[1]]
principal_basement <- response$response$editedFacts$basement[[1]]
principal_home_type <- response$response$editedFacts$useCode[[1]]
principal_roof <- response$response$editedFacts$roof[[1]]
principal_view <- response$response$editedFacts$view[[1]]
principal_parking <- response$response$editedFacts$parkingType[[1]]
principal_heating_sources <- response$response$editedFacts$heatingSources[[1]]
principal_heating_system <- response$response$editedFacts$heatingSystem[[1]]

observation_count <- nrow(comp_df)

# Store this new property information into the comparable data frame
comp_df[observation_count + 1, "ZPID"] <- principal_zpid
comp_df[observation_count + 1, "Zestimate"] <- principal_zestimate
comp_df[observation_count + 1, "Last Sold Date"] <- principal_last_sold_date
comp_df[observation_count + 1, "Last Sold Price"] <- principal_last_sold_price
comp_df[observation_count + 1, "Year Built"] <- principal_year_built
comp_df[observation_count + 1, "Floors"] <- principal_floors
comp_df[observation_count + 1, "Basement"] <- principal_basement
comp_df[observation_count + 1, "Bathrooms"] <- principal_bathrooms
comp_df[observation_count + 1, "Finished SQFT"] <- principal_finished_sqft
comp_df[observation_count + 1, "Lot Size SQFT"] <- principal_lot_size
comp_df[observation_count + 1, "Home Type"] <- principal_home_type
comp_df[observation_count + 1, "Roof"] <- principal_roof
comp_df[observation_count + 1, "View"] <- principal_view
comp_df[observation_count + 1, "Parking"] <- principal_parking
comp_df[observation_count + 1, "Heating Sources"] <- principal_heating_sources
comp_df[observation_count + 1, "Heating System"] <- principal_heating_system

# Get comparables for the above property
comp_count = 25
resource_uri <- "http://www.zillow.com/webservice/GetDeepComps.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&zpid=", principal_zpid, "&count=", comp_count)

response <- xmlParse(uri)
response <- xmlToList(response)
response <- toJSON(response, pretty = TRUE)
response <- fromJSON(response)

num_comp <- length(response$response$properties$comparables)
observation_count <- nrow(comp_df)

# Populate the comp data frame with new observations
for (i in 1:num_comp) {
  comp <- response[[3]][[1]][[2]][[i]]
  zpid <- comp$zpid[[1]]
  if (is.null(zpid) == FALSE) {
    comp_df[(i + observation_count), "ZPID"] <- zpid
  }
  zestimate <- comp$zestimate$amount[[1]][[1]]
  if (is.null(zestimate) == FALSE) {
    comp_df[(i + observation_count), "Zestimate"] <- zestimate
  }
  sold_date <- comp$lastSoldDate[[1]]
  if (is.null(sold_date) == FALSE) {
    comp_df[(i + observation_count), "Last Sold Date"] <- sold_date
  }
  sold_price <- comp$lastSoldPrice$text[[1]]
  if (is.null(sold_price) == FALSE) {
    comp_df[(i + observation_count), "Last Sold Price"] <- sold_price
  }
  year_built <- comp$yearBuilt[[1]]
  if (is.null(year_built) == FALSE) {
    comp_df[(i + observation_count), "Year Built"] <- year_built
  }
  finished_sqft <- comp$finishedSqFt[[1]]
  if (is.null(finished_sqft) == FALSE) {
    comp_df[(i + observation_count), "Finished SQFT"] <- finished_sqft
  }
  lot_size <- comp$lotSizeSqFt[[1]]
  if (is.null(lot_size) == FALSE) {
    comp_df[(i + observation_count), "Lot Size SQFT"] <- lot_size
  }
  bathrooms <- comp$bathrooms[[1]]
  if (is.null(bathrooms) == FALSE) {
    comp_df[(i + observation_count), "Bathrooms"] <- bathrooms
  }
  bedrooms <- comp$bedrooms[[1]]
  if (is.null(bedrooms) == FALSE) {
    comp_df[(i + observation_count), "Bedrooms"] <- bedrooms
  }
}

# Get additional information
resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="

# Fill in the additional information
for (i in 1:num_comp) {
  uri <- paste0(resource_uri, zws_key, "&zpid=", comp_df[i, "ZPID"])
  response <- xmlParse(uri)
  response <- xmlToList(response)
  response <- toJSON(response, pretty = TRUE)
  response <- fromJSON(response)
  
  floors <- response$response$editedFacts$numFloors[[1]]
  if (is.null(floors) == FALSE) {
    comp_df[i + observation_count, "Floors"] <- floors
  }
  basement <- response$response$editedFacts$basement[[1]]
  if (is.null(basement) == FALSE) {
    comp_df[i + observation_count, "Basement"] <- basement
  }
  home_type <- response$response$editedFacts$useCode[[1]]
  if (is.null(home_type) == FALSE) {
    comp_df[i + observation_count, "Home Type"] <- home_type
  }
  roof <- response$response$editedFacts$roof[[1]]
  if (is.null(roof) == FALSE) {
    comp_df[i + observation_count, "Roof"] <- roof
  }
  view <- response$response$editedFacts$view[[1]]
  if (is.null(view) == FALSE) {
    comp_df[i + observation_count, "View"] <- view
  }
  parking <- response$response$editedFacts$parkingType[[1]]
  if (is.null(parking) == FALSE) {
    comp_df[i + observation_count, "Parking"] <- parking
  }
  heating_sources <- response$response$editedFacts$heatingSources[[1]]
  if (is.null(heating_sources) == FALSE) {
    comp_df[i + observation_count, "Heating Sources"] <- heating_sources
  }
  heating_system <- response$response$editedFacts$heatingSystem[[1]]
  if (is.null(heating_system) == FALSE) {
    comp_df[i + observation_count, "Heating System"] <- heating_system
  }
}

#########################################################################################

# Repeat for more data extraction
# info for ZPID 49123998 (Palm Ave)
address <- "1432+Palm+Ave+SW&citystatezip=Seattle%2C+WA"
resource_uri <- "http://www.zillow.com/webservice/GetDeepSearchResults.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&address=", address)

# Parse data into body
response <- read_xml(uri)
body <- as_list(response)

# Data for principal
principal_zpid <- body$searchresults$response$results$result$zpid[[1]]
principal_zestimate <- body$searchresults$response$results$result$zestimate$amount[[1]]
principal_last_sold_date <- body$searchresults$response$results$result$lastSoldDate[[1]]
principal_last_sold_price <- body$searchresults$response$results$result$lastSoldPrice[[1]]
principal_year_built <- body$searchresults$response$results$result$yearBuilt[[1]]
principal_finished_sqft <- body$searchresults$response$results$result$finishedSqFt[[1]]
principal_lot_size <- body$searchresults$response$results$result$lotSizeSqFt[[1]]
principal_bathrooms <- body$searchresults$response$results$result$bathrooms[[1]]
principal_bedrooms <- body$searchresults$response$results$result$bedrooms[[1]]

resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&zpid=", "48775056")

response <- xmlParse(uri)
response <- xmlToList(response)
response <- toJSON(response, pretty = TRUE)
response <- fromJSON(response)

principal_floors <- response$response$editedFacts$numFloors[[1]]
principal_basement <- response$response$editedFacts$basement[[1]]
principal_home_type <- response$response$editedFacts$useCode[[1]]
principal_roof <- response$response$editedFacts$roof[[1]]
principal_view <- response$response$editedFacts$view[[1]]
principal_parking <- response$response$editedFacts$parkingType[[1]]
principal_heating_sources <- response$response$editedFacts$heatingSources[[1]]
principal_heating_system <- response$response$editedFacts$heatingSystem[[1]]

observation_count <- nrow(comp_df)

comp_df[observation_count + 1, "ZPID"] <- principal_zpid
comp_df[observation_count + 1, "Zestimate"] <- principal_zestimate
comp_df[observation_count + 1, "Last Sold Date"] <- principal_last_sold_date
comp_df[observation_count + 1, "Last Sold Price"] <- principal_last_sold_price
comp_df[observation_count + 1, "Year Built"] <- principal_year_built
comp_df[observation_count + 1, "Floors"] <- principal_floors
comp_df[observation_count + 1, "Basement"] <- principal_basement
comp_df[observation_count + 1, "Bathrooms"] <- principal_bathrooms
comp_df[observation_count + 1, "Finished SQFT"] <- principal_finished_sqft
comp_df[observation_count + 1, "Lot Size SQFT"] <- principal_lot_size
comp_df[observation_count + 1, "Home Type"] <- principal_home_type
comp_df[observation_count + 1, "Roof"] <- principal_roof
comp_df[observation_count + 1, "View"] <- principal_view
comp_df[observation_count + 1, "Parking"] <- principal_parking
comp_df[observation_count + 1, "Heating Sources"] <- principal_heating_sources
comp_df[observation_count + 1, "Heating System"] <- principal_heating_system

### Comparables for 48775056 ###
comp_count = 25
resource_uri <- "http://www.zillow.com/webservice/GetDeepComps.htm?zws-id="
uri <- paste0(resource_uri, zws_key, "&zpid=", principal_zpid, "&count=", comp_count)

response <- xmlParse(uri)
response <- xmlToList(response)
response <- toJSON(response, pretty = TRUE)
response <- fromJSON(response)

num_comp <- length(response$response$properties$comparables)
observation_count <- nrow(comp_df)

comp <- response[[3]][[1]][[2]][[1]]
zpid <- comp$zpid[[1]]
comp_df[(1 + observation_count), "ZPID"] <- zpid
zestimate <- comp$zestimate$amount[[1]][[1]]
comp_df[1 + observation_count + 1, "Zestimate"] <- zestimate

for (i in 1:num_comp) {
  comp <- response[[3]][[1]][[2]][[i]]
  zpid <- comp$zpid[[1]]
  if (is.null(zpid) == FALSE) {
    comp_df[(i + observation_count), "ZPID"] <- zpid
  }
  zestimate <- comp$zestimate$amount[[1]][[1]]
  if (is.null(zestimate) == FALSE) {
    comp_df[(i + observation_count), "Zestimate"] <- zestimate
  }
  sold_date <- comp$lastSoldDate[[1]]
  if (is.null(sold_date) == FALSE) {
    comp_df[(i + observation_count), "Last Sold Date"] <- sold_date
  }
  sold_price <- comp$lastSoldPrice$text[[1]]
  if (is.null(sold_price) == FALSE) {
    comp_df[(i + observation_count), "Last Sold Price"] <- sold_price
  }
  year_built <- comp$yearBuilt[[1]]
  if (is.null(year_built) == FALSE) {
    comp_df[(i + observation_count), "Year Built"] <- year_built
  }
  finished_sqft <- comp$finishedSqFt[[1]]
  if (is.null(finished_sqft) == FALSE) {
    comp_df[(i + observation_count), "Finished SQFT"] <- finished_sqft
  }
  lot_size <- comp$lotSizeSqFt[[1]]
  if (is.null(lot_size) == FALSE) {
    comp_df[(i + observation_count), "Lot Size SQFT"] <- lot_size
  }
  bathrooms <- comp$bathrooms[[1]]
  if (is.null(bathrooms) == FALSE) {
    comp_df[(i + observation_count), "Bathrooms"] <- bathrooms
  }
  bedrooms <- comp$bedrooms[[1]]
  if (is.null(bedrooms) == FALSE) {
    comp_df[(i + observation_count), "Bedrooms"] <- bedrooms
  }
}

resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="

for (i in 1:num_comp) {
  uri <- paste0(resource_uri, zws_key, "&zpid=", comp_df[i, "ZPID"])
  response <- xmlParse(uri)
  response <- xmlToList(response)
  response <- toJSON(response, pretty = TRUE)
  response <- fromJSON(response)
  
  floors <- response$response$editedFacts$numFloors[[1]]
  if (is.null(floors) == FALSE) {
    comp_df[i + observation_count, "Floors"] <- floors
  }
  basement <- response$response$editedFacts$basement[[1]]
  if (is.null(basement) == FALSE) {
    comp_df[i + observation_count, "Basement"] <- basement
  }
  home_type <- response$response$editedFacts$useCode[[1]]
  if (is.null(home_type) == FALSE) {
    comp_df[i + observation_count, "Home Type"] <- home_type
  }
  roof <- response$response$editedFacts$roof[[1]]
  if (is.null(roof) == FALSE) {
    comp_df[i + observation_count, "Roof"] <- roof
  }
  view <- response$response$editedFacts$view[[1]]
  if (is.null(view) == FALSE) {
    comp_df[i + observation_count, "View"] <- view
  }
  parking <- response$response$editedFacts$parkingType[[1]]
  if (is.null(parking) == FALSE) {
    comp_df[i + observation_count, "Parking"] <- parking
  }
  heating_sources <- response$response$editedFacts$heatingSources[[1]]
  if (is.null(heating_sources) == FALSE) {
    comp_df[i + observation_count, "Heating Sources"] <- heating_sources
  }
  heating_system <- response$response$editedFacts$heatingSystem[[1]]
  if (is.null(heating_system) == FALSE) {
    comp_df[i + observation_count, "Heating System"] <- heating_system
  }
}

# Remove all observations with NA values in them
valid_comp_df <- na.omit(comp_df)

#########################################################################################
# Use the existing ZPIDs in the valid comp data frame to call for more 
# comparable observations
current_ZPID <- as.numeric(valid_comp_df$ZPID)

for (i in current_ZPID) {
  comp_count = 25
  resource_uri <- "http://www.zillow.com/webservice/GetDeepComps.htm?zws-id="
  uri <- paste0(resource_uri, zws_key, "&zpid=", i, "&count=", comp_count)
  
  response <- xmlParse(uri)
  response <- xmlToList(response)
  response <- toJSON(response, pretty = TRUE)
  response <- fromJSON(response)
  
  num_comp <- length(response$response$properties$comparables)
  observation_count <- nrow(comp_df)
  
  comp <- response[[3]][[1]][[2]][[1]]
  zpid <- comp$zpid[[1]]
  comp_df[(1 + observation_count), "ZPID"] <- zpid
  zestimate <- comp$zestimate$amount[[1]][[1]]
  comp_df[1 + observation_count + 1, "Zestimate"] <- zestimate
  
  for (j in 1:num_comp) {
    comp <- response[[3]][[1]][[2]][[j]]
    zpid <- comp$zpid[[1]]
    if (is.null(zpid) == FALSE) {
      comp_df[(j + observation_count), "ZPID"] <- zpid
    }
    zestimate <- comp$zestimate$amount[[1]][[1]]
    if (is.null(zestimate) == FALSE) {
      comp_df[(j + observation_count), "Zestimate"] <- zestimate
    }
    sold_date <- comp$lastSoldDate[[1]]
    if (is.null(sold_date) == FALSE) {
      comp_df[(j + observation_count), "Last Sold Date"] <- sold_date
    }
    sold_price <- comp$lastSoldPrice$text[[1]]
    if (is.null(sold_price) == FALSE) {
      comp_df[(j + observation_count), "Last Sold Price"] <- sold_price
    }
    year_built <- comp$yearBuilt[[1]]
    if (is.null(year_built) == FALSE) {
      comp_df[(j + observation_count), "Year Built"] <- year_built
    }
    finished_sqft <- comp$finishedSqFt[[1]]
    if (is.null(finished_sqft) == FALSE) {
      comp_df[(j + observation_count), "Finished SQFT"] <- finished_sqft
    }
    lot_size <- comp$lotSizeSqFt[[1]]
    if (is.null(lot_size) == FALSE) {
      comp_df[(j + observation_count), "Lot Size SQFT"] <- lot_size
    }
    bathrooms <- comp$bathrooms[[1]]
    if (is.null(bathrooms) == FALSE) {
      comp_df[(j + observation_count), "Bathrooms"] <- bathrooms
    }
    bedrooms <- comp$bedrooms[[1]]
    if (is.null(bedrooms) == FALSE) {
      comp_df[(j + observation_count), "Bedrooms"] <- bedrooms
    }
  }
  
  resource_uri <- "http://www.zillow.com/webservice/GetUpdatedPropertyDetails.htm?zws-id="
  
  for (k in 1:num_comp) {
    uri <- paste0(resource_uri, zws_key, "&zpid=", comp_df[k, "ZPID"])
    response <- xmlParse(uri)
    response <- xmlToList(response)
    response <- toJSON(response, pretty = TRUE)
    response <- fromJSON(response)
    
    floors <- response$response$editedFacts$numFloors[[1]]
    if (is.null(floors) == FALSE) {
      comp_df[k + observation_count, "Floors"] <- floors
    }
    basement <- response$response$editedFacts$basement[[1]]
    if (is.null(basement) == FALSE) {
      comp_df[k + observation_count, "Basement"] <- basement
    }
    home_type <- response$response$editedFacts$useCode[[1]]
    if (is.null(home_type) == FALSE) {
      comp_df[k + observation_count, "Home Type"] <- home_type
    }
    roof <- response$response$editedFacts$roof[[1]]
    if (is.null(roof) == FALSE) {
      comp_df[k + observation_count, "Roof"] <- roof
    }
    view <- response$response$editedFacts$view[[1]]
    if (is.null(view) == FALSE) {
      comp_df[k + observation_count, "View"] <- view
    }
    parking <- response$response$editedFacts$parkingType[[1]]
    if (is.null(parking) == FALSE) {
      comp_df[k + observation_count, "Parking"] <- parking
    }
    heating_sources <- response$response$editedFacts$heatingSources[[1]]
    if (is.null(heating_sources) == FALSE) {
      comp_df[k + observation_count, "Heating Sources"] <- heating_sources
    }
    heating_system <- response$response$editedFacts$heatingSystem[[1]]
    if (is.null(heating_system) == FALSE) {
      comp_df[k + observation_count, "Heating System"] <- heating_system
    }
  }
}

# Remove Duplicate Results
comp_df <- distinct(comp_df)

# Generate a new comp data set with no NA values
valid_comp_df <- na.omit(comp_df)

# Make sure all observations in the zestimate column are in numeric format
final_comp_df <- valid_comp_df
final_comp_df$Zestimate <- as.numeric(final_comp_df$Zestimate)
final_comp_df <- na.omit(final_comp_df)

# Export data frame to csv file
write.csv(final_comp_df, "./data/zillow_data.csv", row.names = FALSE)
