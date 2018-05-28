library("httr")
library("jsonlite")
library("dplyr")
library("knitr")
library("xml2")
library("geosphere")
library("data.frame")

base_uri <- "http://www.zillow.com/webservice/GetRegionChildren.htm"
z_id <- "X1-ZWz1geilg38iyz_9r65p"
state <- "wa"
city <- "seattle"
childtype <- "neighborhood"
uri <- paste0(base_uri, "?zws-id=", z_id, "&state=", state, "&city=", city, "&childtype=", childtype)
response <- xmlParse(uri)
list <- xmlToList(response)
list <- toJSON(list, pretty = TRUE)
list <- fromJSON(list)
regions <- list$response$list
# remove missing values
regions <- regions[lapply(regions,length)>5]
df <- data.frame(matrix(unlist(regions), nrow=7, byrow=F),stringsAsFactors=FALSE)
df <- t(df)
df <- as.data.frame(df, stringsAsFactors = FALSE)
remove
row.names(df) <- 1:84
colnames(df) <- c("id", "name", "price", "currency", "web", "lat", "long")
seattle_lat_long <- c(47.608013, -122.335167)
earth.dist <- function (long1, lat1, long2, lat2)
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
for(i in 1:NROW(df)){
  print(paste(df$long[i], df$lat[i],seattle_lat_long[2], seattle_lat_long[1]))
  df$dist[i] <- earth.dist(as.double(df$long[i]), as.double(df$lat[i]), seattle_lat_long[2], seattle_lat_long[1])
}

ggplot(data = df) +
  geom_smooth(mapping = aes(x = dist, y = price, alpha = 1)) +
  geom_jitter(mapping = aes(x = dist, y = price)) +
  labs(
    title = "Price vs Dist",
    x = "Distance",
    y = "Price"
  )
