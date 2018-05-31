library(dplyr)

# Filter data for only Seattle observation
city_price_df <- read.csv("./data/Sale_Prices_City.csv", stringsAsFactors = FALSE)
city_price_df <- city_price_df %>% filter(RegionName == "Seattle")

# Create new empty data frame
city_price_month_df <- as.data.frame(matrix(ncol = 3, nrow = 108))
colnames(city_price_month_df) <- c("Year", "Month", "Value")

# Populate the year column
j = 0
for (i in 1:9) {
  city_price_month_df[(j + 1):(j + 12), 1] <- (2008 + i)
  j = j + 12
}

# Populate the month column
city_price_month_df[, 2] <- c("JAN", "FEB", "MAR", "APR",
                        "MAY", "JUN", "JUL", "AUG",
                        "SEP", "OCT", "NOV", "DEC")

# Populate the value column
for (i in 1:108) {
  city_price_month_df[i, 3] <- city_price_df[, i + 14]
}

# Create new empty data frame to store average price by month
city_avg_price_df <- as.data.frame(matrix(ncol = 2, nrow = 12))
colnames(city_avg_price_df) <- c("Month", "Avg_Value")

# Populate the month column
city_avg_price_df[, 1] <- c("JAN", "FEB", "MAR", "APR",
                      "MAY", "JUN", "JUL", "AUG",
                      "SEP", "OCT", "NOV", "DEC")

# Populate the avg value column
for (i in 1:12) {
  val <- as.numeric(city_price_month_df %>% 
                      filter(Month == as.character(city_avg_price_df[i, 1])) %>%
                      summarise(avg = mean(Value)))
  city_avg_price_df[i, 2] <- val
}

# Divide the value by 1000 so it is easier to show on plot
city_avg_price_df$Avg_Value <- city_avg_price_df$Avg_Value / 1000

# Order the months so it appears in that order in plots
city_avg_price_df$Month <- factor(city_avg_price_df$Month, 
                            levels = c("JAN", "FEB", "MAR", "APR",
                                       "MAY", "JUN", "JUL", "AUG",
                                       "SEP", "OCT", "NOV", "DEC"))
