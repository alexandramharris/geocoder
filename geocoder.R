# Geocoder
# Alexandra Harris

# Set up ----

# Run libraries
library(tidyverse)
library(janitor)
library(stringr)
library(ggmap)
library(leaflet)

# Register Google API key
# register_google(key = "key_here")

# Import data
data <- read.csv("Data/places.csv")

# Standardize data
data <- clean_names(data)

# Add all additional known information to place names, such as state
data$state <- "New York"

# Create cache
cache <- data.frame(location = character(),
                    latitude = numeric(),
                    longitude = numeric(),
                    address = character(),
                    stringsAsFactors = FALSE)


# Geocode ----
data$latitude <- NA
data$longitude <- NA
data$address <- NA

for (i in 1:nrow(data)) {
  cached_result <- subset(cache, location == paste0(data$location[i],", ",data$state[i]))
  if (nrow(cached_result) > 0) {
    data$latitude[i] <- cached_result$latitude
    data$longitude[i] <- cached_result$longitude
    data$address[i] <- cached_result$address
  } else {
    location_string <- paste0(data$location[i],", ",data$state[i])
    result <- geocode(location_string, output = "more")
    data$latitude[i] <- result$lat
    data$longitude[i] <- result$lon
    data$address[i] <- result$address
    
    cache <- rbind(cache, data.frame(location = location_string,
                                     latitude = result$lat,
                                     longitude = result$lon,
                                     address = result$address))
  }
}

data$address <- str_to_title(data$address)
final <- data


# Map check ----

# Create map to test points
map <- leaflet(data) %>% 
  addTiles() %>% 
  addCircleMarkers(lng = data$longitude, lat = data$latitude, color = "red", radius = 2, popup = data$location)

map


# Export ----
write.csv(final, "final.csv", row.names = FALSE)
