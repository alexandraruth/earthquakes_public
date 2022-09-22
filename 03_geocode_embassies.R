#### This is a script that geocodes embassy and consulate locations. 

# Inputs: Text addresses for each embassy/consulate ("embassy_addresses.Rda")
# Outputs: Geocoded lat/lon coordinates for each embassy/consulate ("embassy_geo.Rda")

# load packages

library(ggmap)
library(tidyverse)
library(sf)
library(tidygeocoder)


# only run once: API key

Sys.setenv(GOOGLEGEOCODE_API_KEY = "INSERT KEY HERE")



# load data

load("embassy_addresses.Rda")

# geocode

embassy_geocoded <- embassy_addresses %>%
  geocode(address, method = 'google', lat = latitude , long = longitude)

# check for missing coords
sum(is.na(embassy_geocoded$latitude))
sum(is.na(embassy_geocoded$longitude))

# save df

save(embassy_geocoded, file = "embassy_geocoded.Rda")

