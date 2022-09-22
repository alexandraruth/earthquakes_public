#### This is a script that cleans the raw earthquakes spreadsheet file and prepares it for analysis.


# Input:  earthquakes_raw.csv
# Output: earthquakes_clean.Rda


# Prepare working environment ----

# Load packages
library(tidyverse)
library(janitor)
library(countrycode)
library(leaflet)

# Load data
earthquakes_raw <- read.csv("earthquakes_raw.csv")

# Clean column names 
earthquakes <- earthquakes_raw %>%
  clean_names() 
 
# Remove extraneous columns 
earthquakes <- earthquakes %>%
  select(-search_parameters)

# remove blank top row
earthquakes <- earthquakes[-c(1), ]

# create new variable with clean country names
earthquakes <- earthquakes %>%
  mutate(location_name = gsub(":", "", location_name)) %>%
  mutate(location_name = gsub("-", " ", location_name)) %>%
  mutate(location_name = gsub(";", "", location_name)) %>%
  mutate(country = countryname(location_name))



# examine country names that didn't match
country_na <- earthquakes %>%
  filter(is.na(country))

# fill in "USA" for state-level locations

country_na <- country_na %>%
  mutate(country = ifelse(str_detect(location_name,"ALASKA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"ALABAMA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"COLORADO"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"CALIFORNIA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"HAWAII"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"IDAHO"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"ILLINOIS"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"KENTUCKY"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"MARYLAND"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"MONTANA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"UTAH"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"WYOMING"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"TEXAS"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"NORTH CAROLINA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"NEVADA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"NEW YORK"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"OKLAHOMA"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"OREGON"), "USA", country)) %>%
  mutate(country = ifelse(str_detect(location_name,"WASHINGTON"), "USA", country)) 


countrylist <- countrycode::codelist$country.name.en


# make quick map of earthquake events in leaflet to visually inspect locations

e <- leaflet(earthquakes) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addCircles(~longitude, ~latitude, weight = 2, radius=20, 
             color="#ffa500", fillOpacity = 0.8) 
e
