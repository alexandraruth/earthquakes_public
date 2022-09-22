#### This is a script that calculates counts of severe earthquakes within specific radii of embassy facilities.

# Inputs: Geographic coordinates for earthquakes ("earthquakes.Rda") and embassies ("embassy_geo.Rda")
# Outputs: Df with earthquake counts for different radii & Poisson probabilities ("embassy_risk.Rda"), Map webpage


#  - Df with earthquake counts for 20mi and 100mi radii for all embassies and Poisson probabilities of severe earthquake within next 5 yrs ("embassy_risk.Rda")
#  - Df with 10 highest-risk embassies for a severe earthquake within 20 mi ("max_20mi.Rda")
#  - Df with 10 highest-risk embassies for a severe earthquake within 100 mi ("max_100mi.Rda")


# Prepare working environment ----

# load packages
library(ggmap)
library(tidyverse)
library(nngeo)
library(leaflet)
library(mapview)
library(htmltools)


# load data
load("earthquakes.Rda")
load("embassy_geocoded.Rda")



# select only necessary columns and rows from df

earthquakes_c <- earthquakes %>%
  select(year, location_name, latitude, longitude, mag, focal_depth_km) %>%
  select(location_name, longitude, latitude, mag) %>%
  filter(!is.na(latitude))


## DISTANCE CALCULATIONS ----


# convert dfs to point layers

earthquakes_sf <- st_as_sf(earthquakes_c, coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE, agr = "constant")

plot(earthquakes_sf)

embassy_sf <- st_as_sf(embassy_geo, coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE, agr = "constant")
plot(embassy_sf, col="grey")

row.names(embassy_sf) <- 1:nrow(embassy_sf)

# nearest neighbors within radius (100mi) 
r_20mi = st_nn(embassy_sf, earthquakes_sf, k=100, maxdist = 32187, returnDist = TRUE)
r_20mi

r_100mi = st_nn(embassy_sf, earthquakes_sf, k=100, maxdist = 160934, returnDist = TRUE)
r

r_200mi = st_nn(embassy_sf, earthquakes_sf, k=100, maxdist = 321869, returnDist = TRUE)
r_200mi

# get counts of quakes within radius for each embassy

counts_20mi <- c(lengths(r_20mi$dist))
counts_20mi

counts_100mi <- c(lengths(r_100mi$dist))
counts_100mi


counts_200mi <- c(lengths(r_200mi$dist))
counts_200mi

# bind counts of earthquakes within 100mi of each location to df

embassy_risk <- cbind(embassy_risk, counts_20mi)

embassy_risk <- cbind(embassy_sf, counts_100mi)

embassy_risk <- cbind(embassy_risk, counts_200mi)


# add a poisson probability of a severe earthquake occurring within 20mi of the embassy in the next 5 years

embassy_risk <- embassy_risk %>%
  mutate(prob_5yrs_20mi = ppois(q=0, lambda = 5*(counts_20mi/52), lower.tail=F))


# add a poisson probability of a severe earthquake occurring within 100mi of the embassy in the next 5 years

embassy_risk <- embassy_risk %>%
  mutate(prob_5yrs_100mi = ppois(q=0, lambda = 5*(counts_100mi/52), lower.tail=F))


# add a poisson probability of a severe earthquake occurring within 200mi of the embassy in the next 5 years

embassy_risk <- embassy_risk %>%
  mutate(prob_5yrs_200mi = ppois(q=0, lambda = 5*(counts_200mi/52), lower.tail=F))



# get clean percentages for Poisson probabilities

embassy_risk <- embassy_risk %>% 
  mutate(pct_20mi = round(100*prob_5yrs_20mi, digits = 2)) %>%
  mutate(pct_100mi = round(100*prob_5yrs_100mi, digits = 2)) %>%
  mutate(pct_200mi = round(100*prob_5yrs_200mi, digits = 2)) 
  
# save file

save(embassy_risk, file = "embassy_risk.Rda")

# get top 10 probabilities for each

max_20mi <- embassy_risk %>% 
  arrange(desc(pct_20mi)) %>% 
  select(address, counts_20mi, pct_20mi) %>%
  rename(counts = counts_20mi) %>%
  rename(prob = pct_20mi) %>%
  rename(lonlat = geometry) %>%
  slice_head(n=10)
save(max_20mi, file = "max_20mi.Rda")

max_100mi <- embassy_risk %>% 
  arrange(desc(pct_100mi)) %>% 
  select(address, counts_100mi, pct_100mi) %>%
  rename(counts = counts_100mi) %>%
  rename(prob = pct_100mi) %>%
  rename(lonlat = geometry) %>%
  slice_head(n=10)
  save(max_100mi, file = "max_100mi.Rda")

# MAPS ----

# map for interactive app

m2 <- leaflet(embassy_geocoded) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addScaleBar(position = "bottomleft", options = scaleBarOptions()) %>%
  addMarkers(~longitude, ~latitude, icon = markerIcon, label = as.character(embassy_risk$address), popup = ~paste0(strong("Chance of significant earthquake in the next 5 years:"), "<br>", strong("Within 20 mi = "), embassy_risk$pct_20mi,"%", "<br>",
                                                                                                                          strong("Within 100 mi = "), embassy_risk$pct_100mi, "%") ) %>%
  addCircles(data = earthquakes_c, ~longitude, ~latitude, weight = 2, radius=20, 
             color="yellow", fillOpacity = 0.2, label = ~paste("Magnitude:", mag))
m2 

     
markerIcon <- makeIcon(
  iconUrl = "https://www.freeiconspng.com/thumbs/star-icon/blue-star-icon-14.png",
  iconWidth = 10, iconHeight = 10
)



# mapview map just for fun


earthquakes_lyr = mapview(earthquakes_sf, legend = FALSE)
embassy_lyr = mapview(embassy_sf, cex = 2, legend = FALSE)

earthquakes_lyr + embassy_lyr

