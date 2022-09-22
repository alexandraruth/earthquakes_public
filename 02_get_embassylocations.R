#### The purpose of this script is to use screen scraping to build a dataframe with info about U.S. embassy locations. 

# Inputs: webpage url listing countries with embassies and consulates
# Outputs: 

# Prepare working environment ----

library(tidyverse)
library(rvest)


# Read embassy page info ----

embassy_page <- read_html("https://www.usembassy.gov/")


# Get list of embassy locations from webpage ----

# get list of all countries on main webpage that have hyperlinks to their own pages
names <- embassy_page %>%
  html_nodes("a.pcs-post-title") %>%
  html_attr('title')

# get urls for each country's webpage listing their embassies and consulates
links <- embassy_page %>% 
  html_nodes("a.pcs-post-title") %>% 
  html_attr('href') 


# create df with each country's webpage
embassy_links <- as.data.frame(links) %>%
  mutate(id = row_number())

# create df with names of all countries
embassy_names <- as.data.frame(names) %>%
  mutate(id = row_number())

# bind country names to their webpages
embassy_locations <- cbind(embassy_names, embassy_links) %>%
  select(-id)

# save intermediate df
save(embassy_locations, file = "embassies.Rda")


# Get names and addresses for individual embassy locations from html code on each country page----

results <- c()

for(i in links) {
  
  page <- read_html(paste0(i))
  
  text <- page %>% 
    html_nodes("div.cityname1") %>%
    html_text()
  
  results <- c(results, text)
}

# clean up address strings
test <- gsub("\n", ", ", results) 
test <- sub("Phone[^Phone]+$", "", test) 
test <- sub("Telephone[^Telephone]+$", "", test)
test <- sub("Tel[^Tel]+$", "", test)
test <- sub("tel[^tel]+$", "", test)
test <- sub("Fax[^Fax]+$", "", test)
test <- gsub("Address: ", " ", test)
test <- sub(",[^,]+$", "", test)



# save df with all embassy addresses for each country

embassy_addresses <- as.data.frame(test) %>%
  rename(address = test)

save(embassy_addresses, file="embassy_addresses.Rda")




