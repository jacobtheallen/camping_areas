#===============================================================================
# Title: camping_areas.R
# Purpose: Generate list of camping areas and their distance from my home
# Author: Jacob Allen
# Date Created: December 6, 2021
# Last Modified: March 12, 2022
#===============================================================================

# Install and load packages -----------------------------------------------

#install.packages("rvest")
#install.packages("dplyr")
#install.packages("janitor")
#install.packages("gsubfn")
#install.packages("stringr")
#install.packages("gmapsdistance")
library(rvest)
library(dplyr)
library(janitor)
library(gsubfn)
library(stringr)
library(gmapsdistance)

# Store URLs for scraping -------------------------------------------------

st_parks_url <- "https://en.wikipedia.org/wiki/List_of_Texas_state_parks"
ntl_parks_url <- "https://en.wikipedia.org/wiki/List_of_national_parks_of_the_United_States"
forests_url <- "https://en.wikipedia.org/wiki/List_of_national_forests_of_the_United_States"
pres_url <- "https://en.wikipedia.org/wiki/National_preserve"

# Scrape and transform data -----------------------------------------------

st_parks <- st_parks_url %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table') %>% 
  html_table(fill = TRUE) %>% 
  clean_names() %>% 
  select(-c(image, year_established, size)) %>% 
  mutate(type = "Texas State Park") %>% 
  dplyr::rename(name = 1,
         location = 2)

ntl_parks <- ntl_parks_url %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>% 
  html_table(fill = TRUE) %>% 
  clean_names() %>% 
  select(-c(image, date_established_as_park_7_12, area_2021_13)) %>% 
  mutate(type = "National Park",
         name = paste(name, "National Park")) %>% 
  dplyr::rename(visitors_last_yr = 3)

forests <- forests_url %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>% 
  html_table(fill = TRUE) %>% 
  clean_names() %>% 
  select(-c(2, 4, 5)) %>%  
  dplyr::rename(name = 1,
         location = 2, 
         description = 3) %>% 
  mutate(type = "National Forest",
         name = paste(name, "National Forest"))

preserves <- pres_url %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>% 
  html_table(fill = TRUE) %>% 
  clean_names() %>% 
  select(-c(2, 5, 6)) %>% 
  mutate(type = "National Preserve",
         name = paste(name, "National Preserve")) %>% 
  dplyr::rename(description = ref,
         visitors_last_yr = 4)

# Merge dataframes and clean up --------------------------------------------

df <- bind_rows(st_parks, ntl_parks, forests, preserves)

df$name <- gsubfn(" - includes.*","", df$name)
df$name <- gsubfn("\\?", "", df$name)
df$name <- gsubfn("-", " ", df$name)
df$name <- gsubfn("–", " ", df$name)
df$name <- gsubfn("[^[:alnum:]]", " ",df$name)
df$name <- gsubfn("  ", " ", df$name)
df$name <- gsubfn("Palo Pinto Mountains State Park under development ", "Palo Pinto Mountains State Park", df$name)

# Generate distance data --------------------------------------------------

# Helpful documentation
  # https://cran.r-project.org/web/packages/gmapsdistance/readme/README.html
  # https://cran.r-project.org/web/packages/gmapsdistance/gmapsdistance.pdf
  # https://rpubs.com/mattdray/gmapsdistance-test

# Set your Google Maps Distance Matrix API
key <- "your_api_key" # replace with your API key
set.api.key(key)

# Create column to pass through the distance search
df <- df %>% 
  mutate(search_name = gsubfn(" ", "+", name))

res <- gmapsdistance(origin = "Dallas+Texas", # replace with your origin city or address
                     destination = df$search_name,
                     mode = "driving",
                     get.api.key(),
                     combinations = "all",
                     shape = "long")

# Unlist, convert to data frame, transform
res_df <- ldply(res, data.frame) %>% 
  select(-.id) %>% 
  replace(is.na(.), "") %>% 
  group_by(or, de) %>% 
  summarize_all(funs(paste(., collapse = ""))) %>% 
  mutate(mins = round(as.numeric(Time) / 60,0),  # By default time is returned in seconds and distance in meters
         hours = round(as.numeric(Time) / 60 / 60,2),
         miles = round(as.numeric(Distance) / 1609,0))

# Join distance data
df2 <- left_join(
  df,
  res_df,
  by = c("search_name" = "de"),
  keep = TRUE
) %>% 
  select(-c(managed_with, search_name, de, or, Time, Distance))

# Write csv file
write.csv(df2,
          "...\\camping_areas.csv",
          row.names = FALSE)
