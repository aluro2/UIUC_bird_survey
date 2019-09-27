

library(ggmap)
library(tidyverse)
library(googlesheets)
library(janitor)


bird_data_url <-
  gs_url("https://docs.google.com/spreadsheets/d/1M2hSw0TBxRry3Z7FA4ttU3QAvnoWHhb2MKV7J4tgUbQ/edit#gid=0")


bird_data <-
  bird_data_url %>% 
  gs_read(., ws = 1) %>% 
  clean_names() %>%
  filter(any_birds_found_on_route_shift == "Yes")

buildings <- unique(bird_data$building_name) %>% 
  sub('\\_.*', '', .) %>% 
  paste(., "University of Illinois", sep = ", ")

building_locations <- geocode(buildings)

unique(bird_data$building_name) %>%
  cbind(., building_locations) %>%
  rename( building_name =  ".") %>% 
  select(building_name, lat, lon) %>% 
  write_csv("campus_building_locations.csv")
