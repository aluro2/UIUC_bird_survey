
library(tidyverse)
library(googlesheets)
library(janitor)
library(ggthemes)
library(extrafont)

loadfonts(device = "win")

##Only need to run if fonts haven't been added to your computer yet (Windows)
# font_import()
# 

bird_data_url <-
  gs_url("https://docs.google.com/spreadsheets/d/1M2hSw0TBxRry3Z7FA4ttU3QAvnoWHhb2MKV7J4tgUbQ/edit#gid=0")


bird_data <-
  bird_data_url %>% 
  gs_read(., ws = 1) %>% 
  clean_names() %>% 
  mutate(date_mm_dd_yyyy = lubridate::mdy(date_mm_dd_yyyy))

unique(bird_data$species)
names(bird_data)

## Birds found by date

bird_data %>%
  filter(any_birds_found_on_route_shift == "Yes",
         !species == "NA",
         !species == "Unidentifiable") %>% 
  select(species, date_mm_dd_yyyy) %>% 
  table(.) %>% 
  as_tibble(.) %>% 
  mutate(species = str_extract(species, "[^_]+")) %>%
  arrange(., order(date_mm_dd_yyyy), species) %>% 
  mutate(species = factor(species, levels = unique(species))) %>%
  select(date_mm_dd_yyyy, n) %>% 
  group_by(date_mm_dd_yyyy) %>% 
  summarise(n_birds_found = sum(n)) %>%
  ggplot(., aes(x = date_mm_dd_yyyy, y = n_birds_found, group = 1)) + 
  geom_area(color = "blue", fill = "blue", alpha = 0.5) +
  xlab("Date") +
  ylab("Number of Birds Found") +
  theme_fivethirtyeight() +
  scale_y_continuous(
    breaks = function(x) unique(
      floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_fivethirtyeight() + 
  theme(#panel.background = element_rect(fill = "black",
    #colour = "black",
    #size = 0.5, linetype = "solid"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(size = 20, face = "bold"),
    text = element_text(family = "Calibri", size = 20),
    legend.position = "none",
    panel.grid.major.x = element_blank())


## Species Plot

  bird_data %>%
    filter(any_birds_found_on_route_shift == "Yes",
           !species == "NA",
           !species == "Unidentifiable") %>% 
    select(species) %>% 
    table(.) %>% 
    as_tibble(.) %>% 
    rename(Species = ".",
           "Number of Birds Found" = n ) %>%
    mutate(Species = str_extract(Species, "[^_]+")) %>%
    arrange(., desc(`Number of Birds Found`), Species) %>% 
    mutate(Species = factor(Species, levels = unique(Species))) %>% 
  ggplot(., aes(x = Species, y = `Number of Birds Found`)) + 
  geom_linerange(
    aes(x = Species, ymin = 0, ymax = `Number of Birds Found`), 
    color = "darkgray",
    size = 1.5) +
  geom_point(aes(color = Species), size = 7 ) +
  scale_y_continuous(
      breaks = function(x) unique(
        floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_fivethirtyeight() + 
  theme(#panel.background = element_rect(fill = "black",
                                        #colour = "black",
                                       #size = 0.5, linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Calibri", size = 20),
        legend.position = "none",
        panel.grid.major.x = element_blank())

ggsave("Week_1_species.png", dpi = 600, width = 10, height = 8, device = "png")

## Area plot

cols <- colorRampPalette(c("darkred", "gray"))

colors <- cols(4)

bird_data %>%
  filter(any_birds_found_on_route_shift == "Yes") %>% 
  select(building_face_direction) %>% 
  table(.) %>% 
  as_tibble(.) %>% 
  rename("Building Face Direction" = ".",
         "Number of Birds Found" = n ) %>%
  arrange(., desc(`Number of Birds Found`), `Building Face Direction`) %>% 
  mutate(`Building Face Direction` = factor(`Building Face Direction`, levels = unique(`Building Face Direction`))) %>% 
  ggplot(., aes(x = `Building Face Direction`, y = `Number of Birds Found`)) + 
  geom_linerange(
    aes(x = `Building Face Direction`, ymin = 0, ymax = `Number of Birds Found`), 
    color = "darkgray",
    size = 1.5) +
  geom_point(size = 10, color =  colors) +
  ggtitle("Week 1 \n Birds Found by Building Face") + 
  theme_fivethirtyeight() + 
  theme(#panel.background = element_rect(fill = "black",
    #colour = "black",
    #size = 0.5, linetype = "solid"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    text = element_text(family = "Calibri"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank())

ggsave("Week_1_building_face.png", dpi = 600, width = 10, height = 8, device = "png")



# Map of bird strikes -----------------------------------------------------

library(ggmap)
library(ggrepel)

building_locations <- read_csv("campus_building_locations.csv")

bird_data_map <- 
  bird_data %>% 
  filter(any_birds_found_on_route_shift == "Yes") %>% 
  left_join(., building_locations, by = "building_name") %>% 
  select(building_name, lat, lon) %>%
  group_by(building_name, lat, lon) %>% 
  summarise(n_birds = n())


UIUC <- get_googlemap("Illini Union",  zoom = 15, maptype = "satellite")

saveRDS(UIUC, "UIUC_map.rds")
UIUC <- readRDS("UIUC_map.rds")

UIUC_stamen <-
  get_googlemap("Illini Union",  zoom = 11, maptype = "satellite") 

UIUC_stamen <- 
  bb2bbox(attr(UIUC_stamen, "bb")) %>% 
  get_stamenmap(maptype = "toner", zoom = 16)

ggmap(UIUC_stamen)

## Map with alternating labels


ggmap(UIUC) + 
  geom_point(data = bird_data_map,
             aes(x = lon,
                 y = lat,
                 size = n_birds),
             fill = "blue",
             color = "orange",
             shape = 21,
             stroke = 4,
             alpha = 0.8) + 
  labs(size = "Number of Birds Found") +
  scale_size_area(max_size = 12) +
  geom_label_repel(data = subset(bird_data_map, lat < 40.110),
                   aes(x = lon,
                       y = lat,
                       label = building_name),
                   force = 15,
                   family = "Calibri",
                   label.size = 0.01,
                   nudge_x = 1,
                   direction = "y",
                   hjust = 0,
                   segment.size = 0.7,
                   segment.color = "blue",
                   color = "blue",
                   alpha = 0.8,
                   size = 5) + 
  geom_label_repel(data = subset(bird_data_map, lat > 40.110),
                   aes(x = lon,
                       y = lat,
                       label = building_name),
                   force =15,
                   family = "Calibri",
                   label.size = 0.01,
                   nudge_x = -1,
                   direction = "y",
                   hjust = 1,
                   segment.size = 0.7,
                   segment.color = "blue",
                   color = "blue",
                   alpha = 0.8,
                   size = 5) +
  theme_void() +
  theme(
    text = element_text(family = "Calibri", size = 20))
  


## Interactive leaflet map

library(leaflet)

bird_data %>% 
  filter(any_birds_found_on_route_shift == "Yes") %>% 
  left_join(., building_locations, by = "building_name") %>% 
  select(species, lat, lon) %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())




