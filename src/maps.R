library(tidyverse)
library(rgdal)
library(rmapshaper)
library(sp)
library(sf)
library(leaflet)

# Read in simulations
state_sims <- read_csv("output/state_sims.csv") %>%
  mutate(party = ordered(party, levels = party_order))

state_summary_stats <- state_sims %>%
  group_by(state, sim_id) %>%
  mutate(state_total_seats = sum(total_seats)) %>%
  group_by(state, party) %>%
  summarise(avg_pct = mean(pct),
            avg_seats = mean(total_seats),
            avg_seat_pct = mean(total_seats / state_total_seats))

state_summary_stats_wide <- state_sims %>%
  group_by(state, sim_id) %>%
  mutate(state_total_seats = sum(total_seats)) %>%
  group_by(state, party)
  

state_largest_party <- state_summary_stats %>%
  group_by(state) %>%
  filter(avg_pct == max(avg_pct)) %>%
  dplyr::select(state, largest_party = party, largest_pct = avg_pct, largest_seats = avg_seats, largest_seat_pct = avg_seat_pct)

# Read in shapefile
state_shp <- readOGR("data/shapes/germany_states.shp") %>%
  st_as_sf() %>%
  ms_simplify() %>%
  
  # Join on largest party data from sims
  left_join(state_largest_party, by = c("name_1" = "state")) %>%
  
  # Then create colors and stuff
  mutate(color = case_when(largest_party == "afd" ~ "#00A0E2",
                           largest_party == "cdu" ~ "black",
                           largest_party == "fdp" ~ "#FEB900",
                           largest_party == "gruene" ~ "#19A229",
                           largest_party == "linke" ~ "#BE3075",
                           largest_party == "spd" ~ "red"),
         alpha = sqrt((largest_pct - 0.2) / 0.2)) %>%
  
  # Label-related things
  mutate(english_name = case_when(is.na(varname_1) ~ name_1,
                                  varname_1 == "<Null>" ~ name_1,
                                  !is.na(varname_1) ~ varname_1),
         mouseover_label = case_when(english_name != name_1 ~ paste0(name_1, " (", english_name, ")"),
                                     english_name == name_1 ~ name_1))

leaflet(state_shp) %>%
  addTiles() %>%
  addPolygons(color = "black", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color, fillOpacity = ~alpha, label = ~mouseover_label)
