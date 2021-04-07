source("src/summary_stats_for_maps.R")

library(tidyverse)
library(scales)
library(rgdal)
library(rmapshaper)
library(sp)
library(sf)
library(leaflet)

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
  left_join(state_summary_stats_1, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_2, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_3, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_4, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_5, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_6, by = c("name_1" = "state")) %>%
  mutate(english_name = case_when(is.na(varname_1) ~ name_1,
                                  varname_1 == "<Null>" ~ name_1,
                                  !is.na(varname_1) ~ varname_1),
         mouseover_label = case_when(english_name != name_1 ~ paste0(name_1, " (", english_name, ")"),
                                     english_name == name_1 ~ name_1),
         popup_label = paste0("<H3><b><u>", name_1, "</u></b></H3>
                              <b><i>Projected vote (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_abbr_1, "</b></font>: <b><font color =", color_1, ">", 
                                percent(vote_pct50_1, accuracy = 0.1), "</font></b> (", percent(vote_pct05_1, accuracy = 0.1), " – ", 
                                percent(vote_pct95_1, accuracy = 0.1), ")<br>
                              <font color = ", color_2, "><b>", party_abbr_2, "</b></font>: <b><font color =", color_2, ">", 
                                percent(vote_pct50_2, accuracy = 0.1), "</font></b> (", percent(vote_pct05_2, accuracy = 0.1), " – ", 
                                percent(vote_pct95_2, accuracy = 0.1), ")<br>
                              <font color = ", color_3, "><b>", party_abbr_3, "</b></font>: <b><font color =", color_3, ">", 
                                percent(vote_pct50_3, accuracy = 0.1), "</font></b> (", percent(vote_pct05_3, accuracy = 0.1), " – ", 
                                percent(vote_pct95_3, accuracy = 0.1), ")<br>
                              <font color = ", color_4, "><b>", party_abbr_4, "</b></font>: <b><font color =", color_4, ">", 
                                percent(vote_pct50_4, accuracy = 0.1), "</font></b> (", percent(vote_pct05_4, accuracy = 0.1), " – ", 
                                percent(vote_pct95_4, accuracy = 0.1), ")<br>
                              <font color = ", color_5, "><b>", party_abbr_5, "</b></font>: <b><font color =", color_5, ">", 
                                percent(vote_pct50_5, accuracy = 0.1), "</font></b> (", percent(vote_pct05_5, accuracy = 0.1), " – ", 
                                percent(vote_pct95_5, accuracy = 0.1), ")<br>
                              <font color = ", color_6, "><b>", party_abbr_6, "</b></font>: <b><font color =", color_6, ">", 
                                percent(vote_pct50_6, accuracy = 0.1), "</font></b> (", percent(vote_pct05_6, accuracy = 0.1), " – ", 
                                percent(vote_pct95_6, accuracy = 0.1), ")<br>
                              <br>
                              <b><i>Projected seats (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_abbr_1, "</b></font>: <b><font color =", color_1, ">", seats_pct50_1, 
                                "</font></b> seats (", seats_pct05_1, " – ", seats_pct95_1, ")<br>
                              <font color = ", color_2, "><b>", party_abbr_2, "</b></font>: <b><font color =", color_2, ">", seats_pct50_2, 
                                "</font></b> seats (", seats_pct05_2, " – ", seats_pct95_2, ")<br>
                              <font color = ", color_3, "><b>", party_abbr_3, "</b></font>: <b><font color =", color_3, ">", seats_pct50_3, 
                                "</font></b> seats (", seats_pct05_3, " – ", seats_pct95_3, ")<br>
                              <font color = ", color_4, "><b>", party_abbr_4, "</b></font>: <b><font color =", color_4, ">", seats_pct50_4, 
                                "</font></b> seats (", seats_pct05_4, " – ", seats_pct95_4, ")<br>
                              <font color = ", color_5, "><b>", party_abbr_5, "</b></font>: <b><font color =", color_5, ">", seats_pct50_5, 
                                "</font></b> seats (", seats_pct05_5, " – ", seats_pct95_5, ")<br>
                              <font color = ", color_6, "><b>", party_abbr_6, "</b></font>: <b><font color =", color_6, ">", seats_pct50_6, 
                                "</font></b> seats (", seats_pct05_6, " – ", seats_pct95_6, ")"))

leaflet(state_shp) %>%
  addTiles() %>%
  addPolygons(color = "black", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color, fillOpacity = ~alpha, label = ~mouseover_label,
              popup = ~popup_label, highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = TRUE, opacity = 1))



const_shp <- readOGR("data/shapes/germany_constituencies.shp") %>%
  st_as_sf() %>%
  ms_simplify() %>%
  
  # Join on constituency summary stats
  left_join(const_summary_stats_1, by = c("WKR_NR" = "id")) %>%
  left_join(const_summary_stats_2, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_3, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_4, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_5, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_6, by = c("WKR_NR" = "id", "constituency"))
  mutate(english_name = case_when(is.na(varname_1) ~ name_1,
                                  varname_1 == "<Null>" ~ name_1,
                                  !is.na(varname_1) ~ varname_1),
         mouseover_label = case_when(english_name != name_1 ~ paste0(name_1, " (", english_name, ")"),
                                     english_name == name_1 ~ name_1),
         popup_label = paste0("<H3><b><u>", name_1, "</u></b></H3>
                              <b><i>Projected vote (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_abbr_1, "</b></font>: <b><font color =", color_1, ">", 
                              percent(vote_pct50_1, accuracy = 0.1), "</font></b> (", percent(vote_pct05_1, accuracy = 0.1), " – ", 
                              percent(vote_pct95_1, accuracy = 0.1), ")<br>
                              <font color = ", color_2, "><b>", party_abbr_2, "</b></font>: <b><font color =", color_2, ">", 
                              percent(vote_pct50_2, accuracy = 0.1), "</font></b> (", percent(vote_pct05_2, accuracy = 0.1), " – ", 
                              percent(vote_pct95_2, accuracy = 0.1), ")<br>
                              <font color = ", color_3, "><b>", party_abbr_3, "</b></font>: <b><font color =", color_3, ">", 
                              percent(vote_pct50_3, accuracy = 0.1), "</font></b> (", percent(vote_pct05_3, accuracy = 0.1), " – ", 
                              percent(vote_pct95_3, accuracy = 0.1), ")<br>
                              <font color = ", color_4, "><b>", party_abbr_4, "</b></font>: <b><font color =", color_4, ">", 
                              percent(vote_pct50_4, accuracy = 0.1), "</font></b> (", percent(vote_pct05_4, accuracy = 0.1), " – ", 
                              percent(vote_pct95_4, accuracy = 0.1), ")<br>
                              <font color = ", color_5, "><b>", party_abbr_5, "</b></font>: <b><font color =", color_5, ">", 
                              percent(vote_pct50_5, accuracy = 0.1), "</font></b> (", percent(vote_pct05_5, accuracy = 0.1), " – ", 
                              percent(vote_pct95_5, accuracy = 0.1), ")<br>
                              <font color = ", color_6, "><b>", party_abbr_6, "</b></font>: <b><font color =", color_6, ">", 
                              percent(vote_pct50_6, accuracy = 0.1), "</font></b> (", percent(vote_pct05_6, accuracy = 0.1), " – ", 
                              percent(vote_pct95_6, accuracy = 0.1), ")<br>
                              <br>
                              <b><i>Projected seats (90% CI)</i></b><br>
                              <font color = ", color_1, "><b>", party_abbr_1, "</b></font>: <b><font color =", color_1, ">", seats_pct50_1, 
                              "</font></b> seats (", seats_pct05_1, " – ", seats_pct95_1, ")<br>
                              <font color = ", color_2, "><b>", party_abbr_2, "</b></font>: <b><font color =", color_2, ">", seats_pct50_2, 
                              "</font></b> seats (", seats_pct05_2, " – ", seats_pct95_2, ")<br>
                              <font color = ", color_3, "><b>", party_abbr_3, "</b></font>: <b><font color =", color_3, ">", seats_pct50_3, 
                              "</font></b> seats (", seats_pct05_3, " – ", seats_pct95_3, ")<br>
                              <font color = ", color_4, "><b>", party_abbr_4, "</b></font>: <b><font color =", color_4, ">", seats_pct50_4, 
                              "</font></b> seats (", seats_pct05_4, " – ", seats_pct95_4, ")<br>
                              <font color = ", color_5, "><b>", party_abbr_5, "</b></font>: <b><font color =", color_5, ">", seats_pct50_5, 
                              "</font></b> seats (", seats_pct05_5, " – ", seats_pct95_5, ")<br>
                              <font color = ", color_6, "><b>", party_abbr_6, "</b></font>: <b><font color =", color_6, ">", seats_pct50_6, 
                              "</font></b> seats (", seats_pct05_6, " – ", seats_pct95_6, ")"))
