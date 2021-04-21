library(tidyverse)
library(readr)
library(scales)
library(rgdal)
library(rmapshaper)
library(sp)
library(sf)
library(leaflet)

# Summary stats for maps ####
party_order = c("linke", "gruene", "spd", "fdp", "cdu", "afd")

# Read in simulations
state_sims <- read_csv("output/state_sims.csv") %>%
  mutate(party = ordered(party, levels = party_order))

state_summary_stats <- state_sims %>%
  group_by(state, sim_id) %>%
  mutate(state_total_seats = sum(total_seats)) %>%
  group_by(state, party) %>%
  summarise(vote_pct05 = quantile(pct, 0.05),
            vote_pct50 = median(pct),
            vote_pct95 = quantile(pct, 0.95),
            seats_pct05 = quantile(total_seats, 0.05),
            seats_pct50 = median(total_seats),
            seats_pct95 = quantile(total_seats, 0.95))

# Separate data frames for largest party, second largest party, and so on
state_summary_stats_1 <- state_summary_stats %>%
  group_by(state) %>%
  arrange(state, desc(vote_pct50)) %>%
  dplyr::slice(1) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state == "Bayern" ~ "CSU",
                                party == "cdu" & state != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_1"), .cols = -state)

state_summary_stats_2 <- state_summary_stats %>%
  group_by(state) %>%
  arrange(state, desc(vote_pct50)) %>%
  dplyr::slice(2) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state == "Bayern" ~ "CSU",
                                party == "cdu" & state != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_2"), .cols = -state)

state_summary_stats_3 <- state_summary_stats %>%
  group_by(state) %>%
  arrange(state, desc(vote_pct50)) %>%
  dplyr::slice(3) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state == "Bayern" ~ "CSU",
                                party == "cdu" & state != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_3"), .cols = -state)

state_summary_stats_4 <- state_summary_stats %>%
  group_by(state) %>%
  arrange(state, desc(vote_pct50)) %>%
  dplyr::slice(4) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state == "Bayern" ~ "CSU",
                                party == "cdu" & state != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_4"), .cols = -state)

state_summary_stats_5 <- state_summary_stats %>%
  group_by(state) %>%
  arrange(state, desc(vote_pct50)) %>%
  dplyr::slice(5) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state == "Bayern" ~ "CSU",
                                party == "cdu" & state != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_5"), .cols = -state)

state_summary_stats_6 <- state_summary_stats %>%
  group_by(state) %>%
  arrange(state, desc(vote_pct50)) %>%
  dplyr::slice(6) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state == "Bayern" ~ "CSU",
                                party == "cdu" & state != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_6"), .cols = -state)

# Do the same for constituencies
constituency_key <- read_csv("data/constituency_results.csv") %>%
  filter(year == 2017) %>%
  dplyr::select(id, constituency, state_name) %>%
  distinct()

const_sims <- read_csv("output/const_sims.csv") %>%
  mutate(party = ordered(party, levels = party_order))

const_summary_stats <- const_sims %>%
  group_by(sim_id, constituency) %>%
  mutate(winner = pct == max(pct)) %>%
  group_by(constituency, party) %>%
  summarise(prob = mean(winner),
            vote_pct05 = quantile(pct, 0.05),
            vote_pct50 = median(pct),
            vote_pct95 = quantile(pct, 0.95)) %>%
  ungroup() %>%
  left_join(constituency_key, by = "constituency")

const_summary_stats_1 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob), desc(vote_pct50)) %>%
  dplyr::slice(1) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state_name == "Bayern" ~ "CSU",
                                party == "cdu" & state_name != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  rename_with(function(name) paste0(name, "_1"), .cols = -c("constituency", "id", "state_name"))

const_summary_stats_2 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob), desc(vote_pct50)) %>%
  dplyr::slice(2) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state_name == "Bayern" ~ "CSU",
                                party == "cdu" & state_name != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  dplyr::select(-state_name) %>%
  rename_with(function(name) paste0(name, "_2"), .cols = -c("constituency", "id"))

const_summary_stats_3 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob), desc(vote_pct50)) %>%
  dplyr::slice(3) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state_name == "Bayern" ~ "CSU",
                                party == "cdu" & state_name != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  dplyr::select(-state_name) %>%
  rename_with(function(name) paste0(name, "_3"), .cols = -c("constituency", "id"))

const_summary_stats_4 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob), desc(vote_pct50)) %>%
  dplyr::slice(4) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state_name == "Bayern" ~ "CSU",
                                party == "cdu" & state_name != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  dplyr::select(-state_name) %>%
  rename_with(function(name) paste0(name, "_4"), .cols = -c("constituency", "id"))

const_summary_stats_5 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob), desc(vote_pct50)) %>%
  dplyr::slice(5) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state_name == "Bayern" ~ "CSU",
                                party == "cdu" & state_name != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  dplyr::select(-state_name) %>%
  rename_with(function(name) paste0(name, "_5"), .cols = -c("constituency", "id"))

const_summary_stats_6 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob), desc(vote_pct50)) %>%
  dplyr::slice(6) %>%
  mutate(color = case_when(party == "afd" ~ "#00A0E2",
                           party == "cdu" ~ "black",
                           party == "fdp" ~ "#FEB900",
                           party == "gruene" ~ "#19A229",
                           party == "linke" ~ "#BE3075",
                           party == "spd" ~ "red"),
         party_abbr = case_when(party == "afd" ~ "AfD",
                                party == "cdu" & state_name == "Bayern" ~ "CSU",
                                party == "cdu" & state_name != "Bayern" ~ "CDU",
                                party == "fdp" ~ "FDP",
                                party == "gruene" ~ "Grüne",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD") %>% enc2utf8) %>%
  dplyr::select(-state_name) %>%
  rename_with(function(name) paste0(name, "_6"), .cols = -c("constituency", "id"))

# Read in shapefile ####
state_shp <- readOGR("data/shapes/germany_states.shp") %>%
  st_as_sf() %>%
  ms_simplify() %>%
  
  # Label-related things
  left_join(state_summary_stats_1, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_2, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_3, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_4, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_5, by = c("name_1" = "state")) %>%
  left_join(state_summary_stats_6, by = c("name_1" = "state")) %>%
  
  # Then create colors and stuff
  mutate(color = case_when(party_1 == "afd" ~ "#00A0E2",
                           party_1 == "cdu" ~ "black",
                           party_1 == "fdp" ~ "#FEB900",
                           party_1 == "gruene" ~ "#19A229",
                           party_1 == "linke" ~ "#BE3075",
                           party_1 == "spd" ~ "red"),
         alpha = sqrt((vote_pct50_1 - 0.2) / 0.2),
         english_name = case_when(is.na(varname_1) ~ name_1,
                                  varname_1 == "<Null>" ~ name_1,
                                  !is.na(varname_1) ~ varname_1),
         mouseover_label = case_when(english_name != name_1 ~ paste0(name_1, " (", english_name, ")"),
                                     english_name == name_1 ~ name_1),
         popup_label = paste0("<H4><b><u>", name_1, "</u></b></H4>
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

# The actual map
leaflet(state_shp) %>%
  addTiles() %>%
  addPolygons(color = "white", weight = 1, opacity = 1, fill = TRUE, fillColor = ~color, fillOpacity = ~alpha, label = ~mouseover_label,
              popup = ~popup_label, highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = TRUE, opacity = 1))

# Constituencies
const_shp <- readOGR("data/shapes/germany_constituencies.shp") %>%
  st_as_sf() %>%
  ms_simplify() %>%
  
  # Join on constituency summary stats
  left_join(const_summary_stats_1, by = c("WKR_NR" = "id")) %>%
  left_join(const_summary_stats_2, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_3, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_4, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_5, by = c("WKR_NR" = "id", "constituency")) %>%
  left_join(const_summary_stats_6, by = c("WKR_NR" = "id", "constituency")) %>%
  mutate(color = case_when(party_1 == "afd" ~ "#00A0E2",
                           party_1 == "cdu" ~ "black",
                           party_1 == "fdp" ~ "#FEB900",
                           party_1 == "gruene" ~ "#19A229",
                           party_1 == "linke" ~ "#BE3075",
                           party_1 == "spd" ~ "red"),
         alpha = sqrt(pmax((prob_1 - 0.4) / (1 - 0.4), 0)),
         mouseover_label = constituency,
         popup_label = paste0("<H4><b><u>", constituency, "</u><br><i>", state_name, "</i></b></H4>
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
                              <b><i>Win probability</i><b><br>
                              <font color = ", color_1, "><b>", party_abbr_1, "</b></font>", ": <font color = ", color_1, "><b>", 
                                percent(prob_1, accuracy = 1), "</b></font><br>
                              <font color = ", color_2, "><b>", party_abbr_2, "</b></font>", ": <font color = ", color_2, "><b>", 
                                percent(prob_2, accuracy = 1), "</b></font><br>
                              <font color = ", color_3, "><b>", party_abbr_3, "</b></font>", ": <font color = ", color_3, "><b>", 
                                percent(prob_3, accuracy = 1), "</b></font><br>
                              <font color = ", color_4, "><b>", party_abbr_4, "</b></font>", ": <font color = ", color_4, "><b>", 
                                percent(prob_4, accuracy = 1), "</b></font><br>
                              <font color = ", color_5, "><b>", party_abbr_5, "</b></font>", ": <font color = ", color_5, "><b>", 
                                percent(prob_5, accuracy = 1), "</b></font><br>
                              <font color = ", color_6, "><b>", party_abbr_6, "</b></font>", ": <font color = ", color_6, "><b>", 
                                percent(prob_6, accuracy = 1), "</b></font>
                              "))

leaflet(const_shp) %>%
  addTiles() %>%
  addPolygons(color = "white", weight = 1, opacity = 0, fill = TRUE, fillColor = ~color, fillOpacity = ~alpha, label = ~mouseover_label,
              popup = ~popup_label, highlightOptions = highlightOptions(color = "white", weight = 4, bringToFront = TRUE, opacity = 1))

# Write the two shapefiles (with data) as RDS objects to the Shiny app data folder
write_rds(state_shp, "shiny-app/data/state_shp.rds")
write_rds(const_shp, "shiny-app/data/const_shp.rds")

# Also write the update time for the Shiny app
write_rds(paste0(as.character(Sys.time() + as.difftime(6, units = "hours")), " CEST"), "shiny-app/data/update_time.rds")
