library(tidyverse)

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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
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
  arrange(id, desc(prob)) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
  rename_with(function(name) paste0(name, "_1"), .cols = -c("constituency", "id"))

const_summary_stats_2 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob)) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
  rename_with(function(name) paste0(name, "_2"), .cols = -c("constituency", "id"))

const_summary_stats_3 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob)) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
  rename_with(function(name) paste0(name, "_3"), .cols = -c("constituency", "id"))

const_summary_stats_4 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob)) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
  rename_with(function(name) paste0(name, "_4"), .cols = -c("constituency", "id"))

const_summary_stats_5 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob)) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
  rename_with(function(name) paste0(name, "_5"), .cols = -c("constituency", "id"))

const_summary_stats_6 <- const_summary_stats %>%
  group_by(constituency) %>%
  arrange(id, desc(prob)) %>%
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
                                party == "gruene" ~ "Greens",
                                party == "linke" ~ "Linke",
                                party == "spd" ~ "SPD")) %>%
  rename_with(function(name) paste0(name, "_6"), .cols = -c("constituency", "id"))