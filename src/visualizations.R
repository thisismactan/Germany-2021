library(tidyverse)
library(reshape2)
library(lubridate)
library(Hmisc)

# Party colors/labels
party_colors = c("afd" = "#00A0E2", "cdu" = "black", "fdp" = "#FEB900", "gruene" = "#19A229", 
                 "linke" = "#BE3075", "other" = "gray", "spd" = "red")

party_names = c("afd" = "Alternative for Germany (AfD)",
                "cdu" = "Union parties (CDU/CSU)",
                "fdp" = "Free Democratic Party (FDP)",
                "gruene" = "Alliance 90/The Greens (Grüne)",
                "linke" = "The Left (Linke)",
                "other" = "Other",
                "spd" = "Social Democratic Party (SPD)")

party_abbr = c("afd" = "AfD", "cdu" = "Union", "fdp" = "FDP", "gruene" = "Grüne", "linke" = "Linke",
               "other" = "Other", "spd" = "SPD")

party_order = c("linke", "gruene", "spd", "fdp", "cdu", "afd")

# Coalition colors/labels
coalition_colors = c("cdu_fdp" = "black", "cdu_fdp_gruene" = "orange1", "cdu_gruene" = "brown", 
                     "cdu_spd" = "purple1", "spd_fdp_gruene" = "green4", "spd_gruene" = "red")

coalition_names = c("cdu_fdp" = "Center-right (CDU/CSU/FDP)",
                    "cdu_fdp_gruene" = "Jamaika (CDU/CSU/FDP/Grüne)",
                    "cdu_gruene" = "Halfghanistan (CDU/CSU/Grüne)",
                    "cdu_spd" = "Grand coalition (CDU/CSU/SPD)",
                    "spd_fdp_gruene" = "Traffic light (SPD/FDP/Grüne)",
                    "spd_gruene" = "Christmas (SPD/Grüne)")

coalition_abbr = c("cdu_fdp" = "Center-right", "cdu_fdp_gruene" = "Jamaika", "cdu_gruene" = "Halfghanistan", 
                   "cdu_spd" = "Grand coalition", "spd_fdp_gruene" = "Traffic light", "spd_gruene" = "Christmas")

# Polling average
polls_2021 <- read_csv("data/polls_2021.csv") %>%
  mutate(age = as.numeric(today() - median_date),
         loess_weight = n^0.25 / ifelse(spread == 1, 3, 1)) %>%
  filter(party != "other") %>%
  mutate(party = ordered(party, levels = party_order))

poll_average <- polls_2021 %>%
  mutate(weight = (age <= 45) * loess_weight / exp((age + 1)^0.5)) %>%
  group_by(party) %>%
  summarise(avg = wtd.mean(pct, weight),
            sd = sqrt(n() * wtd.var(pct, weight) / (n() - 1.5)),
            eff_n = sum(weight)^2 / sum(weight^2))

## The graph
poll_average_graph <- poll_average %>%
  ggplot(aes(x = party, y = avg, fill = party)) +
  geom_hline(yintercept = 0.05) +
  geom_col() +
  geom_errorbar(aes(ymin = avg - 1.645 * sd / eff_n, ymax = avg + 1.645 * sd / eff_n), col = "#888888") +
  geom_text(aes(y = avg + 0.008, label = scales::percent(avg, accuracy = 0.1)), size = 3) +
  scale_x_discrete(labels = party_abbr) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(axis.ticks.x = element_blank()) +
  labs(title = "2021 German federal election polling", x = "", y = "Share of second vote",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Error bars indicate 90% confidence intervals\n5% needed for party list seats")

poll_average_graph

ggsave("output/viz/poll_average_graph.png", plot = poll_average_graph,
       width = 700/100, height = 600/100, dpi = 100)

# Polling average over time
start_date <- as.Date("2021-01-07") - 4
end_date <- today()
date_sequence <- seq(start_date, end_date, by = 1)

poll_average_df_list <- vector("list", length(date_sequence))

## Loop over dates
for(i in 1:length(poll_average_df_list)) {
  suppressMessages(poll_average_df_list[[i]] <- polls_2021 %>%
    filter(end_date <= date_sequence[i]) %>%
    mutate(age = as.numeric(date_sequence[i] - median_date),
           weight = loess_weight / exp((age + 1)^0.5)) %>%
    filter(age <= 45) %>%
    group_by(party) %>%
    summarise(avg = wtd.mean(pct, weight),
              sd = sqrt(n() * wtd.var(pct, weight) / (n() - 1.5)),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(lower = avg - 1.645 * sd / eff_n,
           upper = avg + 1.645 * sd / eff_n,
           date = date_sequence[i]) %>%
    dplyr::select(date, party, avg, lower, upper))
}

poll_averages_over_time <- bind_rows(poll_average_df_list) %>%
  arrange(party, date) %>%
  melt(id.vars = c("date", "party"), value.name = "pct") %>%
  group_by(party, variable) %>%
  mutate(pct = (pct + lag(pct, 1) + lag(pct, 2) + lag(pct, 3) + lag(pct, 4)) / 5) %>%
  spread(variable, pct) %>%
  na.omit()

## The graph
poll_averages_over_time_graph <- poll_averages_over_time %>%
  ggplot(aes(x = date)) +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_point(data = polls_2021, aes(x = median_date, y = pct, col = party), alpha = 0.5, size = 1) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
  geom_line(aes(x = date, y = avg, col = party), size = 1) +
  geom_text(data = poll_averages_over_time %>% filter(date == max(date)), aes(x = today() + 3.5, y = avg, label = scales::percent(avg, accuracy = 0.1), col = party), 
            size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-01-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(name = "Party", labels = party_names, values = party_colors) +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election polling", x = "Date", y = "Share of second vote",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Averages smoothed over past 5 days\nError bands indicate 90% confidence intervals")

poll_averages_over_time_graph

ggsave("output/viz/poll_averages_over_time.png", plot = poll_averages_over_time_graph,
       width = 1600/100, height = 800/100, dpi = 100)

# Forecast stuff
state_sims <- read_csv("output/state_sims.csv") %>%
  mutate(party = ordered(party, levels = party_order))

natl_sims <- read_csv("output/natl_sims.csv") %>%
  left_join(
    state_sims %>%
      group_by(party, sim_id) %>%
      summarise(seats = sum(total_seats)) %>%
      group_by(sim_id) %>%
      mutate(total_seats = sum(seats)) %>%
      ungroup(),
    by = c("sim_id", "party")
  ) %>%
  mutate(party = ordered(party, levels = party_order),
         seat_share = seats / total_seats)

# Timelines
seat_forecast_timeline <- read_csv("output/seat_summary_stats_timeline.csv")
vote_forecast_timeline <- read_csv("output/vote_summary_stats_timeline.csv")

# Key for states
state_key <- read_csv("data/states.csv")

# Size of Bundestag
bundestag_size_graph <- natl_sims %>%
  filter(party == "afd") %>%
  ggplot(aes(x = total_seats)) +
  geom_vline(xintercept = 709, size = 1) +
  geom_text(data = tibble(x = 742, y = 0.018, label = "Current Bundestag: 709 seats"),
            aes(x = x, y = y, label = label), size = 3) +
  geom_histogram(aes(y = ..density..), binwidth = 5, col = "black") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "Projected Bundestag size", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

bundestag_size_graph

ggsave("output/viz/bundestag_size.png", bundestag_size_graph,
       width = 700/100, height = 600/100, dpi = 100)

# Forecasted vote
forecasted_natl_vote_graph <- natl_sims %>%
  ggplot(aes(x = pct, y = ..count.. / 10000, fill = party)) +
  facet_wrap(~party, labeller = labeller(party = party_names), nrow = 2, scales = "free_x") +
  geom_vline(xintercept = 0.05) +
  geom_histogram(binwidth = 0.01, alpha = 0.5, show.legend = FALSE) +
  geom_vline(data = natl_sims %>% 
               group_by(party) %>%
               summarise(avg_pct = mean(pct)),
             aes(xintercept = avg_pct, col = party), size = 1, show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(name = "Party", values = party_colors) +
  scale_fill_manual(name = "Party", values = party_colors) +
  labs(title = "2021 German federal election forecasted vote share", x = "Share of second vote", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "5% of nationwide second vote needed for party list seats")

forecasted_natl_vote_graph

ggsave("output/viz/forecasted_natl_vote_graph.png", plot = forecasted_natl_vote_graph,
       width = 800/100, height = 600/100, dpi = 100)

# Forecasted national vote share
forecasted_natl_vote_graph <- natl_sims %>%
  ggplot(aes(x = seats, y = ..count.. / 10000, fill = party)) +
  facet_wrap(~party, labeller = labeller(party = party_names), nrow = 2) +
  geom_histogram(binwidth = 2, alpha = 0.5, show.legend = FALSE) +
  geom_vline(data = natl_sims %>% 
               group_by(party) %>%
               summarise(avg_pct = mean(seats)),
             aes(xintercept = avg_seats, col = party), size = 1, show.legend = FALSE) +
  scale_x_continuous(breaks = 50 * (0:7)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_colour_manual(name = "Party", values = party_colors) +
  scale_fill_manual(name = "Party", values = party_colors) +
  labs(title = "2021 German federal election forecasted seats", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

forecasted_natl_seats_graph

# Forecasted national seats
forecasted_natl_seats_graph <- natl_sims %>%
  ggplot(aes(x = seats, y = ..count.. / 10000, fill = party)) +
  facet_wrap(~party, labeller = labeller(party = party_names), nrow = 2, scales = "free_x") +
  geom_histogram(binwidth = 2, alpha = 0.5, show.legend = FALSE) +
  geom_vline(data = natl_sims %>% 
               group_by(party) %>%
               summarise(avg_seats = median(seats)),
             aes(xintercept = avg_seats, col = party), size = 1, show.legend = FALSE) +
  scale_x_continuous(breaks = 50 * (0:7), limits = c(-1, 300)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_colour_manual(name = "Party", values = party_colors) +
  scale_fill_manual(name = "Party", values = party_colors) +
  labs(title = "2021 German federal election forecasted seats", x = "Seats", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

forecasted_natl_seats_graph

ggsave("output/viz/forecasted_natl_seats_graph.png", plot = forecasted_natl_seats_graph,
       width = 900/100, height = 600/100, dpi = 100)

# Forecasted national seat share
forecasted_natl_seat_share_graph <- natl_sims %>%
  ggplot(aes(x = seat_share, y = ..count.. / 10000, fill = party)) +
  facet_wrap(~party, labeller = labeller(party = party_names), nrow = 2, scales = "free_x") +
  geom_histogram(binwidth = 0.01, alpha = 0.5, show.legend = FALSE) +
  geom_vline(data = natl_sims %>% 
               group_by(party) %>%
               summarise(avg_seat_share = median(seat_share)),
             aes(xintercept = avg_seat_share, col = party), size = 1, show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = (0:10) / 10,
                     limits = c(-0.02, 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(name = "Party", values = party_colors) +
  scale_fill_manual(name = "Party", values = party_colors) +
  labs(title = "2021 German federal election forecasted seat share", x = "Share of seats in Bundestag", y = "Probability",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())))

forecasted_natl_seat_share_graph

ggsave("output/viz/forecasted_natl_seat_share_graph.png", plot = forecasted_natl_seats_graph,
       width = 900/100, height = 600/100, dpi = 100)

# National vote forecast timeline
national_vote_forecast_timeline <- vote_forecast_timeline %>%
  filter(state == "National") %>%
  mutate(party = ordered(party, levels = party_order),
         radius = 0.5 * (mean - pct_05) + 0.5 * (pct_95 - mean)) %>%
  ggplot(aes(x = date, fill = party)) +
  facet_wrap(~party, labeller = labeller(party = party_names), nrow = 2) +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_ribbon(aes(ymin = pct_05, ymax = pct_95), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(y = mean, col = party), size = 1, show.legend = FALSE) +
  geom_text(data = vote_forecast_timeline %>%
              filter(state == "National", date == max(date)) %>%
              mutate(party = ordered(party, levels = party_order),
                     radius = 0.5 * (mean - pct_05) + 0.5 * (pct_95 - mean),
                     pct_label = paste0(scales::percent(mean, accuracy = 0.1), "±", scales::number(round(100 * radius, 1), accuracy = 0.1), " pp")),
            aes(x = today() + 15, y = mean, label = pct_label, col = party), size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-03-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  scale_colour_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election forecasted national second vote over time", x = "Date", y = "Seats",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Lines indicate average share of nationwide second vote\nError bands indicate 90% confidence intervals")

national_vote_forecast_timeline

ggsave("output/viz/national_vote_forecast_timeline.png", national_vote_forecast_timeline,
       width = 1920/100, height = 1000/100, dpi = 100)

# Vote ant farm
vote_ant_farm <- vote_forecast_timeline %>%
  filter(state != "National") %>%
  mutate(party = ordered(party, levels = party_order),
         radius = 0.5 * (mean - pct_05) + 0.5 * (pct_95 - mean)) %>%
  ggplot(aes(x = date, fill = party)) +
  facet_wrap(~state, nrow = 4) +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_ribbon(aes(ymin = pct_05, ymax = pct_95), alpha = 0.5) +
  geom_line(aes(y = mean, col = party), size = 1) +
  geom_text(data = vote_forecast_timeline %>%
              filter(state != "National", date == max(date)) %>%
              mutate(party = ordered(party, levels = party_order),
                     radius = 0.5 * (mean - pct_05) + 0.5 * (pct_95 - mean),
                     pct_label = paste0(scales::percent(mean, accuracy = 0.1), "±", scales::number(round(100 * radius, 1), accuracy = 0.1), " pp")),
            aes(x = today() + 21, y = mean, label = pct_label, col = party), size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-03-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  scale_colour_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election forecasted second vote over time", x = "Date", y = "Seats", subtitle = "By state",
       caption = "Lines indicate average share of nationwide second vote\nError bands indicate 90% confidence intervals")

vote_ant_farm

ggsave("output/viz/vote_ant_farm.png", vote_ant_farm,
       width = 1920/100, height = 1000/100, dpi = 100)

# National seat forecast timeline
national_seat_forecast_timeline_graph <- seat_forecast_timeline %>%
  filter(coalition %in% c("afd", "cdu", "fdp", "gruene", "linke", "spd"), state == "National") %>%
  mutate(party = ordered(coalition, levels = party_order),
         radius = 0.5 * (pct_50 - pct_05) + 0.5 * (pct_95 - pct_50)) %>%
  ggplot(aes(x = date, fill = party)) +
  facet_wrap(~party, labeller = labeller(party = party_names), nrow = 2) +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_ribbon(aes(ymin = pct_05, ymax = pct_95), alpha = 0.5, show.legend = FALSE) +
  geom_line(aes(y = pct_50, col = party), size = 1, show.legend = FALSE) +
  geom_text(data = seat_forecast_timeline %>%
              filter(coalition %in% c("afd", "cdu", "fdp", "gruene", "linke", "spd"), state == "National", date == max(date)) %>%
              mutate(party = ordered(coalition, levels = party_order),
                     radius = 0.5 * (pct_50 - pct_05) + 0.5 * (pct_95 - pct_50),
                     seats_label = paste0(pct_50, "±", round(radius), " seats")),
            aes(x = today() + 15, y = pct_50, label = seats_label, col = party), size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-03-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  scale_colour_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election forecasted seats over time", x = "Date", y = "Seats",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Lines indicate forecast median seats\nError bands indicate 90% confidence intervals") +
  lims(y = c(0, 300))

national_seat_forecast_timeline_graph

ggsave("output/viz/national_seat_forecast_timeline.png", plot = national_seat_forecast_timeline_graph,
       width = 1920/100, height = 1000/100, dpi = 100)

# Seat ant farm
seat_ant_farm <- seat_forecast_timeline %>%
  filter(coalition %in% c("afd", "cdu", "fdp", "gruene", "linke", "spd"), state != "National") %>%
  mutate(party = ordered(coalition, levels = party_order),
         radius = 0.5 * (pct_50 - pct_05) + 0.5 * (pct_95 - pct_50)) %>%
  ggplot(aes(x = date, fill = party)) +
  facet_wrap(~state, nrow = 4, scales = "free_y") +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_ribbon(aes(ymin = pct_05, ymax = pct_95), alpha = 0.5) +
  geom_line(aes(y = pct_50, col = party), size = 1) +
  geom_text(data = seat_forecast_timeline %>%
              filter(coalition %in% c("afd", "cdu", "fdp", "gruene", "linke", "spd"), state != "National", date == max(date)) %>%
              mutate(party = ordered(coalition, levels = party_order),
                     radius = 0.5 * (pct_50 - pct_05) + 0.5 * (pct_95 - pct_50),
                     seats_label = paste0(pct_50, "±", round(radius), " seats")),
            aes(x = today() + 19, y = pct_50, label = seats_label, col = party), size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-03-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  scale_colour_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election forecasted seats over time", x = "Date", y = "Seats",
       subtitle = "By state", caption = "Lines indicate forecast median seats\nError bands indicate 90% confidence intervals")

seat_ant_farm

ggsave("output/viz/seat_ant_farm.png", plot = seat_ant_farm, 
       width = 1920/100, height = 1000/100, dpi = 100)

# Regional ant farm
regional_seat_ant_farm <- seat_forecast_timeline %>%
  filter(coalition %in% c("afd", "cdu", "fdp", "gruene", "linke", "spd")) %>%
  inner_join(state_key %>% dplyr::select(state = german_name, region), by = "state") %>%
  group_by(date, region, coalition) %>%
  summarise(pct_05 = sum(pct_05),
            pct_50 = sum(pct_50),
            pct_95 = sum(pct_95)) %>%
  mutate(party = ordered(coalition, levels = party_order),
         radius = 0.5 * (pct_50 - pct_05) + 0.5 * (pct_95 - pct_50)) %>%
  ggplot(aes(x = date, fill = party)) +
  facet_wrap(~region, nrow = 2) +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_ribbon(aes(ymin = pct_05, ymax = pct_95), alpha = 0.5) +
  geom_line(aes(y = pct_50, col = party), size = 1) +
  geom_text(data = seat_forecast_timeline %>%
              filter(coalition %in% c("afd", "cdu", "fdp", "gruene", "linke", "spd"), date == max(date)) %>%
              inner_join(state_key %>% dplyr::select(state = german_name, region), by = "state") %>%
              group_by(date, region, coalition) %>%
              summarise(pct_05 = sum(pct_05),
                        pct_50 = sum(pct_50),
                        pct_95 = sum(pct_95)) %>%
              mutate(party = ordered(coalition, levels = party_order),
                     radius = 0.5 * (pct_50 - pct_05) + 0.5 * (pct_95 - pct_50),
                     seats_label = paste0(pct_50, "±", round(radius), " seats")),
            aes(x = today() + 9, y = pct_50, label = seats_label, col = party), size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-03-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_y_continuous(breaks = (0:5) * 20) +
  scale_fill_manual(name = "Party", labels = party_names, values = party_colors) +
  scale_colour_manual(name = "Party", labels = party_names, values = party_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election forecasted seats over time", x = "Date", y = "Seats", subtitle = "By region",
       caption = "Lines indicate forecast median seats\nError bands indicate 90% confidence intervals")

regional_seat_ant_farm

ggsave("output/viz/regional_ant_farm.png", regional_seat_ant_farm,
       width = 1920/100, height = 1000/100, dpi = 100)

# Coalition probability timeline
coalition_prob_timeline <- seat_forecast_timeline %>%
  filter(state == "National", coalition %in% c("cdu_fdp", "cdu_fdp_gruene", "cdu_gruene", "cdu_spd", "spd_fdp_gruene", "spd_gruene")) %>%
  ggplot(aes(x = date, y = prob_majority, col = coalition)) +
  geom_vline(xintercept = as.Date("2021-09-26")) +
  geom_line(size = 1) +
  geom_text(data = seat_forecast_timeline %>%
              filter(state == "National", date == max(date), 
                     coalition %in% c("cdu_fdp", "cdu_fdp_gruene", "cdu_gruene", "cdu_spd", "spd_fdp_gruene", "spd_gruene")),
            aes(x = date + 3, label = scales::percent(prob_majority, accuracy = 1)), size = 3, show.legend = FALSE) +
  scale_x_date(date_breaks = "months", limits = as.Date(c("2021-03-01", "2021-10-01")), date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = (0:5) / 5) + 
  scale_colour_manual(name = "Hypothetical coalition", labels = coalition_names, values = coalition_colors) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "2021 German federal election coalition probabilities over time", x = "Date", y = "Probability of majority",
       subtitle = paste0(month(today(), label = TRUE, abbr = FALSE), " ", day(today()), ", ", year(today())),
       caption = "Probabilities need not sum to 100%")

coalition_prob_timeline

ggsave("output/viz/coalition_prob_timeline.png", coalition_prob_timeline,
       width = 1600/100, height = 800/100, dpi = 100)
