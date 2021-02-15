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
  geom_point(data = polls_2021, aes(x = median_date, y = pct, col = party), alpha = 0.5) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = party), alpha = 0.2) +
  geom_line(aes(x = date, y = avg, col = party), size = 1) +
  geom_text(data = poll_average, aes(x = today() + 3.5, y = avg, label = scales::percent(avg, accuracy = 0.1), col = party), 
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

# Forecasted seats by party
