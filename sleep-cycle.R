
library(tidyverse)
library(lubridate)
library(ggthemes)
library(cowplot)
library(scales)



# Read and clean data -----------------------------------------------------

sleep <- read.csv("./data/sleepdata.csv", sep = ",") %>%
  as_tibble() %>%
  rename(start = Start,
         end = End,
         quality = Sleep.Quality,
         duration = Time.asleep..seconds.) %>%
  mutate(date = as_date(ymd_hms(end)) - 1,
         day = wday(date, label = TRUE),
         weekend = ifelse(grepl("Sat|Sun", day),"Weekend","Weekday") %>%
           as.factor(),
         bedtime = as_datetime(hour(start) + minute(start) / 60),
         rise = as_datetime(hour(end) + minute(end) / 60),
         quality = as.numeric(str_replace(quality, "\\%", "")),
         duration = dseconds(duration) / 3600) %>%
  select(date,
         day,
         weekend,
         bedtime,
         rise,
         duration,
         quality) %>%
  filter(duration != 0) 



# Last 7 days -------------------------------------------------------------

# Duration
duration_7 <- sleep %>%
  filter(date  >= today() - days(7)) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 1, colour = "cadetblue2") + 
  geom_point(shape = 16, size = 3, colour = "cadetblue2") +
  labs(x = "",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(breaks = breaks_extended(4)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_solarized_2(light = FALSE)

# Quality
quality_7 <- sleep %>%
  filter(date  >= today() - days(7)) %>%
  ggplot(aes(x = date, y = quality)) +
  geom_line(size = 1, colour = "cadetblue2") + 
  geom_point(shape = 16, size = 3, colour = "cadetblue2") +
  labs(x = "",
       y = "Quality (%)",
       title = "Sleep Quality",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_solarized_2(light = FALSE)

# Bedtime
bedtime_7 <- sleep %>%
  filter(date  >= today() - days(7)) %>%
  ggplot(aes(x = date, y = bedtime - 24 * (bedtime > 12))) +
  geom_line(size = 1, colour = "cadetblue2") + 
  geom_point(shape = 16, size = 3, colour = "cadetblue2") +
  labs(x = "",
       y = "Bedtime",
       title = "Bedtime",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = breaks_extended(3)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_solarized_2(light = FALSE)

# Plot together
plot_grid(duration_7, quality_7, bedtime_7,
          nrow = 3,
          align = "v"
)



# Last 30 days ------------------------------------------------------------

# Duration
duration_30 <-  sleep %>%
  filter(date  >= today() - days(30)) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = seq(0, 14, 2)) +
  theme_solarized_2(light = FALSE)

# Quality
quality_30 <-  sleep %>%
  filter(date  >= today() - days(30)) %>%
  ggplot(aes(x = date, y = quality)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "",
       y = "Quality (%)",
       title = "Sleep Quality",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous() +
  theme_solarized_2(light = FALSE)

# Bedtime
bedtime_30 <- sleep %>%
  filter(date  >= today() - days(30)) %>%
  ggplot(aes(x = date, y = bedtime - 24 * (bedtime > 12))) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "",
       y = "Bedtime",
       title = "Bedtime",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = breaks_extended(3)) +
  theme_solarized_2(light = FALSE)

# Plot together
plot_grid(duration_30, quality_30, bedtime_30,
          nrow = 3,
          align = "v"
)

# NOTES: Need to fix bedtime plot.



# Long term sleep duration data ------------------------------------------

# Sleep duration - all data
sleep %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 14),
                     breaks = seq(0, 14, 2)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "4 months", date_labels = "%b %y") +
  theme_solarized_2(light = FALSE)

# Sleep duration - 2020
duration_2020 <- sleep %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2021-01-01"))  %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "2020",
       y = "Duration (hours)",
       title = "Sleep Duration 2020",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 14),
                     breaks = seq(0, 14, 2)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "1 month", date_labels = "%b") +
  theme_solarized_2(light = FALSE)

# Sleep duration - 2019
duration_2019 <- sleep %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2020-01-01")) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "2019",
       y = "Duration (hours)",
       title = "Sleep Duration 2019",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 14),
                     breaks = seq(0, 14, 2)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "1 month", date_labels = "%b") +
  theme_solarized_2(light = FALSE)

# Sleep duration - 2018
duration_2018 <- sleep %>%
  filter(date >= as.Date("2018-01-01") & date <= as.Date("2019-01-01")) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "2018",
       y = "Duration (hours)",
       title = "Sleep Duration 2018",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 14),
                     breaks = seq(0, 14, 2)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "1 month", date_labels = "%b") +
  theme_solarized_2(light = FALSE)

# Plot together
plot_grid(duration_2020, duration_2019,
          nrow = 2,
          align = "v"
)

plot_grid(duration_2020, duration_2019, duration_2018,
          nrow = 3,
          align = "v"
)

duration_2020


# Long term sleep quality data --------------------------------------------

quality_2020 <- sleep %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2021-01-01"))  %>%
  ggplot(aes(x = date, y = quality)) +
  geom_line(size = 0.2, colour = "lightsteelblue1") + 
  stat_smooth(geom = "area", 
              span = 0.4, 
              alpha = 0.2,
              fill = "lightsteelblue3") +
  geom_smooth(span = 0.4,
              size = 1,
              colour = "cadetblue2",
              se = FALSE) +
  labs(x = "2020",
       y = "Duration (hours)",
       title = "Sleep Quality 2020",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 105),
                     breaks = seq(0, 100, 25)) +
  scale_x_date(expand = c(0,0),
               date_breaks = "1 month", date_labels = "%b") +
  theme_solarized_2(light = FALSE)

# Plot together
plot_grid(duration_2020, quality_2020,
          nrow = 2,
          align = "v"
)



