
library(tidyverse)
library(lubridate)
library(ggthemes)
library(cowplot)
library(scales)



# Read and clean data -----------------------------------------------------

sleep <- read.csv("./data/sleepdata.csv", sep = ";") %>%
  as_tibble() %>%
  rename(start = Start,
         end = End,
         quality = Sleep.Quality,
         duration = Time.asleep..seconds.) %>%
  mutate(date = as_date(ymd_hms(end)) - 1,
         day = wday(date, label = TRUE),
         weekend = ifelse(grepl("Sat|Sun", day),"Weekend","Weekday") %>%
           as.factor(),
         bedtime = hour(start) + minute(start) / 60,
         rise = hour(end) + minute(end) / 60,
         quality = as.numeric(str_replace(quality, "\\%", "")),
         duration = time_length(as.duration(hm(duration)) / 3600)) %>%
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
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(breaks = breaks_extended(4)) +
  theme_solarized_2(light = FALSE)

# Quality
quality_7 <- sleep %>%
  filter(date  >= today() - days(7)) %>%
  ggplot(aes(x = date, y = quality)) +
  geom_line(size = 1, colour = "cadetblue2") + 
  geom_point(shape = 16, size = 3, colour = "cadetblue2") +
  labs(x = "Date",
       y = "Quality (%)",
       title = "Sleep Quality",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = breaks_extended(3)) +
  theme_solarized_2(light = FALSE)

# Bedtime
bedtime_7 <- sleep %>%
  filter(date  >= today() - days(7)) %>%
  ggplot(aes(x = date, y = bedtime - 24 * (bedtime > 12))) +
  geom_line(size = 1, colour = "cadetblue2") + 
  geom_point(shape = 16, size = 3, colour = "cadetblue2") +
  labs(x = "Date",
       y = "Bedtime",
       title = "Bedtime",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = breaks_extended(3)) +
  theme_solarized_2(light = FALSE)



# Plot together
plot_grid(duration_7, quality_7, bedtime_7,
          nrow = 3,
          align = "v"
)


# Last month --------------------------------------------------------------

# Duration
duration_30 = sleep %>%
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
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = seq(0,14,2)) +
  theme_solarized_2(light = FALSE)

# Quality
quality_30 = sleep %>%
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
  labs(x = "Date",
       y = "Quality (%)",
       title = "Sleep Quality",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous() +
  theme_solarized_2(light = FALSE)

# Bedtime
sleep %>%
  filter(date  >= today() - days(30)) %>%
  ggplot(aes(x = date, y = bedtime - 24 * (bedtime > 12))) +
  geom_line(size = 1, colour = "cadetblue2") + 
  geom_point(shape = 16, size = 3, colour = "cadetblue2") +
  labs(x = "Date",
       y = "Bedtime",
       title = "Bedtime",
       caption = "Data recorded with Sleep Cycle") +
  scale_y_continuous(breaks = breaks_extended(3)) +
  theme_solarized_2(light = FALSE)





# Sleep quality vs duration -----------------------------------------------

# All data
sleep %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(colour = "violetred2") + 
  geom_smooth(span = 0.1, colour = "orangered", fill = "orange") +
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_minimal()

# Narrow duration - 2020
sleep %>%
  filter(date >= as.Date("2020-01-01")) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(colour = "violetred2") + 
  geom_smooth(span = 0.4, colour = "orangered", fill = "orange") +
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_minimal()

# Narrow duration - 2019
sleep %>%
  filter(date >= as.Date("2019-01-01") & date <= as.Date("2019-12-31")) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(colour = "violetred2") + 
  geom_smooth(span = 0.5, colour = "orangered", fill = "orange") +
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_minimal()

# Last week


# Last 3 months
sleep %>%
  filter(date  >= today() - days(90)) %>%
  ggplot(aes(x = date, y = duration)) +
  geom_line(size = 0.5, colour = "lightsteelblue1") + 
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
  scale_y_continuous(breaks = seq(0,12,2)) +
  theme_solarized_2(light = FALSE)

