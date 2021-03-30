# Script modified from Nguyen (2018):
# https://www.seanlnguyen.com/post/analyzing-my-sleep-data/



# Set up ------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(viridis)
library(ggridges)



# Read and clean data -----------------------------------------------------

sleep <- read.csv("./data/sleepdata.csv", sep = ";") %>%
  as_tibble() %>%
  rename(start = Start,
         end = End,
         quality = Sleep.Quality,
         duration = Time.asleep..seconds.,
         steps = Steps) %>%
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
         quality,
         steps) %>%
  filter(duration != 0) 



# Sleep quality vs duration -----------------------------------------------

sleep %>%
  ggplot(aes(x = date, y = duration, colour = quality)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(span = 0.1, colour = "purple") +
  scale_colour_viridis() +
  labs(x = "Date",
       y = "Duration (hours)",
       title = "Sleep Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  scale_y_continuous(breaks = seq(0, 14, by = 2)) +
  theme_bw()



# Sleep quality vs bedtime ------------------------------------------------

sleep %>% 
  ggplot(aes(x = date, y = bedtime, colour = quality)) +
  geom_point() +
  scale_colour_viridis() +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0,24,4),
                     limits = c(0,24.1),
                     labels = c("12 AM", "4 AM", "8 AM", "12 PM", "4 PM", 
                                "8 PM", "11:59 PM")) +
  labs(x = "Date",
       y = "Bedtime",
       title = "Bedtime and Sleep Quality",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  theme_bw()

# Need to do something about holidays and time zones.



# Sleep quality vs duration -----------------------------------------------

sleep %>% 
  ggplot(aes(x = duration, y = quality, colour = weekend)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(aes(group = 1)) +
  scale_x_continuous(breaks = seq(0,12,2)) +
  labs(x = "Duration  (hours)",
       y = "Quality (%)",
       title = "Sleep Quality vs Duration",
       caption = "Data recorded with Sleep Cycle",
       colour = "") +
  theme_bw()



# Duration vs steps -------------------------------------------------------

sleep %>% 
  ggplot(aes(x = steps, y = duration, colour = quality)) +
  geom_point(alpha = 0.6) +
  geom_smooth(colour = "Purple") +
  scale_colour_viridis() +
  scale_y_continuous(breaks = seq(0,12,2)) +
  labs(x = "Steps",
       y = "Duration (hours)",
       title = "Sleep Duration vs Steps",
       caption = "Data recorded with Sleep Cycle",
       colour = "Sleep Quality\n") +
  theme_bw()



# Steps -------------------------------------------------------------------

sleep %>% 
  ggplot(aes(x = steps, y = day, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3) +
  scale_x_continuous() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Steps\n", option = "viridis") +
  labs(x = "Steps",
       title = "Number of Steps Each Day of the Week",
       caption = "Data recorded in Sleep Cycle") +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")



# Duration by day ---------------------------------------------------------

sleep %>% 
  ggplot(aes(x = duration, y = day, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3) +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(0, 14, 2)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Duration\n", option = "viridis") +
  labs(x = "Duration",
       title = "Sleep Duration by Day of the Week",
       caption = "Data recorded in Sleep Cycle") +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")



# Sleep quality by day of the week ----------------------------------------

sleep %>% 
  ggplot(aes(x = quality, y = day, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis() +
  labs(x = "Quality (%)", 
       title = 'Sleep Quality by Day of the Week',
       caption = "Data recorded with Sleep Cycle") +
  theme_ridges(font_size = 13, grid = TRUE) + 
  theme(axis.title.y = element_blank(),
        legend.position = "none")

