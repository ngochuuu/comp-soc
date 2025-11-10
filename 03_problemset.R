EMDAT <- read.csv("week 3.csv")

library(pacman)
pacman::p_load(tidyverse, skimr)
#create updated dataset with solely relevant variables
EMDAT <- EMDAT %>%
  select(Entity, Year, deaths_all_disasters, injured_all_disasters, homeless_all_disasters) %>%
  rename(deaths = deaths_all_disasters, injuries = injured_all_disasters, homelessness = homeless_all_disasters, country = Entity)

#calculate the averages
averages <- EMDAT %>%
  filter(!country %in% c("World", "Soviet Union")) %>%  # Remove "World" and "Soviet Union"
  group_by(country) %>%
  summarise(
    avg_deaths = mean(deaths, na.rm = TRUE),
    avg_injuries = mean(injuries, na.rm = TRUE),
    avg_homelessness = mean(homelessness, na.rm = TRUE)
  )
