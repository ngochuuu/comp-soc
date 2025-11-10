library(pacman)
#install necessary packages
p_load(ggplot2, dplyr, tidyr, readr)
#load data and observe format and variables
data <- read_csv("data.csv")
#generate overview of dataset and count missing data
summary(data)
sapply(data[, sapply(data, is.numeric)], sd, na.rm = TRUE) #standard deviation

#rename variables
data_selected <- data %>%
  select(entity = Entity,
         year = Year,
         with_elec = 'Number of people with access to electricity',
         without_elec = 'Number of people without access to electricity',
         with_cooking = number_with_clean_fuels_cooking,
         without_cooking = number_without_clean_fuels_cooking)

#replace NA values with 0
data_clean <- data_selected %>%
  mutate(across(c(with_cooking, without_cooking),
                ~if_else(is.na(.), 0, .)))
#create a table of percentages of the world in chronological order
data_summary <- data_clean %>%
  group_by(year) %>%
  summarise(
    total_with_elec = sum(with_elec, na.rm = TRUE),
    total_without_elec = sum(without_elec, na.rm = TRUE),
    total_with_cooking = sum(with_cooking, na.rm = TRUE),
    total_without_cooking = sum(without_cooking, na.rm = TRUE)
  ) %>%
  mutate(
    pct_with_elec = 100 * total_with_elec / (total_with_elec + total_without_elec),
    pct_without_elec = 100 * total_without_elec / (total_with_elec + total_without_elec),
    pct_with_cooking = 100 * total_with_cooking / (total_with_cooking + total_without_cooking),
    pct_without_cooking = 100 * total_without_cooking / (total_with_cooking + total_without_cooking)
  ) %>%
  arrange(year)
#create a table of percentages for each country
#find the most recent non-NA/non-zero value for each country
latest_data <- data_clean %>%
  filter(with_cooking > 0 & without_cooking > 0) %>%  # ensure both are non-zero
  group_by(entity) %>%
  filter(year == max(year)) %>%
  ungroup()
#calculate percentages for that year
country_percentages <- latest_data %>%
  mutate(
    elec_with_pct_country = 100 * with_elec / (with_elec + without_elec),
    elec_without_pct_country = 100 * without_elec / (with_elec + without_elec),
    cooking_with_pct_country = 100 * with_cooking / (with_cooking + without_cooking),
    cooking_without_pct_country = 100 * without_cooking / (with_cooking + without_cooking)
  ) %>%
  select(entity, year,
         elec_with_pct_country, elec_without_pct_country,
         cooking_with_pct_country, cooking_without_pct_country) %>%
  arrange(desc(elec_with_pct_country))

#plotting global temporal trends
ggplot(data_summary, aes(x = year)) +
  geom_line(aes(y = pct_with_elec, colour = "Electricity access")) +
  geom_line(aes(y = pct_with_cooking, colour = "Clean cooking fuels")) +
  labs(
    title = "Global Access to Electricity and Clean Cooking Fuels Over Time",
    x = "Year",
    y = "Percentage of Population with Access (%)",
    colour = "Indicator"
  ) +
  theme_minimal()

#plotting country disparities
ggplot(country_percentages, aes(x = reorder(entity, elec_with_pct_country),
                                y = elec_with_pct_country)) +
  geom_point(colour = "steelblue", alpha = 0.7, size = 2) +
  coord_flip() +
  labs(
    title = "Electricity Access Rates by Country (Most Recent Year)",
    x = "Country",
    y = "Electricity Access (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(),  # hide country names
        axis.ticks.y = element_blank())

#plotting global vs country level disparities for 2016
#filter to latest year (2016)
data_2016 <- data_clean %>%
  filter(year == 2016)

#compute global averages for 2016
global_2016 <- data_2016 %>%
  summarise(
    global_elec = 100 * sum(with_elec, na.rm = TRUE) /
      (sum(with_elec, na.rm = TRUE) + sum(without_elec, na.rm = TRUE)),
    global_cooking = 100 * sum(with_cooking, na.rm = TRUE) /
      (sum(with_cooking, na.rm = TRUE) + sum(without_cooking, na.rm = TRUE))
  )

#compute country-level percentages
country_2016 <- data_2016 %>%
  mutate(
    pct_with_elec = 100 * with_elec / (with_elec + without_elec),
    pct_with_cooking = 100 * with_cooking / (with_cooking + without_cooking)
  ) %>%
  select(entity, pct_with_elec, pct_with_cooking)

#convert to long format
plot_data <- country_2016 %>%
  pivot_longer(cols = c(pct_with_elec, pct_with_cooking),
               names_to = "indicator", values_to = "percentage") %>%
  mutate(indicator = recode(indicator,
                            "pct_with_elec" = "Electricity Access",
                            "pct_with_cooking" = "Clean Cooking Fuel"))

#plotting country and global average
ggplot(plot_data, aes(x = indicator, y = percentage, group = entity)) +
  geom_line(alpha = 0.2, colour = "grey60") +  # lines for each country
  geom_point(alpha = 0.2, colour = "grey60") +
  geom_hline(yintercept = global_2016$global_elec, colour = "blue",
             linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = global_2016$global_cooking, colour = "red",
             linewidth = 1, linetype = "dashed") +
  annotate("text", x = 1.5, y = global_2016$global_elec + 1.5,
           label = "Global Avg. Electricity", colour = "blue", size = 3.5) +
  annotate("text", x = 1.5, y = global_2016$global_cooking - 1.5,
           label = "Global Avg. Clean Fuel", colour = "red", size = 3.5) +
  labs(
    title = "Country vs. Global Access to Electricity and Clean Cooking Fuel (2016)",
    x = "",
    y = "Access (%)"
  ) +
  theme_minimal(base_size = 13)