library(tidyverse)
library(effects)

# read in data from all three years
mobility_data_2020 <- read_csv("covid_mobility_data/data/2020_US_Region_Mobility_Report.csv")
mobility_data_2021 <- read_csv("covid_mobility_data/data/2021_US_Region_Mobility_Report.csv")
mobility_data_2022 <- read_csv("covid_mobility_data/data/2022_US_Region_Mobility_Report.csv")

# add year column to each dataframe
mobility_data_2020$year <- 2020
mobility_data_2021$year <- 2021
mobility_data_2022$year <- 2022


tidy_data_2020 <- mobility_data_2020 %>%
  pivot_longer(cols = retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "place",
               values_to = "percent_change_from_baseline") %>%
  mutate(place = gsub("_percent_change_from_baseline", "", place))
tidy_data_2021 <- mobility_data_2021 %>%
  pivot_longer(cols = retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "place",
               values_to = "percent_change_from_baseline") %>%
  mutate(place = gsub("_percent_change_from_baseline", "", place))
tidy_data_2022 <- mobility_data_2022 %>%
  pivot_longer(cols = retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,
               names_to = "place",
               values_to = "percent_change_from_baseline") %>%
  mutate(place = gsub("_percent_change_from_baseline", "", place))


az_data_2020 <- tidy_data_2020 %>%
  filter(sub_region_1 == "Arizona")
az_data_2021 <- tidy_data_2021 %>%
  filter(sub_region_1 == "Arizona")
az_data_2022 <- tidy_data_2022 %>%
  filter(sub_region_1 == "Arizona")


model_2020 <- lm(percent_change_from_baseline ~ sub_region_1 + place, data = tidy_data_2020)
model_2021 <- lm(percent_change_from_baseline ~ sub_region_1 + place, data = tidy_data_2021)
model_2022 <- lm(percent_change_from_baseline ~ sub_region_1 + place, data = tidy_data_2022)
az_model_2020 <- lm(percent_change_from_baseline ~ place, data = az_data_2020)
az_model_2021 <- lm(percent_change_from_baseline ~ place, data = az_data_2021)
az_model_2022 <- lm(percent_change_from_baseline ~ place, data = az_data_2022)


us_mobility_2020 <- effect("place", model_2020) %>%
  data.frame()
us_mobility_2021 <- effect("place", model_2021) %>%
  data.frame()
us_mobility_2022 <- effect("place", model_2022) %>%
  data.frame()
az_mobility_2020 <- effect("place", az_model_2020) %>%
  data.frame()
az_mobility_2021 <- effect("place", az_model_2021) %>%
  data.frame()
az_mobility_2022 <- effect("place", az_model_2022) %>%
  data.frame()


us_mobility_2020$year <- 2020
us_mobility_2021$year <- 2021
us_mobility_2022$year <- 2022
az_mobility_2020$year <- 2020
az_mobility_2021$year <- 2021
az_mobility_2022$year <- 2022


us_mobility <- bind_rows(us_mobility_2020, us_mobility_2021, us_mobility_2022)
az_mobility <- bind_rows(az_mobility_2020, az_mobility_2021, az_mobility_2022)


# save dataframes as csv
write.csv(us_mobility,"covid_mobility_data/data/us_place_effect_mobility.csv", row.names = FALSE)
write.csv(az_mobility,"covid_mobility_data/data/az_place_effect_mobility.csv", row.names = FALSE)
