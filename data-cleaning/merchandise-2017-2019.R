# Load packages
library(tidyverse)
library(readxl)

# Load merchandise and STH data
merchandise <- read_xlsx("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Raw_Revenue Generation Study/Merchandise_2017_Present.xlsx") %>%
  select(SSB_CRMSYSTEM_CONTACT_ID, OrderDate, NetDemand, Productcategory, Productsubcategory, Qtysold)

STH_2019 <- read_xlsx("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Raw_Revenue Generation Study/STH_Info_Renewal.xlsx") %>%
  select(SSB_CRMSYSTEM_CONTACT_ID, AddressPrimaryState) %>%
  mutate(
    FL = ifelse(AddressPrimaryState == "FL", "FL", "Non-FL"),
    FL = replace_na(FL, "Non-FL")
  ) %>%
  select(-AddressPrimaryState)

# Function to generate per game data
generate_game_data <- function(dates, game, outcome, dynamics) {
  game_data <- map2_dfr(dates, seq_along(dates), function(date, day) {
    merchandise %>%
      filter(str_detect(OrderDate, date)) %>%
      select(SSB_CRMSYSTEM_CONTACT_ID, NetDemand) %>%
      group_by(SSB_CRMSYSTEM_CONTACT_ID) %>%
      summarise(NetDemand = sum(NetDemand, na.rm = TRUE), .groups = 'drop') %>%
      mutate(day = as.character(day))
  }) %>%
    mutate(
      game = as.character(game),
      outcome = outcome,
      dynamics = dynamics
    )
  return(game_data)
}

# Define game schedules per year
games_2017 <- list(
  list(dates = as.character(seq(as.Date("2017-09-16"), by = "1 day", length.out = 7)), outcome = "victory", dynamics = "close"),
  list(dates = as.character(seq(as.Date("2017-09-30"), by = "1 day", length.out = 7)), outcome = "victory", dynamics = "decisive"),
  list(dates = as.character(seq(as.Date("2017-10-07"), by = "1 day", length.out = 7)), outcome = "loss", dynamics = "close"),
  list(dates = as.character(seq(as.Date("2017-10-14"), by = "1 day", length.out = 7)), outcome = "loss", dynamics = "close"),
  list(dates = as.character(seq(as.Date("2017-11-18"), by = "1 day", length.out = 7)), outcome = "victory", dynamics = "decisive"),
  list(dates = as.character(seq(as.Date("2017-11-25"), by = "1 day", length.out = 7)), outcome = "loss", dynamics = "decisive")
)

# Repeat for each year
generate_season_data <- function(game_list, year) {
  map2_dfr(game_list, seq_along(game_list), function(game_info, i) {
    generate_game_data(game_info$dates, i, game_info$outcome, game_info$dynamics)
  }) %>%
    left_join(STH_2019, by = "SSB_CRMSYSTEM_CONTACT_ID") %>%
    mutate(season = as.character(year))
}

# Create datasets
merchandise_2017 <- generate_season_data(games_2017, 2017)
# Repeat above for 2018 and 2019 with `games_2018` and `games_2019`

# Final merge (read if precomputed)
merchandise_2017 <- read_csv("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Merchandise_2017_Present/new try/merchandise_2017_time_spending.csv")
merchandise_2018 <- read_csv("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Merchandise_2017_Present/new try/merchandise_2018_time_spending.csv")
merchandise_2019 <- read_csv("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Merchandise_2017_Present/new try/merchandise_2019_time_spending.csv")

# Combine
merchandise <- bind_rows(merchandise_2017, merchandise_2018, merchandise_2019) %>%
  mutate(dynamics = factor(dynamics, levels = c("decisive", "mid", "close")))

