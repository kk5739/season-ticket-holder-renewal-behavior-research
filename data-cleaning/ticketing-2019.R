######################
### Load Libraries ###
######################
library(dplyr)
library(tidyverse)
library(jmv)
library(ggplot2)
library(readxl)
library(patchwork)

#########################
### Utility Function  ###
#########################
load_data <- function(file_path) {
  read_xlsx(file_path)
}

#####################
### Load Raw Data ###
#####################
ticketing_raw <- load_data("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Raw_Revenue Generation Study/Ticketing_2017_Present.xlsx")
ticketing_2019 <- ticketing_raw %>%
  filter(ItemName == "2019 Football Season") %>%
  select(SeatTypeName, SectionName, QtySeat, SSB_CRMSYSTEM_CONTACT_ID)

STH_2019 <- load_data("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Raw_Revenue Generation Study/STH_Info_Renewal.xlsx") %>%
  select(SSB_CRMSYSTEM_CONTACT_ID, AddressPrimaryState, renewed) %>%
  mutate(
    FL = ifelse(AddressPrimaryState == "FL", "FL", "Non-FL"),
    FL = replace_na(FL, "Non-FL")
  ) %>%
  select(-AddressPrimaryState)

#################################
### Seat Type Classification ###
#################################
seat_map <- tibble(
  SeatTypeName = c(
    "Bull Gator Deck", "Bull Gator Zone", "unknown",
    "Premium Dens", "Premium Suites",
    "Champions Club - $2850", "Champions Club - $2550", "Champions Club - $2350",
    "Touchdown Terrace - $2,500", "Touchdown Terrace - $2,200", "Touchdown Terrace - $1,900",
    "Stadium $1250", "Stadium $1100", "Stadium $750", "Stadium $500", "Stadium $350", "Stadium $250",
    "Stadium $150", "Upper Bench",
    "Stadium Mezzanine & QB Armchairs - $750", "Stadium Upper Chairbacks - $550",
    "Stadium Upper Chairbacks - $450", "Stadium Touchdown Chairbacks - $300",
    "Stadium Touchdown Chairbacks - $225",
    "Recent Graduate", "Faculty Staff", "ADA", "Portal Box"
  ),
  tier = c(rep(1, 11), rep(2, 4), rep(3, 3), rep(3, 2), rep(2, 2), rep(3, 2), rep(3, 1), rep(2, 3), rep(3, 2), rep(3, 1), rep(2, 3)),
  seat.1 = c(rep("bull gator", 2), "bull gator", "premium", "premium", rep("champs", 3), rep("terrace", 3),
             rep("stadium", 6), rep("stadium", 2), rep("chairbacks", 2), rep("chairbacks", 2),
             rep("etc", 4))
)

seat_map <- seat_map %>%
  mutate(seat.2 = gsub(".*\\$", "", SeatTypeName))

# Assign tier and seat types
classified <- ticketing_2019 %>%
  left_join(seat_map, by = "SeatTypeName") %>%
  mutate(season = "2019") %>%
  select(-SeatTypeName)

# Merge with STH Info
ticketing_2019 <- classified %>%
  left_join(STH_2019, by = "SSB_CRMSYSTEM_CONTACT_ID") %>%
  mutate(
    season = as.factor(season),
    tier = as.factor(tier)
  )

#######################
### ANOVA Analysis  ###
#######################
jmv::ANOVA(
  formula = QtySeat ~ renewed * tier * FL,
  data = ticketing_2019,
  effectSize = c("eta", "partEta", "omega"),
  modelTest = TRUE,
  homo = TRUE,
  postHoc = ~ renewed:FL:tier,
  postHocCorr = c("none"),
  postHocES = "d",
  emMeans = ~ renewed:FL:tier,
  emmTables = TRUE
)

########################
### Remove Outliers  ###
########################
ticketing_2019 <- ticketing_2019 %>%
  mutate(zQtySeat = scale(QtySeat))
ticketing_2019_no_outlier <- ticketing_2019 %>%
  filter(between(zQtySeat, -3, 3))

jmv::ANOVA(
  formula = QtySeat ~ tier * FL * renewed,
  data = ticketing_2019_no_outlier,
  effectSize = c("eta", "partEta", "omega"),
  modelTest = TRUE,
  homo = TRUE,
  postHoc = ~ FL:tier:renewed,
  postHocCorr = c("none"),
  postHocES = "d",
  emMeans = ~ tier:renewed:FL,
  emmPlots = TRUE,
  emmTables = TRUE
)

########################################
### Bonferroni Adjusted Visualization ###
########################################
summdata <- tibble(
  QtySeat = c(2.640173, 2.891549, 3.142857, 2.513228, 2.348493, 2.497467, 2.520000, 2.427063, 2.168491, 2.356049, 2.225877, 2.257649),
  tier = factor(rep(c("0_tier1", "1_tier2", "2_tier3"), each = 4)),
  FL = factor(rep(c("0_FL", "0_FL", "1_Non-FL", "1_Non-FL"), 3)),
  renewed = factor(rep(c("0_No", "1_Yes"), 6))
) %>%
  mutate(
    margin = 0.003367 * 1.96,
    LB = QtySeat - margin,
    UB = QtySeat + margin
  )

plot_tier <- function(df, title) {
  ggplot(df, aes(x = renewed, y = QtySeat, group = FL, colour = FL)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = LB, ymax = UB), width = .4, size = 1) +
    geom_line(size = 1) +
    theme_bw() +
    labs(
      title = title,
      subtitle = "Bonferroni adjusted error bars, alpha = .05",
      x = "Renewal", y = "Number of purchased seats per home game", color = "Residency"
    ) +
    scale_color_manual(labels = c("FL", "Non-FL"), values = c("blue", "red")) +
    scale_x_discrete(labels = c("0_No" = "no", "1_Yes" = "Yes")) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

(t1 <- plot_tier(filter(summdata, tier == "0_tier1"), "Tier 1")) |
  (t2 <- plot_tier(filter(summdata, tier == "1_tier2"), "Tier 2")) |
  (t3 <- plot_tier(filter(summdata, tier == "2_tier3"), "Tier 3"))

###################
### Demographics ###
###################
demo <- load_data("/Users/gyujikhan/Documents/Research-related/Project_related/2021_UAA/Data_Raw_Revenue Generation Study/DataAppends.xlsx")
count(demo, Gender)
