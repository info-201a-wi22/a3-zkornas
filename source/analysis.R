#Load necessary packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("zoo")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zoo)

# Load data from (https://github.com/vera-institute/incarceration-trends/blob/master/incarceration_trends.csv)

prison_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# Variable 1: Out of all the counties, which has the highest incarcerated population in the current year by race

highest_black_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))
  
highest_white_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE))

highest_latinx_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(latinx_jail_pop == max(latinx_jail_pop, na.rm = TRUE))

highest_aapi_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(aapi_jail_pop == max(aapi_jail_pop, na.rm = TRUE))

highest_other_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(other_race_jail_pop == max(other_race_jail_pop, na.rm = TRUE))

# Variable 2: Out of all the counties, which has the highest incarcerated to ratio of total black vs white population?

inc_ratio_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64)) %>%
  mutate(black_jail_ratio = as.numeric(black_jail_pop/black_pop_15to64)) %>%
  mutate(white_jail_ratio = as.numeric(white_jail_pop/white_pop_15to64))

highest_ratio_black <- inc_ratio_curr %>%
  filter(black_jail_ratio == max(black_jail_ratio))

highest_ratio_white <- inc_ratio_curr %>%
  filter(white_jail_ratio == max(white_jail_ratio))

# Variable 3: In the most recent year, what is the mean ratio of the incarcerated black vs white population 

mean_ratio_black_inc <- mean(inc_ratio_curr$black_jail_ratio, na.rm = TRUE)

mean_ratio_white_inc <- mean(inc_ratio_curr$white_jail_ratio, na.rm = TRUE)

# Variable 4: Ratio of incarcerated black population compared to white population in King County, WA in most recent year (2018)

curr_king <- prison_data %>%
  filter(year == max(year)) %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(state == "WA") %>%
  filter(county_name == "King County")

ratio_black_king_total_curr <- curr_king$black_pop_15to64/curr_king$total_pop_15to64
ratio_white_king_total_curr <- curr_king$white_pop_15to64/curr_king$total_pop_15to64

ratio_black_king_jail_curr <- curr_king$black_jail_pop / curr_king$total_jail_pop
ratio_white_king_jail_curr <- curr_king$white_jail_pop / curr_king$total_jail_pop

# Variable 5: Ratio of incarcerated black population compared to white population in King County, WA in 1990

past_king <- prison_data %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64)) %>%
  filter(state == "WA") %>%
  filter(county_name == "King County") %>%
  filter(year == min(year))

ratio_black_king_total_past <- past_king$black_pop_15to64/past_king$total_pop_15to64
ratio_white_king_total_past <- past_king$white_pop_15to64/past_king$total_pop_15to64

ratio_black_king_jail_past <- past_king$black_jail_pop / past_king$total_jail_pop
ratio_white_king_jail_past <- past_king$white_jail_pop / past_king$total_jail_pop

# Chart 1: Number of incarcerated black people vs incarcerated white people in King County over time

king_inc_ratio_year <- prison_data %>%
  filter(state == "WA") %>%
  filter(county_name == "King County") %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64)) %>%

white_black_ratio_king_year <- ggplot(data = king_inc_ratio_year) +
  geom_line(aes(x = year, y = black_jail_pop), color = "blue") +
  geom_line(aes(x = year, y = white_jail_pop), color = "red")

print(white_black_ratio_king_year)

# Chart 2: Ratio of black to white population vs ratio of black to white inc population in US

black_white_ratios_us <- prison_data %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64))

