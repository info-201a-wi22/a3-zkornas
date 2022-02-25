#Load necessary packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load data from (https://github.com/vera-institute/incarceration-trends/blob/master/incarceration_trends.csv)

prison_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


# Variable 1: Out of all the counties, which has the highest black prison population in the current year

highest_black_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))
  
highest_white_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE))

highest_black_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))

highest_black_jail_curr <- prison_data %>%
  filter(year == max(year)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))

