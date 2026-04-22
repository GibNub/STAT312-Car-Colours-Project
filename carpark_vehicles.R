library(tidyverse)
carpark.all <- read.csv('carpark_data.csv')


# Remove white space
carpark.all <- carpark.all |>
  mutate(Colour = str_trim(Colour))

# Frequency table for UC vehicles
carpark.colour <- carpark.all |>
  count(Colour, name = "UC_Vehicles") |>
  arrange(Colour)