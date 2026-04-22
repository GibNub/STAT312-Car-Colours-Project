library(tidyverse)

source('carpark_vehicles.R')
source('registered_vehicles.R')

# Join UC observed colour counts onto NZ registered colour counts to create a two-way table
two_way <- registered.colour |>
  left_join(carpark.colour, by = "Colour") |>
  mutate(UC_Pct = UC_Vehicles / sum(UC_Vehicles) * 100, # UC percentage for each colour
         NZ_Pct = NZ_Vehicles / sum(NZ_Vehicles) * 100) # NZ percentage for each colour

# Total count of all vehicles
sum(two_way$UC_Vehicles)
sum(two_way$NZ_Vehicles)