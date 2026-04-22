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


# Convert data to longformat resulting in colour,vehicle_source,count
long_format <- two_way %>%
  select(Colour, NZ_Vehicles, UC_Vehicles) %>%
  pivot_longer(cols=c(NZ_Vehicles, UC_Vehicles), names_to='vehicle_source', values_to='count')

# Convert into contingency table where row is vehicle source and columns are colours and values are counts
cont_table <- xtabs(count ~ vehicle_source + Colour, data=long_format)
chi_result <- chisq.test(cont_table)
chi_result
