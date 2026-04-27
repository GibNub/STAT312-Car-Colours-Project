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

# Merge colours that are exceedingly rare with another similar colour
# Allows data to fit assumption of chisq test
long_format_merged <- long_format %>%
  mutate(Colour = case_when(
    Colour %in% c('Brown','Cream') ~ 'Brown/Cream',
    # Colour %in% c('Yellow','Gold') ~ 'Yellow/Gold', # Meets assumption already
    Colour %in% c('Pink','Purple') ~ 'Pink/Purple',
    TRUE ~ Colour
  )) %>%
  group_by(Colour, vehicle_source) %>%
  summarise(count = sum(count))

# Convert into contingency table where row is vehicle source and columns are colours and values are counts
cont_table <- xtabs(count ~ vehicle_source + Colour, data=long_format_merged)
chi_result <- chisq.test(cont_table)
chi_result


# Convert percentage data to long format for plotting
two_way_long <- two_way |>
  select(Colour, UC_Pct, NZ_Pct) |>
  pivot_longer(cols = c(UC_Pct, NZ_Pct),
               names_to = "Source",
               values_to = "Percentage") |>
  mutate(Source = recode(Source,
                         "UC_Pct" = "UC Vehicles",
                         "NZ_Pct" = "NZ National"))

# Grouped bar chart comparing distributions by percentage
ggplot(two_way_long, aes(x = Colour, y = Percentage, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vehicle Colour Distribution: UC Carparks vs NZ National",
       x = "Colour",
       y = "Percentage (%)",
       fill = "Source") +
  theme_minimal()


# Manual colourmap for scale_fill_manual
colour_map <- c(
  "Black"  = "grey20",
  "Blue"   = "steelblue3",
  "Brown"  = "sienna3",
  "Cream"  = "cornsilk3",
  "Gold"   = "goldenrod2",
  "Green"  = "seagreen3",
  "Grey"   = "grey60",
  "Orange" = "darkorange2",
  "Pink"   = "lightpink3",
  "Purple" = "mediumpurple3",
  "Red"    = "indianred3",
  "Silver" = "grey80",
  "White"  = "grey95",
  "Yellow" = "goldenrod1"
)


# Create dataframe to only include carpark, colour, and number of cars from UC parking spots
carpark_grouped <- carpark.all %>%
  select(c(Colour, Carpark)) %>%
  group_by(Colour, Carpark) %>%
  summarise(count = n())
#  %>% filter(Carpark %in% c('Clyde', 'Zoology', 'UCSA', 'Dovedale')) # Keep largest carparks


# Bargraph showing colour distributions between carparks
ggplot(carpark_grouped, aes(x=Carpark, y=count, fill=Colour)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() + 
  labs(title = 'Distribution of car colours between UC carparks',
       y = 'Number of cars') + 
  scale_fill_manual(values = colour_map)



