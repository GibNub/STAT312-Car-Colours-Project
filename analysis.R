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


## Chisq Testing
# Use raw data
cont_table_unmerged = xtabs(count ~ vehicle_source + Colour, data=long_format)
chi_unmerged = chisq.test(cont_table_unmerged)
chi_unmerged$expected
exp_counts_unmerged = stack(chi_unmerged$expected[2,])

# Vertical bar plot of expected counts for each vehicle colour with threshold = 5
ggplot(exp_counts_unmerged, aes(x=ind, y=values)) + 
  geom_col(aes(fill = values > 5)) + 
  geom_abline(intercept = 5, slope = 0, linetype=2, lwd=1) +
  labs(
    title = 'Expected counts of each colours for UC vehicles',
    subtitle = 'Dashed line shows minimum threshold',
    x = 'Colour',
    y = 'Expected count'
  ) + 
  theme_minimal() + 
  scale_fill_manual(values = c('indianred3', 'steelblue'))


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


## Plotting standardised residuals for UC Vehicles
# Standardised residuals for UC
results_std_residuals <- chi_result$stdres |>
  as_tibble() |>
  rename(Colour = Colour,
         Source = vehicle_source,
         Std_Residual = n) |>
  filter(Source == "UC_Vehicles")

# Horizontal bar chart of standardised residuals for UC vehicles by colour
ggplot(results_std_residuals, aes(x = reorder(Colour, Std_Residual), y = Std_Residual)) +
  geom_col(aes(fill = Std_Residual > 2 | Std_Residual < -2)) +
  coord_flip() +
  scale_fill_manual(values = c("gray", "red"), guide = "none") +
  labs(title = "Standardised Residuals for UC Vehicles by Colour",
       x = "Vehicle Colour",
       y = "Standardised Residual",
       subtitle = "Red Bars (+-2) indicate significant deviation from NZ national distribution") +
  theme_minimal()


# Show summary plot using merged colours
merged_percent <- long_format_merged %>%
  group_by(vehicle_source) %>%
  mutate(percent = count / sum(count) * 100)

# Grouped bar chart comparing distributions by percentage for merged data
ggplot(merged_percent, aes(x = Colour, y = percent, fill = vehicle_source)) +
  geom_bar(stat = "identity", position = "dodge", aes(weight = )) +
  labs(title = "Vehicle Colour Distribution: UC Carparks vs NZ National",
       subtitle = 'Colours with low expected counts are merged with another colour',
       x = "Colour",
       y = "Percentage (%)",
       fill = "Source") +
  theme_minimal()


