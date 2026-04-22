library(tidyverse)
registered.all <- read_csv('Registered_vehicles_2025.csv')
registered.colour <- registered.all %>% 
                      filter(`Grouping variable` == 'Basic colour', 
                             `Vehicle type` == 'Passenger Car/Van') 
