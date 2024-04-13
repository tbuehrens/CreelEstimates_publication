# Load necessary libraries
library(ggplot2)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(nhdplusTools)
library(patchwork)

# Download map data for Washington state
wa_map <- ne_states(country = 'United States of America', returnclass = 'sf') %>% 
  filter(name == 'Washington')

# Get NHD Plus Flowlines
area <- sf::st_as_sfc(sf::st_bbox(c(xmin = -121.787749, xmax = -121.384038,
                                    ymax = 48.525218, ymin = 48.246599), crs = 4326))

Rivers <- get_nhdplus(
  area,
  realization = "flowline",
  streamorder = NULL
) %>%
  st_zm() %>%
  filter(gnis_name %in% c("Skagit River", "Sauk River"))

study_area <- ggplot() + 
  geom_sf(data = wa_map, fill = "lightgray", color = "black") + # Washington state map
  geom_sf(data = Rivers, color = "blue", size = 1.5) +
  coord_sf(xlim = c(-121.787749, -121.3840383), ylim = c(48.525218, 48.246599)) + # Set map extent by lat and long
  theme_minimal() + # Minimal theme
  ylab(NULL) +
  xlab(NULL)

# Create the inset map
inset_map <- ggplot() +
  geom_sf(data = wa_map, fill = "gray") +
  geom_sf(data = Rivers, color = "blue") +
  theme_minimal() + 
  geom_sf(data = area, fill = NA, color = "black") 

wrap_plots(study_area,inset_map)


