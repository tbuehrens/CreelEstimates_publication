# Load necessary libraries
library(ggplot2)
library(rnaturalearth)
library(sf)
library(tidyverse)
library(nhdplusTools)
library(patchwork)


#load 2021 Skagit fishery points
effort_locs<-read_csv(here::here("input_files/creel_location_master_skagit_winter_steelhead_2021.csv"))%>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
  filter(location_type!="mgmt section upstream point")

fishing_bounds<-read_csv(here::here("input_files/creel_location_master_skagit_winter_steelhead_2021.csv"))%>%
  filter(location_type=="mgmt section upstream point")%>%
  bind_rows(tibble("longitude" = -121.771535, "latitude" =  48.524704, "water_body" = "Skagit River",  "location" ="Dalles Bridge" ))%>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

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
  filter(gnis_name %in% c("Skagit River","Sauk River","Suiattle River")) %>%
  st_transform( crs = st_crs(4326))%>%
  st_union()


wa_rivers <- get_nhdplus(
  wa_map,
  realization = "flowline",
  streamorder = 4
)

# Create the main map
study_area <- ggplot() + 
  #geom_sf(data = wa_map, fill = "lightgray", color = "black") + # Washington state map
  geom_sf(data = Rivers, color = "blue", size = 1.5) +
  geom_sf(data = effort_locs, aes(color = factor(water_body)),size=3) +
  geom_sf(data = fishing_bounds, color = "black",size=5, shape="X") +
  scale_color_viridis_d() +
  coord_sf(xlim = c(-121.787749, -121.3840383), ylim = c(48.525218, 48.246599)) + # Set map extent by lat and long
  theme_bw() + # Minimal theme
  ylab(NULL) +
  xlab(NULL)+
  labs(color=NULL,shape=NULL)+
  theme(legend.position = "top")
  

# Create the inset map
inset_map <- ggplot() +
  geom_sf(data = wa_map, fill = "gray") +
  geom_sf(data = wa_rivers, color = "blue") +
  theme_minimal() + 
  geom_sf(data = area, fill = NA, color = "black", lwd = 1.0)

# Combine the maps using ggplot2
p<-study_area +
  annotation_custom(
    ggplotGrob(inset_map + theme_void()),
    xmin = -121.7999, xmax = -121.57,
    ymin = 48.3, ymax = 48.4
  )
ggsave(plot=p,"map.png",width = 8,height=8,units="in")


# study_area <- ggplot() + 
#   geom_sf(data = wa_map, fill = "lightgray", color = "black") + # Washington state map
#   geom_sf(data = Rivers, color = "blue", size = 1.5) +
#   geom_sf(data=locs,aes(color=factor(location_type)))+
#   scale_color_viridis_d() +
#   coord_sf(xlim = c(-121.787749, -121.3840383), ylim = c(48.525218, 48.246599)) + # Set map extent by lat and long
#   theme_minimal() + # Minimal theme
#   ylab(NULL) +
#   xlab(NULL)
# 
# 
# # Create the inset map
# inset_map <- ggplot() +
#   geom_sf(data = wa_map, fill = "gray") +
#   geom_sf(data = wa_rivers, color = "blue") +
#   theme_minimal() + 
#   geom_sf(data = area, fill = NA, color = "black",linewidth=1.5)
# 
# wrap_plots(study_area,inset_map)

