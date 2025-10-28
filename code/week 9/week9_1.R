# ------------------------------------------------------------------------------

# Week 9 (Part I): Mapping in ggplot2
# SOCI 269 at Amherst College
# SPDX-License-Identifier: MIT
# Copyright (c) 2025 Sakeef M. Karim

# PRELIMINARIES ----------------------------------------------------------------

library(tidyverse)

# Mapping in ggplot2

library(sf)
library(sp)
library(terra)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(tidygeocoder)
library(geojsonsf)
library(tidycensus)
library(cancensus)
library(mapcan)
library(usmap)
library(usmapdata)
library(mapview)
library(leaflet)
library(leafpop)

# Misc

library(colorspace)
library(patchwork)
library(see)
library(scales)
library(ggrepel)
library(gt)

# LOADING THE DATA --------------------------------------------------------

load(url("https://github.com/sakeefkarim/soci-269-f25/raw/refs/heads/main/data/week%209/week9.RData"))

# BASIC MAPS --------------------------------------------------------------

# A Map of the World -----------------------------------------------------------

# Using rnaturalearth to generate a map of the world in seconds:

ne_countries(scale = "medium",
             returnclass = "sf") |> 
# Removing Antarctica from the map ... 
filter(!name == "Antarctica") |>  
ggplot() + 
# The workhorse geom (via sf) to produce maps -- but not our only option:
geom_sf() +
theme_map() +
labs(title = "A Map of the World")

# We can easily adjust or experiment with different map projections:

ne_countries(scale = "medium",
             returnclass = "sf") |>  
filter(!name == "Antarctica") |> 
ggplot() + 
geom_sf() +
# Changing the coordinate system
# Here, the popular Robinson projection:
coord_sf(crs = st_crs("ESRI:53030")) +
# More projections can be found here:
# https://en.wikipedia.org/wiki/List_of_map_projections
# Search for details here: https://epsg.io/?q=
# Example: orthographic, north pole
#  coord_sf(crs = st_crs("ESRI:102035")) +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "A Map of the World")

# INTRODUCING STATS ------------------------------------------------------------

# Like any other geom, we can fill in our maps based on some variables 
# embedded in our input data frame!

map_data |> 
filter(!name == "Antarctica",
       # For simplicity, isolating 2015:
       year == 2015) |>  
ggplot() + 
geom_sf(colour = "white",
        linewidth = 0.1,
        # Filling in data as a function of TFR (percentiles):
        mapping = aes(fill = ntile(fertility_rate, 100))) +
# Creating our own gradient scale:
scale_fill_gradient2(low = muted("pink"), 
                     high = muted("red")) +
coord_sf(crs = st_crs("ESRI:53030")) +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "A Map of the World",
     fill = "Fertility Rate in 2015 (Percentile)") +
theme(legend.position = "bottom",
      legend.justification = "center",
      legend.title.position = "top")

# Quick Exercise ---------------------------------------------------------------

# Play around with some of the indicators in the data. These include, but are
# not limited to ...:

# year, age_dependency (old age dependency), fertility_rate, death_rate,
# net_migration, foreign_share

# Create some new maps using some of the skills you picked up in Day 1:

# Scroll down for an example ...
















map_data |> 
filter(!name == "Antarctica") |>  
drop_na(year) |>  
ggplot() + 
geom_sf(colour = "white",
        linewidth = 0.05,
        mapping = aes(fill = death_rate)) +
scale_fill_viridis_c(option = "magma", direction = -1) +
facet_wrap(~year, ncol = 2) +
coord_sf(crs = st_crs("ESRI:53030")) +
theme_map(base_family = "Inconsolata") +
labs(title = "Mortality Around the World",
     subtitle = "1990 to 2015",
     fill = "Death Rate (per 1000)") +
theme(plot.title = element_text(size = 15,
                                face = "bold"),
      strip.text = element_text(size = 10),
      legend.position = "bottom",
      legend.justification = "right") +
guides(fill = guide_colourbar(title.position = "top"))

# A Brief Detour ----------------------------------------------------------

# Patchwork ---------------------------------------------------------------

# Here's a fun way to sneak in a new package --- patchwork. Thanks to patchwork
# (and packages like ggpubr), we can easily combine plots.

# Below, we use the brilliant patchwork library to bring two distinct plots
# together in a single graphic. 

# In the spirit of simplicity, this exercise draws on map_data to
# (once again) analyse the uneven distribution of mortality around the world.

mortality_2015_robinson <-  map_data |> 
                            # Isolating 2015, removing Antarctica:
                            filter(year == 2015, 
                                   !name == "Antarctica") |> 
                            ggplot() + 
                            geom_sf(colour = "white",
                                    linewidth = 0.05,
                                    mapping = aes(fill = death_rate)) +
                            scale_fill_viridis_c(option = "magma", 
                                                 direction = -1) +
                            # Robinson Projection
                            coord_sf(crs = st_crs("ESRI:53030")) +
                            theme_map(base_family = "IBM Plex Sans") +
                            labs(fill = "Death Rate (per 1000)") +
                            guides(fill = guide_colourbar(title.position = "top"))

mortality_2015_orthographic <- mortality_2015_robinson + 
                              # Orthographic projection
                               coord_sf(crs = st_crs("ESRI:102035"))

two_maps <- mortality_2015_robinson + 
            # Adding the two maps together:
            mortality_2015_orthographic + 
            # Adding common titles, subtitles:
            plot_annotation(title = "Mortality Around the World in 2015",
                            subtitle = "Per World Bank Estimates") +
            # "Collecting" legends to avoid duplication:
            plot_layout(guides = "collect") &
            # Adjusting base themes:
            theme(legend.position = "bottom",
                  text = element_text(family = "IBM Plex Sans"),
                  plot.title = element_text(face = "bold"))

# If plots are stored as objects, we need to explicitly call 
# them to generate a preview:

two_maps

# Zooming in on Canada ---------------------------------------------------------

# Data comes from the mapcan package:

class(province_territories_2017)


province_territories_2017 |> 
  ggplot(aes(x = long, y = lat, group = group)) +
  # Notice something?
  geom_polygon(mapping = aes(fill = population),
               colour = "white",
               linewidth = 0.2) +
  # Why'd we switch things up?
  coord_sf() +
  theme_map(base_family = "IBM Plex Sans") +
  theme(legend.position = "top") +
  scale_fill_viridis_c(labels = function(x) paste((x)/1000000, "mil"),
                       direction = -1) +
  labs(fill = "Canadian Population in 2017") +
  guides(fill = guide_colorbar(title.position = "bottom")) +
  theme(legend.key.width = unit(0.85, "cm"))

# Zooming in on London ---------------------------------------------------------

# Data on ethnicity (non-white share) in London at the Lower Super Output Area

# See london_sf

# A custom, gradient colour palette:

custom_pal <- c("#edf8fb", "#bfd3e6", "#9ebcda",
                "#8c96c6", "#8c6bb1", "#88419d", "#6e016b")

london_sf |> 
# For LSOAs with missing data, essentially ...
ggplot() +
geom_sf(aes(fill = non_white), colour = "white",
        linewidth = 0.009) +
facet_wrap(~year) +
scale_fill_gradientn(# Note the inclusion of a custom palette here:
                     colours = custom_pal,
                     #  Fill colour for LSOAs with missing data:
                     na.value = "grey80",
                     labels =  label_percent()) +
theme_map(base_family = "IBM Plex Sans") +
labs(title = "Spatial Distribution of Non-White Londoners, 2011-2021",
     subtitle = "Data from the 2011 and 2021 UK Census",
     fill = "Non-White Share") +
theme(plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(colour = "grey50", size = 12),
      strip.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title.position = "top",
      legend.key.width = unit(1, "cm"),
      legend.justification = "centre",
      legend.title = element_text(size = 12, hjust = 0.5),
      legend.text = element_text(size = 11)) 

# Zooming in even more/geocoding -----------------------------------------------

# Creating some data:

locations <- tibble(site = c("The Lyceum",
                             "McGill University"),
                    address = c("197 S Pleasant St, Amherst, MA 01002",
                                "McGill University")) |>
              geocode(address = address, 
                      method = "arcgis")

# Amherst (Point) -----------------------------------------------------------

amherst_shp |>
ggplot() +
geom_sf()

# Here, we mark the Lyceum  using geom_point()

amherst_shp |>
ggplot() +
geom_sf() +
# Notice us isolating the first row via the slice function:
geom_point(data = locations |> slice(1),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(data = locations |> slice(1),
                 mapping = aes(x = long, y = lat,
                               label = site))

# Adding some Amherst-specific colours:

amherst_shp |>
ggplot() +
# Adding Amherst colours:
geom_sf(fill = "#b7a5d3", colour = "white", linewidth = 0.05) +
geom_point(colour = "#3f1f69",
           size = 3.5,
           data = locations |> slice(1),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(family = "Inconsolata",
                 fill = "#3f1f69",
                 colour = "white",
                 size = 4.5,
                 nudge_y = 0.02,
                 data = locations |> slice(1),
                 mapping = aes(x = long, y = lat,
                               label = site)) +
theme_void(base_family = "Inconsolata") +
labs(title = "Amherst, MA") +
theme(plot.title = element_text(size = 18))


# Montreal (Homework) ----------------------------------------------------------

# canada_cd from Monica Alexander ...

mtl <- canada_cd |>
       ggplot() +
       geom_sf(colour = "grey") +
       theme_void() +
       coord_sf(xlim = c(-75, -73),
                ylim = c(45, 47))

# How can we highlight McGill University (see above for some guidance ---
# or scroll down for the answer!)







mtl +
geom_point(colour = "#ed1b2f",
           size = 3.5,
           data = locations |>slice(2),
           mapping = aes(x = long, y = lat)) +
geom_label_repel(family = "IBM Plex Sans",
                 fill = "#ed1b2f",
                 colour = "white",
                 nudge_y = 0.15,
                 data = locations |> slice(2),
                 mapping = aes(x = long, y = lat,
                               label = site))


# TIDYCENSUS -------------------------------------------------------------------

census_api_key("YOUR CENSUS KEY")

# Fetching data from the American Community Survey

# Variables in 2021:

acs_2021 <- load_variables(2021, "acs5")

# Searching for ... non-white share:

acs_2021 |>  filter(concept == "RACE",
                    str_detect(label, "White|Tota"))

# Manhattan County  -----------------------------------------------------------

ny_county <- get_acs(state = "NY",
                     county = "New York County",
                     geography = "tract",
                     variables = "B02001_002",
                     summary_var = "B02001_001",
                     geometry = TRUE,
                     year = 2021) |>  mutate(non_white_share = 1 - 
                                             estimate/summary_est)

ny_county |> 
ggplot() +
geom_sf(mapping = aes(fill = non_white_share), 
        colour = "white") +
theme_map(base_family = "IBM Plex Sans") +
scale_fill_viridis_c(labels = label_percent(),
                     na.value = "grey90") +
labs(fill = "Non-White Share", 
     title = "New York City",
     subtitle = "Manhattan") +
theme(legend.position = "bottom",
      plot.title = element_text(face = "bold",
                                size = 15),
      plot.subtitle = element_text(size = 12,
                                   margin = margin(t = -3)),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 11),
      legend.title.position = "top",
      legend.key.width = unit(0.8, "cm"),
      legend.text.position = "bottom")


# CANCENSUS --------------------------------------------------------------------

# Montreal  --------------------------------------------------------------------

set_cancensus_api_key('YOUR KEY', install = TRUE)

# Interactively find variable(s) of interest:

explore_census_vectors(dataset = "CA21")

# Want to explore total non-white share in MTL ...

# Extracting MTL's census code:

mtl <- list_census_regions(dataset = "CA21") |>  
       filter(name == "Montréal") |> 
       slice(1) |> 
       pull(1)

# Returning data for visible minority share ...

mtl_data <- get_census(dataset = "CA21",
                       regions = list(CMA = mtl),
                       vectors = "v_CA21_4875",
                       level = "CT",
                       geo_format = "sf",
                       labels = "short")

# Generating measure of VM share:

mtl_data <- mtl_data |> mutate(vm_share = v_CA21_4875/Population)

mtl_data |> 
ggplot() +
geom_sf(mapping = aes(fill = vm_share),
        colour = "white",
        linewidth = 0.01) +
theme_map(base_family = "Inconsolata") + 
scale_fill_viridis_c(option = "inferno") +
labs(title = "Grand Montréal",
     subtitle = "Data from the 2021 Canadian Census",
     fill = "Visible Minority Share") +
theme(legend.position = "top",
      plot.title = element_text(size = 15, 
                                face = "bold"))

# Feel free to adjust the plot to your liking --- and find other indicators to
# visualize using cancensus OR tidycensus.


# AN INTERACTIVE MAP ------------------------------------------------------

mapView(amherst_shp, color = "white", 
        col.regions = "#b7a5d3",
        layer.name = "Amherst, MA")

amherst_college <- tibble(site = "Amherst College",
                          address = "Amherst College") |>   
                   geocode(address = address)

amherst_sf <- st_as_sf(amherst_college, coords = c("long", "lat"),
                       # WGS84 projection:
                       crs = 4326)

mapView(amherst_sf, color = "white", 
        col.regions = "#b7a5d3",
        layer.name = "Amherst College",
        # Size of point:
        cex = 25,
        popup = popupIframe("https://www.youtube.com/embed/3X3hhL4B1Eg?si=2Ktvl0WmnLtFXduo",
                            width = 400, height = 400))