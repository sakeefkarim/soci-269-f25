# ------------------------------------------------------------------------------

# Week 8: Introduction to ggplot2 (Part 2)
# SOCI 269 at Amherst College
# SPDX-License-Identifier: MIT
# Copyright (c) 2025 Sakeef M. Karim

# PRELIMINARIES ----------------------------------------------------------------

library(systemfonts)
library(tidyverse)

# Engines behind the data frames:

library(palmerpenguins)
library(gapminder)
library(demography)
#library(covdata)
#library(cansim)
#library(WDI)

# Themes, Schemes

library(ggthemes)
library(hrbrthemes)
library(see)

# Additional packages, geoms, tools

library(lemon)
library(summarytools)
library(skimr)
library(lattice)
library(tinyplot)

# SCALES -----------------------------------------------------------------------

# Pruning axes -----------------------------------------------------------------

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = # Adjusts hue of the confidence intervals:
            aes(fill = country),
            alpha = 0.5)  +
scale_x_continuous(# Prunes the x-axis by setting new limits:
                   # 2000-2020 instead of 1970-2020
                   limits = c(2000, 2020),
                   # How is this range sliced up? 
                   # Here, 2000 to 2020 in increments of 5 (years):
                   breaks = seq(2000, 2020, by = 5)) 

# Modifying linetype aesthetics ------------------------------------------------

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = aes(fill = country),
            alpha = 0.5)  +
scale_x_continuous(limits = c(2000, 2020),
                   breaks = seq(2000, 2020, by = 5)) +
geom_hline(mapping = aes(yintercept = 2.1, 
                         linetype = "Replacement Level Fertility"),
             # Modifying colour of line:
             colour = "grey") +
             # Adding vertical line as well
geom_vline(mapping = aes(xintercept = 2007.5,
                         linetype = "Global Recession"),
           colour = "black") +
scale_linetype_manual(name = "",
                      # Linking lines to distinct linetypes
                      values = c("Replacement Level Fertility" = "dashed", 
                                 "Global Recession" = "dotted"))

# Modifying colour/fill of geometric layers ------------------------------------

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = aes(fill = country),
            alpha = 0.5)  +
scale_x_continuous(limits = c(2000, 2020),
                   breaks = seq(2000, 2020, by = 5)) +
geom_hline(mapping = aes(yintercept = 2.1, 
                         linetype = "Replacement Level Fertility"),
           colour = "grey") +
geom_vline(mapping = aes(xintercept = 2007.5,
                         linetype = "Global Recession"),
           colour = "black") +
scale_linetype_manual(name = "",
                      values = c("Replacement Level Fertility" = "dashed", 
                                 "Global Recession" = "dotted")) +
# Using the "Dark 2" palette from the inbuilt
# colour_brewer() family of functions:
scale_colour_brewer(palette = "Dark2") +
scale_fill_brewer(palette = "Dark2")

# Working with "dates" ---------------------------------------------------------

mobility_covdata

# Data can be piped in as a part of a longer code sequence:
mobility_covdata |> 
# Isolating data from Boston:
filter(str_detect(city, "Bos")) |>  
ggplot(aes(x = date, 
           y = score, 
           colour = transportation_type, 
           fill = transportation_type)) +
geom_smooth()
 
# Using scale_x_date()

mobility_covdata |> 
filter(str_detect(city, "Bos")) |>  
ggplot(aes(x = date, 
           y = score, 
           colour = transportation_type, 
           fill = transportation_type)) +
geom_smooth() +
# Using the inbuilt viridis functions to adjust colour/fill aesthetics:
scale_colour_viridis_d(option = "inferno") +
scale_fill_viridis_d(option = "inferno") +
# Modifying how dates are displayed on the plot:
scale_x_date(# Breaks between dates:
             date_breaks = "2 months",
             # Date format --- run ?strptime for more information:
             date_labels = "%D")

?strptime

# COORDS -----------------------------------------------------------------------

ggplot(data = penguins_modified,
       mapping = aes(x = variable, 
                     y = value,
                     group = species,
                     fill = species, 
                     colour = species))  +
geom_polygon(alpha = 0.4)

# Adding coord_radar() from the see package:

ggplot(data = penguins_modified,
       mapping = aes(x = variable, 
                     y = value,
                     group = species,
                     fill = species, 
                     colour = species))  +
geom_polygon(alpha = 0.4) +
coord_radar()

# An aside: pie charts ---------------------------------------------------------

# Pie charts are popular, but should generally be avoided 
# (https://kieranhealy.org/blog/archives/2017/04/06/saying-no-to-pie/)

# That said, you may have to make one at some point. With that in mind, 
# here's a very simple example ... but beware: this snippet includes code 
# we have yet to cover.

select_countries_sex |> 
filter(year == 1980, country == "United Kingdom") |> 
mutate(label = paste0(round(pop_share), "%")) |> 
ggplot(mapping = aes(x = "", y = pop_share, fill = sex)) +
geom_bar(stat = "identity") + 
coord_polar(theta = "y") +
theme_void() +
geom_text(mapping = aes(label = label),
          colour = "white",
          position = position_stack(vjust = 0.5)) +
labs(title = "Sex Distribution in the United Kingdom (1980)") +
scale_fill_brewer(palette = "Set1")

# FACETS -----------------------------------------------------------------------

# facet_wrap() -----------------------------------------------------------------

mobility_covdata |> 
ggplot(aes(x = date, 
           y = score, 
           colour = transportation_type, 
           fill = transportation_type)) +
geom_smooth() +
scale_colour_viridis_d(option = "inferno") +
scale_fill_viridis_d(option = "inferno") +
scale_x_date(date_breaks = "2 months",
             date_labels = "%D") +
# Creating small multiples of the data:
# Here, we're conditioning on city/generating two rows of
# facets (or panels):
facet_wrap(~city, nrow = 2)

# facet_grid() -----------------------------------------------------------------

select_countries |> 
# Reorienting data (to long/"tidy" format):
pivot_longer(!c(country, year),
             names_to = "indicator",
             values_to = "value") |> 
ggplot(aes(x = year, 
           y = value, 
           colour = country, fill = country)) +
geom_smooth(alpha = 0.5) +
scale_x_continuous(breaks = seq(1970, 2020, by = 25)) +
# Creating a grid of small plots (row ~ column):
facet_grid(indicator ~ country, 
           # Ensures that both panels can have their own
           # x/y limits:
           scales = "free") +
scale_colour_brewer(palette = "Dark2") +
scale_fill_brewer(palette = "Dark2")

# ADJUSTING LABELS -------------------------------------------------------------

ggplot(gapminder |>  
       filter(year == max(year) |
              year == min(year)),
       aes(x = log(gdpPercap), y = lifeExp))  +
facet_wrap(~year, nrow = 2) +
geom_point(aes(colour = continent, size = pop), alpha = 0.65)  +
geom_smooth(colour = "black", alpha = 0.35,
            method = "lm",
            linewidth = 0.5) +
labs(# Editing x-axis title:
     x = "Log of Per Capita GDP", 
     # Editing y-axis title:
     y = "Life Expectancy in Years", 
     # Removing legend title for the colour aesthetic:
     colour = "",
     # Changing legend title for the size aesthetic:
     size = "Population") +
# Using functions within scales function to clean up labels ---
# in this case, simply adding a "+" sign
scale_size_continuous(labels = scales::label_comma(suffix = " +")) 

?label_comma

# ADJUSTING THEMES -------------------------------------------------------------

# Zeroing in on the first and last year in the gapminder df:
gapminder |> 
filter(year == max(year) |
       year == min(year)) |> 
ggplot(aes(x = log(gdpPercap), y = lifeExp))   +
facet_wrap(~year) +
geom_point(aes(colour = continent, size = pop), alpha = 0.65)  +
geom_smooth(colour = "black", alpha = 0.35,
            method = "lm",
            linewidth = 0.5) +
labs(title = "Relationship Between GDP and Life Expectancy",
     subtitle = "Over 50 Years Apart",
     x = "Log of Per Capita GDP",
     y = "Life Expectancy in Years", 
     colour = "",
     size = "Population") +
scale_colour_brewer(palette = "Dark2") +
scale_size_continuous(labels = function(x) paste(x/1000000, "mil")) +
# Using theme_bw() to modify default "look" of the plot; using the
# IBM Plex Sans plot:
theme_bw(base_family = "IBM Plex Sans") + 
theme(# Ensuring that the plot title is in boldface:
      plot.title = element_text(face = "bold"),
      # Changing the colour of the subtitle:
      plot.subtitle = element_text(colour = "grey45"),
      # Adding space to the right of the y-axis title 
      # (pushing text away from the plot panel):
      axis.title.y = element_text(margin = margin(r = 15)),
      # Adding space to the top of the x-axis title:
      axis.title.x = element_text(margin = margin(t = 15)),
      # Removing minor gridlines not linked to axis labels:
      panel.grid.minor = element_blank(),
      # Placing legend on the bottom of the plot:
      legend.position = "bottom",
      # Increasing the size of the legend keys:
      legend.key.size = unit(1, "cm"),
      # Arranging multiple legends vertically (more than one row):
      legend.box = "vertical")

# ADJUSTING GUIDES -------------------------------------------------------------

ggplot(gapminder |>  
       filter(year == max(year) |
              year == min(year)), 
       aes(x = log(gdpPercap), y = lifeExp))   +
facet_wrap(~year) +
geom_point(aes(colour = continent, 
               # Sizing plots based on log of population:
               size = log(pop)), 
           alpha = 0.65)  +
geom_smooth(colour = "black", 
            alpha = 0.35,
            method = "lm",
            linewidth = 0.5) +
labs(title = "Relationship Between GDP and Life Expectancy",
     subtitle = "Over 50 Years Apart",
     x = "Log of Per Capita GDP", 
     y = "Life Expectancy in Years", colour = "",
     size = "Log of Population") +
scale_size_binned(# Range of plot sizes:
                  range = c(0.1, 3.5),
                  labels = function(x) paste(x, "+")) +
scale_colour_brewer(palette = "Dark2") +
theme_bw(base_family = "IBM Plex Sans") + 
theme(plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey45"),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.title.x = element_text(margin = margin(t = 15)),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(1, "cm"),
      legend.box = "vertical") +
guides(size = guide_bins(# Push legend title to the bottom:
                         title.position = "bottom",
                         # Centring legend title.
                         title.hjust = 0.5)) +
guides(# Rearranging order of legends; colour now appears first.
       colour = guide_legend(order = 1,
                             # Overriding aes - all keys are 
                             # at size = 5.
                             override.aes = list(size = 5))) 
