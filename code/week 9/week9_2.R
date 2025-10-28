# ------------------------------------------------------------------------------

# Week 9 (Part 2): Reviewing ggplot2
# SOCI 269 at Amherst College
# SPDX-License-Identifier: MIT
# Copyright (c) 2025 Sakeef M. Karim

# PRELIMINARIES ----------------------------------------------------------------

library(tidyverse)

# Engines behind the data frames:

library(palmerpenguins)
library(gapminder)

# library(demography)
# library(covdata)
# library(cansim)
# library(WDI)

# Themes, Colour Schemes

library(ggthemes)
library(hrbrthemes)
library(see)
library(paletteer)
library(colorspace)

# Additional packages, geoms, tools

library(lemon)
library(summarytools)
library(skimr)
library(lattice)
library(tinyplot)
library(ggrepel)
library(ggtext)
library(ggdist)

# An alternative procedure
# 
# pacman::p_load(
#   ggthemes,
#   hrbrthemes,
#   see,
#   paletteer,
#   colorspace,
#   lemon,
#   summarytools,
#   skimr,
#   lattice,
#   tinyplot,
#   ggrepel,
#   ggridges,
#   ggtext,
#   ggdist
# )

# LOADING THE DATA --------------------------------------------------------

load(url("https://github.com/sakeefkarim/soci-269-f25/raw/refs/heads/main/data/week%209/week9.RData"))


# TWO FINAL EXAMPLES ------------------------------------------------------

gapminder |> 
filter(!continent == "Oceania") |> 
mutate(continent = paste(continent, "Adding a long piece of descriptive text")) |> 
ggplot(mapping = aes(x = year, y = lifeExp)) +
facet_wrap(~continent,
           labeller = labeller(continent = label_wrap_gen(20))) +
geom_point(colour = "grey90") +
stat_summary(fun.data = "mean_cl_boot",  
             colour = "red", 
             linewidth = 1,
             size = 0.5) +
labs(x = "", y = "Life Expectancy") +
theme_bw()

# CUSTOM COLOUR PALETTES --------------------------------------------------

pop_pyramid <-  can_binned_age |> 
                filter(year == 2024) |> 
                mutate(share = ifelse(gender == "Men+", -share, share)) |> 
                ggplot(aes(x = share,
                           y = age_group,
                           colour = gender,
                           fill = gender)) +
                geom_col(alpha = 0.7, 
                         colour = "white") + 
                theme_modern(base_family = "Inconsolata") + 
                labs(fill = "",  colour = "",
                     x = "Share of Canadian Population in 2024",
                     y = "Age Group") +
                scale_fill_manual(values = c("#311a4d", "#4a6f8c")) +
                scale_colour_manual(values = c("#311a4d", "#4a6f8c")) +
                scale_x_continuous(labels = ~paste0(abs(.), "%")) +
                theme(axis.title.x = element_text(margin = margin(t = 10)),
                      axis.title.y = element_text(margin = margin(r = 10),
                                                  angle = 0),
                      axis.text = element_text(size = 13),
                      legend.position = "top",
                      legend.text = element_text(size = 13))

# SAVING A PLOT -----------------------------------------------------------

ggsave(pop_pyramid,
       dpi = 400,
       bg = "white",
       device = png,
       filename = "population_pyramid.png",
       width = 10, height = 7)