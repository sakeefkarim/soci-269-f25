# ------------------------------------------------------------------------------

# Week 8: Introduction to ggplot2 (Part I)
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

# PRELIMINARY EXAMPLE ----------------------------------------------------------

data(penguins)

skim(penguins)

#dfSummary(penguins, method = "viewer") |> view()

# Three histograms -------------------------------------------------------------

# Base R:

hist(penguins$bill_depth_mm)

# tinyplot(~ bill_depth_mm, data = penguins, type = "histogram")
# 
# tpar(family = "IBM Plex Sans")
# 
# tinyplot(~ bill_depth_mm | species, data = penguins, 
#          type = "histogram", 
#          legend = "none",
#          palette = "tableau",
#          grid = TRUE,
#          facet = ~species,
#          facet.args = list(bg = "grey90"))

# lattice

histogram(~bill_depth_mm, data = penguins)

# ggplot2

ggplot(data = penguins, mapping = aes(x = bill_depth_mm)) + 
geom_histogram()

# Question

# What are the differences between the three histograms/their underlying code?

# THE GRAMMAR OF ggplot2 -------------------------------------------------------

# AESTHETICS -------------------------------------------------------------------

?aes

skim(gapminder)

# First example: gapminder

# No mapping aesthetics:

ggplot(data = gapminder)

# + positional aesthetics:

ggplot(data = gapminder,
       mapping = aes(x = year, y = lifeExp))

# The code for this week is verbose by design, but you do not *have* to 
# spell out `mapping` and `data` each time you generate a plot. For instance, 
# you can produce the graphic by running:
  
ggplot(gapminder,
       aes(year, lifeExp))

# GEOMS ------------------------------------------------------------------------

# All the base geoms:

ls(pattern = '^geom_', env = as.environment('package:ggplot2'))

# Scatterplots -----------------------------------------------------------------

# Focus: relationship between the log of GDP per capita and life expectancy
# in gapminder. 

# To make matters easier, we'll home-in on the latest year included 
# in `gapminder` (2007). 

ggplot(# Note that we're subsetting the data within the ggplot function:
       data = gapminder |>
              filter(year == max(year)),
       # Here, we're mapping variables in our data to
       # the 'x' and 'y' positions in our plot space:
              mapping = aes(x = log(gdpPercap), y = lifeExp)) +
geom_point(# Adjusts the colour of the points:
           colour = "#002147",
           # Adjusts the size of the points:
           size = 3,
           # Adjusts the transparency of the points:
           alpha = 0.5)

# In the plot above, we added a geometric object (points or circles) to our 
# graphic by including the + geom_point() argument. 

# We can now tune or modify the aesthetic attributes of our `geom_point()` layer. 

# For instance, we can adjust `colour` within our global `aes` function to
# ensure that points are shaded pursuant to the `continent` variable in our data:
  
ggplot(data = gapminder |> 
              filter(year == max(year)),
              mapping = aes(x = log(gdpPercap), y = lifeExp,
                            # Sets colour globally --- mapping it to the
                            # `continent` variable in the data.
                            colour = continent)) +
geom_point(# Adjusts the size of the points:
           size = 3,
           # Adjusts the transparency of the points:
           alpha = 0.5)

# We can also systematically adjust the *size* of our points. Below,
# the size of the points corresponds to a country's population in 2007 (logged).

ggplot(data = gapminder |>
              filter(year == max(year)),
       mapping = aes(x = log(gdpPercap), 
                     y = lifeExp,
                     colour = continent,
                     # Sets "size" globally--mapping it to the
                     # population variable in the data.
                     size = log(pop))) +
geom_point(# Adjusts the transparency of the points:
           alpha = 0.5)

# Question

# How can we adjust the `shape` of the points in our plot to ensure that they 
# vary as a function of our `continent` variable?
  
# Scroll down for the answer:





# Answer:

ggplot(data = gapminder |> 
              filter(year == max(year)),
       mapping = aes(x = log(gdpPercap), 
                     y = lifeExp,
                     colour = continent,
                     # Include the shape attribute in your aes() call:
                     shape = continent,
                     size = log(pop))) +
geom_point(# Adjusts the transparency of the points:
           alpha = 0.5)


# Line Plots -------------------------------------------------------------------

# Let's play around with some of the other data we have at our disposal ---
# specifically, data from `select_countries`.

# Here's an overview of the data frame:

select_countries

skim(select_countries)

# Let's say we want to visualise how old-age dependency (`age_dependency`) has
# evolved over time for each of the nation-states (Canada, the United States,
# the United Kingdom, Japan) featured in our data. 

# To kick things off, let's produce a rudimentary plot that makes use of the `
# geom_line()` function.

ggplot(data = select_countries,
       aes(x = year, 
           y = age_dependency,
           # This ensures that we produce unique lines for each country.
       group = country)) +
geom_line(colour = "dodgerblue",
          linetype = "dashed")

# We've produced four unique trajectories, but we don't know what these 
# trajectories mean.

# What can we do about it?
  
# Scroll down for the answer!










# Answer

ggplot(data = select_countries,
       aes(x = year, 
           y = age_dependency, 
           # Sets colour to country:
           colour = country)) +
geom_line(linetype = "dashed")


# We can also ensure that our `linetype`s vary as a function of the `country`
# variable: 
  
ggplot(data = select_countries,
       aes(x = year,
           y = age_dependency, 
           colour = country,
           # Sets linetype to country as well:
           linetype = country)) +
geom_line()


# Question 

# How can we adjust the `linewidth` of the trajectories in our plot to ensure 
# that they vary as a function of our `fertility_rate` variable? 

# For this exercise, do not include a `linetype` argument in your `aes()` call.
# Scroll down for the answer.










# Answer

ggplot(data = select_countries,
       aes(x = year, 
           y = age_dependency, 
           colour = country,
           # Ensures that the width of the line varies by TFR:
           linewidth = fertility_rate)) +
geom_line()

# Bar Plots --------------------------------------------------------------------

# To produce a few basic bar plots in `ggplot2`, let's use the 
# `select_countries_sex` data frame. 

# Here's a look at the data frame in question.

select_countries_sex

skim(select_countries_sex)

# For now, let's begin by producing a bar plot that highlights sex differences 
# in life expectancy in Canada, the United States, the United Kingdom and Japan in the 
# year 2020.

ggplot(data = select_countries_sex |> 
              filter(year == max(year)),
       mapping = aes(x = country, y = life_expectancy,
                     # To produce different quantities along 
                     # the lines of sex:
       group = sex)) +
geom_col(# To ensure that bars are placed
         # side-by-side --- and not stacked!
         position = "dodge", 
         colour = "white") 

?position_dodge

# Here's what would happen if our `position` argument was left alone.

ggplot(select_countries_sex |> 
       filter(year == max(year)),
       aes(x = country, 
           y = life_expectancy,
           group = sex)) +
geom_col(colour = "white") 


# Now, let's clarify what the two bars (per country) actually represent. 

# How can we do this in a straightforward manner? 

# A simple approach is shown below: here, we simply change the `fill` of our 
# bars so they correspond to the `sex` variable in our input data:
  
ggplot(data = select_countries_sex |> 
              filter(year == max(year)),
       mapping = aes(x = country,
                     y = life_expectancy,
                     # Ensuring that the colour inside the bars
                     #  (the "fill") varies by sex:
                     fill = sex)) +
geom_col(position = "dodge", 
         colour = "white")

# Let's add a bit more complexity. To do so, we'll produce a graph that:

# 1. Reproduces the bar plot from above --- which illustrates sex differences in
#    life expectancy across four countries in the year 2020.

# 2. Displays the *distribution* of life expectancy for females/males across 
#    these 4 countries in the last half century (1970-2020). 

# Adding geoms -----------------------------------------------------------------

ggplot(data = select_countries_sex |> 
              filter(year == max(year)),
              mapping = aes(x = country, y = life_expectancy,
                            fill = sex)) +
geom_col(# Allows for more fine-grained control of
         # the space between bars:
         position = position_dodge(width = 0.9), 
         colour = "white") +
geom_boxplot(linewidth = 0.3,
             width = 0.35,
             # Space between boxplots constrained to
             # be equal to space between bars:
             position = position_dodge(width = 0.9),
             # Using original data frame that
             # has not been subsetted.
             data = select_countries_sex)

# What do you notice about the arguments within the `geom_boxplot()` function?

ggplot(data = select_countries_sex |> 
       filter(year == max(year)) |> 
       # Rearranging the order of the discrete 
       # y-axis labels using forcats functions:
       mutate(country = 
                       fct_rev(fct_relevel(country, 
                                           "Canada",
                                           "United States",
                                           "United Kingdom"))),
              mapping = aes(# Note the inversion of the x and y axes:
              x = life_expectancy,
              y = country,
              fill = sex)) +
geom_col(position = "dodge", 
         colour = "white") 

# STATISTICAL TRANSFORMATIONS --------------------------------------------------

# *Most* of the plots covered thus far feature an explicit mapping of variables
# to visuals. However, many of the `geom`s available in `ggplot2` feature 
# statistical transformations of the inputs to ease interpretation of the 
# relationships between variables in our data. 

# For our purposes, we'll focus on a couple of statistical transformations and a
# associated functions. To this end, we'll work with some new `geom`s that are 
# powered by `stat_*` functions under the hood.


# Smoothed Conditional Means ---------------------------------------------------

# We'll begin by visualising how the fertility rate has evolved over time in 
# Canada, the United States, the United Kingdom and Japan. 

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_line()

# The plot below uses `geom_smooth()` to simplify the same trends we encountered 
# above. 

# The function is powered by `stat_smooth()` under the hood, which (by default) 
# uses local polynomial regressions or general additive models.

ggplot(data = select_countries,
       mapping = aes(x = year, 
                     y = fertility_rate, 
                     colour = country)) +
geom_smooth(mapping = # Adjusts hue of the confidence intervals:
                      aes(fill = country),
            alpha = 0.3)

?geom_smooth

# We can adjust how we're "smoothing" our data by toggling the `method` 
# argument within `geom_smooth()`. 

# Here's how to generate estimates from a linear model (in lieu of the default
# non-parametric approaches),

ggplot(data = select_countries,
       aes(x = year,
           y = fertility_rate, 
           colour = country)) +
geom_smooth(aes(fill = country),
            method = "lm",
            alpha = 0.5)

# Density of observations ------------------------------------------------------

ggplot(data = gapminder |> 
       # Zeroing in on latest year and removing Oceania
       # which has only two observations:
             filter(year == max(year),
                   !continent == "Oceania"),
       mapping = aes(x = lifeExp,
                     # Ensuring the "fill" (colour inside the distribution) 
                     # and "colour" (the line) have the same attributes:
                     colour = continent)) +
geom_freqpoly(mapping = aes(y = after_stat(density)),
              binwidth = 5)

?stat_density

gapminder |>  
filter(year == max(year), 
       !continent == "Oceania") |> 
ggplot(aes(x = lifeExp,
           colour = continent)) +
geom_freqpoly(binwidth = 5)

# Questions:

# What are the differences between these two plots? 
# Why is the second plot misleading? What happens if we retain the 
# observations from Oceania?

# Removing the middleman:

ggplot(data = gapminder |> 
       # Zeroing in on latest year and removing Oceania
       # which has only two observations:
       filter(year == max(year),
              !continent == "Oceania"),
       mapping = aes(x = lifeExp,
                     # Ensuring the "fill" (colour inside the distribution) 
                     # and "colour" (the line) have the same attributes:
                     colour = continent,
                     fill = continent)) +
geom_density(alpha = 0.5)