# us map of states and populations
library(tidyverse)
library(maps)
us_states <- map_data("state")
library(ggplot2)


us_cities <- data("us.cities")

ggplot(data = us_states, mapping = aes(x = long, y = lat,
                                       group = group)) +
  geom_polygon(fill= "white", color = "black") +
  geom_point(data = us.cities, aes( x = long, y = lat,
                                    size = pop, color = "purple", alpha = 0.5),
             inherit.aes = FALSE)
