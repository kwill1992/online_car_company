# us map of states and populations
library(tidyverse)

library(ggplot2)

# "us.cities" in maps package contains This database is of us cities of population
# greater than about 40,000. Also included are state capitals of any 
# population size.
# "state" database produces a map of the states of the United States
# mainland generated from US De- partment of the Census data
library(maps)

us_states <- as_tibble(map_data("state"))
us_cities <- as_tibble(us.cities)

us_cities <-us_cities %>% 
  filter(country.etc != "AK") %>% 
  filter(country.etc != "HI")

ggplot(data = us_states, mapping = aes(x = long, y = lat,
                                       group = group)) +
  geom_polygon(fill= "white", color = "black") +
  geom_point(data = us_cities, aes( x = long, y = lat,
                                    size = pop, color = "purple", alpha = 0.5),
             inherit.aes = FALSE)
class(us_states)
class(us.cities)
class(state)
head(us_states)
head(us_cities)

library(totalcensus)
#us_metros <- as_tibble(data("dict_cbsa"))
us_metros <- as_tibble(dict_cbsa)
