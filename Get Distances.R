#### import data set ####
library(readr)
library(tidyr)
# Read in .csv file and create Tibble DF.
cities <- read_csv("my_top_50.csv")


#### find cities online - maybe with lat/long? ####
# To Do: get top xxx cities by population from online webscrape


#### link to google API ####
# Load libraries first
library(ggplot2)
library(ggmap)

# mapdist() in ggmap requires YOUR OWN Google API
# register_google(key = "YOUR KEY HERE")
# Consider NOT putting your API in your code.  If anyone sees it, they can use it.
# And your credit card is now linked to your API account with Google.


#### compute distances between cities ####
# Use mapdist function.  Default mode is "mode = driving" if nothing added.


#### Get top 10 to work and save ####
# Get top 10 by population from cities
# Get percent of Population by city from total Population.
# If 4,000 cars moving per month, with a ratio by Population, calc cars by city.
num_cars_month <- 4000
library(dplyr)
cities_top10 <- cities %>% 
  arrange(desc(Population)) %>% # sort Tibble by Population and descending
  slice_head(n = 10) %>% # Get only the first n rows.
  mutate(freq = Population / sum(Population)) %>% # percent of Population
  mutate(num_cars = round(freq * num_cars_month,0)) # Calc number cars moving each month

# Create empty Tibble with 9 columns
distances <- tibble(from=character(0),to=character(0),distance=numeric(0),
                      from_population=numeric(0),from_pop_ratio=numeric(0),
                      from_num_cars=numeric(0),to_population=numeric(0),
                      to_pop_ratio=numeric(0),to_num_cars=numeric(0))
# For loop to calculate distances between each city combination
x <- 1
for (i in 1:length(from)){
  for (j in 1:length(to)){
    if (i == j){
      # If city == city, then skip as distance will be 0.
      next
    }
    #k <- mapdist(from[i],to[j])[5]
    ### Important, mapdist returns a Tibble w/ 9 columns.
    ### The fifth value has miles.
    distances[x,1] <- from[i] # Add name of "from" city
    distances[x,2] <- to[j] # Add name of "to" city
    distances[x,3] <- mapdist(from[i],to[j])[5] # Add calcuated distance
    distances[x,4] <- cities_top10$Population[i] # Add "from" population
    distances[x,5] <- cities_top10$freq[i] # Add "from" population ratio
    distances[x,6] <- cities_top10$num_cars[i] #Add "from" number of cars moving each month
    distances[x,7] <- cities_top10$Population[j] # Add "to" population
    distances[x,8] <- cities_top10$freq[j] # Add "to" population ratio
    distances[x,9] <- cities_top10$num_cars[j] #Add "to" number of cars moving each month
    x <- x + 1 # go to next row in Tibble
  }
}

# Save to .csv file
write_csv(distances,"distances_top_10.csv")


### Add 