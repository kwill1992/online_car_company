# Author: Kevin Williams
# Date: October 2020
# Purpose: Get distances from GOOGLE API between two cities:

# Overview: 
# This will get distances from one city to another.
# It will then get the lat/long of each "from" and "to" city.
# Put in a data.frame and then write a .csv
# For 6 city, 10 city, and 50 city dataset.


#### Details: What this does ####
# Organize data by top50, top10, and top6.
# Cut off the rest.  Top50 doesn't cut off any.
# Using a XX number of cars moved per month, find the amount moved from a city using ratio of population.
# Create data.frame.
# From GOOGLE API, get distances for each city From/To pair.
# Then get lat/long of each To and From city.
# Save as .csv.

#### Load libraries ####
library(tidyverse)
library(ggmap)


#### import data set ####
# library(readr)
# library(tidyr)
# Read in 50 city .csv file and create Tibble DF.
# City file was created eslewhere and includes about 45 of top metro areas and a few stragglers.
cities_raw <- read_csv("my_top_50.csv")




#### Mutate Data ####
# Organize by descending Population
# Get percent of Population by city from total Population.
# If 4,000 cars bought and sold and moving per month, with a ratio by Population, calc cars by city.
num_cars_month <- 4000
#library(dplyr)
cities_mytop50 <- cities_raw %>% 
  arrange(desc(Population)) %>% # sort Tibble by Population and descending
  #slice_head(n = 11) %>% # Get only the first n rows.
  mutate(freq = Population / sum(Population)) %>% # percent of Population
  mutate(num_cars = round(freq * num_cars_month,0)) # Calc number cars moving each month

# Get top 10 by population from cities
cities_top10 <- cities_raw %>% 
  arrange(desc(Population)) %>% # sort Tibble by Population and descending
  slice_head(n = 10) %>% # Get only the first n rows.
  mutate(freq = Population / sum(Population)) %>% # percent of Population
  mutate(num_cars = round(freq * num_cars_month,0)) # Calc number cars moving each month

# Get top 6 by population form cities
cities_top6 <- cities_raw %>% 
  arrange(desc(Population)) %>% # sort Tibble by Population and descending
  slice_head(n = 6) %>% # Get only the first n rows.
  mutate(freq = Population / sum(Population)) %>% # percent of Population
  mutate(num_cars = round(freq * num_cars_month,0)) # Calc number cars moving each month

#### To Do: find cities online - maybe with lat/long? ####
# To Do: get top xxx cities by population from online webscrape
# Format data into something usable


#### link to google API ####
library(ggplot2)
library(ggmap)

# mapdist() in ggmap requires YOUR OWN Google API
# register_google(key = "YOUR KEY HERE")
# Consider NOT putting your API in your code.  If anyone sees it, they can use it.
# And your credit card is now linked to your API account with Google.


#### compute distances between cities ####
# Use mapdist function.  Default mode is "mode = driving" if nothing added.

#### Compute Distance and lat/longs for Top 6 Cities and save ####

# Create empty Tibble with 9 columns
distances <- tibble(from=character(0),to=character(0),distance=numeric(0),
                    from_population=numeric(0),from_pop_ratio=numeric(0),
                    from_num_cars=numeric(0),to_population=numeric(0),
                    to_pop_ratio=numeric(0),to_num_cars=numeric(0))
# Set from and to as vector of cities
from <- cities_top6$`City, State`
to <- cities_top6$`City, State`
# For loop to calculate distances between each city combination
x <- 1 # Set initial x value
for (i in 1:length(from)){
  for (j in 1:length(to)){
    # if (i == j){
    #   # If city == city, then skip as distance will be 0.
    #   next
    # }
    #k <- mapdist(from[i],to[j])[5]
    ### Important, mapdist returns a Tibble w/ 9 columns.
    ### The fifth value has miles.
    distances[x,1] <- from[i] # Add name of "from" city
    distances[x,2] <- to[j] # Add name of "to" city
    distances[x,3] <- mapdist(from[i],to[j])[5] # Add calcuated distance
    distances[x,4] <- cities_top6$Population[i] # Add "from" population
    distances[x,5] <- cities_top6$freq[i] # Add "from" population ratio
    distances[x,6] <- cities_top6$num_cars[i] #Add "from" number of cars moving each month
    distances[x,7] <- cities_top6$Population[j] # Add "to" population
    distances[x,8] <- cities_top6$freq[j] # Add "to" population ratio
    distances[x,9] <- cities_top6$num_cars[j] #Add "to" number of cars moving each month
    x <- x + 1 # go to next row in Tibble
  }
}

# Get lat/long of From cities and add to from of data.frame
distances <- cbind(geocode(as.character(distances$from)), distances)
# Rename lat/long columns
names(distances)[1] <- "lon.from"
names(distances)[2] <- "lat.from"
#head(distances)
distances <- cbind(geocode(as.character(distances$to)), distances)
# Rename lat/long columns
names(distances)[1] <- "lon.to"
names(distances)[2] <- "lat.to"
# Save to .csv file
write_csv(distances,"distances_top_6.csv")



#### Compute Distance for Top 10 Cities and save ####

# Create empty Tibble with 9 columns
distances <- tibble(from=character(0),to=character(0),distance=numeric(0),
                      from_population=numeric(0),from_pop_ratio=numeric(0),
                      from_num_cars=numeric(0),to_population=numeric(0),
                      to_pop_ratio=numeric(0),to_num_cars=numeric(0))
# Set from and to as vector of cities
from <- cities_top10$`City, State`
to <- cities_top10$`City, State`
# For loop to calculate distances between each city combination
x <- 1 # Set initial x value
for (i in 1:length(from)){
  for (j in 1:length(to)){
    # if (i == j){
    #   # If city == city, then skip as distance will be 0.
    #   next
    # }
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

# Get lat/long of From cities and add to from of data.frame
distances <- cbind(geocode(as.character(distances$from)), distances)
# Rename lat/long columns
names(distances)[1] <- "lon.from"
names(distances)[2] <- "lat.from"
#head(distances)
distances <- cbind(geocode(as.character(distances$to)), distances)
# Rename lat/long columns
names(distances)[1] <- "lon.to"
names(distances)[2] <- "lat.to"

# Save to .csv file
write_csv(distances,"distances_top_10.csv")


#### Compute Distance for My Top 50 Cities and save ####

# Create empty Tibble with 9 columns
distances <- tibble(from=character(0),to=character(0),distance=numeric(0),
                    from_population=numeric(0),from_pop_ratio=numeric(0),
                    from_num_cars=numeric(0),to_population=numeric(0),
                    to_pop_ratio=numeric(0),to_num_cars=numeric(0))
# set from and to as list of cities
from <- cities_mytop50$`City, State`
to <- cities_mytop50$`City, State`
# For loop to calculate distances between each city combination
x <- 1
for (i in 1:length(from)){
  for (j in 1:length(to)){
    # if (i == j){
    #   # If city == city, then skip as distance will be 0.
    #   next
    # }
    #k <- mapdist(from[i],to[j])[5]
    ### Important, mapdist returns a Tibble w/ 9 columns.
    ### The fifth value has miles.
    distances[x,1] <- from[i] # Add name of "from" city
    distances[x,2] <- to[j] # Add name of "to" city
    distances[x,3] <- mapdist(from[i],to[j])[5] # Add calcuated distance
    distances[x,4] <- cities_mytop50$Population[i] # Add "from" population
    distances[x,5] <- cities_mytop50$freq[i] # Add "from" population ratio
    distances[x,6] <- cities_mytop50$num_cars[i] #Add "from" number of cars moving each month
    distances[x,7] <- cities_mytop50$Population[j] # Add "to" population
    distances[x,8] <- cities_mytop50$freq[j] # Add "to" population ratio
    distances[x,9] <- cities_mytop50$num_cars[j] #Add "to" number of cars moving each month
    x <- x + 1 # go to next row in Tibble
  }
}

# Get lat/long of From cities and add to from of data.frame
distances <- cbind(geocode(as.character(distances$from)), distances)
# Rename lat/long columns
names(distances)[1] <- "lon.from"
names(distances)[2] <- "lat.from"
distances <- cbind(geocode(as.character(distances$to)), distances)
# Rename lat/long columns
names(distances)[1] <- "lon.to"
names(distances)[2] <- "lat.to"

# Save to .csv file
write_csv(distances,"distances_my_top_50.csv")




