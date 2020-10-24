#### import data set ####
library(readr)
library(tidyr)
# Read in .csv file and create Tibble DF.
cities <- read_csv("my_top_50.csv")


#### find cities online - maybe with lat/long? ####



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
# get top 10 by population from cities
library(dplyr)
cities_top10 <- cities %>% 
  arrange(desc(Population)) %>% # sort Tibble by Population and descending
  slice_head(n = 10) # Get only the first n rows.

# Create empty Tibble with 3 columns
distances <- tibble(from=character(0),to=character(0),distance=numeric(0))
# set from and to as list of cities
from <- cities_top10$`City, State`
to <- cities_top10$`City, State`
# loop to get map distances
x <- 1
for (i in 1:length(from)){
  for (j in 1:length(to)){
    if (i == j){
      next
    }
    distances[x,1] <- from[i]
    distances[x,2] <- to[j]
    distances[x,3] <- i*j
    x <- x + 1
  }
  
}


distances <- tibble(from=character(0),to=character(0),distance=numeric(0))
# set 'from' and 'to' as list of cities
from <- cities_top10$`City, State`
to <- cities_top10$`City, State`
# loop to get map distances
x <- 1
for (i in 1:length(from)){
  for (j in 1:length(to)){
    if (i == j){
      # If city == city, then skip as distance will be 0.
      next
    }
    # print (i)
    # print (from[i])
    # print (j)
    # print (to[j])
    k <- mapdist(from[i],to[j])[5]
    ### Important, mapdist returns a Tibble w/ 9 columns.
    ### The fifth value has miles.
    # print (k)

    distances[x,1] <- from[i]
    distances[x,2] <- to[j]
    distances[x,3] <- k[5]
    x <- x + 1
  }
  
}


