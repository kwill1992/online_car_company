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
register_google(key = "AIzaSyBtKLkvXdQNXuGT7zaWyWc-fZgcWmvtesc")


#### compute distances between cities ####
### This code from: https://www.rdocumentation.org/packages/ggmap/versions/3.0.0/topics/mapdist
## basic usage
########################################

mapdist("waco, texas", "houston, texas")

from <- c("houston, texas", "dallas")
to <- "waco, texas"
mapdist(from, to)
mapdist(from, to, mode = "bicycling")
mapdist(from, to, mode = "walking")

# from <- c(
#   "1600 Amphitheatre Parkway, Mountain View, CA",
#   "3111 World Drive Walt Disney World, Orlando, FL"
# )
# to <- "1600 Pennsylvania Avenue, Washington DC"
# mapdist(from, to)
# 
# from <- "st lukes hospital houston texas"
# to <- "houston zoo, houston texas"
# mapdist(from, to, mode = "transit")

from <- c("houston", "houston", "dallas")
to <- c("waco, texas", "san antonio", "houston")
mapdist(from, to)


## geographic coordinates are accepted as well
########################################
# (wh <- as.numeric(geocode("the white house, dc")))
# (lm <- as.numeric(geocode("lincoln memorial washington dc")))
# mapdist(wh, lm, mode = "walking")

# }
# NOT RUN {
# }


#### Get top 10 to work and save ####
# get top 10 by population from citiies
library(dplyr)
cities_top10 <- cities %>% 
  arrange(desc(Population)) %>% 
  slice_head(n = 10)

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
# set from and to as list of cities
from <- cities_top10$`City, State`
to <- cities_top10$`City, State`
#mapdist(from[1],to[2])
# loop to get map distances
x <- 1
for (i in 1:3){
  for (j in 1:3){
    if (i == j){
      next
    }
    print (i)
    print (from[i])
    print (j)
    print (to[j])
    k <- mapdist(from[i],to[j])
    print (k)

    distances[x,1] <- from[i]
    distances[x,2] <- to[j]
    distances[x,3] <- k[5]
    x <- x + 1
  }
  
}

mapdist(distances[1,1],distances[1,2])
