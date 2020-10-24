#### import data set ####


#### find cities online - maybe with lat/long? ####



#### link to google API ####



#### compute distances between cities ####
# Load libraries
library(ggplot2)
library(ggmap)

# mapdist() in ggmap requires YOUR OWN Google API
register_google(key = "AIzaSyBtKLkvXdQNXuGT7zaWyWc-fZgcWmvtesc")

### This code from: https://www.rdocumentation.org/packages/ggmap/versions/3.0.0/topics/mapdist

# NOT RUN {
# }
# NOT RUN {
requires Google API key, see ?register_google

## basic usage
########################################

mapdist("waco, texas", "houston, texas")

from <- c("houston, texas", "dallas")
to <- "waco, texas"
mapdist(from, to)
mapdist(from, to, mode = "bicycling")
mapdist(from, to, mode = "walking")

from <- c(
  "1600 Amphitheatre Parkway, Mountain View, CA",
  "3111 World Drive Walt Disney World, Orlando, FL"
)
to <- "1600 Pennsylvania Avenue, Washington DC"
mapdist(from, to)

from <- "st lukes hospital houston texas"
to <- "houston zoo, houston texas"
mapdist(from, to, mode = "transit")

from <- c("houston", "houston", "dallas")
to <- c("waco, texas", "san antonio", "houston")
mapdist(from, to)


## geographic coordinates are accepted as well
########################################
(wh <- as.numeric(geocode("the white house, dc")))
(lm <- as.numeric(geocode("lincoln memorial washington dc")))
mapdist(wh, lm, mode = "walking")

# }
# NOT RUN {
# }

