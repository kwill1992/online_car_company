# Minimum county data in red from:
# https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/maps.html#county-population-data


if (! file.exists("PEP_2018_PEPANNRES.zip")) {
  download.file("http://www.stat.uiowa.edu/~luke/data/PEP_2018_PEPANNRES.zip",
                "PEP_2018_PEPANNRES.zip")
  unzip("PEP_2018_PEPANNRES.zip")
}

pep2018 <- read.csv("PEP_2018_PEPANNRES_with_ann.csv")
pepvars <- names(pep2018)
pep2018 <- read.csv("PEP_2018_PEPANNRES_with_ann.csv", stringsAsFactors = FALSE,
                    head = FALSE, skip = 2)

library(maps)
#head(county.fips)

# Basic Map Data
# Map data from the map function in package maps consists of the x and y coordinates of polygons and names for the regions.

usa <- map("state", fill = TRUE, plot = FALSE)
#plot(usa$x, usa$y, type = "n")
#polygon(usa$x, usa$y)

sum(is.na(usa$x))
## [1] 62
length(usa$names)
## [1] 63
usa$names

library(ggplot2)
gusa <- map_data("state")