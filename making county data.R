#### county data

### From https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/maps.html#county-population-data


# County Population Data
# The census bureau provides estimates of populations of US counties.
# 
# Estimates are available in several formats, including CSV.
# 
# The CSV file for 2018 is avaiable at http://www.stat.uiowa.edu/~luke/data/PEP_2018_PEPANNRES.zip.
# 
# For 2018 the CSV file contains an awkward second row, so the file is read twice: once to get the header, then to get the data
if (! file.exists("PEP_2018_PEPANNRES.zip")) {
  download.file("http://www.stat.uiowa.edu/~luke/data/PEP_2018_PEPANNRES.zip",
                "PEP_2018_PEPANNRES.zip")
  unzip("PEP_2018_PEPANNRES.zip")
}

pep2018 <- read.csv("PEP_2018_PEPANNRES_with_ann.csv")
pepvars <- names(pep2018)
pep2018 <- read.csv("PEP_2018_PEPANNRES_with_ann.csv", stringsAsFactors = FALSE,
                    head = FALSE, skip = 2)
names(pep2018) <- pepvars

head(pep2018)
tail(pep2018)

#Working with the county names can be tricky:
  
head(filter(pep2018, grepl(", Iowa", GEO.display.label)))

pep2018[1803,]
filter(pep2018, GEO.id2 == 19141)
filter(pep2018, GEO.id2 == 22001)

# For US counties it is safer to work with the FIPS county code, which is the GEO.id2 variable.
# 
# The county.fips data frame in the maps package links the FIPS code to region names used by the map data in the maps package.
library(maps)
head(county.fips)

# Basic Map Data
# Map data from the map function in package maps consists of the x and y coordinates of polygons and names for the regions.

usa <- map("state", fill = TRUE, plot = FALSE)
plot(usa$x, usa$y, type = "n")
polygon(usa$x, usa$y)

sum(is.na(usa$x))
## [1] 62
length(usa$names)
## [1] 63
usa$names

library(ggplot2)
gusa <- map_data("state")
head(gusa)
##        long      lat group order  region subregion
## 1 -87.46201 30.38968     1     1 alabama      <NA>
## 2 -87.48493 30.37249     1     2 alabama      <NA>
## 3 -87.52503 30.37249     1     3 alabama      <NA>
## 4 -87.53076 30.33239     1     4 alabama      <NA>
## 5 -87.57087 30.32665     1     5 alabama      <NA>
## 6 -87.58806 30.32665     1     6 alabama      <NA>
head(filter(gusa, region == "virginia"))
##        long      lat group order   region  subregion
## 1 -75.64188 37.96418    53 13482 virginia chesapeake
## 2 -75.61897 37.99856    53 13483 virginia chesapeake
## 3 -75.36114 38.02721    53 13484 virginia chesapeake
## 4 -75.39552 37.99283    53 13485 virginia chesapeake
## 5 -75.41843 37.96991    53 13486 virginia chesapeake
## 6 -75.42989 37.94127    53 13487 virginia chesapeake
p <- ggplot(gusa) + geom_polygon(aes(long, lat, group = group), color = "white")
p


# Approximate Centroids
# A quick approximation to the centroids (centers of gravity) of the polygons is to compute the center of the bounding rectangle.
# 
# This is easiest to do with the data from map_data.
library(dplyr)
state_centroids <- summarize(group_by(gusa, region),
                             x = mean(range(long)), y = mean(range(lat)))
names(state_centroids)[1] <- "state"
head(state_centroids)

# Symbol Plots of State Population
# Aggregate the county populations to the state level:
  
state_pops <- mutate(pep2018,
                     state = tolower(sub(".*, ", "", GEO.display.label)),
                     pop = respop72018)
unique(state_pops$state)

state_pops <- summarize(group_by(state_pops, state),
                        pop = sum(pop, na.rm = TRUE))
# Using tolower matches the case in the state_centroids table.
# 
# An alternative would be to use the state FIPS code and the state.fips table.
# 
# Merge in the centroid locations. Using inner_join drops cases not included in the lower-48 map data.

state_pops <- inner_join(state_pops, state_centroids, "state")

head(state_pops)


# Choropleth Maps of State Population
# A choropleth map needs to have the information for coloring all the pieces of a region.
# 
# For ggplot this can be done by merging:
  
sp <- select(state_pops, region = state, pop)
gusa_pop <- left_join(gusa, sp, "region")
head(gusa_pop)
##        long      lat group order  region subregion     pop
## 1 -87.46201 30.38968     1     1 alabama      <NA> 4887871
## 2 -87.48493 30.37249     1     2 alabama      <NA> 4887871
## 3 -87.52503 30.37249     1     3 alabama      <NA> 4887871
## 4 -87.53076 30.33239     1     4 alabama      <NA> 4887871
## 5 -87.57087 30.32665     1     5 alabama      <NA> 4887871
## 6 -87.58806 30.32665     1     6 alabama      <NA> 4887871

#A first attempt:
  
ggplot(gusa_pop) +
  geom_polygon(aes(long, lat, group = group, fill = pop)) +
  coord_map("bonne", parameters=45) +
  ggthemes::theme_map()

# This image is dominated by the fact that most state populations are small.
# 
# Showing population ranks, or percentile values, can help see the variation a bit better.
spr <- mutate(sp, rpop = rank(pop))
gusa_rpop <- left_join(gusa, spr, "region")
ggplot(gusa_rpop) +
  geom_polygon(aes(long, lat, group = group, fill = rpop)) +
  coord_map("bonne", parameters=45) +
  ggthemes::theme_map()

# Using quintile bins instead of a continuous scale:
ncls <- 6
spr <- mutate(spr,
              pcls = cut(pop, quantile(pop, seq(0, 1, len = ncls)),
                         include.lowest = TRUE))
gusa_rpop <- left_join(gusa, spr, "region")
ggplot(gusa_rpop) +
  geom_polygon(aes(long, lat, group = group, fill = pcls),
               color = "grey") +
  coord_map("bonne", parameters=45) +
  ggthemes::theme_map() +
  scale_fill_brewer(palette = "Reds")

#A percentile bin map using the map function requires a vector of colors for the regions:
  
usa_states <- sub(":.*", "", usa$names)
usa_pcls <- spr$pcls[match(usa_states, spr$region)]
pal <- RColorBrewer::brewer.pal(nlevels(usa_pcls), "Reds")
map("state", fill = TRUE, col = pal[usa_pcls], border = "grey")

#This uses the match function to find indices for each polygon’s state in the regions vector.

#Another way to compute the classes for the pieces:
  
usa_pieces <- data.frame(names = usa$names)
usa_pieces <- separate(usa_pieces, "names", c("region", "subregion"),
                       sep = ":", fill = "right")
usa_pieces <- left_join(usa_pieces, spr, "region")
map("state", fill = TRUE, col = pal[usa_pieces$pcls], border = "grey")


# Choropleth Maps of County Population
# For a county-level ggplot map, first get the polygon data frame:
library(purrr)
library(tidyr)
library(ggplot2)
gcounty <- map_data("county")
head(gcounty)

#To attach the FIPS code we first need to clean up the county.fips table a bit:
  
head(filter(county.fips, grepl(":", polyname)))

#Remove the sub-county regions, remove duplicate rows, and split the polyname variable into region and subregion:
  
fipstab <-
  transmute(county.fips, fips, county = sub(":.*", "", polyname))
fipstab <- unique(fipstab)
fipstab <-
  separate(fipstab, county, c("region", "subregion"), sep = ",")
head(fipstab)

#Now use left_join to merge the FIPS code into gcounty:
  
gcounty <- left_join(gcounty, fipstab, c("region", "subregion"))
head(gcounty)


#Pull together the data for the map:
ncls <- 6 
cpop <- select(pep2018,
               fips = GEO.id2,
               pop10 = rescen42010,
               pop18 = respop72018)
cpop <- mutate(cpop, rpop18 = rank(pop18))
cpop <- mutate(cpop,
               pcls18 = cut(pop18, quantile(pop18, seq(0, 1, len = ncls)),
                            include.lowest = TRUE))
head(cpop)

#Some of the counties in the polygon data base may not appear in the population data:
  
unique(select(filter(gcounty, ! fips %in% cpop$fips), region, subregion))
##         region subregion
## 1 south dakota   shannon
unique(select(anti_join(gcounty, cpop, "fips"), region, subregion))
##         region subregion
## 1 south dakota   shannon
#A left_join with cpop will give these NA values.

gcounty_pop <- left_join(gcounty, cpop, "fips")
unique(select(filter(gcounty_pop, is.na(rpop18)), region, subregion))
##         region subregion
## 1 south dakota   shannon
#County level population plots using the default continuous scale:

ggplot(gcounty_pop) +
  geom_polygon(aes(long, lat, group = group, fill = rpop18),
               color = "grey", size = 0.1) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA, data = gusa, color = "lightgrey") +
  coord_map("bonne", parameters=45) + ggthemes::theme_map()


#A discrete scale with a very different color to highlight the counties with missing information:
ggplot(gcounty_pop) +
  geom_polygon(aes(long, lat, group = group, fill = pcls18),
               color = "grey", size = 0.1) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA, data = gusa, color = "lightgrey") +
  coord_map("bonne", parameters=45) + ggthemes::theme_map() +
  scale_fill_brewer(palette = "Reds", na.value = "blue")


 
## add on cities
# Below is out of order right now as of 29 Oct




ggplot(gcounty_pop) +
  geom_polygon(aes(long, lat, group = group, fill = pcls18),
               color = "grey", size = 0.1) +
  geom_polygon(aes(long, lat, group = group),
               fill = NA, data = gusa, color = "lightgrey") +
  coord_map("bonne", parameters=45) + ggthemes::theme_map() +
  scale_fill_brewer(palette = "Reds", na.value = "white") +
  theme(legend.background = element_rect(fill = NA)) +
  geom_point(data = cities_ggmap, aes(x = lon, y= lat)) +

  
  
  
## using ggmap data from below  
  geom_text(data = cities_ggmap, aes(x = lon, y =lat, label = Cities)) +
  geom_segment(data = cities_ggmap, aes(x = lon[1], y = lat[1], xend = lon[2],
                                        yend = lat[2]), color = "blue", size = 0.3,
                                        arrow = arrow())
#data(world.cities)
#### get lat longs ####
library(ggmap)
register_google(key = 'your key')
# create a list of cities
cities <- c("New York, New York", "Los Angeles, California")
cities_df <- data.frame(Cities = cities, stringsAsFactors = FALSE)

# run the geocode function from ggmap package
cities_ggmap <- geocode(location = cities, output = "more", source = "google")
cities_ggmap <- cbind(cities_df, cities_ggmap)

# print the results
cities_ggmap[, 1:6]


# us cities
data("us.cities")
