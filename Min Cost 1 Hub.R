#### Header ####
# Add description about what I'm doing.
# Trivial problem for network flow.
#### Get a basic model working ####
library(lpSolve)
library(lpSolveAPI)
# Maximize 
#   x1 + 9 x2 +   x3 
# Subject to: 
#   x1 + 2 x2 + 3 x3 <= 9
# 3 x1 + 2 x2 + 2 x3 <= 15
f.obj <- c(1, 9, 3)
f.con <- matrix(c(1, 2, 3, 3, 2, 2), nrow = 2, byrow = TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)

lp("max", f.obj, f.con, f.dir, f.rhs)
lp("max", f.obj, f.con, f.dir, f.rhs)$solution

#### Use ompr package ####
library(ompr)
library(magrittr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
model <- MIPModel() %>% 
  add_variable(x1, type = "continuous", lb = 0) %>% 
  add_variable(x2, type = "continuous", lb = 0) %>% 
  add_variable(x3, type = "continuous", lb = 0) %>% 
  set_objective(x1 + 9*x2 + x3, sense = "max") %>% 
  add_constraint(x1 + 2*x2 + 3*x3 <= 9) %>% 
  add_constraint(3*x1 + 2*x2 + 2*x3 <= 15)

result <- ROI_solve(model, solver = "glpk")
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x1)
get_solution(result, x2)
get_solution(result, x3)
solution(result)
  

#### 10 Cities with one hub in Houston ####
f.obj <- c(705.04,690.03,593.44,332.01,100,662.94,617.15,689.96,522.36,614.42)
f.con <- matrix(c(1,0,0,0,0,0,0,0,0,0,
                  0,1,0,0,0,0,0,0,0,0,
                  0,0,1,0,0,0,0,0,0,0,
                  0,0,0,1,0,0,0,0,0,0,
                  0,0,0,0,1,0,0,0,0,0,
                  0,0,0,0,0,1,0,0,0,0,
                  0,0,0,0,0,0,1,0,0,0,
                  0,0,0,0,0,0,0,1,0,0,
                  0,0,0,0,0,0,0,0,1,0,
                  0,0,0,0,0,0,0,0,0,1), nrow = 10, byrow = TRUE)
f.dir <- c(">=",">=",">=",">=",">=",">=",">=",">=",">=",">=")
f.rhs <- c(893,614,440,352,328,292,287,284,280,230)

lp("min", f.obj, f.con, f.dir, f.rhs)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution


#### Try working with ompr package
library(ompr)
library(magrittr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
# Shipping Cost
cost <- c(705.04,690.03,593.44,332.01,100.00,662.94,617.15,689.96,522.36,614.42)
# Supply to move from each cities
supply <- c(893,614,440,352,328,292,287,284,280,230)

model <- MIPModel()  %>% 
  # Number of cars shiped from Xi to Xj
  add_variable(x[i], i = 1:10, type = "integer", lb = 0) %>% 
  # minimize shipping cost
  set_objective(sum_expr(cost[i] * x[i], i = 1:10), "min") %>% 
  # must use supply from each city
  add_constraint(x[i] >= supply[i], i = 1:10) #%>% 
  # use only one Y
  #add_constraint(sum_expr(y[j], j = 1:2) == 1) %>% 
  # add linking variables
  
#result <- ROI_solve(model, solver = "glpk")
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x[i])
cities_10 <- read_csv("distances_top_10.csv")
solution <- as_tibble(get_solution(result, x[i]))
solution$FROM_city <- c(cities_10$from[1:10])
solution$TO_city <- "Houston, Texas"
names(solution)[3] <- "# Cars Shipped"
solution
### This works!!!


#### now map it ####
library(tidyverse)


# "us.cities" in maps package contains This database is of us cities of population
# greater than about 40,000. Also included are state capitals of any 
# population size.
# "state" database produces a map of the states of the United States
# mainland generated from US De- partment of the Census data
library(maps)

# Read in 10 city data with distances made in "Get Distances"
cities_10 <- read_csv("distances_top_10.csv")

# Get states for plotting state map
us_states <- as_tibble(map_data("state"))

# library(ggmap)
# LA <- map_data("state", region="louisiana")
# 
# salesCalls <- data.frame(State=rep("louisiana",5), 
#                          City=c("Baton Rouge", "New Orleans", "Shreveport", 
#                                 "Lafayette", "Mandeville"),
#                          Calls=c(10,5,8,13,2))
# 
# cities_10 <- cbind(geocode(as.character(salesCalls$City)), salesCalls)
# cities_10 <- cbind(geocode(as.character(cities_10$from)), cities_10)
# 
# salesCalls
# #         lon      lat     State        City Calls
# # 1 -91.14032 30.45828 louisiana Baton Rouge    10
# # 2 -90.07153 29.95107 louisiana New Orleans     5
# # 3 -93.75018 32.52515 louisiana  Shreveport     8
# # 4 -92.01984 30.22409 louisiana   Lafayette    13
# # 5 -90.06563 30.35825 louisiana  Mandeville     2

# ggplot(LA, aes(x=long, y=lat)) +
#   geom_polygon() +
#   coord_map() +
#   geom_point(data=salesCalls, aes(x=lon, y=lat, size=Calls), color="orange")
# 

ggplot(data = us_states, mapping = aes(x = long, y = lat,
                                       group = group)) +
  geom_polygon(fill= "white", color = "black") +
  geom_point(data = cities_10, aes( x = lon.from, y = lat.from,
                                    size = from_population, color = "purple", alpha = 0.5),
             inherit.aes = FALSE) +
  geom_text(data = cities_10, aes(x = lon.from, y = lat.from, label = from), inherit.aes = FALSE) +
  geom_segment(data = cities_10, aes(x = lon.from, y = lat.from, xend = lon.to[5],
                                        yend = lat.to[5]), color = "blue", size = 0.3,
               arrow = arrow(), inherit.aes = FALSE)



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
