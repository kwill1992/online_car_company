# Choose more than two, any two

#### import data set ####
library(readr)
library(tidyr)
library(dplyr)
# Read in .csv file and create Tibble DF.
cities_raw <- read_csv("distances_my_top_50.csv")
# Turn into a dataframe
city_data <- as_tibble(cities_raw)
get_supply <- as_tibble(cities_raw)
# Add a number for each city 1 to 50
city_data <- city_data %>% 
  add_column(num.from = 0, .after = 4) %>% 
  add_column(num.to = 0, .after = 6)
  
# number from and to numbers
xx <- 1
for (ii in 1:50){
  for (jj in 1:50) {
    city_data$num.from[xx] <- ii
    city_data$num.to[xx] <- jj
    xx <- xx + 1
  }
}


# Choose number of cities to use
num_cities <- 10

#data <- as.data.frame(Network_Modeling)
# Get top six in each set of TO and FROM
# city_data <- city_data %>% 
#   group_by(num.from) %>%
#   slice_min(order_by = num.from, n = num_cities) %>%
#   group_by(num.to) %>%
#   slice_min(order_by = num.to, n = num_cities) %>%
#   arrange(num.from)

city_data <- city_data %>% 
  group_by(num.from) %>%
  slice_head(n = num_cities) %>% 
  group_by(num.to) %>% 
  slice_head(n = num_cities) %>% 
  group_by(num.from)
  

# redo number of cars from each city
# get supply
num_cars_month <- 4000
#library(dplyr)
get_supply <- get_supply %>% 
  slice_head(n = num_cities) %>% 
  #arrange(desc(Population)) %>% # sort Tibble by Population and descending
  #slice_head(n = 11) %>% # Get only the first n rows.
  mutate(to_pop_ratio = to_population / sum(to_population)) %>% # percent of Population
  mutate(to_num_cars = round(to_pop_ratio * num_cars_month,0)) # Calc number cars moving each month

# make into a cost matrix
cost <- city_data$cost
cost_6_city <- matrix(cost, nrow = num_cities, byrow = FALSE)


library(ompr)
library(magrittr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
# Shipping Cost
# cost <- c(705.04,690.03,593.44,332.01,100.00,662.94,617.15,689.96,522.36,614.42,
#           326.04,874.85,496.92,646.68,662.94,100.00,586.57,277.47,478.96,819.49)
# cost_m <- matrix(cost, nrow = 10, byrow = FALSE)
# cost_m
# Supply to move from each cities
#supply <- as.vector(city_data$`Number of cars Shipped From`[1:6])
# the above wasn't resized for 6 cities
supply <- as.vector(get_supply$to_num_cars)

# model <- MIPModel()  %>% 
#   # Number of cars shiped from Xi to Xj
#   add_variable(x[i,j], i = 1:6, j = 1:6, type = "integer", lb = 0) %>% 
#   # Choose Houston (Y1) or Washington (Y2)
#   add_variable(y[j], j = 1:6, type = "binary") %>% 
#   #add_variable(y[j], j = 1:2, type = "integer", lb = 0, ub = 1)
#   # minimize shipping cost
#   set_objective(sum_expr(cost_6_city[i,j] * x[i,j], i = 1:6, j = 1:6), "min") %>% 
#   # must use supply from each city
#   
#   ### fix this with J's, not 1 and 2
#   #add_constraint(x[i, 1] + x[i, 2] >= supply[i], i = 1:10) #%>%
#   # FIXED! works with j's
#   add_constraint(sum_expr(x[i, j], j = 1:6) >= supply[i], i = 1:6) %>% 
#   # add this to keep Houston
#   add_constraint(y[5] == 1) %>% 
#   add_constraint(sum_expr(y[j], j = 1:6) == 2) %>% 
#   # add linking variables
#   # 1500 because the new limit should be 1224
#   add_constraint(x[i,j] <= 1500*y[j], i = 1:6, j = 1:6)
# 
# 
# #result <- ROI_solve(model, solver = "glpk")
# result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
# result
# get_solution(result, x[i,j])
# get_solution(result, y[j])


num_hubs <- 3

model <- MIPModel()  %>% 
  # Number of cars shiped from Xi to Xj
  add_variable(x[i,j], i = 1:length(supply), j = 1:length(supply), type = "integer", lb = 0) %>% 
  # Choose Houston (Y1) or Washington (Y2)
  add_variable(y[j], j = 1:length(supply), type = "binary") %>% 
  #add_variable(y[j], j = 1:2, type = "integer", lb = 0, ub = 1)
  # minimize shipping cost
  set_objective(sum_expr(cost_6_city[i,j] * x[i,j], i = 1:length(supply), j = 1:length(supply)), "min") %>% 
  # must use supply from each city
  
  ### fix this with J's, not 1 and 2
  #add_constraint(x[i, 1] + x[i, 2] >= supply[i], i = 1:10) #%>%
  # FIXED! works with j's
  add_constraint(sum_expr(x[i, j], j = 1:length(supply)) >= supply[i], i = 1:length(supply)) %>% 
  # add this to keep Houston
  #add_constraint(y[5] == 1) %>% 
  add_constraint(sum_expr(y[j], j = 1:length(supply)) == num_hubs) %>% 
  # add linking variables
  # 1500 because the new limit should be 1224
  add_constraint(x[i,j] <= max(supply)*y[j], i = 1:length(supply), j = 1:length(supply))


#result <- ROI_solve(model, solver = "glpk")
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x[i,j])
get_solution(result, y[j])



#cities_10 <- read_csv("distances_top_10.csv")
solution <- as_tibble(get_solution(result, x[i,j]))
solution


solution <- solution %>% 
  filter(value > 0)
solution
### This works!!!
# no clean it up
# solution_display <- solution %>% 
#   filter(value > 0) %>% 
#   select(variable, i, j, value, FROM_city, TO_city)
# solution_display


library(dplyr)
# get hub solution
solution_hub <- as_tibble(get_solution(result, y[j]))
to.column <- as.vector(get_supply$to)
solution_hub <- solution_hub %>% 
  add_column(Hub = 0)
solution_hub$Hub <- to.column
solution_hub

# Adds after the second column
solution <- solution %>%
  add_column(FROM_city = 0) %>% 
  add_column(TO_city = 0) %>% 
  add_column(lon.to = 0) %>%
  add_column(lat.to = 0) %>%
  add_column(lon.from = 0) %>%
  add_column(lat.from = 0)

#m <- 1
for ( k in 1:length(city_data$lon.to)){
  solution$FROM_city[k] <- city_data$from[k]
  solution$lon.from[k] <- city_data$lon.from[k]
  solution$lat.from[k] <- city_data$lat.from[k]
  solution$TO_city[k] <- city_data$to[k]
  solution$lon.to[k] <- city_data$lon.to[k]
  solution$lat.to[k] <- city_data$lat.to[k]
  #m < m + 1

}
solution
# from.column <- c(cities_10$to[1:10])
# to.column <- c("Houston, Texas", "Washington, District of Columbia")
# m <- 1
# n <- 0
# for (k in 1:2){
#   for (l in 1:10){
#     solution$FROM_city[m] <- from.column[l]
#     solution$lon.from[m] <- cities_10$lon.to[l]
#     solution$lat.from[m] <- cities_10$lat.to[l]
#     solution$TO_city[m] <- to.column[k]
#     solution$lon.to[m] <- cities_10$lon.to[5 + n]
#     solution$lat.to[m] <- cities_10$lat.to[5 + n]
#     m <- m + 1
#   }
#   n <- n + 1
# }

### This works!!!
# no clean it up
solution <- solution %>% 
  filter(value > 0)



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


ggplot(data = us_states, mapping = aes(x = long, y = lat,
                                       group = group)) +
  geom_polygon(fill= "white", color = "black") +
  geom_point(data = city_data, aes( x = lon.from, y = lat.from,
                                    size = from_population, color = "purple", alpha = 0.5),
             inherit.aes = FALSE) +
  geom_text(data = city_data, aes(x = lon.from, y = lat.from, label = from), inherit.aes = FALSE) +
  geom_segment(data = solution, aes(x = lon.from, y = lat.from, xend = lon.to,
                                    yend = lat.to), color = "blue", size = 0.3,
               arrow = arrow(), inherit.aes = FALSE)

