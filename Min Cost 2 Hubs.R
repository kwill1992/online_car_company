#### Header ####
# Add description about what I'm doing.
# Trivial problem for network flow.
#### Get a basic model working ####
library(lpSolve)
library(lpSolveAPI)
#### 10 Cities with one hub in Houston and a new one in washing dc ####
f.obj <- c(705.04,690.03,593.44,332.01,100.00,662.94,617.15,689.96,522.36,614.42,
           326.04,874.85,496.92,646.68,662.94,100.00,586.57,277.47,478.96,819.49)
f.con <- matrix(c(1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,
                  0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
                  0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,
                  0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
                  0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,
                  0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
                  0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,
                  0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,
                  0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,
                  0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1), nrow = 10, byrow = TRUE)
f.dir <- c(">=",">=",">=",">=",">=",">=",">=",">=",">=",">=")
f.rhs <- c(893,614,440,352,328,292,287,284,280,230)

lp("min", f.obj, f.con, f.dir, f.rhs)
lp("min", f.obj, f.con, f.dir, f.rhs)$solution

#### Get 10 Cities working with importing data into matrices or vectors. ####

#### Try working with ompr package
library(ompr)
library(magrittr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
# Shipping Cost
cost <- c(705.04,690.03,593.44,332.01,100.00,662.94,617.15,689.96,522.36,614.42,
          326.04,874.85,496.92,646.68,662.94,100.00,586.57,277.47,478.96,819.49)
cost_m <- matrix(cost, nrow = 10, byrow = FALSE)
cost_m
# Supply to move from each cities
supply <- c(893,614,440,352,328,292,287,284,280,230)

model <- MIPModel()  %>% 
  # Number of cars shiped from Xi to Xj
  add_variable(x[i,j], i = 1:10, j = 1:2, type = "integer", lb = 0) %>% 
  # minimize shipping cost
  set_objective(sum_expr(cost_m[i,j] * x[i,j], i = 1:10, j = 1:2), "min") %>% 
  # must use supply from each city
  
  
  ### fix this with J's, not 1 and 2
  #add_constraint(x[i, 1] + x[i, 2] >= supply[i], i = 1:10) #%>%
  # FIXED! works with j's
  add_constraint(sum_expr(x[i, j], j = 1:2) >= supply[i], i = 1:10) #%>% 
# use only one Y
#add_constraint(sum_expr(y[j], j = 1:2) == 1) %>% 
# add linking variables

#result <- ROI_solve(model, solver = "glpk")
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x[i,j])
temp_df <- as_tibble(get_solution(result, x[i,j]))
temp_df
### This works!!


#result <- ROI_solve(model, solver = "glpk")
#result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
#result
#get_solution(result, x[i])
cities_10 <- read_csv("distances_top_10.csv")
solution <- as_tibble(get_solution(result, x[i,j]))
library(dplyr)
solution
# Adds after the second column
solution <- solution %>%
  add_column(FROM_city = 0) %>% 
  add_column(TO_city = 0) %>% 
  add_column(lon.to = 0) %>%
  add_column(lat.to = 0) %>%
  add_column(lon.from = 0) %>%
  add_column(lat.from = 0)
from.column <- c(cities_10$to[1:10])
to.column <- c("Houston, Texas", "Washington, District of Columbia")
m <- 1
n <- 0
for (k in 1:2){
  for (l in 1:10){
    solution$FROM_city[m] <- from.column[l]
    solution$lon.from[m] <- cities_10$lon.to[l]
    solution$lat.from[m] <- cities_10$lat.to[l]
    solution$TO_city[m] <- to.column[k]
    solution$lon.to[m] <- cities_10$lon.to[5 + n]
    solution$lat.to[m] <- cities_10$lat.to[5 + n]
    m <- m + 1
  }
  n <- n + 1
}

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
  geom_point(data = cities_10, aes( x = lon.from, y = lat.from,
                                    size = from_population, color = "purple", alpha = 0.5),
             inherit.aes = FALSE) +
  geom_text(data = cities_10, aes(x = lon.from, y = lat.from, label = from), inherit.aes = FALSE) +
  geom_segment(data = solution, aes(x = lon.from, y = lat.from, xend = lon.to,
                                     yend = lat.to), color = "blue", size = 0.3,
               arrow = arrow(), inherit.aes = FALSE)


