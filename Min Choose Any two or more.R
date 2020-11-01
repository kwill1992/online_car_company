# Choose more than two, any two

#### import data set ####
library(readr)
library(tidyr)
library(dplyr)
# Read in .csv file and create Tibble DF.
cities_raw <- read_csv("distances_my_top_50.csv")
# Turn into a dataframe
city_data <- as_tibble(cities_raw)
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
num_cities <- 6

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
supply <- as.vector(city_data$`Number of cars Shipped From`[1:6])
# the above wasn't resized for 6 cities
supply <- c(1224,841,603,482,449,400)

model <- MIPModel()  %>% 
  # Number of cars shiped from Xi to Xj
  add_variable(x[i,j], i = 1:6, j = 1:6, type = "integer", lb = 0) %>% 
  # Choose Houston (Y1) or Washington (Y2)
  add_variable(y[j], j = 1:6, type = "binary") %>% 
  #add_variable(y[j], j = 1:2, type = "integer", lb = 0, ub = 1)
  # minimize shipping cost
  set_objective(sum_expr(cost_6_city[i,j] * x[i,j], i = 1:6, j = 1:6), "min") %>% 
  # must use supply from each city
  
  ### fix this with J's, not 1 and 2
  #add_constraint(x[i, 1] + x[i, 2] >= supply[i], i = 1:10) #%>%
  # FIXED! works with j's
  add_constraint(sum_expr(x[i, j], j = 1:6) >= supply[i], i = 1:6) %>% 
  # add this to keep Houston
  add_constraint(y[5] == 1) %>% 
  add_constraint(sum_expr(y[j], j = 1:6) == 2) %>% 
  # add linking variables
  # 1500 because the new limit should be 1224
  add_constraint(x[i,j] <= 1500*y[j], i = 1:6, j = 1:6)
# add_constraint(x[i,1] <= 1000*y[1], i = 1:10) %>% 
# add_constraint(x[i,2] <= 1000*y[2], i = 1:10)
# 
# # Not working
# add_constraint(x[1,1] <= 1000*y[1]) %>% 
# add_constraint(x[2,1] <= 1000*y[1]) %>%
# add_constraint(x[3,1] <= 1000*y[1]) %>%
# add_constraint(x[4,1] <= 1000*y[1]) %>%
# add_constraint(x[5,1] <= 1000*y[1]) %>%
# add_constraint(x[6,1] <= 1000*y[1]) %>%
# add_constraint(x[7,1] <= 1000*y[1]) %>%
# add_constraint(x[8,1] <= 1000*y[1]) %>%
# add_constraint(x[9,1] <= 1000*y[1]) %>%
# add_constraint(x[10,1] <= 1000*y[1]) %>%
# add_constraint(x[1,2] <= 1000*y[2]) %>%
# add_constraint(x[2,2] <= 1000*y[2]) %>%
# add_constraint(x[3,2] <= 1000*y[2]) %>%
# add_constraint(x[4,2] <= 1000*y[2]) %>%
# add_constraint(x[5,2] <= 1000*y[2]) %>%
# add_constraint(x[6,2] <= 1000*y[2]) %>%
# add_constraint(x[7,2] <= 1000*y[2]) %>%
# add_constraint(x[8,2] <= 1000*y[2]) %>%
# add_constraint(x[9,2] <= 1000*y[2]) %>%
# add_constraint(x[10,2] <= 1000*y[2]) 

#result <- ROI_solve(model, solver = "glpk")
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x[i,j])
get_solution(result, y[j])



#### Add maps ####
library(maps)
map('usa')

population_raw <- read_csv("co-est2019-alldata.csv")
