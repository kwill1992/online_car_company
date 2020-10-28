# MILP



#### Try choose Wash or Houston hub ####
library(ompr)
library(magrittr)

# Shipping Cost
cost <- c(705.04,690.03,593.44,332.01,100.00,662.94,617.15,689.96,522.36,614.42,
          326.04,874.85,496.92,646.68,662.94,100.00,586.57,277.47,478.96,819.49)
# Supply to move from each cities
supply <- c(893,614,440,352,328,292,287,284,280,230)

model <- MIPModel()  %>% 
  # Number of cars shipped from Xi to Xj
  add_variable(x[i, j], i = 1:10, j = 1:2, type = "integer", lb = 0) %>% 
  # Choose Houston (Y1) or Washington (Y2)
  add_variable(y[j], j = 1:2, type = "binary") %>% 
  # minimize shipping cost
  set_objective(sum_expr(cost[i,j] * x[i, j], i = 1:10, j = 1:2), "min") %>% 
  # must use supply from each city
  add_constraint(sum_expr(x[i, j], j = 1:2) >= supply[i], i = 1:10) %>% 
  # use only one Y
  add_constraint(sum_expr(y[j], j = 1:2) == 1) #%>% 
  # add linking variables
  
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x[i,j])
get_solution(result, y[j])


#### this works ####
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
  # Choose Houston (Y1) or Washington (Y2)
  add_variable(y[j], j = 1:2, type = "binary") %>% 
  #add_variable(y[j], j = 1:2, type = "integer", lb = 0, ub = 1)
  # minimize shipping cost
  set_objective(sum_expr(cost_m[i,j] * x[i,j], i = 1:10, j = 1:2), "min") %>% 
  # must use supply from each city
 
  ### fix this with J's, not 1 and 2
  #add_constraint(x[i, 1] + x[i, 2] >= supply[i], i = 1:10) #%>%
  # FIXED! works with j's
  add_constraint(sum_expr(x[i, j], j = 1:2) >= supply[i], i = 1:10) %>% 
  # use only one Y
  add_constraint(sum_expr(y[j], j = 1:2) == 1) %>% 
  # add linking variables
  add_constraint(x[i,j] <= 1000*y[j], i = 1:10, j = 1:2)
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
