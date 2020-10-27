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

### This works!!!
