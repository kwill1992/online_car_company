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
  add_constraint(x[i, 1] + x[i, 2] >= supply[i], i = 1:10) #%>% 
# use only one Y
#add_constraint(sum_expr(y[j], j = 1:2) == 1) %>% 
# add linking variables

#result <- ROI_solve(model, solver = "glpk")
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result
get_solution(result, x[i,j])

### This works!!
