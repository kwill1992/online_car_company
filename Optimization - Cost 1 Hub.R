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

#### Get 10 Cities working with importing data into matrices or vectors. ####





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


#### Try choose Wash or Houston hub ####
library(ompr)
library(magrittr)

# Shipping Cost
cost <- c(705.04,690.03,593.44,332.01,100.00,662.94,617.15,689.96,522.36,614.42,
              326.04,874.85,496.92,646.68,662.94,100.00,586.57,277.47,478.96,819.49)
# Supply to move from each cities
supply <- c(893,614,440,352,328,292,287,284,280,230)

model <- MIPModel()  %>% 
  # Number of cars shiped from Xi to Xj
  add_variable(x[i, j], i = 1:10, j = 1:2, type = "integer") %>% 
  # Choose Houston (Y1) or Washington (Y2)
  add_variable(y[j], j = 1:2, type = "binary") %>% 
  # minimize shipping cost
  set_objective(sum_expr(shipcost(i,j) * x[i, j], i = 1:10, j = 1:2), "min") %>% 
  # must use supply from each city
  add_constraint(sum_expr(x[i, j], j = 1:2) >= supply[i], i = 1:10) %>% 
  # use only one Y
  add_constraint(sum_expr(y[j], j = 1:2) == 1) %>% 
  # add linking variables
  
