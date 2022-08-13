# Choose more than two, any two

#### import data set ####
# library(readr)
# library(tidyr)
# library(dplyr)

library(tidyverse)
#library(ompr)
library(magrittr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

#### Create blank data frame
# 3 rent functions
# 3 cost miles functions
# 50 hubs
final <- as.data.frame(matrix(0, ncol = 155, nrow = 225))
names(final)[1] <- "rent.func"
names(final)[2] <- "miles.func"
names(final)[3] <- "cost"
names(final)[4] <- "num.hubs"
names(final)[5] <- "Hubs.used"
names(final)[55] <- "cars.shipped"
names(final)[105] <- "cost.hubs"
names(final)[155] <- "total.cost"

# Read in .csv file and create Tibble DF.
cities_raw <- read_csv("distances_my_top_50.csv")
# Turn into a dataframe
city_data <- as_tibble(cities_raw)
# add new costing
city_data$cost2 <- sapply(city_data$distance, function(x) 50 + 12.5*sqrt(x))
city_data$cost3 <- sapply(city_data$distance, function(x) 150 + 17.5*sqrt(x))



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

num_cities <- 50
#below not need for 50. Only anything less than 50.
city_data <- city_data %>% 
  group_by(num.from) %>%
  slice_head(n = num_cities) %>% 
  group_by(num.to) %>% 
  slice_head(n = num_cities) %>% 
  group_by(num.from)

# Choose number of cities to use


#data <- as.data.frame(Network_Modeling)
# Get top six in each set of TO and FROM
# city_data <- city_data %>% 
#   group_by(num.from) %>%
#   slice_min(order_by = num.from, n = num_cities) %>%
#   group_by(num.to) %>%
#   slice_min(order_by = num.to, n = num_cities) %>%
#   arrange(num.from)



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



supply <- as.vector(get_supply$to_num_cars)

##### FOR medium cost per mile
# make into a cost matrix
cost <- city_data$cost
cost_6_city <- matrix(cost, nrow = num_cities, byrow = FALSE)

#num_hubs <- 2


row <-1
num_hubs <-1
# 1 to 49 hubs by twos
for (num_hubs_counter in 1:25){
  #num_hubs <-1
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
  
  #final$miles.func[num_hubs] <- "M"
  #final$cost
  #result <- ROI_solve(model, solver = "glpk")
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  #result
  #final$cost[row] <- result[2]
  #final$num.hubs[row] <- num_hubs
  get_solution(result, x[i,j])
  # for (col in 1:50) {
  #   final[row,4+col] <- get_solution(result, y[j])[col]
  # }
  #cities_10 <- read_csv("distances_top_10.csv")
  solution <- as_tibble(get_solution(result, x[i,j]))
  solution <- solution %>% 
    group_by(j) %>% 
    summarise(value = sum(value))
  #solution
  
  
  library(dplyr)
  # get hub solution
  solution_hub <- as_tibble(get_solution(result, y[j]))
  to.column <- as.vector(get_supply$to)
  solution_hub <- solution_hub %>% 
    add_column(Hub = 0)
  solution_hub$Hub <- to.column
  #solution_hub
  for (f in 1:3){
    final$miles.func[row] <- "M"
    final$cost[row] <- result[2]
    final$num.hubs[row] <- num_hubs
    
    for (col in 1:50) {
      final[row,4+col] <- solution_hub$value[col]
      final[row,54+col] <- solution$value[col]
      
    }
    row <- row + 1
  }
  #row <- row + 1
  num_hubs <- num_hubs + 2
}

##### FOR high cost per mile
cost <- city_data$cost3
cost_6_city <- matrix(cost, nrow = num_cities, byrow = FALSE)

#num_hubs <- 2


row <-76
num_hubs <-1
for (num_hubs_counter in 1:25){
  #num_hubs <-1
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
  
  #final$miles.func[num_hubs] <- "M"
  #final$cost
  #result <- ROI_solve(model, solver = "glpk")
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  #result
  #final$cost[row] <- result[2]
  #final$num.hubs[row] <- num_hubs
  get_solution(result, x[i,j])
  # for (col in 1:50) {
  #   final[row,4+col] <- get_solution(result, y[j])[col]
  # }
  #cities_10 <- read_csv("distances_top_10.csv")
  solution <- as_tibble(get_solution(result, x[i,j]))
  solution <- solution %>% 
    group_by(j) %>% 
    summarise(value = sum(value))
  #solution
  
  
  library(dplyr)
  # get hub solution
  solution_hub <- as_tibble(get_solution(result, y[j]))
  to.column <- as.vector(get_supply$to)
  solution_hub <- solution_hub %>% 
    add_column(Hub = 0)
  solution_hub$Hub <- to.column
  #solution_hub
  for (f in 1:3){
    final$miles.func[row] <- "H"
    final$cost[row] <- result[2]
    final$num.hubs[row] <- num_hubs
    
    for (col in 1:50) {
      final[row,4+col] <- solution_hub$value[col]
      final[row,54+col] <- solution$value[col]
      
    }
    row <- row + 1
  }
  #row <- row + 1
  num_hubs <- num_hubs + 2
}


##### FOR low cost per mile
cost <- city_data$cost2
cost_6_city <- matrix(cost, nrow = num_cities, byrow = FALSE)

#num_hubs <- 2


row <-151
num_hubs <-1
for (num_hubs_counter in 1:25){
  #num_hubs <-1
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
  
  #final$miles.func[num_hubs] <- "L"
  #final$cost
  #result <- ROI_solve(model, solver = "glpk")
  result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
  #result
  #final$cost[row] <- result[2]
  #final$num.hubs[row] <- num_hubs
  get_solution(result, x[i,j])
  # for (col in 1:50) {
  #   final[row,4+col] <- get_solution(result, y[j])[col]
  # }
  #cities_10 <- read_csv("distances_top_10.csv")
  solution <- as_tibble(get_solution(result, x[i,j]))
  solution <- solution %>% 
    group_by(j) %>% 
    summarise(value = sum(value))
  #solution
  
  
  library(dplyr)
  # get hub solution
  solution_hub <- as_tibble(get_solution(result, y[j]))
  to.column <- as.vector(get_supply$to)
  solution_hub <- solution_hub %>% 
    add_column(Hub = 0)
  solution_hub$Hub <- to.column
  #solution_hub
  for (f in 1:3){
    final$miles.func[row] <- "L"
    final$cost[row] <- result[2]
    final$num.hubs[row] <- num_hubs
    
    for (col in 1:50) {
      final[row,4+col] <- solution_hub$value[col]
      final[row,54+col] <- solution$value[col]
      
    }
    row <- row + 1
  }
  #row <- row + 1
  num_hubs <- num_hubs + 2
}
final <- as.data.frame(final)

typeof(final[3,3][[1]])
final[2,3][[1]]
final[3,2][[1]]

for (fix in 1:225){
  final$newcost[fix] <- final[fix,3][[1]]
}
final2 <- final[ -c(3) ]
write_csv(final2,"50_city_results.csv")





# get_supply <- as_tibble(cities_raw)
# # Add a number for each city 1 to 50
# city_data <- city_data %>% 
#   add_column(num.from = 0, .after = 4) %>% 
#   add_column(num.to = 0, .after = 6)
### Fix H, M, L for cost column
final2 <- read_csv("50city_results.csv")
final2 <- as_tibble(final2)
final2 <- final2 %>% 
  add_column(total.rent = 0, .after = 155) %>% 
  add_column(total.labor = 0, .after = 156)
# for (fix2 in 151:225){
#   final2$miles.func[fix2] <- "L"
# }
# write_csv(final2,"25_city_results.csv")
#final2[1,2]
#row,column
#numcars <- 4000/20
row <- 1
for (iii in 1:75){
  final2$rent.func[row] <- "H"
  #counter <- 29
  for(jjj in 54:78){
    #officespace <- 0
    numcars <- final2[row,jjj-25]/20
    officespace <- if(numcars > 0) 1 else 0
    num.foreman <- if(numcars > 0) 2 else 0
    #if (numcars >0) then (officespace == 1)
    rent <- 3*(20*30*ceiling(numcars/2) + 15*30*ceiling(numcars/2) + 20*30*2*ceiling(numcars/10) + 20*30*ceiling(numcars/4) + officespace*(30*30))
    labor <- 1.25*(75*8*20*ceiling(numcars/2) + 35*8*20*ceiling(numcars/2) + 75*8*20*2*ceiling(numcars/10) + 45*8*20*ceiling(numcars/4) + num.foreman*90*8*20)
    final2[row,jjj] <- rent + labor
    final2[row,81] <- rowSums(final2[row, c(54:78)])
    final2[row,79] <- final2[row,80] + final2[row,81]
    #counter <- counter + 1
  }
  row <- row + 1
  final2$rent.func[row] <- "M"
  #counter <- 29
  for(jjj in 54:78){
    #officespace <- 0
    numcars <- final2[row,jjj-25]/20
    officespace <- if(numcars > 0) 1 else 0
    num.foreman <- if(numcars > 0) 2 else 0
    #if (numcars >0) then (officespace == 1)
    rent <- 1.75*(20*30*ceiling(numcars/2) + 15*30*ceiling(numcars/2) + 20*30*2*ceiling(numcars/10) + 20*30*ceiling(numcars/4) + officespace*(30*30))
    labor <- 1*(75*8*20*ceiling(numcars/2) + 35*8*20*ceiling(numcars/2) + 75*8*20*2*ceiling(numcars/10) + 45*8*20*ceiling(numcars/4) + num.foreman*90*8*20)
    final2[row,jjj] <- rent + labor
    final2[row,81] <- rowSums(final2[row, c(54:78)])
    final2[row,79] <- final2[row,80] + final2[row,81]
    #counter <- counter + 1
  }
  row <- row + 1
  final2$rent.func[row] <- "L"
  #counter <- 29
  for(jjj in 54:78){
    #officespace <- 0
    numcars <- final2[row,jjj-25]/20
    officespace <- if(numcars > 0) 1 else 0
    num.foreman <- if(numcars > 0) 2 else 0
    #if (numcars >0) then (officespace == 1)
    rent <- .5*(20*30*ceiling(numcars/2) + 15*30*ceiling(numcars/2) + 20*30*2*ceiling(numcars/10) + 20*30*ceiling(numcars/4) + officespace*(30*30))
    labor <- .5*(75*8*20*ceiling(numcars/2) + 35*8*20*ceiling(numcars/2) + 75*8*20*2*ceiling(numcars/10) + 45*8*20*ceiling(numcars/4) + num.foreman*90*8*20)
    final2[row,jjj] <- rent + labor
    final2[row,81] <- rowSums(final2[row, c(54:78)])
    final2[row,79] <- final2[row,80] + final2[row,81]
    #counter <- counter + 1
  }
  row <- row +1
}


library(scales)
ggplot(data = final2, mapping = aes(x = num.hubs, y = total.cost)) +
  geom_point(data = final2, aes(x = num.hubs, y = total.cost, shape=miles.func, color = rent.func)) +
  labs(title = "Total Cost (per month) of Shipping and Preparing Cars for Speedy Car Sales, Inc.", x = "Number of Hubs", y = "Total Cost ($)", shape = "Shipping Cost", color = "Rent & Labor Cost") +
  theme(legend.position="bottom") +
  scale_y_continuous(label=dollar_format())

write_csv(final2,"25_city_results_to_plot.csv")

                                       group = group)) +

  
  
  div=1000
  
  ggplot(melt(df, id.var="Date"), 
         aes(x = Date, y = value/div, color=variable, linetype=variable))+
    geom_line() +
    xlim(c(left, right)) + 
    labs(x="", y="Cost (Thousands)") +
    scale_y_continuous(label=dollar_format(),
                       limits=c(min(df$Cost[df$Date > left]), 
                                max(df$Cost[df$Date > left]))/div)
ggplot(df, aes(x=wt, y=mpg, group=cyl)) +
  geom_point(aes(shape=cyl))
  geom_polygon(fill= "white", color = "black") +
  geom_point(data = final2, aes( x = num.hubs, y = total.cost))
                                    size = from_population, color = "purple", alpha = 0.5),
             inherit.aes = FALSE) +
  geom_text(data = city_data, aes(x = lon.from, y = lat.from, label = from), inherit.aes = FALSE) +
  geom_segment(data = solution, aes(x = lon.from, y = lat.from, xend = lon.to,
                                    yend = lat.to), color = "blue", size = 0.3,
               arrow = arrow(), inherit.aes = FALSE)

p <- ggplot(data = mtcars, aes(x = mpg, y = wt))+
  geom_point(aes(color = cyl, size = qsec, shape = gear))
p


#$$B = 20*30*\frac{N_{cars}}{2} + 15*30*\frac{N_{cars}}{2} + 20*30*2*\frac{N_{cars}}{10} + 20*30*\frac{N_{cars}}{4} + 30*30$$
#  ~ where $B$ = sqft of building needed, $N_{cars}$ = number of cars per month/20, [rounded up to next whole number for each work case].  
#A quick search for leasing auto mechanical space in Maryland yielded rates between 75cents/sqft to $3/sqft. We'll used three levels:
High: $3 per sqft
Med: $1.75 per sqft
Low: $0.75 per sqft  

#### add rent function
final




plot(final2$num.hubs,final2$newcost)
length(final2$num_hubs)





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

