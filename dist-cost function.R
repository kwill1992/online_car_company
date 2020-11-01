### make plot for distance vs cost function for r markdown
m <- matrix(0, ncol = 2, nrow = 4000)
dist <- data.frame(m)
x <- c("Distance", "Cost")
colnames(dist) <- x
dist$Distance <- c(seq(1, 4000))
dist$Cost <- sapply(dist$Distance, function(x) 100 + 15*sqrt(x))
#head(dist)
plot(dist$Distance,dist$Cost)
