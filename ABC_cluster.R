source("ABC.R")
library(tidyverse)
k <- 3
d <- 2
# k*d
cluster_obj <- function(par, x, k, d){
  solution <- matrix(par, nrow = k, ncol = d)
  centre <- split(solution, seq_len(k))
  alloc <- closest_allocation(x, centre)
  sum(sapply(seq_along(alloc), function(i) norm(x[i,]-centre[[alloc[[i]]]], "2")))
}


closest_allocation <- function(D, centre){
  k <- length(centre)
  n <- dim(D)[[1]]
  d <- dim(D)[[2]]
  alloc <- numeric(n)
  for(i in 1:n){
    dis <- sapply(1:k, function(j) norm(D[i,]-centre[[j]], "2"))
    # minDist <- min(dis)
    alloc[[i]] <- which.min(dis)
  }
  return(alloc)
}


data <- iris[,c(2,4, 5)]
data[,1] <- data[,1]/sum(data[,1])
data[,2] <- data[,2]/sum(data[,2])
ggplot(data, aes(x=Sepal.Width, y=Petal.Width)) +
  geom_point(aes(color = Species))
x <- data[,1:2]
lb <- 0
ub <- 1
par <- runif(6)

partition <- ABC(par, cluster_obj, k =k, d=d, x=x,  lb=lb, ub=ub, n.stop=50, max.cycle = 1e3)
# func <-function(par) cluster_obj(par, x, k, d)

 # func(foods[1,])
 # 
 # for(i in seq_len(SN)){
 #   smell_neighbor()
 # }

