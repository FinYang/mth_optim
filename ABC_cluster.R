source("ABC.R")
source("ABC_cluster_func.R")
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


closest_allocation <- function(x, centre){
  k <- length(centre)
  n <- dim(x)[[1]]
  d <- dim(x)[[2]]
  alloc <- numeric(n)
  for(i in 1:n){
    dis <- sapply(1:k, function(j) norm(x[i,]-centre[[j]], "2"))
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

partition <- ABC_cluster(par, cluster_obj, d=d,data = x, k =k, x=x,  lb=lb, ub=ub, n.stop=500, max.cycle = 1e3)
# func <-function(par) cluster_obj(par, x, k, d)

 # func(foods[1,])
 # 
 # for(i in seq_len(SN)){
 #   smell_neighbor()
 # }
alloc <- closest_allocation(x, split( matrix(partition$par, nrow = k, ncol = d), seq_len(k)))
ggplot() +
  geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  geom_point(aes(x=partition2$par[1:3], y=partition2$par[4:6]), size=2)

step <- 2
ggplot() +
  geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  geom_point(aes(x=partition2$path[step,1:3], y=partition2$path[step,4:6]), size=2)
step <- step+1


partition <- ABC_cluster_results$partition
partition$par


data$Species 
alloc <- as.factor(alloc)
levels(alloc) <- levels(data$Species)
alloc
cbind(alloc, data$Species)

sum(alloc != data$Species)/length(alloc)


partition2 <- ABC_cluster(par, cluster_obj, d=d,data = x, k =k, x=x,  lb=lb, ub=ub, n.stop=10, max.cycle = 1e2)
