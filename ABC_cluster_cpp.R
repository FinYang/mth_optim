library(Rcpp)
library(tidyverse)
sourceCpp("ABC.cpp")
sourceCpp("cluster.cpp")
source("ABC_cpp.R")
library(ggpubr)


data <- iris[,c(2,4, 5)]
data[,1] <- data[,1]/sum(data[,1])
data[,2] <- data[,2]/sum(data[,2])
ori <- ggplot(data, aes(x=Sepal.Width, y=Petal.Width)) +
  geom_point(aes(color = Species))
x <- data[,1:2]
lb <- 0
ub <- 1
par <- runif(6)
k <- 3
d <- 2
partition <- ABC_cluster_cpp(par, .cluster_obj_cpp, d=d,data = as.matrix(x), 
                             k =k, x=as.matrix(x),  lb=lb, ub=ub, n.stop=5000, max.cycle = 1e4)


alloc <- .closest_allocation_cpp(as.matrix(x), matrix(partition$par, nrow = k, ncol = d))
new <- ggplot() +
  geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  geom_point(aes(x=partition$par[1:3], y=partition$par[4:6]), size=2)
ggarrange(ori, new, common.legend = T)

step <- 2
ggplot() +
  geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  geom_point(aes(x=partition2$path[step,1:3], y=partition2$path[step,4:6]), size=2)
step <- step+1

# fun <- .cluster_obj_cpp