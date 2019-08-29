library(Rcpp)
library(tidyverse)
sourceCpp("ABC.cpp")
sourceCpp("cluster.cpp")
source("ABC_cpp.R")
library(ggpubr)

# clusterSim::index.DB()



data <- iris[,c(2,4, 5)]
data[,1] <- data[,1]/max(data[,1])
data[,2] <- data[,2]/max(data[,2])
ori <- ggplot(data, aes(x=Sepal.Width, y=Petal.Width)) +
  geom_point(aes(color = Species))
x <- data[,1:2]
lb <- 0
ub <- 1
par <- runif(6)
k <- 3
d <- 2
###
partition <- ABC(par, .cluster_obj_cpp, d=d,
                             k =k, x=as.matrix(x),  lb=lb, ub=ub, n.stop=5000, max.cycle = 1e4)


alloc <- .closest_allocation_cpp(as.matrix(x), matrix(partition$par, nrow = k, ncol = d))
new <- ggplot() +
  geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  geom_point(aes(x=partition$par[1:3], y=partition$par[4:6]), size=2)
ggarrange(ori, new, common.legend = T)
###
# step <- 2
# ggplot() +
#   geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
#   geom_point(aes(x=partition$path[step,1:3], y=partition$path[step,4:6]), size=2)
# step <- step+1

# fun <- .cluster_obj_cpp




data <- read_csv("train/9_diamond.csv", col_names = F)
ori <- ggplot(data, aes(x=X1, y=X2)) +
  geom_point()
lb <- -2
ub <- 5
k <- 9
d <- 2
par <- runif(k*d, lb, ub)
###
partition <- ABC(par, .cluster_obj_cpp, d=d, k =k, x=as.matrix(data),  lb=lb, ub=ub,limit = 500,n.stop = 500,  max.cycle = 1e3, SN=30)

.cluster_obj_cpp(d=d, k =k, x=as.matrix(data), par=partition$foods[p,])
p <- p+1
alloc <- .closest_allocation_cpp(as.matrix(data), matrix(partition$par, nrow = k, ncol = d))

clusterSim::index.DB(data, alloc)[[1]]
new <- ggplot() +
  geom_point(data=data,mapping =  aes(x=X1, y=X2,color = as.factor(alloc)))  +
  geom_point(aes(x=partition$par[1:k], y=partition$par[(k+1):(2*k)]), size=2)
ggarrange(ori, new, common.legend = T)
###
# or <- 1
# alloc <- .closest_allocation_cpp(as.matrix(data), matrix(foods[or,], nrow = k, ncol = d))
# ggplot() +
#   geom_point(data=data,mapping =  aes(x=X1, y=X2,color = as.factor(alloc))) +
#   geom_point(aes(x=foods[or,][1:k], y=foods[or,][(k+1):(2*k)]), size=2)



data <- read_csv("train/9_diamond.csv", col_names = F)
ori <- ggplot(data, aes(x=X1, y=X2)) +
  geom_point()
lb <- -2
ub <- 5
k <- 9
d <- 2
par <- runif(k*d, lb, ub)
###
partition <- abc_cpp(par, .cluster_obj_cpp, d=d, k =k, x=as.matrix(data),  lb=lb, ub=ub, limit=500, criter =  200, maxCycle = 1e3, FoodNumber =30)

ppar <- partition$par
alloc <- .closest_allocation_cpp(as.matrix(data), matrix(partition$par, nrow = k, ncol = d))
.cluster_obj_cpp(partition$foods[1,], as.matrix(data), k, d)
.cluster_obj_cpp(par, as.matrix(data), k, d)
clusterSim::index.DB(data, alloc)[[1]]
new <- ggplot() +
  geom_point(data=data,mapping =  aes(x=X1, y=X2,color = as.factor(alloc)))  +
  geom_point(aes(x=partition$par[1:k], y=partition$par[(k+1):(2*k)]), size=2)
ggarrange(ori, new, common.legend = T)
