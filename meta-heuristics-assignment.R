## ---- library ----
library(tidyverse)
library(Rcpp)
library(ggpubr)
source("ABC.R")
sourceCpp("cluster.cpp")


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
size_limit(par, x=x, k=k, d=d)
###
partition <- ABC(par, .db, d=d,
                 k =k, x=as.matrix(x),  lb=lb, ub=ub, n.stop=500, max.cycle = 1e3)


alloc <- .closest_allocation_cpp(as.matrix(x), matrix(partition$par, nrow = k, ncol = d))
new <- ggplot() +
  geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  geom_point(aes(x=partition$par[1:3], y=partition$par[4:6]), size=2)
ggarrange(ori, new, common.legend = T)


.db(rep(0,6), x=as.matrix(x), d=d, k=k)

clusterSim::index.DB(as.matrix(x), alloc+1)$DB
.db(partition$par, as.matrix(x), k=k, d=d)
