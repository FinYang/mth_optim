## ---- library ----
library(tidyverse)
library(Rcpp)
library(ggpubr)
library(furrr)
source("ABC.R")
sourceCpp("cluster.cpp")

stan <- function(x){
  max_x <- apply(x, 2, max)
  out <- mapply(function(x, max_x) x/max_x, x=as.data.frame(x), max_x = max_x)
  return(out)
}

# return TRUE if violates the size limit
size_limit <- function(nu,x, k, d){
  centerr <- matrix(nu, nrow = k, ncol = d)
  allocc <- .closest_allocation_cpp(as.matrix(x), centerr)
  any(table(allocc)<(NROW(x)/(2*k))) || length(table(allocc)) <k
}

run_ABC <- function(x, k, n_stop = 200, max_cycle = 1e3, limit = NULL, obj_func = c( "db", "euclidean")){
  x <- as.matrix(stan(x))
  d <- NCOL(x)
  if(is.null(limit)) limit <- d*k
  par <-c(x[sample(NROW(x), k),])
  try_par <- 0
  while(size_limit(par, x=x, k=k, d=d)){
    par <- c(x[sample(NROW(x), k),])
    try_par <- try_par +1
    cat("\rNumber of initial par tried:", try_par)
    if(try_par == 2000) break
  }
  cat("\n")
  lb <- 0
  ub <- 1
  if(obj_func[[1]] == "euclidean"){
    func <- .cluster_obj_cpp
  } else if(obj_func[[1]] == "db"){
    func <- .db
  } else {
    stop("You need to choose from euclidean distance or db index")
  }
  partition <- ABC(par, func, d=d,
                   k =k, x=as.matrix(x),  lb=lb, ub=ub, n.stop=n_stop, max.cycle = max_cycle, limit=limit)
  alloc <- .closest_allocation_cpp(as.matrix(x), matrix(partition$par, nrow = k, ncol = d))
  db <- .db(partition$par, x=as.matrix(x), d=d, k=k)
  return(c(ini_par = list(par), partition, alloc = list(alloc), db = list(db)))
  # ori <- ggplot(data, aes(x=Sepal.Width, y=Petal.Width)) +
  #   geom_point(aes(color = Species))
  # new <- ggplot() +
  #   geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
  #   geom_point(aes(x=partition$par[1:3], y=partition$par[4:6]), size=2)
  # ggarrange(ori, new, common.legend = T)
}

plot_cluster <- function(data, alloc, k, par){
  if(NCOL(data)!=2) stop("high dimension")
  data <- as.matrix(stan(data))
  ori <- qplot(x=data[,1], y= data[,2])
  # qplot(x=data[,1], y= data[,2])
  new <- ggplot() +
    geom_point(mapping =  aes(x=data[,1], y=data[,2],color = as.factor(alloc))) +
    geom_point(aes(x=par[1:k], y=par[(k+1):(2*k)]), size=2)
  ggarrange(ori, new, common.legend = T)
}


## ---- train-data ----

tra_names <- dir("train")
tra_k <- sub("_.*", "", tra_names) %>% 
  as.numeric()
train <- lapply(paste0("train/",tra_names), read_csv, col_names = F)

# future::plan(future::multiprocess)
# test <- run_ABC(train[[1]], k=tra_k[[1]])
# plot_cluster(as.matrix(train[[4]]), alloc = test$alloc , k=3, par = test$par)
# train_3.3 <- furrr::future_map2(train, tra_k, get_stats, .progress = TRUE)
train_3_3 <- pbapply::pbmapply(run_ABC, x=train, k = tra_k, SIMPLIFY = F)
saveRDS(train_3_3, "train_3_3.rds")


# test <- t(test) %>% 
#   as.data.frame()
# test <- cbind(tra_names, test)
# colnames(test) <- c("Data","Final Objective Value", "Number of Iteration", "Best solution was first found")
# knitr::kable(test)




# -------------------------------------------------------------------------
# 
# data <- iris[,c(2,4, 5)]
# data[,1] <- data[,1]/max(data[,1])
# data[,2] <- data[,2]/max(data[,2])
# # data[,1] <- (data[,1]-mean(data[,1]))/sd(data[,1])
# # data[,2] <- (data[,2]-mean(data[,2]))/sd(data[,2])
# ori <- ggplot(data, aes(x=Sepal.Width, y=Petal.Width)) +
#   geom_point(aes(color = Species))
# x <- data[,1:2]
# # lb <- c(min(x[,1]), min(x[,2])) %>% rep(each = k)
# lb <- 0
# # ub <- c(max(x[,1]), max(x[,2])) %>% rep(each = k)
# ub <- 1
# par <- runif(6)
# k <- 3
# d <- 2
# size_limit(par, x=x, k=k, d=d)
# ###
# partition <- ABC(par, .cluster_obj_cpp, d=d,
#                  k =k, x=as.matrix(x),  lb=lb, ub=ub, n.stop=200, max.cycle = 1e3)
# 
# 
# alloc <- .closest_allocation_cpp(as.matrix(x), matrix(partition$par, nrow = k, ncol = d))
# new <- ggplot() +
#   geom_point(data=data,mapping =  aes(x=Sepal.Width, y=Petal.Width,color = as.factor(alloc))) +
#   geom_point(aes(x=partition$par[1:3], y=partition$par[4:6]), size=2)
# ggarrange(ori, new, common.legend = T)
# 
# 
# .db(rep(0,6), x=as.matrix(x), d=d, k=k)
# 
# clusterSim::index.DB(as.matrix(x), alloc+1)$DB
# .db(partition$par, as.matrix(x), k=k, d=d)
