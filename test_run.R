# 200, 700, 40

## ---- library ----
library(tidyverse)
library(Rcpp)
library(ggpubr)
# library(furrr)
source("ABC.R")
sourceCpp("cluster.cpp")
stan <- function(x){
  max_x <- apply(x, 2, function(x) max(abs(x)))
  out <- mapply(function(x, max_x) x/max_x, x=as.data.frame(x), max_x = max_x)
  return(out)
}


run_ABC <- function(x, k, n_stop = 200, max_cycle = 1e3, limit = NULL, obj_func = c( "db", "euclidean")){
  # return TRUE if violates the size limit
  size_limit <- function(nu,x, k, d, const){
    centerr <- matrix(nu, nrow = k, ncol = d)
    allocc <- .closest_allocation_cpp(as.matrix(x), centerr)
    any(table(allocc)<const) || length(table(allocc)) <k
  }
  x <- as.matrix(stan(x))
  d <- NCOL(x)
  if(is.null(limit)) limit <- d*k
  const <- (NROW(x)/(2*k))
  par <-c(x[sample(NROW(x), k),])
  try_par <- 0
  cat("\n")
  while(size_limit(par, x=x, k=k, d=d, const = const)){
    par <- c(x[sample(NROW(x), k),])
    try_par <- try_par +1
    cat("\rNumber of initial par tried:", try_par)
    if(try_par == 4000){
      const <- (NROW(x)/(10*k))
      break
    } 
    
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
                   k =k, x=as.matrix(x),  lb=lb, ub=ub, n.stop=n_stop, max.cycle = max_cycle, limit=limit, const = const)
  alloc <- .closest_allocation_cpp(as.matrix(x), matrix(partition$par, nrow = k, ncol = d))
  db <- .db(partition$par, x=as.matrix(x), d=d, k=k)
  cat("\n")
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


## ---- test-data ----

tes_names <- dir("test")
tes_k <- sub("_.*", "", tes_names) %>% 
  as.numeric()

test <- lapply(paste0("test/",tes_names), read_csv, col_names = F)
# saveRDS(train, "train.rds")
# future::plan(future::multiprocess)
# test <- run_ABC(train[[1]], k=tra_k[[1]])
# plot_cluster(as.matrix(train[[4]]), alloc = test$alloc , k=3, par = test$par)
# train_3.3 <- furrr::future_map2(train, tra_k, get_stats, .progress = TRUE)
# train_3_3 <- pbapply::pbmapply(run_ABC, x=train, k = tra_k, SIMPLIFY = F)

## ---- train-est1 ----


test_3_6 <- NULL
for(tr in 1:10){
  
  test_3_6[[tr]] <- replicate(10, run_ABC(as.matrix(test[[tr]]), k=tes_k[[tr]], n_stop = 200, max_cycle = 700, limit = 40), 
                              simplify = F)
  cat("\rData", tr, "\n")
  saveRDS(test_3_6[[tr]], paste0("2test_3_6", sub(".csv$", "", tes_names[[tr]]), ".rds"))
  
}
saveRDS(test_3_6, "2test_3_6.rds")


## ---- test-all ----

test_3_6 <- readRDS("2test_3_6.rds")
obj_value <-  sapply(test_3_6, function(eachdata) sapply(eachdata, function(eachrun) eachrun$value))
colnames(obj_value) <- tes_names
table2 <- apply(obj_value, 2, function(x) c(min(x), mean(x), max(x)))
rownames(table2) <- c("Best", "Average", "Worest")

