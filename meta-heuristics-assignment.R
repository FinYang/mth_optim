## ---- library ----
library(tidyverse)
library(Rcpp)
library(ggpubr)
library(kableExtra)
# library(furrr)
source("ABC.R")
sourceCpp("cluster.cpp")


# standardising the data
stan <- function(x){
  max_x <- apply(x, 2, function(x) max(abs(x)))
  out <- mapply(function(x, max_x) x/max_x, x=as.data.frame(x), max_x = max_x)
  return(out)
}

# wraper to run the ABC algorithm and report desired values
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
  # simulate initial value until it fits the minimum cluster size
  while(size_limit(par, x=x, k=k, d=d, const = const)){
    par <- c(x[sample(NROW(x), k),])
    try_par <- try_par +1
    cat("\rNumber of initial par tried:", try_par)
    if(try_par == 4000){
      # relax the constraint
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




## ---- train-data ----

tra_names <- dir("train")
tra_k <- sub("_.*", "", tra_names) %>% 
  as.numeric()

train <- lapply(paste0("train/",tra_names), read_csv, col_names = F)

## ---- train-est1 ----


train_3_3 <- NULL
for(tr in 1:10){
  
  train_3_3[[tr]] <- run_ABC(train[[tr]], k=tra_k[[tr]])
  cat("\rData", tr, "\n")
}
saveRDS(train_3_3, "train_3_3.rds")



## ---- table1 ----

train_3_3 <- readRDS("train_3_3.rds")
tab1 <- sapply(train_3_3, function(x) c(x$value, x$n_iter, x$first_optim))
tab1 <- t(tab1) %>%
  as.data.frame()
tra_di <- sapply(train, function(x) NCOL(as.data.frame(x)))
tab1 <- cbind(tra_names,tra_di, tab1)
colnames(tab1) <- c("Data","Dimension","Final Objective Value", "Number of Iteration", "Best solution was first found")
knitr::kable(tab1, caption = "Performance measures using the initial parameters for the training data")
# plot_cluster(as.matrix(train[[3]]), alloc = train_3_3[[3]]$alloc , k=tra_k[[3]], par = train_3_3[[3]]$par)



## ---- tu_tab ----


## ---- 3-5 ----
train_3_5 <- NULL
for(tr in 1:10){
  
  train_3_5[[tr]] <- run_ABC(train[[tr]], k=tra_k[[tr]], n_stop = 200, max_cycle = 700, limit = 40)
  cat("\rData", tr, "\n")
}
saveRDS(train_3_5, "train_3_5.rds")

## ---- table2 ----

train_3_5 <- readRDS("train_3_5.rds")
tab2 <- sapply(train_3_5, function(x) c(x$value, x$n_iter, x$first_optim))
tab2 <- t(tab2) %>%
  as.data.frame()
tra_di <- sapply(train, function(x) NCOL(as.data.frame(x)))
tab2 <- cbind(tra_names,tra_di, tab2)
colnames(tab2) <- c("Data","Dimension","Final Objective Value", "Number of Iteration", "Best solution was first found")
knitr::kable(tab2, caption = "Performance measures using the initial parameters for the training data")
# plot_cluster(as.matrix(train[[3]]), alloc = train_3_3[[3]]$alloc , k=tra_k[[3]], par = train_3_3[[3]]$par)


# 200, 700, 40


## ---- test-data ----

tes_names <- dir("test")
tes_k <- sub("_.*", "", tes_names) %>% 
  as.numeric()

test <- lapply(paste0("test/",tes_names), read_csv, col_names = F)


## ---- train-est1 ----


test_3_6 <- NULL
for(tr in 1:10){
  
  test_3_6[[tr]] <- replicate(10, run_ABC(as.matrix(test[[tr]]), k=tes_k[[tr]], n_stop = 200, max_cycle = 700, limit = 40), 
                              simplify = F)
  cat("\rData", tr, "\n")
  saveRDS(test_3_6[[tr]], paste0("2test_3_6", sub(".csv$", "", tes_names[[tr]]), ".rds"))
  
}
saveRDS(test_3_6, "2test_3_6.rds")


## ---- table3 ----

test_3_6 <- readRDS("2test_3_6.rds")
obj_value <-  sapply(test_3_6, function(eachdata) sapply(eachdata, function(eachrun) eachrun$value))
tem <-   sub(".csv$", "", tes_names)
tem <-   sub("^.*_", "", tem)
colnames(obj_value) <- tem
table3 <- apply(obj_value, 2, function(x) c(min(x), mean(x), max(x)))
table3 <- rbind(tes_k, table3)
rownames(table3) <- c("No.Clusters", "Best", "Average", "Worest")
knitr::kable(table3, caption = "The Best, Average, Worest objective value for the test data, 10 runs each", digits = 4)







## ---- tuning ----

index <- which(tra_names %in% c("3_square2.csv", "4_tetra.csv"))
tr_tu <- train[index]
tr_tu_k <- tra_k[index]
tune1 <- NULL
for(istop in seq(300, 700, 100)){
  tune1[[(istop/100)-2]] <- mapply(run_ABC,x= as.matrix(tr_tu), k=tr_tu_k, 
                                   MoreArgs =  list(n_stop = istop, max_cycle = 1e3, limit = NULL), 
                                   SIMPLIFY = F)
}

# saveRDS(tune1, "tune1.rds")
## ---- sq1 ----

tune1 <- readRDS("tune1.rds")
stopn <- seq(300, 700, 100)
value1 <- sapply(tune1, function(x) c(x[[1]]$value, x[[2]]$value))
runn1 <- sapply(tune1, function(x) c(x[[1]]$n_iter, x[[2]]$n_iter))
first1 <- sapply(tune1, function(x) c(x[[1]]$first_optim, x[[2]]$first_optim))
sq1 <- rbind(value1[1,], runn1[1,], first1[1,])
colnames(sq1) <- paste0("MNU ",stopn)
rownames(sq1) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
te1 <- rbind(value1[2,], runn1[2,], first1[2,])
colnames(te1) <- paste0("MNU ",stopn)
rownames(te1) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
knitr::kable(sq1, caption = "Performance on data square2 using different MNU with fixed MCN of 1000 and limit of SN*D", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")
## ---- te1 ----
knitr::kable(te1, caption = "Performance on data tetra using different MNU with fixed MCN of 1000 and limit of SN*D", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")


## ----  ----

tune2 <- NULL
for(istop in seq(300, 700, 100)){
  tune2[[(istop/100)-2]] <- mapply(run_ABC,x= as.matrix(tr_tu), k=tr_tu_k,
                                   MoreArgs =  list(n_stop = 200, max_cycle = istop, limit = NULL),
                                   SIMPLIFY = F)
}

# saveRDS(tune2, "tune2.rds")
## ---- sq2 ----
tune2 <- readRDS("tune2.rds")
stopn <- seq(300, 700, 100)
value2 <- sapply(tune2, function(x) c(x[[1]]$value, x[[2]]$value))
runn2 <- sapply(tune2, function(x) c(x[[1]]$n_iter, x[[2]]$n_iter))
first2 <- sapply(tune2, function(x) c(x[[1]]$first_optim, x[[2]]$first_optim))
sq2 <- rbind(value2[1,], runn2[1,], first2[1,])
colnames(sq2) <- paste0("MCN ",stopn)
rownames(sq2) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
te2 <- rbind(value2[2,], runn2[2,], first2[2,])
colnames(te2) <- paste0("MCN ",stopn)
rownames(te2) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
knitr::kable(sq2, caption = "Performance on data square2 using different MCN with fixed MNU of 200 and limit of SN*D", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")
## ---- te2 ----
knitr::kable(te2, caption = "Performance on data tetra using different MCN with fixed MNU of 200 and limit of SN*D", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")


## ----  ----
##


tune3 <- NULL
for(istop in seq(20, 70, 10)){
  tune3[[(istop/10)-1]] <- mapply(run_ABC,x= as.matrix(tr_tu), k=tr_tu_k,
                                  MoreArgs =  list(n_stop = 200, max_cycle = 550, limit = istop), 
                                  SIMPLIFY = F)
}

# saveRDS(tune3, "tune3.rds")
## ---- sq3 ----
tune3 <- readRDS("tune3.rds")
stopn <- seq(20, 70, 10)
value3 <- sapply(tune3, function(x) c(x[[1]]$value, x[[2]]$value))
runn3 <- sapply(tune3, function(x) c(x[[1]]$n_iter, x[[2]]$n_iter))
first3 <- sapply(tune3, function(x) c(x[[1]]$first_optim, x[[2]]$first_optim))
sq3 <- rbind(value3[1,], runn3[1,], first3[1,])
colnames(sq3) <- paste0("Limit ",stopn)
rownames(sq3) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
te3 <- rbind(value3[2,], runn3[2,], first3[2,])
colnames(te3) <- paste0("Limit ",stopn)
rownames(te3) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
knitr::kable(sq3, caption = "Performance on data square2 using different limit with fixed MNU of 200 and MCN of 550", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")
## ---- te3 ----
knitr::kable(te3, caption = "Performance on data tetra using different limit with fixed MNU of 200 and MCN of 550", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")

## ---- tubest ----

index <- which(tra_names %in% c("3_square2.csv", "4_tetra.csv"))
tr_tu <- train[index]
tr_tu_k <- tra_k[index]
tune4 <- mapply(run_ABC,x= as.matrix(tr_tu), k=tr_tu_k, 
                MoreArgs =  list(n_stop = 200, max_cycle = 700, limit = 40), 
                SIMPLIFY = F)
# saveRDS(tune4, "tune4.rds")

## ---- sqte4 ----

tune4 <- readRDS("tune4.rds")
stopn <- seq(20, 70, 10)
value4 <- sapply(tune4, function(x) c(x$value))
runn4 <- sapply(tune4, function(x) c(x$n_iter))
first4 <- sapply(tune4, function(x) c(x$first_optim))
sqte4 <- rbind(value4, runn4, first4)
colnames(sqte4) <- c("3_square2", "4_tetra")
rownames(sqte4) <- c("Final Objective Value", "Number of Iteration", "Best solution was first found")
knitr::kable(sqte4, caption = "Performance on data square2 and tetra with limit of 40, MNU of 200 and MCN of 550", digits = 4) %>% 
  kable_styling(latex_options = "hold_position")


## ---- plot-3-7 ----

plot_cluster <- function(data, alloc, k, par){
  if(NCOL(data)!=2) stop("high dimension")
  data <- as.matrix(stan(data))
  ori <- qplot(x=data[,1], y= data[,2]) + theme(axis.title = element_blank())
  # qplot(x=data[,1], y= data[,2])
  new <- ggplot() +
    geom_point(mapping =  aes(x=data[,1], y=data[,2],color = as.factor(alloc))) +
    geom_point(aes(x=par[1:k], y=par[(k+1):(2*k)]), size=2)  +
    theme(axis.title = element_blank())
  ggarrange(ori, new, legend = "none")
}


best_run <- apply(obj_value, 2, which.max)
plotdata <- mapply(function(data, i) data[[i]] ,  data = test_3_6, i = best_run,  SIMPLIFY = F)
# plots <- mapply(function(data, dalloc, k, dpar) plot_cluster(data, alloc = dalloc$alloc, k=k, par = dpar$par), 
#                 data = test, dalloc = plotdata,  k  =tes_k, dpar=plotdata)

# saveRDS(plotdata, "test_plotdata.rds")

plot_test <- function(index){
  plot_cluster(data = test[[index]], alloc = plotdata[[index]]$alloc, k = length(plotdata[[index]]$par)/2, par = plotdata[[index]]$par)
}

## ---- p1 ----
plot_test(1)

## ---- p2 ----
plot_test(2)

## ---- p3 ----
plot_test(3)

## ---- p4 ----
plot_test(4)

## ---- p5 ----
plot_test(5)

## ---- p6 ----
plot_test(6)

## ---- p7 ----
plot_test(7)

## ---- p8 ----
plot_test(8)

## ---- p9 ----
plot_test(9)

## ---- p10 ----
plot_test(10)

