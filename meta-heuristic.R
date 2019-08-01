library(tidyverse)
set.seed(01082019)
n <- 60
xy <- cbind(x=runif(n, 1, 20),y= runif(n, 1, 10))
starting_point <- 1
xy <- xy[c(starting_point, seq(1:60)[-starting_point]),]
qplot(x=xy[,1], y=xy[,2])
#distance matrix
grid <- expand.grid(1:60, 1:60) %>% 
  split(1:60^2)
d <- sapply(grid, function(ij) norm(xy[ij[[1]],]-xy[ij[[2]],], "2")) %>% 
  matrix(60)

greedy_tour_position <- function(d){
  remain <- 2:n
  tour <- 1
  for(i in 2:n){
    index <- which.min(d[tour[[length(tour)]], remain])
    next_stop <- remain[[index]] 
    tour <- c(tour, next_stop)
    remain <- remain[-index]
  }
  return(tour)
}
tour <- greedy_tour_position(d)

plot_tour <- function(xy, tour){
  n <- length(tour)
  xy <- as.data.frame(xy)
  xy <- xy[tour, ]
  ggplot(xy, aes(x=x, y=y)) + 
    geom_point() +
    geom_path() +
    geom_path(aes(x=x, y=y), xy[c(nrow(xy),1),])
}
plot_tour(xy, tour)

tour_cost <- function(pos, d){
  sapply(1:length(pos), function(i) 
    d[pos[[i]], pos[[ifelse((i+1) %% length(pos)==0, 60, (i+1) %% length(pos))]]]) %>% 
    sum()
}
tour_cost(tour, d)

greedy_tour_sus <- function(d){
  remain <- 2:n
  tour <- sus <- 1
  succ <- numeric(n)
  last <- 1
  for(i in 2:n){
    index <- which.min(d[sus, remain])
    sus <- remain[[index]] 
    remain <- remain[-index]
    last <- succ[[last]] <- sus
    # print(c(index, sus))
  }
  succ[[which(succ==0)]] <- 1
  return(succ)
}

sus <- greedy_tour_sus(d)
greedy_tour_position(d)

