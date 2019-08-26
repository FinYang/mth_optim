library(Rcpp)

# sourceCpp("ABC.cpp")

#' @param par Vector. Initial value to search for parameter
#' @param fn The function to optimize over
#' @param ... Other arguments to the function
#' @param SN Integer. The number of solutions; The number of food source
#' @param limit Integer. The number of waiting perios before dropping a not-improving food source
#' @param max.cycle Integer. The maxmum number of iteration.
#' @param n.stop Integer. The number of unchanged results to stop optimizing
ABC <- function(par, fun, ..., SN  = 20, limit= 100, max.cycle= 1000, n.stop = 50, lb= rep(-Inf, length(par)), ub= rep(+Inf, length(par))){
  func <- function(par) fun(par)
  D <- length(par)
  # ini
  # foods <- matrix(runif(SN*D, lb, ub), nrow = SN, ncol = D)
  foods <- matrix(rep(seq(lb,ub,length.out=SN), D), nrow = SN, ncol = D)
  obj <- apply(foods, 1, func)
  nectar <- taste_nectar(obj)
  n_stay <- numeric(SN)
  
  # ABC_cpp(par, func, SN, limit, max.cycle, n.stop, lb, ub)
  
  global_par <- par
  global_min <- func(par)
  
  appreciate_best_food()
  round <- 0
  n_unchange <- 0
  
  path <- c(global_par, min = global_min)
  while(round < max.cycle && n_unchange < n.stop){
    # employed bee
    for(i in seq_len(SN)){
      smell_neighbor()
    }
    #
    comment_on_food()
    
    
    # CalculateProbabilities <- function() {
    #   maxfit <- nectar[1]
    #   for (i in 1:SN) 
    #     if (nectar[i] > maxfit) maxfit <- nectar[i]
    #     
    #     prob <<- .9*(nectar/(maxfit+1e-20)) + .1
    #     #     prob[is.nan(prob)]  <<- .1
    # }
    # CalculateProbabilities()
    
    # onlooker bee
    i <- 1
    t <- 0
    while(t < SN){
      if(runif(1) < prob[[i]]){
        t <- t+1
        smell_neighbor()
      }
      i <- i+1
      if(i > SN) i <- 1
    }
    #
    appreciate_best_food()
    path <- rbind(path, c(global_par,  global_min))
    # scout bee
    send_scout_bees()
    round <- round + 1
  }
  
  # return(list(par = global_par, value = global_min, 
  #             foods = foods, obj = obj, nectar = nectar, 
  #             n_stay = n_stay, path = path, n_iter = round))
  return(par)
  
}

# send_employed_bees <- function(){
#   for(i in seq_len(SN)){
#     smell_neighbor()
#   }
# }

#get probabiliyoes
comment_on_food <- function(){
  prob <<- nectar/sum(nectar)
}

# send_onlooker_bees <- function(){
#   i <- 1
#   t <- 0
#   while(t < SN){
#     if(runif(1) < prob[[i]]){
#       t <- t+1
#       smell_neighbor()
#     }
#     i <- i+1
#     if(i > SN) i <- 1
#   }
# }

send_scout_bees <- function(){
  max_stay <- max(n_stay)
  if(max(n_stay) >= limit){
    leave <-   which.max(n_stay)
    foods[leave, ] <<- runif(D, lb, ub)
    obj[[leave]] <<- func(foods[leave,])
    nectar[[leave]] <<- taste_nectar(obj[[leave]])
    n_stay[[leave]] <<- 0
  }
    
}

smell_neighbor <- function(food){
  par_change <- sample(D, 1)
  sample_nei <- function(x){
    if(length(x)==1){ 
      return(x)
    } else {
        sample(x, 1)
      }
  }
  neighbor <- sample_nei((1:SN)[-i])
  nu <- foods[neighbor,]
  
  nu[[par_change]] <- foods[[i, par_change]] + runif(1, -1, 1)*(foods[[i, par_change]] - foods[[neighbor, par_change]])
  if(nu[[par_change]]<lb) nu[[par_change]] <- lb 
  if(nu[[par_change]]>ub) nu[[par_change]] <- ub 
  
  obj_nu <- func(nu)
  nectar_nu <- taste_nectar(obj_nu)
  
  if(nectar_nu >= nectar[[i]]){
    n_stay[[i]]  <<- 0
    foods[i,] <<- nu
    obj[[i]] <<- func(nu)
    nectar[[i]] <<- taste_nectar(obj[[i]])
  } else {
    n_stay [[i]] <<- n_stay[[i]] + 1
  }
  
}

taste_nectar <- function(obj){
  1/(1+obj)
}

appreciate_best_food <- function(){
  if(any(obj<=global_min)){
    global_min <<- min(obj)
    global_par <<- foods[which.min(obj),]
    n_unchange <<- 0
  } else {
    n_unchange <<- n_unchange+1
  }
}

a <- ABC(rep(0,2), fun, lb=-10, ub=10, n.stop=50, max.cycle = 1e3)
