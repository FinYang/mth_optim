


#' @param par Vector. Initial value to search for parameter
#' @param fn The function to optimize over
#' @param ... Other arguments to the function
#' @param SN Integer. The number of solutions; The number of food source
#' @param limit Integer. The number of waiting perios before dropping a not-improving food source
#' @param max.cycle Integer. The maxmum number of iteration.
#' @param n.stop Integer. The number of unchanged results to stop optimizing
ABC_cluster <- function(par, fun, ...,data, k, SN  = 20, limit= 100, max.cycle= 1000, n.stop = 50, lb= rep(-Inf, length(par)), ub= rep(+Inf, length(par))){
  
  
  # send_employed_bees <- function(){
  #   for(i in seq_len(SN)){
  #     smell_neighbor()
  #   }
  # }
  
  #get probabiliyoes
  comment_on_food <- function(){
    prob <<- nectar/sum(nectar)
    prob[is.nan(prob)] <<- 0
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
    if(max_stay >= limit){
      leave <-   which.max(n_stay)
      foods[leave, ] <<- mapply(function(lb, ub) runif(D, lb, ub), lb=lb, ub=ub)
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
    if(nu[[par_change]]<lb[[par_change]]) nu[[par_change]] <- lb[[par_change]] 
    if(nu[[par_change]]>ub[[par_change]]) nu[[par_change]] <- ub[[par_change]] 
    
    obj_nu <- func(nu)
    nectar_nu <- taste_nectar(obj_nu)
    
    if(nectar_nu >= nectar[[i]]){
      n_stay[[i]]  <<- 0
      foods[i,] <<- nu
      obj[[i]] <<- obj_nu
      nectar[[i]] <<- taste_nectar(obj[[i]])
    } else {
      n_stay [[i]] <<- n_stay[[i]] + 1
    }
    
  }
  
  taste_nectar <- function(obj){
    1/(1+obj)
  }
  
  appreciate_best_food <- function(){
    if(any(obj<global_min)){
      global_min <<- min(obj)
      global_par <<- foods[which.min(obj),]
      n_unchange <<- 0
    } else {
      n_unchange <<- n_unchange+1
    }
  }
  
  func <- function(par) fun(par,k=k, ...)
  D <- length(par)
  if (length(lb) == 1 && length(par) > 1) 
    lb <- rep(lb, D)
  if (length(ub) == 1 && length(par) > 1) 
    ub <- rep(ub, D)
  lb[is.infinite(lb)] <- -.Machine$double.xmax * 1e-10
  ub[is.infinite(ub)] <- .Machine$double.xmax * 1e-10
  # ini
  # foods <- matrix(runif(SN*D, lb, ub), nrow = SN, ncol = D)
  # foods <- mapply(function(lb, ub) seq(lb,ub,length.out=SN), lb=lb, ub=ub)
  foods <- t(replicate(SN, c(as.matrix(data[sample(NROW(data), k),]))))
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
    
    i <- 1
    t <- 0
    while(t < SN){
      if(runif(1) < prob[[i]]){
        t <- t+1
        smell_neighbor()
      }
      if(all(prob==0)) break
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
  
  return(list(par = global_par, value = global_min,
              foods = foods, obj = obj, nectar = nectar,
              n_stay = n_stay, path = path, n_iter = round, n_unchange = n_unchange))
}


