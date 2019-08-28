ABC_cpp <- function(par, fun, ..., SN  = 20, limit= 100, max.cycle= 1000, 
                    n.stop = 50, lb= rep(-Inf, length(par)), ub= rep(+Inf, length(par))){
  if (length(lb) == 1) lb <- rep(lb, length(par))
  if (length(ub) == 1) ub <- rep(ub, length(par))
  
  lb[is.infinite(lb)] <- -(.Machine$double.xmax*1e-10)
  ub[is.infinite(ub)] <- +(.Machine$double.xmax*1e-10)
  func <- function(par) fun(par, ...)
  foods <- mapply(function(lb, ub) seq(lb,ub,length.out=SN), lb=lb, ub=ub)
  .abc_cpp(par = par, fun=func, foods = foods, SN = SN, limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)
  
}
