par <- rep(0,2)
funpi <- function(x) {
  -cos(x[1])*cos(x[2])*exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
}
lb=-10
ub=10

n.stop=50
max.cycle = 1e3
SN  = 20
limit= 100


# ABC_cpp(rep(0,2), fun, lb=-10, ub=10, n.stop=50)



a <- ABC_cpp(rep(0,2), funpi, lb=-10, ub=10, n.stop=50)
b <- ABC(rep(0,2), fun, lb=-10, ub=10, n.stop=50)
microbenchmark::microbenchmark(ABC_cpp(rep(0,2), funpi, lb=-10, ub=10, n.stop=50),
                               ABC(rep(0,2), funpi, lb=-10, ub=10, n.stop=50) )


test <- function(){
  mapply(function(lb, ub) runif(SN,lb,ub), lb=lb, ub=ub) %>% return()
}
