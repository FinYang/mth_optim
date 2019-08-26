par <- rep(0,2)
fun <- function(x) {
  -cos(x[1])*cos(x[2])*exp(-((x[1] - pi)^2 + (x[2] - pi)^2))
}
lb=-10
ub=10

n.stop=50
max.cycle = 1e3
SN  = 20
limit= 100

library(Rcpp)


sourceCpp("ABC.cpp")
x

a <- ABC(rep(0,2), fun, lb=-10, ub=10, n.stop=50, max.cycle = 1e3)