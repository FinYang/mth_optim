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


ABC_cpp(rep(0,2), fun, lb=-10, ub=10, n.stop=50)


library(Rcpp)


sourceCpp("ABC.cpp")
x

a <- ABC(rep(0,2), fun, lb=-10, ub=10, n.stop=50, max.cycle = 1e3)

.nectar(obj[[1]])
obj


.apprec(global_min, obj, foods, global_par, n_unchange)
evalCpp("2+2")

cppFunction("void appreciate_best_food(double & global_min, 
                          NumericVector & obj, 
                          NumericMatrix & foods, 
                          NumericVector & global_par, 
                          int & n_unchange){
  double before = global_min;
  for(int i=0; i < obj.size(); i++){
    if(obj[i] < global_min){
      global_min = obj[i];
      global_par = foods(i,_);
      n_unchange = 0;
    }
  }
  if(before == global_min){
    n_unchange++;
  }
  return;
}")
appreciate_best_food(global_min, obj, foods, global_par, n_unchange)


Rcpp::evalCpp("as<int>(Rcpp::sample(6,1))")


a <- ABC_cpp(rep(0,2), fun, lb=-10, ub=10, n.stop=50)
b <- ABC(rep(0,2), fun, lb=-10, ub=10, n.stop=50)
microbenchmark::microbenchmark(ABC_cpp(rep(0,2), fun, lb=-10, ub=10, n.stop=50),ABC(rep(0,2), fun, lb=-10, ub=10, n.stop=50) )
