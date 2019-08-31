#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name=".test_cpp")]]
NumericVector test(NumericVector x){
  x[0]++;
  return x;
  
}





/***R
# .test_cpp(par = par, fun=func, foods = foods, SN = SN, 
#           limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)

.test_cpp(1:3)
*/