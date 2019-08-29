#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name=".test_cpp")]]
List test(NumericVector par, 
          Function fun, 
          NumericMatrix foods,
          int SN, 
          int limit, 
          int max_cycle, 
          int n_stop, 
          NumericVector lb, 
          NumericVector ub){
 
  return List::create(SN, limit, max_cycle, n_stop, lb, ub, fun);
  
}





/***R
.test_cpp(par = par, fun=func, foods = foods, SN = SN, 
          limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)


*/