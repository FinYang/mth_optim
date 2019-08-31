#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name=".test_cpp")]]
double ecu_dist(NumericVector x) {
  double out = 0;
  double n= x.size();
  for(int i=0;i<x.size();i++){
    out += x[i]*x[i];
  }
  double power = 1/n;
  return pow(out, power);
}





/***R
# .test_cpp(par = par, fun=func, foods = foods, SN = SN, 
#           limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)

.test_cpp(c(0,0))
*/