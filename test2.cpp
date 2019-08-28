#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name=".test_cpp")]]
NumericVector test(NumericVector prob){
  int n = 4;
  return prob[Range(0,n-1)];
}  





/***R
.test_cpp(1:10)


*/