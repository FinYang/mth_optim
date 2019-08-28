#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name=".test_cpp")]]
void lalala(int & c){
  c++;
}


/***R
a <- 1:10
b <- 1:10
c <- 10
d <- 10
lb <- rep(0, 2)
ub <- rep(1,2)

.test_cpp(c)


*/