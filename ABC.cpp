#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name="ABC_cpp")]]
List ABC_cpp(NumericVector par, Function fun, int SN, int limit, int maxCycle, int nStop, double lb, double ub){
  // Initialize
  NumericMatrix foods(SN, par.size());
  NumericVector fitness(SN);
  
  NumericVector globalPar = clone(par);
  double globalMin = as<double>(fun(globalPar));
  
  for(int j=0; j < par.size(); j++){
    foods(_,j) = runif(SN);
  };
    
  
  // Employed bees
  
  
  
  
  
  return List::create(_["foods"] = foods);
}


/***R
ABC(1:2, sum)

*/

