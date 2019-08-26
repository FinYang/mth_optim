#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(name=".abc_cpp")]]
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


void comment_on_food(NumericVector & prob, 
                     NumericVector & nectar, 
                     int & SN){
  double total = 0;
  for (int i = 0; i < SN; ++i){
    total += nectar[i];
  }
  
  for (int i = 0; i < SN; ++i){
    prob[i] = nectar[i]/total;
  }
  
  return;
}


// [[Rcpp::export(name=".test_cpp")]]

void send_scout_bees(NumericVector & n_stay, 
                     int & limit, 
                     NumericVector & lb, 
                     NumericVector & ub, 
                     NumericMatrix & foods, 
                     NumericVector & obj, 
                     int & D, 
                     NumericVector & nectar){
  double max_stay=0;
  int leave = 0;
  for(int i=0, i< n_stay.size(); i++){
    if(n_stay[i] > max_stay){
      max_stay = n_stay[i];
      leave = i;
    }
  }
  if(max_stay >= limit){
    foods(leave,_) = 
  }
  
  
  
}







/***R

*/

