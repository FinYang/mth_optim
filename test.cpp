#include <Rcpp.h>
using namespace Rcpp;

void smell_neighbor(NumericMatrix & foods,
                    int & D,
                    Function & fn, 
                    NumericVector & n_stay,
                    NumericVector & obj, 
                    NumericVector & nectar, 
                    int & SN, 
                    int current_bee, 
                    NumericVector & lb, 
                    NumericVector & ub){
  int par_change = as<int>(Rcpp::sample(D,1));
  int neighbor(1);
  do{
    neighbor = as<int>(Rcpp::sample(SN,1));
  } while (neighbor != current_bee);
  NumericVector nu = foods(neighbor,_);
  
  nu[par_change] = foods(current_bee, par_change) + R::runif(-1,1)*(foods(current_bee, par_change)-foods(neighbor, par_change));
  if(nu[par_change]<lb[par_change]) nu[par_change] = lb[par_change]; 
  if(nu[par_change]>ub[par_change]) nu[par_change] = ub[par_change];
  
  double obj_nu = as<double>(fn(nu));
  double nectar_nu = taste_nectar(obj_nu);
  
  if(nectar_nu >= nectar[current_bee]){
    n_stay[current_bee]  = 0;
    foods(current_bee,_) = nu;
    obj[current_bee] = obj_nu;
    nectar[current_bee] = nectar_nu;
  } else {
    n_stay[current_bee]++;
  }
  
}
// [[Rcpp::export(name=".test_cpp")]]
List test(NumericMatrix & foods,
          int & D,
          Function & fn, 
          NumericVector & n_stay,
          NumericVector & obj, 
          NumericVector & nectar, 
          int & SN, 
          int current_bee, 
          NumericVector & lb, 
          NumericVector & ub){
  smell_neighbor(NumericMatrix & foods,
                 int & D,
                 Function & fn, 
                 NumericVector & n_stay,
                 NumericVector & obj, 
                 NumericVector & nectar, 
                 int & SN, 
                 int current_bee, 
                 NumericVector & lb, 
                 NumericVector & ub);
  return List::create(prob, path, path_value, 
                      global_min, obj, foods, global_par, n_unchange, nectar, round, n_unchange, tpa);
  
}





/***R
.test_cpp(global_min, obj, foods, global_par, n_unchange)


*/