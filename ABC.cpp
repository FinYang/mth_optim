#include <Rcpp.h>
using namespace Rcpp;





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
  
}

// [[Rcpp::export(name=".nectar")]]
double taste_nectar(double obj){
  return 1/(1+obj);
}

// [[Rcpp::export(name=".scout")]]
void send_scout_bees(NumericVector & n_stay, 
                     int & limit, 
                     NumericVector & lb, 
                     NumericVector & ub, 
                     NumericMatrix & foods, 
                     NumericVector & obj, 
                     int & D, 
                     NumericVector & nectar, 
                     Function & fn){
  // Function here -------------------------------------
  int max_stay=0;
  int leave = 0;
  for(int i=0; i< n_stay.size(); i++){
    if(n_stay[i] > max_stay){
      max_stay = n_stay[i];
      leave = i;
    }
  }
  if(max_stay >= limit){
    for(int i =0; i<D; i++)
      foods(leave,i) = R::runif(lb[i], ub[i]);
    obj[leave] = as<double>(fn(foods(leave,_)));
    nectar[leave] = taste_nectar(obj[leave]);
    n_stay[leave] = 0;
  }
}

// [[Rcpp::export(name=".apprec")]]
void appreciate_best_food(double & global_min, 
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
}




// [[Rcpp::export(name=".smell")]]
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



// [[Rcpp::export(name=".abc_cpp")]]
List ABC_cpp(NumericVector par, 
             Function fun, 
             NumericMatrix foods,
             int SN, 
             int limit, 
             int max_cycle, 
             int n_stop, 
             NumericVector lb, 
             NumericVector ub){
  // Initialize
  int D=par.size();
  
  // NumericMatrix foods(SN, D);
  // 
  // for(int j=0; j < D; j++){
  //   foods(_,j) = runif(SN);
  // };
  
  NumericVector obj(SN);
  NumericVector nectar(SN);
  for(int i=0; i<SN;i++){
    obj[i] = as<double>(fun(foods(i,_)));
    nectar[i] = taste_nectar(obj[i]);
  }
  NumericVector n_stay(SN);
  
  NumericVector global_par = clone(par);
  double global_min = as<double>(fun(global_par));
  int n_unchange = 0;
  
  appreciate_best_food(global_min, obj, foods, global_par, n_unchange);
  int round = 0;
  
  NumericVector prob(SN);
  NumericMatrix path(max_cycle, D+1);
  NumericVector path_value(max_cycle);
  //  employed bees
  while(round < max_cycle && n_unchange<n_stop){
    for(int i=0; i<SN;i++){
      smell_neighbor(foods, D, fun, n_stay, obj, nectar, SN, i, lb, ub);
    }
    
    
    comment_on_food(prob, nectar, SN);
    //  onlooker
    int j = 0;
    int t = 0;
    while(t<SN){
      if(R::runif(0,1) < prob[j]){
        t++;
        smell_neighbor(foods, D, fun, n_stay, obj, nectar, SN, j, lb, ub);
      }
      if(is_true(Rcpp::all(prob==0))) break;
      j++;
      if(j == SN) j=0;
    }
    
    
    appreciate_best_food(global_min, obj, foods, global_par,  n_unchange);
    path(round,_) = global_par;
    path_value[round] = global_min;
    //  scout
    send_scout_bees(n_stay, limit, lb, ub, foods, obj, D, nectar, fun);
    round++;
    
  }
  path = path(Rcpp::Range(0,round),_);
  path_value = Rcpp::na_omit(path_value);
  
  
  
  return List::create(_["par"] = global_par, 
                      _["value"] = global_min,
                      _["foods"] = foods, 
                      _["obj"] = obj, 
                      _["nectar"] = nectar,
                      _["n_stay"] = n_stay, 
                      _["path"] = path,
                      _["path_value"] = path_value,
                      _["n_iter"] = round+1, 
                      _["n_unchange"] = n_unchange, 
                      _["prob"] = prob);
}











/***R

*/

