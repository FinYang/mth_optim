#include <Rcpp.h>
using namespace Rcpp;





////////////////////////////////////////////////////////////////////////////////////////////////
// void comment_on_food(NumericVector & prob, 
//                      NumericVector & nectar, 
//                      int & SN){
//   double total = 0;
//   for (int i = 0; i < SN; ++i){
//     total += nectar[i];
//   }
//   
//   for (int i = 0; i < SN; ++i){
//     prob[i] = nectar[i]/total;
//   }
//   
// }
void comment_on_food(NumericVector & prob,
                     NumericVector  nectar,
                     int & SN){
  NumericVector tem = clone(nectar);
  double maxfit = tem[0];
  for (int i = 0; i < SN; i++){
    if(nectar[i] > maxfit) maxfit = nectar[i];
  }
  for (int i = 0; i < SN; i++){
    prob[i] = (0.9*((nectar[i] + 1e-40)/(maxfit + 1e-40))) + 0.1;
    
  }

}

////////////////////////////////////////////////////////////////////////////////////////////////
double taste_nectar(double obj){
  if(obj >= 0){
    return 1/(1+obj);
  } else {
    return 1-obj;
  }
}

// send_scout_bees(n_stay, limit, lb, ub, foods, obj, D, nectar, fun);
////////////////////////////////////////////////////////////////////////////////////////////////
void send_scout_bees(NumericVector & n_stay, 
                     int & limit, 
                     NumericVector & lb, 
                     NumericVector & ub, 
                     NumericMatrix & foods, 
                     NumericVector & obj, 
                     int & D, 
                     NumericVector & nectar, 
                     Function & fn){
  // int max_stay=0;
  int leave = 0;
  for(int i=0; i< n_stay.size(); i++){
    if(n_stay[i] > n_stay[leave]){
      // max_stay = n_stay[i];
      leave = i;
    }
  }
  if(n_stay[leave] >= limit){
    for(int i =0; i<D; i++){
      foods(leave,i) = R::runif(lb[i], ub[i]);
    }
    obj[leave] = as<double>(fn(foods(leave,_)));
    nectar[leave] = taste_nectar(obj[leave]);
    n_stay[leave] = 0;
  }
}

// appreciate_best_food(global_min, obj, foods, global_par, n_unchange);
////////////////////////////////////////////////////////////////////////////////////////////////
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
    }
  }
  if(before == global_min){
    n_unchange++;
  } else {
      n_unchange = 0;
  }
}


// smell_neighbor(foods, D, fun, n_stay, obj, nectar, SN, i, lb, ub);
////////////////////////////////////////////////////////////////////////////////////////////////
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
  // int par_change = as<int>(Rcpp::sample(D,1)-1);
  int par_change = Rcpp::sample(D,1)[0]-1;
  int neighbor(1);
  
  do{
    neighbor = Rcpp::sample(SN,1)[0]-1;
  } while (neighbor == current_bee);
  NumericVector tem = foods(current_bee,_);
  NumericVector nu = clone(tem);
  
  nu[par_change] = foods(current_bee, par_change) + R::runif(-1,1)*(foods(current_bee, par_change)-foods(neighbor, par_change));
  if(nu[par_change]<lb[par_change]) nu[par_change] = lb[par_change]; 
  if(nu[par_change]>ub[par_change]) nu[par_change] = ub[par_change];
  
  // change another par
  
  // par_change = Rcpp::sample(D,1)[0]-1;
  // nu[par_change] = foods(current_bee, par_change) + R::runif(-1,1)*(foods(current_bee, par_change)-foods(neighbor, par_change));
  // if(nu[par_change]<lb[par_change]) nu[par_change] = lb[par_change]; 
  // if(nu[par_change]>ub[par_change]) nu[par_change] = ub[par_change];
  
  double obj_nu = as<double>(fn(nu));
  double nectar_nu = taste_nectar(obj_nu);
  
  if(nectar_nu > nectar[current_bee]){
    n_stay[current_bee]  = 0;
    foods(current_bee,_) = nu;
    obj[current_bee] = obj_nu;
    nectar[current_bee] = nectar_nu;
  } else {
    n_stay[current_bee]++;
  }
  
}

void rcpp_rprintf(NumericVector v){
  // printing values of all the elements of Rcpp vector  
  for(int i=0; i<v.length(); ++i){
    Rprintf("the value of v[%i] : %f \n", i, v[i]);
  }
}

// 
// 
// 
// 
// 
// 
// 
// 
// //////////////////////////////////////////////////////////////////////////////////////////////
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
  
  // f
  NumericVector obj(SN);
  // fitness
  NumericVector nectar(SN);
  // trail
  NumericVector n_stay(SN);
  // prob
  NumericVector prob(SN);
  //  solution ****
  // solution    <- double(D)
  //   ObjValSol   <- double(1)
  //   FitnessSol  <- double(1)
  //   neighbour   <- integer(1)
  //   param2change<- integer(1)
  
  // globalmin
  double global_min = as<double>(fun(par));
  // globalparams
  NumericVector global_par = par;
  // r *****
  
  
  for(int i=0; i<SN;i++){
    obj[i] = as<double>(fun(foods(i,_)));
    nectar[i] = taste_nectar(obj[i]);
    n_stay[i] = 0;
  }
  
  int n_unchange = 0;
  
  appreciate_best_food(global_min, obj, foods, global_par, n_unchange);
  
  NumericMatrix path(max_cycle, D);
  NumericVector path_value(max_cycle);
  path(0,_) = global_par;
  path_value[0] = global_min;
  
  int round = 0;
  while(round < max_cycle){
  //  employed bees
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
      if(is_true(Rcpp::all(Rcpp::na_omit(prob)==0)) || Rcpp::na_omit(prob).size() == 0) break;
      
      j++;
      if(j == SN) j=0;
    }
    
    appreciate_best_food(global_min, obj, foods, global_par,  n_unchange);
    path(round+1,_) = global_par;
    path_value[round+1] = global_min;
    if(n_unchange >= n_stop) break;
    //  scout
    send_scout_bees(n_stay, limit, lb, ub, foods, obj, D, nectar, fun);
    
    round++;
    
    
  }
  path = path(Rcpp::Range(0,round),_);
  path_value = path_value[Range(0,round)];
  
  
  
  return List::create(_["par"] = global_par,
                      _["value"] = global_min,
                      _["foods"] = foods,
                      _["obj"] = obj,
                      _["nectar"] = nectar,
                      _["n_stay"] = n_stay,
                      _["path"] = path,
                      _["path_value"] = path_value,
                      _["n_iter"] = round,
                      _["n_unchange"] = n_unchange,
                      _["prob"] = prob);
}











/***R
# .abc_cpp(par = par, fun=func, foods = foods, SN = SN, 
#          limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)
ABC_cpp <- function(par, fun, ..., SN  = 20, limit= 100, max.cycle= 1000,
                    n.stop = 50, lb= rep(-Inf, length(par)), ub= rep(+Inf, length(par))){
  if (length(lb) == 1) lb <- rep(lb, length(par))
  if (length(ub) == 1) ub <- rep(ub, length(par))

  lb[is.infinite(lb)] <- -(.Machine$double.xmax*1e-10)
  ub[is.infinite(ub)] <- +(.Machine$double.xmax*1e-10)
  func <- function(par) fun(par, ...)
  foods <- mapply(function(lb, ub) seq(lb,ub,length.out=SN), lb=lb, ub=ub)
  .abc_cpp(par = par, fun=func, foods = foods, SN = SN, limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)

}

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

a <- ABC_cpp(rep(0,2), fun, lb=-10, ub=10, n.stop=100)
*/

