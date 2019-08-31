#include <Rcpp.h>
using namespace Rcpp;


//   
//   
//   

double ecu_dist(NumericVector x) {
  double out = 0;
  double n= x.size();
  for(int i=0;i<x.size();i++){
    out += x[i]*x[i];
  }
  double power = 1/n;
  return pow(out, power);
}

// [[Rcpp::export(name=".closest_allocation_cpp")]]
IntegerVector closest_allocation(NumericMatrix x, NumericMatrix centre){
  int k = centre.nrow();
  int n = x.nrow();
  IntegerVector alloc(n);
  NumericMatrix dist(n,k);
  for(int i=0;i<k;i++){
    for(int j=0;j<n;j++){
      NumericVector poi = x(j,_)-centre(i,_);
      dist(j,i) = ecu_dist(poi);
    }
  }
  for(int i=0;i<n;i++){
    alloc[i] = Rcpp::which_min(dist(i,_));
  }
  return alloc;
  
}

// [[Rcpp::export(name=".cluster_obj_cpp")]]
double cluster_obj_cpp(NumericVector par, NumericMatrix x, int k, int d){
  NumericVector cen = clone(par);
  cen.attr("dim") = Dimension(k, d);
  NumericMatrix centre = as<NumericMatrix>(cen);
  IntegerVector alloc = closest_allocation(x, centre);
  double obj = 0;
  for(int i=0;i<alloc.size();i++){
    int n_cen = alloc[i];
    NumericVector poi = x(i,_) - centre(n_cen,_);
    obj += ecu_dist(poi);
  }
  return obj;
}

// [[Rcpp::export(name=".db")]]
NumericVector db(NumericVector par, NumericMatrix x, int k, int d){
  NumericVector cen = clone(par);
  cen.attr("dim") = Dimension(k, d);
  NumericMatrix centre = as<NumericMatrix>(cen);
  IntegerVector alloc = closest_allocation(x, centre);
  NumericVector dist_in(k);
  NumericVector size_in(k);
  for(int i=0; i<alloc.size();i++){
    int n_cen = alloc[i];
    NumericVector poi = x(i,_) - centre(n_cen,_);
    dist_in[n_cen] += ecu_dist(poi);
    size_in[n_cen]++;
  }
  for(int i=0; i<k;i++){
    dist_in[i] = dist_in[i]/size_in[i];
  }
  // NumericMatrix dist_intra(k,k);
  // for(int i=0;i<k;i++){
  //   for(int j=0;j<k;j++){
  //     NumericVector poii = centre(i,_) - centre(j,_);
  //     dist_intra(i,j) = ecu_dist(poii);
  //   }
  // }
  return dist_in;
}

/***R
# .test_cpp(par = par, fun=func, foods = foods, SN = SN, 
#           limit = limit, max_cycle = max.cycle, n_stop = n.stop, lb=lb,ub=ub)

s2 <- .db(partition$par, as.matrix(x), k=k, d=d)
*/