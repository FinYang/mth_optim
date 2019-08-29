#include <Rcpp.h>
using namespace Rcpp;


//   
//   
//   

double ecu_dist( double x, double y) {
  return sqrt( x*x + y*y ) ;
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
      dist(j,i) = ecu_dist(poi[0], poi[1]);
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
    obj += ecu_dist(poi[0], poi[1]);
  }
  return obj;
}


/***R


# 
# 
# alloc1 <- .cluster_obj_cpp(par, as.matrix(x), k, d)
# 
# alloc2 <- cluster_obj(par, x, k, d)
# 
# # alloc3 <- allo_test(pts, centre)
# c(alloc1, alloc2)
# 
# # View(cbind(alloc1, alloc2, alloc3))
# # 
# # plot_clusters <- function(D, alloc, centre){
# #   ggplot() +
# #     geom_point(aes(x=D[,1], y=D[,2], color = as.factor(alloc)))+
# #     theme(legend.position="none") +
# #     geom_text(aes(x=X1, y=X2), data = as.data.frame(do.call(rbind, centre)),
# #               size=10, label = "+")
# # }
# # 
# bench::mark(.cluster_obj_cpp(par, as.matrix(x), k, d),
#             cluster_obj(par, x, k, d), check = F)
*/




