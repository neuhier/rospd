// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
double cDOptimality(const Eigen::MatrixXd& currentDesign) {
  Eigen::MatrixXd XtX = currentDesign.transpose() * currentDesign;
  return(XtX.partialPivLu().determinant());
}

