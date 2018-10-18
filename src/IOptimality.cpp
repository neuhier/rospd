// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
double IOptimality(const Eigen::MatrixXd& currentDesign, const Eigen::MatrixXd& momentsMatrix, const Eigen::MatrixXd& blockedVar) {
  Eigen::MatrixXd XtX = (currentDesign.transpose() * (blockedVar.householderQr().solve(currentDesign))).householderQr().solve(momentsMatrix);
  return(XtX.trace());
}

