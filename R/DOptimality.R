#---------------------------------------+
# Calculate d-optimality for a doeDesign.
#---------------------------------------+
#' Calculate the D-optimality of a design.
#'
#' This function calculates the D-Optimality of a design (\eqn{d_{opt} = |X'X|}, where \eqn{X} is the design matrix). D-optimality is
#' calculated for completely randomized designs or for split-plot-designs. For split plot designs D-Optimality is calculated based on Jones, Goos (2017):
#' \eqn{|M| = |X'V^{-1}X|} with V being the covariance matrix of the responses: \eqn{V=\sigma^2_{\epsilon} I_n + \sigma_{\gamma}^2ZZ'} and
#' \eqn{\sigma^2_{\epsilon}} represents the error variation and \eqn{\sigma^2_{\gamma}} represents the whole-plot-variation.
#'
#' @export
#'
#' @references Bradley Jones & Peter Goos (2017) I-Optimal Versus D-Optimal Split-Plot Response Surface Designs, Journal of Quality Technology, 44:2, 85-101, DOI: 10.1080/00224065.2012.11917886
#'
#' @examples
#'splitplot <- GenerateNewDoeDesign(
#'  factors = list(
#'   new("doeFactor", name="X1", levels=c("A", "B", "C"), type="categorical", changes="hard"),
#'   new("doeFactor", name="X2", levels=c(0, 1))
#'  ),
#'  number.runs = as.integer(20),
#'  whole.plot.structure=data.frame(X1=rep(1:5, each=4)),
#'  design.model = ~X1*X2,
#'  optimality.function=DOptimality,
#'  optimality.criterion = "D-Optimality",
#'  random.doe = TRUE
#' )
#'
#' splitplot.doe <- GenerateOptimalDesign(splitplot, random.start=1, max.iter=3)
#'
#' DOptimality(splitplot.doe[[1]])
DOptimality <- function(doeDesign) {
  # Calculate D-optimality of crds or split-plot-designs.
  #
  # Args:
  #   doeDesign: an object of class doeDesign.
  #
  # Returns:
  #   A numeric that is the d-optimality of the given design.


  # DOptimality calculates differently depending on if there is a wholeplot effect or not
  designMatrixes = GenerateDesignMatrix(doeDesign)
  X = designMatrixes$X
  Z = designMatrixes$Z

  if (length(getHtcFactors(doeDesign)) > 0) {
    V <-  diag(rep(1, nrow(X))) + doeDesign@variance.ratio * Z %*% t(Z)# sigma_e^2*I + sigma_g^2*Z%*%t(Z)
    dopt <- det(t(X) %*% solve(V) %*% X)

  } else {
    dopt <- cDOptimality(X) # Using the c-code
    # dopt = det(t(X) %*% X)

  }
  dopt

}

#' Calculate the D-Efficiency for Designs without hard to change factors
#'
#' D-Efficiency expresses the quality of a design in terms of D-Optimality as a percentage. 100% D-Efficiency is the
#' best possible design (in terms of D-Optimality) for a given model and a given number of runs. D-Efficiency is calculated
#' as \eqn{|(X'X)^{-1}|^{1/p}}.
#'
#' Currently there is no implementation for split-plot-designs (or any design that uses a hard to change or semi hard to
#' change factor).
#'
#' @export
#'
#' @param doeDesign An object of class \code{\link{doeDesign-class}} for which the D-Efficiency should be calculated.
#'
#' @return A numeric representing the d-efficiency of the design on a range from 0 to 100.
#'
#' @examples
#'doe <- GenerateNewDoeDesign(
#'  factors = list(
#'   new("doeFactor", name="X1"),
#'   new("doeFactor", name="X2")
#'  ),
#'  number.runs = as.integer(20),
#'  design.model = ~X1*X2,
#'  optimality.function=DOptimality,
#'  optimality.criterion = "D-Optimality",
#'  random.doe = TRUE
#' )
#'
#' # Defficiency of the randon initial design
#' DEfficiency(doe)
#'
#' # Defficiency of the optimized design
#' doe.dopt <- GenerateOptimalDesign(doe, 1, 10)
#' DEfficiency(doe.dopt[[1]])
#'
DEfficiency <- function(doeDesign) {

  designMatrixes = GenerateDesignMatrix(doeDesign)
  X = designMatrixes$X
  Z = designMatrixes$Z
  n = nrow(X)
  p = ncol(X)

  if (length(getHtcFactors(doeDesign)) > 0) {
    # See: http://www.okstate.edu/sas/v8/sashtml/qc/chap24/sect28.htm
    # A <- diag(rep(1, nrow(X))) - Z%*%solve(t(Z)%*%Z)%*%t(Z)
    #
    # eigenvalues <- svd(A)$d
    # eigenvalues <- eigenvalues[order(eigenvalues,decreasing = TRUE)]
    # eigenvalues <- eigenvalues[1:p]
    #
    # deff <- (100 * prod(eigenvalues^(1/p))^(-1) * det(t(X)%*%A%*%X)^(1/p))/n

    # V = diag(rep(1, nrow(X))) + doeDesign@variance.ratio*Z%*%t(Z)# sigma_e^2*I + sigma_g^2*Z%*%t(Z)
    #
    # eigenvalues <- svd(V)$d
    # eigenvalues <- eigenvalues[order(eigenvalues,decreasing = TRUE)]
    # eigenvalues <- eigenvalues[1:p]
    #
    # deff <- ((DOptimality(doeDesign)^(1/p))/n)*100*prod(eigenvalues^(1/p))

    deff <-
      NA # Somehow this calculation does not work correctly right now

  } else {
    deff <- ((DOptimality(doeDesign) ^ (1 / p)) / n) * 100

  }

  deff
}


DEfficiency.compare <- function(doeDesign1, doeDesign2) {
  # Compare two designs in terms of Doptimality.
  #
  # Args:
  #   doeDesign: an object of class doeDesign.
  #
  # Returns:
  #   A numeric that is the percentual advantage of doeDesign1 over doeDesign2 in terms of Doptimality.

  X = GenerateDesignMatrix(doeDesign1)$X
  p = ncol(X)

  d1 = DOptimality(doeDesign1)
  d2 = DOptimality(doeDesign2)

  deff = (d1 / d2) ^ (1 / p)
  deff
}
