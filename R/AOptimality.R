#---------------------------------------+
# Calculate d-optimality for a doeDesign.
#---------------------------------------+

# TODO: Add weights

#' Calculate A Optimality for a design.
#'
#' @param doeDesign the design to calculate the A-Optimality from-
#'
#' @export
#'
AOptimality <- function(doeDesign) {
  # Calculate A-optimality of crds or split-plot-designs.
  #
  # Args:
  #   doeDesign: an object of class doeDesign.
  #
  # Returns:
  #   A numeric that is the A-optimality of the given design.

  aopt = 0

  # DOptimality calculates differently depending on if there is a wholeplot effect or not
  designMatrixes <- GenerateDesignMatrix(doeDesign)
  X <- designMatrixes$X
  Z <- designMatrixes$Z

  if (length(getHtcFactors(doeDesign)) > 0) {
    V <-
      diag(rep(1, nrow(X))) + doeDesign@variance.ratio * Z %*% t(Z)# sigma_e^2*I + sigma_g^2*Z%*%t(Z)
    tryCatch({
      aopt <- sum(diag((t(X) %*% solve(V) %*% X)))
    },
    warning = function(w) {
      aopt <- 0
      # Do not print warnings
    },
    error = function(e) {
      aopt <- 0
    })
  } else {
    tryCatch({
      aopt <- sum(diag(solve(t(X) %*% X)))
    },
    warning = function(w) {
      aopt <- 0
      # Do not print warnings
    },
    error = function(e) {
      aopt <- 0
    })

  }

  - aopt #

}


#' Calculate the A-Efficiency for Designs without hard to change factors
#'
#' A-Efficiency expresses the quality of a design in terms of A-Optimality as a percentage. 100% A-Efficiency is the
#' best possible design (in terms of A-Optimality) for a given model and a given number of runs. A-Efficiency is calculated
#' as \eqn{100*(1/n*trace((X'X)^{-1})/p)}.
#'
#' Currently there is no implementation for split-plot-designs (or any design that uses a hard to change or semi hard to
#' change factor).
#'
#' @export
#'
#' @param doeDesign An object of class \code{\link{doeDesign-class}} for which the A-Efficiency should be calculated.
#'
#' @return A numeric representing the A-efficiency of the design on a range from 0 to 100.
#'
#' @examples
#'doe <- GenerateNewDoeDesign(
#'  factors = list(
#'   new("doeFactor", name="X1"),
#'   new("doeFactor", name="X2")
#'  ),
#'  number.runs = as.integer(20),
#'  design.model = ~X1*X2,
#'  optimality.function=AOptimality,
#'  optimality.criterion = "A-Optimality",
#'  random.doe = TRUE
#' )
#'
#' # AEfficiency of the randon initial design
#' AEfficiency(doe)
#'
#' # Aefficiency of the optimized design
#' doe.Aopt <- GenerateOptimalDesign(doe, 1, 10)
#' AEfficiency(doe.Aopt[[1]])
#'
AEfficiency <- function(doeDesign) {
  designMatrixes = GenerateDesignMatrix(doeDesign)
  X = designMatrixes$X
  #Z = designMatrixes$Z
  n = doeDesign@number.runs
  p = ncol(X)

  if (length(getHtcFactors(doeDesign)) > 0) {
    aeff <-
      NA # Somehow this calculation does not work correctly right now

  } else {
    tryCatch({
      aeff <- 100 * (1 / (n * sum(diag(
        solve(t(X) %*% X)
      )) / p))
    },
    warning = function(w) {
      aeff <- 0
      # Do not print warnings
    },
    error = function(e) {
      aeff <- 0
    })

  }
  aeff
}
