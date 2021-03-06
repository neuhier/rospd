% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doeDesign.R
\name{GenerateNewDoeDesign}
\alias{GenerateNewDoeDesign}
\title{This function is a wrapper for new("doeDesign", ...).}
\usage{
GenerateNewDoeDesign(responses = list("Y"),
  factors = list(new("doeFactor")), check.validity = function(doe) {    
  TRUE }, number.runs = as.integer(0), replicate.type = "none",
  replicate.n = as.integer(0), whole.plot.structure = data.frame(),
  design.model = formula(Y ~ x), design.matrix = list(X = data.frame(), Y =
  data.frame()), design.table = data.frame(),
  iteration.table = data.frame(), variance.ratio = 1,
  replicate.structure = vector(), optimality.function = function() { },
  optimality.criterion = "", optimality = NA_real_, random.doe = FALSE)
}
\description{
This is the recommended way of initializing a new doeDesign. The function creates a new instance of
an object of class \code{\link{doeDesign-class}} and initializes some relevant slots in the background
(iteration.matrix, sorting of factors, ...).
}
\details{
For more details about the arguments see \code{\link{doeDesign-class}}.
}
