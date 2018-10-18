
GEfficiency <- function(doeDesign){

  #Test
  doeDesign <- doe.dopt[[1]]
  #TEST

  designMatrixes = GenerateDesignMatrix(doeDesign)
  X = designMatrixes$X
  n = doeDesign@number.runs
  p = ncol(X)
  v = length(doeDesign@factors)

  if (length(getHtcFactors(doeDesign)) > 0) {

    geff <-
      NA # Somehow this calculation does not work correctly right now

  } else {

    apply(X, 1, function(x){t(x)%*%x})


    geff <- NA
  }
  geff
}


