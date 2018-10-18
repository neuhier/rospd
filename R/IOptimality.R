
#' Calculates the i-optimality of a design.
#'
#' I-optimality is the average prediction variance of a design. It is calculated using
#' the vdg-package.
#'
#' @param doe ...
#'
#' @export
#'
IOptimality <- function(doe){

  if (length(getHtcFactors(doe)) > 0) {

    stop("I-Optimality is currently not implemented for split plot designs.")
    # Not implemented

  } else {

    iopt <- -Inf

    tryCatch({
      i = 1
      repeat{
        # print(paste("still alive ", i))
        sim.results <- spv(n=2000, design = doe@design.table, type="cuboidal", formula=doe@design.model, unscaled=TRUE)
        iopt <- mean(sim.results$spv, na.rm = TRUE)
        i <- i+1
        if(!is.nan(iopt) | i >=50) break # sometimes it happens that spv returns only Nans
      }
    },
    warning = function(w){
    # print(w)
    },
    error = function(e){
    # print(e)
    }
    )

  }
  (-1)*iopt # Algorithm is for maximizing the optimality -> we want to minimize prediction variance

}
