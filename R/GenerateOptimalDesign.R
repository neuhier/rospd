
#----------------------------------------+
# Generate an optimal design.
#----------------------------------------+

#' Algorithm to generate optimal designs.
#'
#' This function implements a variant of the candidate-free optimal algorithm described in Jones (2007).
#'
#' @export
#'
#' @param doeSpec An object of class \code{\link{doeDesign-class}} defining the attributes of the DOE.
#' @param random.start How many random starts shall be tried. For each random start one DOE will be generated. Depending on the complexity of the DOE (especially: number of factors and number of factor levels) the runtime of this function might increased by a lot when using many random starts.
#' @param max.iter The number of iterations that are used for each random start. One iteration is updating each cell in the DOE once. Depending on the complexity of the DOE (especially: number of factors and number of factor levels) the runtime of this function might increased by a lot when using many iterations.
#' @param quick.shtc.search (boolean, default is TRUE) Testing all possible combinations for SHTC factors in the updating of the design table can be extremly
#'                   time consuming. Quick shtc search follows a fast heuristic approach.
#' @param continue (boolean, default is FALSE) If TRUE the current design table is used as starting design instead of a
#'                 random design. If TRUE the argument random.start is ignored.
#' @param silent   (boolean, default is FALSE) Should there be a status bar be printed in the console or not.
#' 
#' @return A list with on optimized DOE for each random start. The list is ordered from best to worst in terms of optimality.
#'
#' @references Jones, B., & Goos, P. (2007). A candidate-set-free algorithm for generating D-optimal split-plot designs. Journal of the Royal Statistical Society. Series C, Applied Statistics, 56(3), 347–364. http://doi.org/10.1111/j.1467-9876.2007.00581.x
#'
GenerateOptimalDesign <-
  function(doeSpec,
           random.start = 1,
           max.iter = 25,
           quick.shtc.search = TRUE,
           continue = FALSE,
           silent=FALSE) {

    # Reset random.start in case of continuing
    if(continue){
      random.start <- 1
    }
      
    #-----------------------+
    # Error checking
    #-----------------------+
    if(continue & !is.data.frame(doeSpec@design.table)){
      stop("To continue design generate a starting design table needs to be provided in desig.table of doeSpec.")
    }
    
    all.does <- list() # Store all generated does here

    # Some required parameters
    p <- length(doeSpec@factors)
    this.optimality.function <- doeSpec@optimality.function

    all.levels <- sapply(doeSpec@factors, getPossibleLevels, simplify = FALSE)
    names(all.levels) <- getFactorNames(doeSpec)

    # Get all unique values in iteration table
    indices <- unlist(sapply(doeSpec@iteration.table, unique))
    indices <- indices[order(indices, decreasing = FALSE)]

    # Set up a progress bar
    if(!silent){
      pb <- txtProgressBar(min = 0, max = random.start * max.iter, style = 3)
    }

    # GENERATE ONE RANDOM STARTING DESIGN
    for (r in 1:random.start) {
      # Initialize a random DOE
      tmp.doe <- doeSpec # tmp.doe always stores the currently best design
      
      if(!continue){
        tmp.doe@design.table <- GenerateRandomDesign(doe = doeSpec)
      } else {
        tmp.doe@design.table <- doeSpec@design.table
      }

      tmp.doe@optimality <- this.optimality.function(tmp.doe) # Calculate the current optimality
      tmp2.doe <- tmp.doe # tmp2.doe is for updating values in the design

      # UPDATE EACH VALUE OF THE DESIGN TABLE ONCE IN EACH ITERATION
      for (iter in 1:max.iter) {
        
        update <- FALSE # Track if the current design was changed -> stop if no updates were made in one iteration
        
        for (i in indices) {
          # indices are the elements in the ITERATION TABLE

          change.pos <- which(doeSpec@iteration.table == i, arr.ind = TRUE) # Identify the elements of the design that are to update
          current.factor <- doeSpec@factors[[change.pos[1, 2]]] # Identify the factor that is updated currently

          #------------------------------------+
          # Update Semi HTC factors
          #------------------------------------+
          if (current.factor@changes == "semi.hard" & !quick.shtc.search) {

            try.values <- all.levels[[current.factor@name]]
            values.grid <- combn(try.values, current.factor@semi.htc.group.size) # All possible combinatoins of levels under the group size restriction

            #----------------TEST-----------------+
            all.values.grid <- apply(values.grid, 2, function(x) {
              expand.grid(
                replicate(nrow(change.pos), x, simplify = FALSE)
              )
            })
            all.values.grid <- do.call(rbind, all.values.grid)

            for (k in 1:nrow(all.values.grid)) {

              tmp2.doe@design.table[change.pos] <- t(all.values.grid[k, ])

              # CHECK IF MODIFIED DATA TABLE IS VALID (CONSTRAINTS)
              if (!doeSpec@check.validity(tmp2.doe)) next()

              # CHECK FOR IMPROVEMENT
              tmp2.doe@optimality <- this.optimality.function(tmp2.doe)
              if (tmp2.doe@optimality > tmp.doe@optimality) {
                # Update when better
                tmp.doe <- tmp2.doe
                update <- TRUE
              } else {
                tmp2.doe <- tmp.doe
              }

            }
          } else if(current.factor@changes == "semi.hard" & quick.shtc.search) {
            #-------------------------------------+
            # SHTC Quicksearch
            #-------------------------------------+
            # The quicksearch generates all possible factor level combinations for the
            # SHTC factor. Then for each of these combinations the cells in the current
            # whole plot are updated once. The best solution in terms of optimality is 
            # used then.
            
            factor.levels <- all.levels[[current.factor@name]] # All possible factor levels
            factor.level.tuples <- combn(factor.levels, current.factor@semi.htc.group.size) # All possible combinatoins of levels under the group size restriction
            
            # Iterate over all factor.level.tuples
            for(current.tuple in 1:ncol(factor.level.tuples)){

              # Generate initial whole plot settings that: 1.) are only from factor.level.tuple and 2.) fullfill the constraints of the doe
              tmp2.doe@design.table[change.pos[,"row"], current.factor@name] <- 
                factor.level.tuples[
                  seq(1, current.factor@semi.htc.group.size, length.out = nrow(change.pos)),
                  current.tuple]
              #-------------------------+
              # ÜBERPRÜFEN!
              #-------------------------+
              base.doe <-tmp2.doe
              #-------------------------+
              # ÜBERPRÜFEN!
              #-------------------------+

              # TODO: Check for conflict with constraints!
              
              # For each cell in the whole plot try each value from the current factor.level.tuple and check if the optimality is improved
              for(updateCell in change.pos[,"row"]) {
                # Changepos is a matrix with two columns, the first column "row" contains the 
                # row numbers of the current whole plot. The second column "col" contains the 
                # column number of the current factor.
                
                for(try.value in factor.level.tuples[,current.tuple]){ # Try all possible settings for the current whole plot
                  
                  tmp2.doe@design.table[updateCell, current.factor@name] <- try.value
                  
                  # CHECK IF MODIFIED DATA TABLE IS VALID (CONSTRAINTS)
                  if (!doeSpec@check.validity(tmp2.doe)) next()
                  
                  # CHECK FOR IMPROVEMENT
                  tmp2.doe@optimality <- this.optimality.function(tmp2.doe)
                  if (tmp2.doe@optimality > tmp.doe@optimality) {
                    # Update when better
                    tmp.doe <- tmp2.doe
                    base.doe <- tmp2.doe
                    update <- TRUE
                  } else {
                    tmp2.doe <- base.doe 
                  }
                }
                
              }
            }
          # END OF QUICKSEARCH
          } else {
            #------------------------------------+
            # HTC or ETC Factors
            #------------------------------------+
            for (try.value in all.levels[[current.factor@name]]) {

              tmp2.doe@design.table[change.pos] <- try.value
              # CHECK IF MODIFIED DATA TABLE IS VALID (CONSTRAINTS)
              if (!doeSpec@check.validity(tmp2.doe))
                next()

              # CHECK FOR IMPROVEMENT
              tmp2.doe@optimality <-
                this.optimality.function(tmp2.doe)
              if (tmp2.doe@optimality > tmp.doe@optimality) {
                # Update when better
                tmp.doe <- tmp2.doe
                update <- TRUE
              } else {
                tmp2.doe <- tmp.doe
              }
            }
          }

        }
       
        if(!update){ # If the DOE did not change at all during this iteration stop the algorithm
          # Check if optimality is > 0 & reject all designs with optimality of 0
          if(this.optimality.function(tmp.doe) == 0){
            message("Algorithm did not converge. Optimality is 0.")
            tmp.doe@design.table <- tmp.doe@design.table[0,]
          } else {
            message(paste("Algorithm converged after ", iter, " iterations."))
          }
          break()
        }
          
        if(!silent){
          setTxtProgressBar(pb, (r - 1) * max.iter + iter) # Update Progress bar
        }
      } # End: factor changing

      tmp.doe@design.matrix <- GenerateDesignMatrix(tmp.doe) # Store the current design matrix in the doe as well

      all.does[[r]] <- tmp.doe

    } # End: iterate random.starts

    # Sort the DOEs in the list by optimality
    optims <- sapply(all.does, function(x){doeSpec@optimality.function(x)})
    all.does <- all.does[order(optims, decreasing = TRUE)]
    all.does
  }
