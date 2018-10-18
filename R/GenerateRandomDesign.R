#-----------------------------------------+
# Generate a random initial design.
#-----------------------------------------+

GenerateRandomDesign <- function(doe){
  # Function to generate a random design. This design
  # represents the required design structure and
  # respects given constraints.
  #
  # Args:
  #   doe: An object of class doeDesign.
  #
  # Returns:
  #   a data frame with a random design, meaning it
  #   contains a potential but randomly generated
  #   design for the given problem.
  #

  if(length(doe@factors)==1){ #For one-factor designs
    
    new.design.table <- as.data.frame(sample(x = getPossibleLevels(doe@factors[[1]]), size = doe@number.runs, replace = TRUE))
    colnames(new.design.table) <- doe@factors[[1]]@name
    
  } else { # More than one factor
    # Generate all possible factor settings ignoring the constraints
    all.values <- list()
    for(i in doe@factors){
      all.values[i@name] <- list(getPossibleLevels(i))
    }
    
    all.combinations <- expand.grid(all.values)
    
    # Remove all factor settings that are not valid due to the constraints
    all.allowed.index <- NULL
    tmp.doe <- doe
    for(i in 1:nrow(all.combinations)){
      tmp.doe@design.table <- all.combinations[i,]
      all.allowed.index <- c(all.allowed.index, doe@check.validity(tmp.doe))
    }
    
    all.allowed.combinations <- all.combinations[all.allowed.index,]
    
    # Generate random design based on allowed factor settings
    new.design.table <- as.data.frame(matrix(data = NA, nrow=doe@number.runs, ncol = length(doe@factors)))
    colnames(new.design.table) <- getFactorNames(doe)
    
    # Iteration table to fill the matrix
    doe@iteration.table <- GenerateIterationMatrix(doe)
    
    # Get all unique values in iteration table
    indices <- unlist(sapply(doe@iteration.table, unique))
    indices <- indices[order(indices, decreasing = FALSE)]
    for( i in indices) {
      
      # Which rows/cols to change
      change.pos <- which(doe@iteration.table == i, arr.ind = TRUE)
      current.factor <- doe@factors[[change.pos[1, 2]]]
      
      # randomly chose a possible value
      this.allowed.combinations <- all.allowed.combinations
      # Filter for possible values based on what is in the current row and the previous columns
      if(change.pos[1,2] > 1){
        for(j in 1:(change.pos[1,2]-1)){
         
          this.col.value <- new.design.table[change.pos[1,1], j]
          
          if(is.numeric(this.col.value)){
            this.allowed.combinations <- this.allowed.combinations[which(abs(this.allowed.combinations[,j]-this.col.value)<1.5e-8),]
          } else {
            this.allowed.combinations <- this.allowed.combinations[which(this.allowed.combinations[,j]==this.col.value),]
          }
        }
      }
      
      possible.values <- names(table(this.allowed.combinations[,current.factor@name]))
      set.value <- sample(possible.values, size=1)
      
      # Make sure continuous levels are numeric
      if(current.factor@type=="continuous"){
        set.value <- as.numeric(set.value)
      }
      
      # Set the value
      new.design.table[change.pos] <- set.value
      
    }
  }


  new.design.table

}



# GenerateRandomDesign <- function(doe) {
#   # Function to generate the random initial design table.
#   #
#   # Args:
#   #   doe: An object of class doeDesign.
#   #
#   # Returns:
#   #   a data frame with random settings for the factors of the doe. The general structure of the design is correct (whole-plots).
#
#   p = length(doe@factors)
#
#   # Make a random table using the right structure but ignoring possible constraints
#   random.doe.df <- data.frame(matrix(nrow = doe@number.runs, ncol = p))
#
#   for (i in 1:p) {
#     current.factor.changes <- doe@factors[[i]]@changes
#
#     # ETC Factors
#     if (current.factor.changes == "easy") {
#       for (j in 1:doe@number.runs) {
#         random.doe.df[j, i] <- getRandomLevel(doe@factors[[i]])
#
#       }
#     } else {
#       # HTC or SHTC factor
#       current.whole.plot <- doe@whole.plot.structure[, doe@factors[[i]]@name]
#       number.wholeplots <- length(unique(current.whole.plot))
#
#       for (j in 1:number.wholeplots) {
#
#         rows.to.change <- which(current.whole.plot == j)
#
#         newValue <- getRandomLevel(object = doe@factors[[i]])
#
#         if (current.factor.changes == "hard")
#           newValue <- rep(newValue, length(rows.to.change))
#         if (current.factor.changes == "semi.hard")
#           newValue <-
#           sample(newValue, length(rows.to.change) , replace = TRUE)
#
#         random.doe.df[rows.to.change, i] <- newValue
#
#       }
#
#     }
#
#   }
#
#   colnames(random.doe.df) <- getFactorNames(doe)  # Rename accordingly
#
#   # If there are constraints: Change points until the random design is valid
#   # doe@design.table <- random.doe.df
#   # doe@iteration.table <- GenerateIterationMatrix(doe)
#   # is.valid <- doe@check.validity(doe)
#   #
#   # for (i in 1:max(doeSpec@iteration.table)) {
#     #   Iterate rowise and make sure that all rows are valid on their own?
#   # }
#
#   # Reorder the columns by how hard they are to change
#   new.order <- colnames(GenerateStructureMatrix(doe))
#   random.doe.df <- random.doe.df[, new.order]
#   random.doe.df
#
# }
