#-----------------------------------+
# Generate the iteration matrix for
# a DOE.
#-----------------------------------+

# The iteration matrix controls in which order
# the elements in the design.table of the doe
# are changed during the generation of the
# optimal design.

GenerateIterationMatrix <- function(doeSpec) {
  # Generate the iteration matrix
  #
  # Args
  # - doeSpec: an object of class doeDesign
  #
  # Returns
  # a data.frame that represents the different elements
  # of the design table and the order in which they
  # are supposed to be modified in the algorithm.
  # Different groups of values in the matrix represent
  # blocks that are modified together. The indices themselfes
  # represent the order in which the elements are modified.
  
  structure.matrix <- as.matrix(GenerateStructureMatrix(doeSpec))
  
  iteration.matrix <- matrix(NA, nrow=nrow(structure.matrix), ncol=ncol(structure.matrix))
  
  # "Re-Index" hardest-to-change-factor
  current.value <- structure.matrix[1, 1]
  current.index <- 1
  
  for (i in 1:nrow(structure.matrix)) { # Index the first column from 1:...
    if (structure.matrix[i, 1] != current.value) {
      current.index <- current.index + 1
      current.value <- structure.matrix[i, 1]
    }
    iteration.matrix[i, 1] <- current.index
  }
  
  first.group.values <- unique(structure.matrix[,1]) # Structure matrix first whole plot values
  current.index <- 1 # Reset index
  
  # Update from left to rigth for each whole plot
  for (i in first.group.values) { # For each whole plot
    
    current.rows <- which(structure.matrix[, 1] == i) # Rows to modify

    #Update the next whole plot index if there is one
    if(current.index>1) { # If it is not the first whole plot
      current.index <- current.index + 1
      iteration.matrix[current.rows, 1] <- current.index
    }
    
    if(ncol(structure.matrix)>=2) {
      for (j in 2:ncol(structure.matrix)) { # For each other column
        
        # Update iteration matrix
        current.index <- current.index + 1 # Increase index whenever col changes
        current.value <- structure.matrix[current.rows[1], j] # Value of the first element of the col in the given block
        
        if(is.na(iteration.matrix[current.rows[1], j])){ # Only update if there is no value in the matrix already
          iteration.matrix[current.rows[1], j] <- current.index
        }
        
        for (k in current.rows[-1]) {
          if (current.value != structure.matrix[k, j]) {
            current.value <- structure.matrix[k, j]
            current.index <- current.index + 1
          }
          if(is.na(iteration.matrix[k, j])){ # Only update if there is no value in the matrix already
            iteration.matrix[k, j] <- current.index
          }
        }
        
        # Continue in case of overlapping whole plots
        if(tail(current.rows,1)<nrow(structure.matrix)){
          k <- tail(current.rows,1)+1
          while(current.value == structure.matrix[k, j] & k < nrow(structure.matrix)){
            if(is.na(iteration.matrix[k, j])){ # Only update if there is no value in the matrix already
              iteration.matrix[k,j] <- current.index
            }
            k <- k+1
          }
          
        }
      }  
    }
    
  }
  
  as.data.frame(iteration.matrix)
  
}

GenerateStructureMatrix <- function(doeSpec) {
  # Creates a matrix that represents the different
  # elements of the design table.
  #
  # Args
  # - doeSpec: an object of class doeDesign
  #
  # Returns
  # - A data.frame representing the different elements(blocks)
  #   of the DOE by different index numbers.
  
  structure.matrix <- NULL
  index.counter <- 1
  for (i in doeSpec@factors) {
    
    if (i@changes == "easy") {
      structure.matrix <- cbind(structure.matrix, index.counter:(index.counter + doeSpec@number.runs - 1))
    } else { # HTC factor

      structure.matrix <-
        cbind(structure.matrix,
              doeSpec@whole.plot.structure[,i@name] + index.counter)
    }
    index.counter <- max(structure.matrix) + 1
  }
  
  colnames(structure.matrix) <- getFactorNames(doeSpec)
  
  #Sort the columns by number of changes so that hard-to-change factors are at the beginning
  number.of.changes <-
     apply(structure.matrix, 2, function(x) {
       length(table(x))
     })
  structure.matrix <- structure.matrix[, order(number.of.changes)]
  structure.matrix
}
