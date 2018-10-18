#------------------------------------------------------+
# Change an element in the design table of a doeDesign
#------------------------------------------------------+

SetElement <- function(doe, factor.index, element.index, value){
  # Function to replace an element in the design.matrix of a doeDesign.
  #
  # Args:
  # - doe: an object of class doeDesign
  # - factor.index: an integer representing the factor in the list of factors of the DOE
  # - element.index: an index representing which rowise part in the design matrix is to be changed.
  # - value:  the new value that is replacing the original values in the design table. For etc and 
  #           htc factors this is just a single value. For semi.htc it is a list of
  #           values. Those values are randomly assigned to the relevant parts of the design table.
  #
  # Returns:
  # - a data.frame that is the updated version of the design matrix
  
  new.design.table  <- doe@design.table
  factor.changes    <- doe@factors[[factor.index]]@changes
  
  if(factor.changes == "hard" | factor.changes == "semi.hard") {
    
    # The rows to change correspond to the element.index-th whole plot. 
    change.rows <- which(doe@whole.plot.structure[,getWholePlotColumn(doe, factor.index)] == element.index)
    
  } 
  
  if(factor.changes == "easy") {

    new.design.table[element.index, factor.index] <- value 

  } else if(factor.changes == "hard") {

    new.design.table[change.rows, factor.index] <- value

  } else if(factor.changes == "semi.hard") {
    
    new.design.table[change.rows, factor.index] <- sample(value, size=length(change.rows), replace=TRUE) 
  }
 
  new.design.table
   
}
