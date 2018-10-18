
#---------------------------------------+
# Generate the design matrix for a 
# given design model and design data
#---------------------------------------+

GenerateDesignMatrix <- function(doeDesign){
  # Generate the design matrix of a doe based on the design model and 
  # based on the design table. 
  #  
  # Args: 
  #   doeDesign: an object of class doeDesign
  #
  # Returns:
  #   A list with two elements:
  #     - X: The design matrix for the fixed effects
  #     - Z: The design matrix for the whole-plots
  
  # Make sure that all categorical factors have the right factor levels
  for(i in doeDesign@factors){
    if(i@type == "categorical") {
      doeDesign@design.table[, i@name] <- as.factor(doeDesign@design.table[,i@name])
      levels(doeDesign@design.table[, i@name]) <- i@levels
    }
  }
  
  if(length(getHtcFactors(doeDesign)) > 0){ # Split plot designs

    X <- model.matrix(doeDesign@design.model, data=doeDesign@design.table)

    # Generate a formula representing the whole plot structure
    htc.factors <- getHtcFactors(doeDesign)
    
    tmp.formula <- as.formula(paste("~ ",  (htc.factors[[1]]@name)))
    
    if(length(htc.factors)>1){
      for(i in htc.factors[-1]){
         tmp.formula <- update(tmp.formula, paste("~.+", i@name))
      }
    }
  
    tmp.formula <- update(tmp.formula, ~.-1)
    
    for(i in 1:ncol(doeDesign@whole.plot.structure)){
      
      doeDesign@whole.plot.structure[,i] <- as.factor(doeDesign@whole.plot.structure[,i])

    }
      
    Z <- model.matrix(tmp.formula, doeDesign@whole.plot.structure)
    
     
  } else { # No HTCs
    
    X <- model.matrix(doeDesign@design.model, data=doeDesign@design.table)
    Z <- matrix()
    
  }
  list(X=X, Z=Z)
}

# splitplot <- new("doeDesign",
#                  factors=list(new("doeFactor", name="X1"), new("doeFactor", name="X2", changes="hard")),
#                  number.runs = as.integer(5),
#                  whole.plot.structure = data.frame(w1=as.factor(c(1,1,2,2,2))),
#                  design.model = ~X1+X2+I(X1*X2)+I(X1*X1),
#                  design.table = data.frame(
#                    X1=c(1,-1,1,-1,0), 
#                    X2=c(1,1,-1,-1,-1)
#                  )
# )
# 
# 
# GenerateDesignMatrix(splitplot)