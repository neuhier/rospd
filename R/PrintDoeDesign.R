#------------------------------------+
# Generic print function for an 
# object of class doeDesign
#------------------------------------+

setMethod("show", signature("doeDesign"), function(object){
  
  has.design.table <- if(nrow(object@design.table)>0){1} else {0}
  
  # Print Metainfo Header: Number of runs, number of factors, split plot/completely randomized design (?)
  cat(paste(
    "Design with ",
    object@number.runs,
    " runs and ",
    length(object@factors),
    " factors.\n\n"
  ))
  
  # Print the data table including the whole plot structure
  whole.plots <- object@whole.plot.structure
  
  n.whole.plots <- length(getHtcFactors(object))
  
  if (n.whole.plots > 0) {
    wp.names <- NULL
    for (i in 1:n.whole.plots) {
      wp.names <- c(wp.names, paste("Whole Plot", i))
    }
    colnames(whole.plots) <- wp.names
    
    if(nrow(object@design.table) == nrow(whole.plots)){
      whole.tbl <- cbind(whole.plots, object@design.table)
    } else {
      whole.tbl <- whole.plots
    }
    
  } else {
    whole.tbl <- object@design.table
  }
  
  # Cut long tables
  whole.tbl <- head(whole.tbl, 10)
  
  cat(paste(capture.output(whole.tbl), "\n", sep = ""))
  
  if(object@number.runs>10) cat("...\n")
  
  # Print the most relevant information for design evaluation: D-Optimality/Efficiency, I-Optimality, (?)
  if(has.design.table){
    dopt <- DOptimality(object)
    deff <- DEfficiency(object)
    
    cat(paste("D-Optimality: \t", round(dopt), "\n"))
    cat(paste("D-Efficiency: \t", round(deff)))
  }
})
