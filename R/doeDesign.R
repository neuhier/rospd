#----------------------------------------+
# doeDesign class.
#----------------------------------------+

# Check the validity of a doeDesign object.
#' Checks the validity of an object of class \code{\link{doeDesign-class}}.
#'
#' @keywords internal
#'
check_doeDesign <- function(object){

      errors <- character()

      # Factors need different names
      factornames = sapply(object@factors, function(x){x@name})
      if(any(duplicated(factornames))){
        msg <- "Each factor needs to use an individual name."
        errors <- c(errors, msg)
      }

      # Check if wholeplotstructure and number of runs (incl. replicates) match
      if(length(getHtcFactors(object))>0){
        n.wholeplots <- nrow(object@whole.plot.structure)
        n.runs <- object@number.runs
        if(object@replicate.type == "all") {n.runs <- n.runs*object@replicate.n}
        if(object@replicate.type == "individual") {n.runs <- n.runs + object@replicate.n}
        if(n.runs != n.wholeplots){
          msg <- "The length of the whole plot structure does not match the number of runs (incl. replicates)."
          errors <- c(errors, msg)
        }
      }

      # Check if there are enough levels for each factor depending on the model

      # Check if the whole plot structure is ok (enough changes)

      if(length(errors)==0) TRUE else errors
}

#' A class representing a designed experiment.
#'
#' The doeDesign class represents all relevant information of a designed experiment.
#' That is not only the data table but different aspects, like the design model and
#' used optimality criterion as well.
#'
#' @slot responses An optional list of names that are the responses measured in the experiment.
#' @slot factors A list of objects of class \code{\link{doeFactor-class}} representing all factors of the DOE.
#' @slot check.validity A function that represents constraints to the factor space. This function should test if all
#'                      rows of the design.table in the DOE are acceptable and return FALSE if this is not the case.
#' @slot number.runs The number of experiments that are available for the experiment as an integer.
#' @slot replicate.type CURRENTLY NOT USED.
#' @slot replicate.n CURRENTLY NOT USED.
#' @slot whole.plot.structure A data frame representing the whole plot structure of the design. The data frame must contain one column for each hard- and semi-hard-to-change-factor. The column name is the same like the factor name. The values represent blocks of experiments in which the corresponding factor is not varied independently.
#' @slot design.model An object of class \code{\link{formula}} representing the design model for the DOE.
#' @slot design.matrix A list containing the design matrix X and the matrix of random effects Y for the DOE.
#' @slot design.table The actual data table that represents the experiments in the DOE.
#' @slot iteration.table A \code{\link{data.frame}} representing in which order cells are modified during the design creation. This is only relevant for internal use.
#' @slot variance.ratio A numeric representing the variance ratio of residual variance to whole-plot-variance. This is relevant for the generation of optimal split-plot-designs.
#' @slot replicate.structure CURRENTLY NOT USED.
#' @slot optimality.function A function that calculates the optimality of a design. It is assumed that larger values of optimality represent better designs. The function has to accept an object of class \code{\link{doeDesign-class}} as argument.
#' @slot optimality.criterion A character representing the name of the used optimality criterion.
#' @slot optmiality A numeric representing the actual value of the optimality criterion for the current design.
#'
#' @export
#' @examples
#'
#' # A simple completely randomized design
#' crmd <- GenerateNewDoeDesign(
#'     factors = list(
#'        new("doeFactor", name="X1", levels=c(0, 1)),
#'        new("doeFactor", name="X2", levels=c(0, 1))
#'     ),
#'     number.runs = as.integer(20),
#'     design.model = ~X1*X2,
#'     random.doe = TRUE
#' )
#' crmd
#'
#' # A split plot design
#' splitplot <- GenerateNewDoeDesign(
#'     factors = list(
#'        new("doeFactor", name="X1", levels=c("A", "B", "C"), type="categorical", changes="hard"),
#'        new("doeFactor", name="X2", levels=c(0, 1))
#'     ),
#'     number.runs = as.integer(20),
#'     whole.plot.structure=data.frame(X1=rep(1:5, each=4)),
#'     design.model = ~X1*X2,
#'     random.doe = TRUE
#' )
#' splitplot # This is just a random design (not optimal)
#'
#' # A design using semi hard to change factors
#' semisplitplot <- GenerateNewDoeDesign(
#'     factors = list(
#'        new("doeFactor", name="X1",
#'                         levels=c("A", "B", "C", "D"),
#'                         type="categorical",
#'                         changes="semi.hard",
#'                         semi.htc.group.size=as.integer(2)),
#'        new("doeFactor", name="X2", levels=c(0, 1))
#'     ),
#'     number.runs = as.integer(20),
#'     whole.plot.structure=data.frame(X1=rep(1:5, each=4)),
#'     design.model = ~X1*X2,
#'     optimality.function=DOptimality,
#'     optimality.criterion = "D-Optimality",
#'     random.doe = TRUE
#' )
#' GenerateOptimalDesign(semisplitplot, random.start = 1, max.iter = 5)
#'
#' # Using constrained factor spaces
#' my.constraint <- function(doe){
#'  return(!any(doe@design.table$X1 > 0.5 & doe@design.table$X2 > 0.5))
#' }
#'
#' constrained.doe <- GenerateNewDoeDesign(
#'     factors = list(
#'        new("doeFactor", name="X1", levels=c(0, 1)),
#'        new("doeFactor", name="X2", levels=c(0, 1))
#'     ),
#'     check.validity = my.constraint,
#'     number.runs = as.integer(20),
#'     design.model = ~X1*X2,
#'     random.doe = TRUE
#' ) # This is just a random design (not optimal)
#'
#' plot(constrained.doe@design.table$X1, constrained.doe@design.table$X2)
#'
setClass("doeDesign",
         representation(
           responses = "list",
           factors = "list",
           check.validity = "function",
           number.runs = "integer",
           replicate.type = "character",
           replicate.n = "integer",
           whole.plot.structure = "data.frame",
           design.model = "formula",
           design.matrix = "list", # Is that really needed?
           design.table = "data.frame",
           iteration.table = "data.frame",
           variance.ratio = "numeric",
           replicate.structure = "vector",
           optimality.function = "function",
           optimality.criterion = "character",
           optimality = "numeric"
         ),
         prototype(
           responses = list("Y"),
           factors = list(new("doeFactor")),
           check.validity = function(doe){TRUE},
           number.runs = as.integer(0),
           replicate.type = "none",
           replicate.n = as.integer(0),
           whole.plot.structure = data.frame(),
           design.model = formula(Y~x),
           design.matrix = list(X=data.frame(), Y=data.frame()),
           design.table = data.frame(),
           iteration.table = data.frame(),
           variance.ratio = 1,
           replicate.structure = vector(),
           optimality.function = function(){},
           optimality.criterion = "",
           optimality = NA_real_
           ),
         validity = check_doeDesign
         )

# Helper functions
#-------------------------------------------+
# Create a new design
#-------------------------------------------+
#' This function is a wrapper for new("doeDesign", ...).
#'
#' This is the recommended way of initializing a new doeDesign. The function creates a new instance of
#' an object of class \code{\link{doeDesign-class}} and initializes some relevant slots in the background
#' (iteration.matrix, sorting of factors, ...).
#'
#' For more details about the arguments see \code{\link{doeDesign-class}}.
#'
#' @export
#'
GenerateNewDoeDesign <- function(responses = list("Y"),
                                 factors = list(new("doeFactor")),
                                 check.validity = function(doe) { TRUE },
                                 number.runs = as.integer(0),
                                 replicate.type = "none",
                                 replicate.n = as.integer(0),
                                 whole.plot.structure = data.frame(),
                                 design.model = formula(Y ~ x),
                                 design.matrix = list(X = data.frame(), Y = data.frame()),
                                 design.table = data.frame(),
                                 iteration.table = data.frame(),
                                 variance.ratio = 1,
                                 replicate.structure = vector(),
                                 optimality.function = function() {
                                 },
                                 optimality.criterion = "",
                                 optimality = NA_real_,
                                 random.doe = FALSE) {
  # More user friendly function to create a new design
  #
  # Args:
  # ...
  #
  # Returns an initialized object of class doeDesign.

  # Initialize number of runs if not given, if design table is provided
  if(number.runs==as.integer(0) & nrow(design.table)>0){
    number.runs <- nrow(design.table)
  }

  new.design <- new("doeDesign",
                   responses = responses,
                   factors = factors,
                   check.validity = check.validity,
                   number.runs = number.runs,
                   replicate.type = replicate.type,
                   replicate.n = replicate.n,
                   replicate.structure= replicate.structure,
                   whole.plot.structure = whole.plot.structure,
                   design.model = design.model,
                   design.matrix = design.matrix,
                   design.table = design.table,
                   iteration.table = iteration.table,
                   variance.ratio =variance.ratio,
                   optimality.function = optimality.function,
                   optimality.criterion = optimality.criterion,
                   optimality = optimality
                  )

  # Sort factors by how hard to change they are
  number.of.changes <- apply(new.design@whole.plot.structure, 2, function(x) { length(table(x))})
  names.by.hardness <- names(number.of.changes)[order(number.of.changes)]

  update.factors <- list()
  for(i in names.by.hardness){
    update.factors <- c(update.factors, getFactorByName(new.design, i))
  }

  # Attach the easy to change factors as well
  for(i in getEtcFactors(new.design)){
    update.factors <- c(update.factors, i)
  }

  new.design@factors <- update.factors

  # Generate the iteration matrix which is needed for point exchange
  new.design@iteration.table <- GenerateIterationMatrix(new.design)

  # Generate a random starting design
  if(random.doe){
    new.design@design.table <- GenerateRandomDesign(new.design)

    # Make sure that all categorical factors have the right factor levels
    for(i in new.design@factors){
      if(i@type == "categorical") {
        new.design@design.table[, i@name] <- as.factor(new.design@design.table[,i@name])
        levels(new.design@design.table[, i@name]) <- i@levels
      }
    }

  }

  new.design

}

#' This function returns the design matrix of a doeDesign.
#' 
#' @param includeWholePlots (logical, default=TRUE) if TRUE the data frame contains the whole plot structure as part of the data frame.
#'
#' @export
#'
getDesignTable <- function(object, includeWholePlots=TRUE){
  
  if(includeWholePlots){
    wp.part <- object@whole.plot.structure
    # Rename columns to indicate whole plots
    colnames(wp.part) <- paste0("wp_", colnames(wp.part))
    
    rslt <- data.frame(wp.part, object@design.table)
    return(rslt)
    
  } else {
   return(object@design.table)
  }
  
}

#-------------------------------------------+
# Get a list of all HTC-Factors in the DOE
#-------------------------------------------+
#' Function that returns a list with all hard-to-change factors of a DOE.
#'
#' Mostly for internal use.
#'
#' @param doeD An object of class \code{\link{doeDesign-class}}.
#'
#' @return A list of \code{\link{doeFactor-class}} in the DOE that are hard to change.
#'
#' @export
#'
getHtcFactors <- function(doeD){

  if(class(doeD) != "doeDesign"){
    stop("getHtcFactors is supposed to work with object of class 'doeDesign'.")
  }

  htcFactors = doeD@factors[sapply(doeD@factors, function(x){x@changes == "hard" | x@changes == "semi.hard"})]
  as.list(htcFactors)
}

#-------------------------------------------+
# Get a list of all ETC-Factors in the DOE
#-------------------------------------------+
#' Function that returns a list with all easy-to-change factors of a DOE.
#'
#' Mostly for internal use.
#'
#' @param doeD An object of class \code{\link{doeDesign-class}}.
#'
#' @return A list of \code{\link{doeFactor-class}} in the DOE that are easy to change.
#'
#'
getEtcFactors <- function(doeD){

  if(class(doeD) != "doeDesign"){
    stop("getHtcFactors is supposed to work with object of class 'doeDesign'.")
  }

  etcFactors = doeD@factors[sapply(doeD@factors, function(x){x@changes == "easy" })]
  as.list(etcFactors)
}

#-------------------------------------------+
# Get a list of all factor names
#-------------------------------------------+
#' Function that returns a list containing the names of all factors of a DOE.
#'
#' @param object An object of class \code{\link{doeDesign-class}}.
#'
#'
getFactorNames <- function(object){

  factor.names <- NULL
  for(i in object@factors){
    factor.names <- c(factor.names, i@name)
  }

  factor.names

}

#------------------------------+
# Get factor by name
#------------------------------+
#' Function that returns a factor in a doe identifying it via its name.
#'
#' @keywords internal
#'
#' @param doe An object of class \code{\link{doeDesign-class}}.
#' @param factor.name A character representing the name of the factor of interest.
#'
#'
getFactorByName <- function(doe, factor.name){
  for(i in doe@factors){
    if(i@name == factor.name) return(i)
  }
}

#------------------------------+
# Get the whole plot column
#------------------------------+
#' A function that returns the correpsonding whole plot structure column for a (semi-)hard to change factor.
#'
#' @keywords internal
#'
getWholePlotColumn <- function(doe, factor.name){
  # Matches a hard to change or semi htc factor to the corresponding
  # column in the whole.plot.structure in a design.
  #
  # Args:
  # - doe: an object of the class doeDesign
  # - factor.id: the id identifying the relevant factor in the list of factors of the doe.
  #
  # Returns:
  #   NA if the factor is a etc-factor or the column number that corresponds to the (semi)htc-factor
  #   in the whole.plot.structure of the doeDesign object.

  factor.changes <- (getFactorByName(doe, factor.name))@changes

  if(factor.changes=="easy") {
    return(NA)
  } else {

    doe@whole.plot.structure[, factor.name]

  }

}


#------------------------------+
# Reorder the list of factors
# from htc to etc
#------------------------------+
#' Function to reorder the factors in a DOE from hard to change to easy to change.
#'
#' This function is supposed to be used internally. Mostly the \code{\link{GenerateOptimalDesign}}-functions
#' expects that the factors are sorted from hard to change to easy to change. This happens automatically
#' when a design is created using \code{\link{GenerateNewDoeDesign}}.
#'
#' @param doe An object of class \code{\link{doeDesign-class}}.
#'
#' @return The same object that was passed as argument just with sorted factors.
#'
#'
reorderFactors <- function(doe){
  # Sorts the list of factors in a DOE from hardest to change to easiest to change.
  #
  # Args:
  # - doe: an object of class doeDesign
  #
  # Return:
  # - an object of class doeDesign, where the factors are sorted as described above.

  new.order <- colnames(GenerateStructureMatrix(doe))
  factors.list <- doe@factors

  sorted.factors.list <- list()
  for(i in new.order) {

    for(j in doe@factors){
      if(j@name == i){
        sorted.factors.list <- c(sorted.factors.list, j)
      }
    }

  }

  doe@factors <- sorted.factors.list
  doe
}

