#' Function to export a DOE stored in an object of class \code{\link{doeDesign-class}} to JMP.
#'
#' This function generates a JMP script that will produce a data table with the most relevant information
#' about the DOE. That includes the design table and model and the factor attributes as well.
#'
#' @export
#'
#' @param doe A list of DOEs that are being exported.
#' @param filepath A character representing the path to store the JMP script.
#'
SaveDOEtoJMP <- function(doe, filepath){

  # TODO: Save constraints in some capacity to the JMP Tables?

  # This function saves all relevant information of a doe
  # so that it can be processed by JMP. Therefore a JMP-script
  # is generated that creates the right table containing
  # model-script and factor information.
  #
  # Args:
  # - An object of class doeDesign or a list of doeDesigns
  # - A path to store the produced JMP script
  #
  # Returns:
  # - Nothing in R. Saves the script to produce the DOE in JMP
  #   as a text file at the given location.

  # Error Checking
  if(class(doe)=="list"){
    if(any(sapply(doe, class)!="doeDesign")){
      stop("All elements of list 'doe' need to be of class 'doeDesign'")
    }
  } else {
    if(class(doe)!= "doeDesign"){
      stop("doe needs to be of class 'doeDesign' or a list of 'doeDesign's.")
    }
  }

  if(class(doe)=="list"){
    JMP.code <- ""
    for(i in doe){
      JMP.code <- paste(JMP.code, hlpr.JMP.single.doe(i), ";")
    }
  } else if(class(doe)=="doeDesign"){
    JMP.code <- hlpr.JMP.single.doe(doe)
  }

  # Save the JMP code to file
  fileConn <- file(filepath)
  writeLines(JMP.code, fileConn)
  close(fileConn)
}

#' Collects all information of one doe and generates a JMP script
#' that creates the corresponding data set in JMP.
#'
#' @keywords internal
#'
hlpr.JMP.single.doe <- function(doe){

  # New Table and numbe rof runs
  JMP.code <- paste("New Table(\"Custom Desing - made in R\", Add Rows(",
                    doe@number.runs,
                    "), ",
                    "New Table Variable(\"Optimality Criterion\", \" ",
                    doe@optimality.criterion,
                    "\"),",
                    "New Table Variable(\" Optimality:\", \" ",
                    doe@optimality.function(doe),
                    "\"),",
                    hlpr.JMP.model.script(doe), sep="")

  if(length(getHtcFactors(doe))>0){
    JMP.code <- paste(JMP.code, hlpr.JMP.new.column.random(doe))
  }

  for(i in 1:length(doe@factors)){
    JMP.code <- paste(JMP.code, ",", hlpr.JMP.new.column(doe, i))
  }

  # Closing brackets
  JMP.code <- paste(JMP.code, ")")

  JMP.code
}

#' Function that creates the design model script part for JMP.
#'
#' @keywords internal
#'
hlpr.JMP.model.script <- function(doe){
  # Function to generate JMP code that adds the model
  # script to the data table. The model script specifices
  # the design model.
  #
  # Args:
  # - doe:  The object of class doeDesign that is saved to JMP
  #
  # Returns:
  # - The piece of JMP Code adding the model script
  #   to the data table (as string).

  JMP.code <- "New Script(\"Model\", Fit Model(Effects("

  # Start with random effects for htc-factors
  if(length(getHtcFactors(doe))>0){
    wholeplots <- colnames(doe@whole.plot.structure)
    for(i in wholeplots){
      JMP.code <- paste(JMP.code, ":wholeplot_", i , " & Random,", sep="")
    }
  }

  # Add model effects to JMP.code
  terms <- attr(terms.formula(doe@design.model, data=doe@design.table), "term.labels")

  for(i in 1:length(terms)){

    if(i > 1) { # Add , if required
      JMP.code <- paste(JMP.code, ",")
    }

    if(!grepl("[()*/^:]", terms[[i]])){ # Main Effect
      JMP.code <- paste(JMP.code, ":", terms[[i]], sep="")
    } else if(grepl(":", terms[[i]])){ # Interactions
      JMP.code <- paste(JMP.code, ":", gsub(":", "*:", terms[[i]]), sep="")
    } else if(grepl("I\\(", terms[[i]])){ # Assume polynomial term
      JMP.code <- paste(JMP.code, ":", gsub("\\*", "\\*:",gsub("[I\\(\\) ]", "", terms[[i]])) )
    }

  }
  JMP.code <- paste(JMP.code, "), Y(") # End of 'Effects'-part

  # Add Responses
  responses = unlist(doe@responses)

  for(i in 1:length(responses)){
    if(i > 1) { # Add , if required
      JMP.code <- paste(JMP.code, ",")
    }
    JMP.code <- paste(JMP.code, ":", responses[i])
  }

  JMP.code <- paste(JMP.code, ")") # End of 'Y'-part

  # Close brackets
  JMP.code <- paste(JMP.code, "))")

  JMP.code
}


#' Function that creates the individual columns of the
#' whole plot effects in the data table based on the r-doe.
#'
#' @keywords internal
#'
hlpr.JMP.new.column.random <- function(doe) {
  # Function to generate JMP code that creates
  # a column for each whole plot in the design.
  #
  # Args:
  # - doe: the object of class doeDesign to be saved
  #
  # Returns:
  # - JMP code as strings to create the whole plot columns

  JMP.code <- ""

  wholeplots <- colnames(doe@whole.plot.structure)

  for(i in wholeplots){

    # FACTOR NAME
    JMP.code <- paste(", New Column(\" wholeplot_", i, "\",", sep="")

    JMP.code <- paste(JMP.code, "Character, \"Nominal\", Set Property(\"Design Role\", DesignRole(Random Block)),")

    vals <- paste("\"", doe@whole.plot.structure[,i], "\"", collapse=",")
    vals <- paste("{", vals, "}")

    JMP.code <- paste(JMP.code, ", Set Values(", vals, "))")

  }

  JMP.code

}


#' Function that creates the JMP code to create the
#' columns representing the factors of the DOE.
#'
#' @keywords internal
#'
hlpr.JMP.new.column <- function(doe, index){

  # Function to generate the JMP code that creates
  # a new column in the data table. The column
  # has all relevant information in form of
  # column properties.
  #
  # Args:
  # - the doe to save for JMP
  # - the index number of the current factor
  #
  # Returns:
  # - the piece of JMP code producing the column in
  #   the JMP table

  this.factor <- doe@factors[[index]]

  # FACTOR NAME
  JMP.code <- paste("New Column(\"", this.factor@name, "\",")

  # DATA TYPE & MODELLING TYPE & Design Role
  if(this.factor@type == "continuous"){
    JMP.code <- paste(JMP.code, "numeric, \"continuous\", Set Property(\"Design Role\", DesignRole(continuous)),")
  } else {
    JMP.code <- paste(JMP.code, "character, \"nominal\", Set Property(\"Design Role\", DesignRole(continuous)),")
  }

  # Factor Changes
  if(this.factor@changes == "easy"){
    JMP.code <- paste(JMP.code, " Set Property(\"Factor Changes\", easy)")
  } else {
    JMP.code <- paste(JMP.code, " Set Property(\"Factor Changes\", hard)")
  }

  # Coding (if numeric)
  if(this.factor@type=="continuous"){
    JMP.code <- paste(JMP.code, ", Set Property(\" Coding \", {", min(this.factor@levels), ",", max(this.factor@levels) , "})")
  }

  # Add values
  if(this.factor@type=="continuous"){
    vals <- paste(doe@design.table[, this.factor@name],collapse=",")
    vals <- paste("[", vals, "]")
  } else {
    vals <- paste("\"", doe@design.table[, this.factor@name], "\"", collapse=",")
    vals <- paste("{", vals, "}")
  }

  JMP.code <- paste(JMP.code, ", Set Values(", vals, ")")

  # Final brace
  JMP.code <- paste(JMP.code, ")")

  JMP.code
}
