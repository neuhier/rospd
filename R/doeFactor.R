#----------------------------------------------+
# doeFactor class.
#----------------------------------------------+

#' Check validity of doeFactor class
#'
#' @keywords internal
#'
#' This function does some error checking for doeFactor-objects. It is used during
#' declaration of the object and returns error if needed.
#'
#' @param object An object of class doeFactor.
#'
check_doeFactor <- function(object) {
  errors <- character()

  # Check factor type
  if(!object@type %in% list("continuous", "categorical")){
    msg <- "doeFactor type needs to be either 'continuous' or 'categorical'."
    errors <- c(errors, msg)
  }

  # Reasonable number of factors
  if(length(object@levels) < 2) {
    msg <- "A doeFactor needs to use at least 2 different levels."
    errors <- c(errors, msg)
  }

  # Correctly specified factor changes
  if(!object@changes %in% list("easy", "hard", "semi.hard")){
    msg <- "doeFactor needs to be either 'easy', 'hard' or 'semi.hard' to change."
    errors <- c(errors, msg)
  }

  # Make sure everything works correctly for semi.htc.factors
  if(object@changes == "semi.hard") {
    if(is.na(object@semi.htc.group.size)){
      msg <- "If the doeFactor is semi.hard to change provide a semi.htc.group.size (is NA currently)."
      errors <- c(errors, msg)
    } else {
      if((object@type == "categorical" & object@semi.htc.group.size >= length(object@levels)) |
         (object@type=="continuous" & object@semi.htc.group.size >= length(getPossibleLevels(object))) ){
        msg <- "The semi-hard-to-change group size needs to be smaller than the number of levels."
        errors <- c(errors, msg)
      }
    }
  }

  # Make sure factor levels are numeric for continuous factors
  if(object@type =="continuous" & !is.numeric(object@levels)){
    msg <- "For a continuous factor all levels need to be numerical."
    errors <- c(errors, msg)
  }

  if(length(errors)==0) TRUE else errors
}

#' An S4 class representing a factor in a designed experiment.
#'
#' A factor is a parameter that is varied during the experimentation. This is not to confuse with R's factors that are categorical variables.
#'
#' @export
#'
#' @slot name A character representing the name of the factor.
#' @slot levels A vector containing the different levels of the factor. For continuous factors the minimum and maximum levels should be provided.
#' @slot number.levels An integer representing the number of different levels a continuous factor can take. The number of levels and the 'levels' (minimum/maximum) define what levels a continuous factor can take. Design generation might be very slow for many factor levels.
#' @slot type A character representing the modeling type of the factor. Accepted values are: continuous or categorical.
#' @slot changes A character representing if a factor is 'easy', 'hard' or 'semi.hard' -to-change.
#' @slot semi.htc.group.size An integer specifing how many different levels can be used in one block for a semi.hard-to-change-factor.
#'
setClass("doeFactor",
        representation(
          name = "character",
          levels = "vector",
          number.levels = "integer",
          type = "character",
          changes = "character",
          semi.htc.group.size = "integer"
        ),
        prototype(
          name    = "X1",
          levels  = c(-1, 1),
          number.levels = as.integer(3),
          type    = "continuous",
          changes = "easy",
          semi.htc.group.size = NA_integer_
        ),
        validity = check_doeFactor
      )

# Generic functions

#--------------+
# Show Method
#--------------+
#' Default print function for an object of class doeFactor.
#'
#' @param object An object of class doeFactor. A basic overview of this object will be printed to the console.
#'
#' @examples
#' newFactor <- new("doeFactor",
#'                      name="Temperature",
#'                      levels=c(180, 210),
#'                      number.levels=as.integer(5),
#'                      type="continuous",
#'                      changes="easy")
#' newFactor
setMethod("show", signature("doeFactor"), function(object){
  
    # Prepare the factor levels to be displayed
    all.levels <- getPossibleLevels(object)
    if(is.numeric(all.levels)){
      all.levels <- round(all.levels, digits = 2)
    }
    levels <- c()
    if(object@number.levels > 6){ # If there are too many factor levels to display all
      for(i in 1:3){
        levels <- paste(levels, all.levels[i], sep="  ")
      }
      levels <- paste(levels, "...", all.levels[object@number.levels], sep=" ")
    } else {
      levels <- paste(all.levels, collapes=" ")
    }
  
    cat(object@type, "Factor:\t", object@name, "\n")
    cat("Changes:\t\t\t\t", object@changes, "\n")
    cat("Levels: \t\t\t\t", levels)
    if(object@changes=="semi.hard"){
      cat("\n#Levels in Whole Plot:\t", object@semi.htc.group.size)
    }
  })


#---------------------------+
# Get possible factor levels
#---------------------------+
#' Function that returns a list with all possible levels of a factor.
#'
#' For categoric factors this is just the list of all levels. For continuous factors
#' the list is determined by the minimum and maximum levels provided in the slot levels
#' and the number of levels-slot of the factor.
#'
#' @keywords internal
#'
#' @param object An object of class doeFactor.
#'
#' @examples
#' newFactor <- new("doeFactor",
#'                      name="Temperature",
#'                      levels=c(180, 210),
#'                      number.levels=as.integer(5),
#'                      type="continuous",
#'                      changes="easy")
#' getPossibleLevels(newFactor)
getPossibleLevels <- function(object){
  # Returns a vector of possible factor levels. For categorical factors this is equal to the
  # objects slot @levels. For continuous factors it is a list of all possible values.
  #
  # Args:
  #   object: an object of class doeFactor
  #
  # Returns:
  #   A vector with numbers or characters containing all possible values that the factor could use.

  if(object@type == "continuous"){

    all.levels <- seq(from=min(object@levels), to=max(object@levels), by=(max(object@levels)-min(object@levels))/(object@number.levels-1))

  } else {

    all.levels <- object@levels
  }

  all.levels

}

#---------------------------+
# Get random factor levels
#---------------------------+
#' Function that returns a random value that could be chosen for the given factor.
#'
#' @keywords internal
#'
#' @param object An object of class doeFactor
#' @param skip A list of values that should not be used for the random selection of one factor level.
#'
#' @return The function returns either a single value (numeric or character) for hard- and easy-to-change factors.
#'         For semi-hard-to-change-factors a list of different values is generated. The length of the list is
#'         the number of possible values a shtc-factor can take in a given block (=whole plot).
#'
#' @examples
#' etc.factor <- new("doeFactor",
#'                      name="Temperature",
#'                      levels=c(180, 210),
#'                      number.levels=as.integer(5),
#'                      type="continuous",
#'                      changes="easy")
#' getRandomLevel(etc.factor)
#'
#' semi.htc.factor <- new("doeFactor",
#'                      name="Temperature",
#'                      levels=c(180, 210),
#'                      number.levels=as.integer(5),
#'                      type="continuous",
#'                      changes="semi.hard",
#'                      semi.htc.group.size=as.integer(3))
#' getRandomLevel(semi.htc.factor)
#'
getRandomLevel <- function(object, skip=NA){
  # Function to get random settings for an element (single or multiple cells in design matrix) for a
  # given factor.
  #
  # Args:
  #   object: the factor for which randomly chosen settings need to be generated
  #   skip: possible settings for that factor that should not be used.
  #
  # Returns:
  #   A vector containing the randomly assigned factor settings.

  all.levels <- getPossibleLevels(object)
  if(object@type == "continuous" & is.numeric(skip)){
    all.levels <- setdiff(all.levels, skip)
  }

  if(object@type == "categorical" & is.character(skip)){
    all.levels <- setdiff(all.levels,skip)
  }

  if(object@changes == "easy") {
    factor.settings <- sample(all.levels, size=1)
  } else if(object@changes == "hard"){
    new.setting <- sample(all.levels, size=1)
    factor.settings <- new.setting
    # factor.settings <- rep(new.setting, times=element.size)
  } else if(object@changes == "semi.hard"){
    subset.levels = sample(all.levels, size=object@semi.htc.group.size, replace=FALSE)
    factor.settings <- subset.levels
    # factor.settings <- sample(subset.levels, size=element.size, replace=TRUE)
  }
  factor.settings
}
