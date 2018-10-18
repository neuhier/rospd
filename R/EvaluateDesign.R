#--------------------------------------+
# Generating some reports for design
# evaluation.
#--------------------------------------+

#' Draw a color map of correlations
#'
#' @param doe the design to evaluate
#' @param fml a (optional) formula representing the design model of interest (including aliased terms)
#'
#' @export
#'
#' @examples
#'doe <- GenerateNewDoeDesign(
#'  factors = list(
#'   new("doeFactor", name="X1"),
#'   new("doeFactor", name="X2"),
#'   new("doeFactor", name="X3"),
#'   new("doeFactor", name="X4")
#'  ),
#'  number.runs = as.integer(8),
#'  design.model = ~X1+X2+X3+X4,
#'  optimality.function=DOptimality,
#'  optimality.criterion = "D-Optimality",
#'  random.doe = TRUE
#' )
#'
#' # Generate a d-optimal design
#' doe.Dopt <- GenerateOptimalDesign(doe, 1, 10)
#' AliasingHeatMap(doe.Dopt[[1]], fml=~X1+X2+X3+X4)
#' a.matrix1 <- AliasingHeatMap(doe.Dopt[[1]], fml=~X1+X2+X3+X4+X1*X2+X1*X3+ X1*X4+X2*X3+X2*X4+X3*X4)
#' a.matrix1
#'
#' # Generate a A-optimal design
#' doe@optimality.function<-AOptimality
#' doe.Aopt <- GenerateOptimalDesign(doe, 1, 10)
#' a.matrix2 <- AliasingHeatMap(doe.Aopt[[1]], fml=~X1+X2+X3+X4+X1*X2+X1*X3+ X1*X4+X2*X3+X2*X4+X3*X4)
#' a.matrix2
#'
#' # Compare both designs (using the multiplot function from here: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/)
#' multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#' library(grid)
#' plots <- c(list(...), plotlist)
#' numPlots = length(plots)
#' if (is.null(layout)) {
#'   layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#'                    ncol = cols, nrow = ceiling(numPlots/cols))
#' }
#' if (numPlots==1) {
#'   print(plots[[1]])
#'
#' } else {
#'   # Set up the page
#'   grid.newpage()
#'   pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#'
#'   # Make each plot, in the correct location
#'   for (i in 1:numPlots) {
#'     matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#'
#'     print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#'                                     layout.pos.col = matchidx$col))
#'   }
#' }
#' }
#' multiplot(a.matrix1, a.matrix2, cols=2)
AliasingHeatMap <- function(doe, fml=NULL, includeWholePlots=FALSE){

  if(!is.null(fml)){  # Modifying the design matrix and model to analyse terms that are not
                      # part of the initial model.
    doe@design.model <- fml
    doe@design.matrix <- GenerateDesignMatrix(doe)
  }

  # If whole plots should be considered
  if(includeWholePlots & length(getHtcFactors(doe))>0){
    
    # Update the formula to include the main effect of the whole plots
    wps <- colnames(doe@whole.plot.structure)
    for(i in wps){
      doe@design.model <- update(doe@design.model, paste0("~ . + wp_", i))
    }
    
    # Update the design table to include the whole plots
    doe@design.table <- getDesignTable(doe, includeWholePlots = TRUE)
    doe@design.matrix <- GenerateDesignMatrix(doe)
  } else if(length(getHtcFactors(doe))==0){
    warning("There are no hard to change factors in the design. Whole Plots will be ignored.")
  }
  
  # Calculate the correlations
  correlation.matrix <- cor(doe@design.matrix$X)

  # Show correlation matrix
  print(correlation.matrix)

  correlation.long <- melt(correlation.matrix) # Long format for ggplot

  # Filter out everything with intercept
  correlation.long <- correlation.long[-which(correlation.long$X1 == "(Intercept)" | correlation.long$X2=="(Intercept)"),]
  correlation.long$value <- abs(correlation.long$value) # use the absolute value for simpler color-coding in the heatmap

  # Control the order of model effects
  # correlation.long$X1 <- factor(correlation.long$X1, levels=attr(terms(doe@design.model), "term.labels"))
  # correlation.long$X2 <- factor(correlation.long$X2, levels=attr(terms(doe@design.model), "term.labels"))
  correlation.long$X1 <- factor(correlation.long$X1, levels = sort.modeleffects(unique(as.character(correlation.long$X1))))
  correlation.long$X2 <- factor(correlation.long$X2, levels = sort.modeleffects(unique(as.character(correlation.long$X2))))
  
    p <- ggplot(correlation.long, aes(x=X2, y=X1, fill=value)) +
      scale_fill_gradientn(
#        colors=c("#e4ff7a", "#ffe81a", "#ffbd00", "#ffa000", "#fc7f00"), # https://github.com/wistia/heatmap-palette/blob/master/LICENSE
        colors = c("#67a9cf", "#ffffbf", "#ef8a62"),
        breaks=c(0, 0.25, 0.5, 0.75, 1),
        limits=c(0, 1)
      ) + 
      geom_tile(color="black") +
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="right") +
      xlab("") + ylab("")
  p

}

#' Helper function to order the model effects in the aliasing heatmap
#' 
#' @param effectnames a vector of strings representing the effects
#'                    visualized in the aliasing heatmap.
#' 
sort.modeleffects <- function(effectnames){
  
  levelOfHierarchy <- unlist(lapply(effectnames, getHierarchyLevel))
  
  orderedNames <- c()
  for(i in min(levelOfHierarchy):max(levelOfHierarchy)){ # Sort names by level of hierarchy
    currentLevel <- effectnames[levelOfHierarchy==i]
    currentLevel <- currentLevel[order(currentLevel)] # In each level of hierarchy sort alphabethically
    orderedNames <- c(orderedNames, currentLevel)
  }
  
  orderedNames
}

#' Get the hirarchie level of a model effect by its name
#' 
#' @param effectname
#' 
getHierarchyLevel <- function(effectname){
  
  # Make sure effectname is a character
  effectname <- as.character(effectname)
  
  # If effectname does not contain any *, : or ^ it must be a main effect
  if(!grepl("[\\^\\*\\:]", effectname)) return(1)
  
  # If effectname contains ^ the number after ^ defines the level 
  if(grepl("\\^", effectname)){
    lastPart <- substr(effectname, start = gregexpr("\\^", effectname), nchar(effectname))
    rslt <- as.integer(gsub("[^0-9]", "", lastPart))
    return(rslt)
  } 
  # If effectname contains : (or *?) the number of : defines the level
  if(grepl("\\*", effectname)){
    rslt <- sum(charToRaw(effectname) == charToRaw("*"))+1 # Count number of * in string
    return(rslt)
  }
  # Treat : as interactions
  if(grepl("\\:", effectname)){ 
    rslt <- sum(charToRaw(effectname) == charToRaw(":"))+1 # Count number of * in string
    return(rslt)
  }
} 
