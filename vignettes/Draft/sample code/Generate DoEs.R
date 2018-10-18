#-----------------------------+
# Create and save all designs 
# for the vignette
#-----------------------------+

# 1. Introduction
set.seed(153142)

# CRD 
crdSpec <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name="Blanket Type", levels=c("1", "2"), type="categorical"),
    new("doeFactor", name="Cylinder Gap", levels=c("low", "high"), type="categorical"),
    new("doeFactor", name="Press Speed", levels=c("low", "high"), type="categorical")
  ),
  design.model = ~`Blanket Type`*`Cylinder Gap`*`Press Speed`,
  number.runs = as.integer(8),
  optimality.function = DOptimality
)

intro.crd <- GenerateOptimalDesign(crdSpec, 10, 10, silent = TRUE)
save(intro.crd, file="vignettes/designs/introcrd.rda")

set.seed(153142)  

splitplotSpec <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name="Blanket_Type", levels=c("1", "2"), type="categorical", changes="hard"),
    new("doeFactor", name="Cylinder Gap", levels=c("low", "high"), type="categorical"),
    new("doeFactor", name="Press Speed", levels=c("low", "high"), type="categorical")
  ),
  design.model = ~`Blanket_Type`*`Cylinder Gap`*`Press Speed`,
  number.runs = as.integer(8),
  whole.plot.structure = data.frame("Blanket_Type"=c(1,1,2,2,3,3,4,4)),
  optimality.function = DOptimality
)

intro.splitplot <- GenerateOptimalDesign(splitplotSpec, 5, 10, silent = TRUE)

save(intro.splitplot, file="vignettes/designs/introsplitplot.rda")

# 2. Main Part

# pH - an ETC continuous factor
phFactor <- new("doeFactor",
                name="pH",
                type="continuous",
                levels = c(3,12),
                number.levels = as.integer(2),
                changes="easy"
)

# time - an ETC continuous factor
timeFactor <- new("doeFactor",
                  name="time",
                  type="continuous",
                  levels = c(10,20),
                  number.levels = as.integer(2),
                  changes="easy"
)

# solvent - a SHTC categorical factor
solventFactor <- new("doeFactor",
                     name="solvent",
                     type="categorical",
                     levels=c("A", "B", "C", "D", "E", "F"),
                     changes="semi.hard",
                     semi.htc.group.size=as.integer(4)
)

doeSpecification <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  whole.plot.structure = data.frame(solvent=rep(1:6, each=10)),
  number.runs = as.integer(60),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality
)

set.seed(42)
optimalDesign <- GenerateOptimalDesign(doeSpecification, 
                                       random.start = 1,
                                       max.iter = 10,
                                       silent=TRUE)

save(optimalDesign, file="vignettes/designs/optimalDesign.rda")

# Alternative Designs

#--------------------------+
# Generate CRD version
#--------------------------+

solventFactor <- new("doeFactor",
                     name="solvent",
                     type="categorical",
                     levels=c("A", "B", "C", "D", "E", "F"),
                     changes="easy")

crdDesignSpec <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  number.runs = as.integer(60),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality
)

crd <-  GenerateOptimalDesign(crdDesignSpec, 
                              random.start = 1,
                              max.iter = 10,
                              silent=TRUE)

save(crd, file="vignettes/designs/crd.rda")
#----------------------------+
# Generate split plot version
#----------------------------+
solventFactor <- new("doeFactor",
                     name="solvent",
                     type="categorical",
                     levels=c("A", "B", "C", "D", "E", "F"),
                     changes="hard")

splitPlotSpec <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  whole.plot.structure = data.frame(solvent=rep(1:12, each=5)),
  number.runs = as.integer(60),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality
)

splitplot1 <-  GenerateOptimalDesign(splitPlotSpec, 
                                     random.start = 1,
                                     max.iter = 10,
                                     silent=TRUE)
save(splitplot1, file="vignettes/designs/splitplot1.rda")

splitPlotSpec <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  whole.plot.structure = data.frame(solvent=rep(1:8, each=8)),
  number.runs = as.integer(64),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality
)

splitplot2 <-  GenerateOptimalDesign(splitPlotSpec, 
                                     random.start = 1,
                                     max.iter = 10,
                                     silent=TRUE)
save(splitplot2, file="vignettes/designs/splitplot2.rda")
