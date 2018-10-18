#--------------------------------+
# Generate a semi split plot 
# with only 2 whole plots
#--------------------------------+

library(ggplot2)
library(easyGgplot2)


#--------------------------------+
# Factors
#--------------------------------+
phFactor <- new("doeFactor",
                name="pH",
                type="continuous",
                levels = c(3,12),
                number.levels = as.integer(2),
                changes="easy"
)
timeFactor <- new("doeFactor",
                  name="time",
                  type="continuous",
                  levels = c(10,20),
                  number.levels = as.integer(2),
                  changes="easy"
)
solventFactor <- new("doeFactor",
                     name="solvent",
                     type="categorical",
                     levels=c("A", "B", "C", "D", "E", "F"),
                     changes="semi.hard",
                     semi.htc.group.size=as.integer(4)
)

#--------------------------------+
# Specify DOE
#--------------------------------+
doeSpecification <- GenerateNewDoeDesign(
         factors = list(solventFactor, phFactor, timeFactor),
         whole.plot.structure = data.frame(solvent=rep(1:2, each=30)),
         number.runs = as.integer(60),
         design.model = ~solvent+pH+time,
         optimality.function = DOptimality,
         random.doe = TRUE
)

# Modify the random starting design to cover all levels of solvent
doeSpecification@design.table$solvent[1:30] <- sample(c("A", "B", "C", "D"), size = 30, replace = T)
doeSpecification@design.table$solvent[31:60] <- sample(c("A", "B", "E", "F"), size = 30, replace = T)

# Generate the optimal design
optimalDesign2wp <- GenerateOptimalDesign(doeSpecification, random.start = 1, max.iter = 20, continue = TRUE)

# AliasingHeatMap(optimalDesign2wp[[1]],includeWholePlots = T)
# ggplot(optimalDesign2wp[[1]]@design.table, aes(x=solvent)) + geom_bar()

save(optimalDesign2wp,file = "vignettes/designs/semi2wp.rda")

#--------------------------------+
# Generate a semi split plot with 
# 3 whole plots
#--------------------------------+
doeSpecification <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  whole.plot.structure = data.frame(solvent=rep(1:3, each=20)),
  number.runs = as.integer(60),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality,
  random.doe = TRUE
)

# Generate the optimal design
optimalDesign3wp <- GenerateOptimalDesign(doeSpecification, random.start = 3, max.iter = 10)

save(optimalDesign3wp,file = "vignettes/designs/semi3wp.rda")

# AliasingHeatMap(optimalDesign3wp[[1]],includeWholePlots = T)
# ggplot(optimalDesign3wp[[1]]@design.table, aes(x=solvent)) + geom_bar()

#--------------------------------+
# Generate a semi split plot with 
# 4 whole plots
#--------------------------------+
doeSpecification <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  whole.plot.structure = data.frame(solvent=rep(1:4, each=15)),
  number.runs = as.integer(60),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality,
  random.doe = TRUE
)

# Generate the optimal design
optimalDesign4wp <- GenerateOptimalDesign(doeSpecification, random.start = 5, max.iter = 10)

save(optimalDesign4wp,file = "vignettes/designs/semi4wp.rda")

# AliasingHeatMap(optimalDesign4wp[[1]],includeWholePlots = T)
# ggplot(optimalDesign4wp[[1]]@design.table, aes(x=solvent)) + geom_bar()


#--------------------------------+
# Generate a semi split plot with 
# 6 whole plots
#--------------------------------+
doeSpecification <- GenerateNewDoeDesign(
  factors = list(solventFactor, phFactor, timeFactor),
  whole.plot.structure = data.frame(solvent=rep(1:6, each=10)),
  number.runs = as.integer(60),
  design.model = ~solvent+pH+time,
  optimality.function = DOptimality,
  random.doe = TRUE
)

# Generate the optimal design
optimalDesign6wp <- GenerateOptimalDesign(doeSpecification, random.start = 3, max.iter = 10)

save(optimalDesign6wp,file = "vignettes/designs/semi6wp.rda")
