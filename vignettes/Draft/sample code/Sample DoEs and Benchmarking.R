#-----------------------------+
# Mostly for testing purposes:
# - Create some sample does
# - do some benchmarking
#-----------------------------+
library(rospd)
library(profvis)

# D-Optimal Completely Randomized Design
doeSpec.dopt.simple <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name="X1"),
    new("doeFactor", name="X2"),
    new("doeFactor", name="X3", type="categorical", levels=c("A", "V", "C"))
  ),
  design.model = ~X1*X2*X3,
  number.runs = as.integer(20),
  optimality.function = DOptimality,
  random.doe = TRUE
)

profvis({
  GenerateOptimalDesign(doeSpec.dopt.simple, random.start = 5, max.iter = 10)  
})


#----------------------------------+
# Splitplot Dopt Design
#----------------------------------+
# D-Optimal Completely Randomized Design
doeSpec.dopt.splitplot <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name="x1"),
    new("doeFactor", name="x2"),
    new("doeFactor", name="x3", type="categorical", levels=c("A", "V", "C"), changes="hard")
  ),
  design.model = ~x1*x2*x3,
  number.runs = as.integer(20),
  whole.plot.structure = data.frame(x3 = rep(1:5, each=4)),
  optimality.function = DOptimality,
  random.doe = TRUE
)

profvis({
  GenerateOptimalDesign(doeSpec.dopt.splitplot, random.start = 2, max.iter = 5)  
})
