#-----------------------------------+
# A design that cannot work
#-----------------------------------+
notworking <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name="X1", levels=c(0,1), number.levels = as.integer(2)),
    new("doeFactor", name="X2")),
  design.model = ~ X1+X2+I(X1^2),
  number.runs = as.integer(4),
  optimality.function = DOptimality
)

#-----------------------------------+
# Generate simple CRDs
#-----------------------------------+
bivariate <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name = "X")),
  design.model = ~ X,
  design.table = data.frame(X = c(1, 1, -1, -1)),
  number.runs = as.integer(4)
)

multivariate <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name = "X1"),
                 new("doeFactor", name = "X2")),
  design.model = ~ X1 + X2,
  design.table = data.frame(X1 = c(1, 1, -1, -1), X2 = c(1, -1, 1, -1)),
  number.runs = as.integer(4)
)

multivariate.interactions <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name ="X1"),
                 new("doeFactor", name = "X2")),
  design.model = ~ X1 + X2 + I(X1 * X2),
  design.table = data.frame(X1 = c(1, 1, -1, -1), X2 =
                              c(1, -1, 1, -1)),
  number.runs = as.integer(4)
)

multivariate.polynomial <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name = "X1"),
                 new("doeFactor", name = "X2")),
  design.model = ~ X1 + X2 +
    I(X1 * X2) + I(X1 * X1),
  design.table = data.frame(X1 =
                              c(1, 1, -1, -1, 0), X2 = c(1, -1, 1, -1, 0)),
  number.runs = as.integer(5)
)

#-----------------------------------+
# Use categoric variables as well
#-----------------------------------+
multivariate.categoric <- GenerateNewDoeDesign(
                              factors = list(new("doeFactor", name="Temperature", levels=c(-1,1), number.levels = as.integer(11)),
                                             new("doeFactor", name="Condition", levels=c("A", "B"), type="categorical")
                                             ),
                              design.model = ~ Temperature:Condition,
                              number.runs = as.integer(8)
                              )

splitplot.categoric <- GenerateNewDoeDesign(
                           factors = list(
                             new("doeFactor", name="X1", levels=c(-1,1), number.levels = as.integer(11)),
                             new("doeFactor", name="X2", levels=c("A", "B"), type="categorical"),
                             new("doeFactor", name="X3", levels=c(-1,1), changes = "hard" ),
                             new("doeFactor", name="X4", levels=c("A", "B", "C"), type="categorical", changes="semi.hard", semi.htc.group.size=as.integer(2))
                           ),
                           design.model = ~ X1+X2+X3+X4,
                           whole.plot.structure = data.frame(X3 = c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2),
                                                             X4 = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)),
                           number.runs = as.integer(16)

                           )

#-----------------------------------+
# Generate split plots and semi
# split plots
#-----------------------------------+

splitplot <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name = "X1"),
                 new("doeFactor", name = "X2"),
                 new("doeFactor", name = "W", changes="hard", type="continuous", levels=c(-1,1))),
  design.model = ~ X1 + X2 + W,
  design.table = data.frame(X1 = c(1, 1, -1, -1, 1, 1, -1, -1),
                            X2 = c(1, -1, 1, -1, 1, -1, 1, -1),
                            W = c(-1,-1,1,1,-1,-1,-1,-1)),
  whole.plot.structure = data.frame(W = c(1,1,2,2,3,3,4,4)),
  number.runs = as.integer(8)
)

splitplot2 <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name = "X1"),
                 new("doeFactor", name = "X2"),
                 new("doeFactor", name = "W", changes="hard", type="continuous", levels=c(-1,1)),
                 new("doeFactor", name = "W2", changes="hard", type="continuous", levels=c(-1,1))
  ),
  design.model = ~ X1 + X2 + W + W2,
  design.table = data.frame(X1 = c(1, 1, -1, -1, 1, 1, -1, -1),
                            X2 = c(1, -1, 1, -1, 1, -1, 1, -1),
                            W = c(-1,-1,1,1,-1,-1,-1,-1),
                            W2 = c(1,1,1,1,-1,-1,-1,-1)),
  whole.plot.structure = data.frame(W = c(1,1,2,2,3,3,4,4),
                                    W2 = c(1,1,1,1,2,2,2,2)),
  number.runs = as.integer(8)
)


semisplitplot <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name = "X1"),
                 new("doeFactor", name = "X2"),
                 new("doeFactor", name = "W", changes="hard", type="continuous", levels=c(-1,1)),
                 new("doeFactor", name = "SW", changes="semi.hard", type="continuous", levels=c(-1,1), number.levels=as.integer(11), semi.htc.group.size=as.integer(3))
  ),
  design.model = ~ X1 + X2 + W + W2,
  design.table = data.frame(X1 = c(1, 1, -1, -1, 1, 1, -1, -1),
                            X2 = c(1, -1, 1, -1, 1, -1, 1, -1),
                            W = c(-1,-1,1,1,-1,-1,-1,-1),
                            SW = c(1,1,1,1,-1,-1,-1,-1)),
  whole.plot.structure = data.frame(W = c(1,1,2,2,3,3,4,4),
                                    SW = c(1,1,1,1,2,2,2,2)),
  number.runs = as.integer(8),
  optimality.function = DOptimality,
  optimality.criterion = "DOptimality"
)

splitplot.overlapping <- GenerateNewDoeDesign(
  factors = list(new("doeFactor", name="X1", changes="hard"),
                 new("doeFactor", name="X2", changes="hard"),
                 new("doeFactor", name="X3")
                 ),
  design.model = ~ X1+X2+X3,
  whole.plot.structure = data.frame(X1 = c(1,1,1,1,2,2,2,2,3,3,3,3), X2 = c(1,1,1,2,2,2,3,3,3,4,4,4)),
  number.runs = as.integer(12),
  optimality.function = DOptimality,
  optimality.criterion = "DOptimality"
)

#-----------------------------------+
# Textbook examples
#-----------------------------------+
# Reproduce a definitive screening that was created in JMP13.
definitive.screening <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name = "X1"),
    new("doeFactor", name = "X2"),
    new("doeFactor", name = "X3"),
    new("doeFactor", name = "X4"),
    new("doeFactor", name = "X5")
  ),
  number.runs = as.integer(17),
  design.model = ~ X1 + X2 + X3 + X4 + X5,
  design.table = structure(
    list(
      X1 = c(-1L, 1L, 1L,-1L, 0L, 1L,-1L, 1L, 1L, 1L,-1L, 1L,-1L,-1L,-1L, 0L, 0L),
      X2 = c(-1L,-1L, 0L, 1L, 0L, 1L, 1L,-1L,-1L, 1L, 1L, 1L,-1L,-1L, 0L, 1L,-1L),
      X3 = c(1L, 0L, 1L,-1L, 0L, 1L, 0L, 1L,-1L,-1L, 1L,-1L,-1L, 1L,-1L, 1L,-1L),
      X4 = c(1L, 1L, 1L, 1L, 0L,-1L,-1L,-1L, 0L,-1L, 0L, 1L, 1L,-1L,-1L, 1L,-1L),
      X5 = c(0L, 1L,-1L, 1L, 0L, 1L,-1L,-1L, 1L, 0L,-1L,-1L,-1L, 1L, 1L, 1L,-1L)
    ),
    .Names = c("X1", "X2", "X3", "X4", "X5"),
    class = "data.frame",
    row.names = c(NA, -17L)
  )
)

# Data from: http://www.itl.nist.gov/div898/handbook/pri/section5/pri521.htm
nist.example <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name = "X1"),
    new("doeFactor", name = "X2"),
    new("doeFactor", name = "X3")
  ),
  design.model = ~ X1 + X2 + X3 + I(X1 * X1),
  design.table = structure(
    list(
      X1 = c(-1,-1,-1,-1, 0, 0, 0, 0, 1, 1, 1, 1),
      X2 = c(-1,-1, 1, 1,-1,-1, 1, 1,-1,-1, 1, 1),
      X3 = c(-1,
             1,-1, 1,-1, 1,-1, 1,-1, 1,-1, 1),
      colnames = structure(
        c(1L,
          2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
        class = "factor",
        .Label = c("X1",
                   "X2", "X3")
      )
    ),
    .Names = c("X1", "X2", "X3", "colnames"),
    row.names = c(NA,-12L),
    class = "data.frame"
  )
)

# From: Jones/Goos: I-optimal versus D-optimal split-plot response surface designs
splitplot.jones.goos.Doptim <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name = "w", changes = "hard"),
    new("doeFactor", name = "s", changes = "easy")
  ),
  design.model = ~ s + w + I(s * w) + I(w *
                                          w) + I(s * s),
  design.table = data.frame(
    w = c(rep(-1, 5), rep(0, 5), rep(1, 5), rep(1, 5)),
    s = c(-1, -1, 0, 1, 1, -1, -1, 0, 0, 1, -1, -1, 0, 1, 1, -1, -1, 0, 1, 1)
  ),
  variance.ratio = 1,
  whole.plot.structure = data.frame(w = rep(1:4, each =
                                               5)),
  number.runs = as.integer(20)
)

splitplot.jones.goos.Ioptim <- GenerateNewDoeDesign(
  factors = list(
    new("doeFactor", name = "w", changes = "hard"),
    new("doeFactor", name = "s", changes = "easy")
  ),
  design.model = ~ s + w + I(s * w) + I(w *
                                          w) + I(s * s),
  design.table = data.frame(
    w = c(rep(-1, 5), rep(0, 5), rep(1, 5), rep(1, 5)),
    s = c(-1, -1, 0, 1, 1,-1, 0, 0, 0, 1,-1, 0, 0, 0, 1, -1, -1, 0, 1, 1)
  ),
  variance.ratio = 1,
  whole.plot.structure = data.frame(w = rep(1:4, each =
                                               5)),
  number.runs = as.integer(20)
)
