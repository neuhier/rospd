#----------------------------------------------+
# Test cases for generate random design.
#----------------------------------------------+
context("Generate Random Design")

test_that(" a random design for continuous and categorical factors is created.",{
  
  set.seed(5)
  expect_equal(
    GenerateRandomDesign(multivariate.categoric),
    structure(
      list(
        Temperature = c(-0.6, 1,-0.8, 0, 1,-0.4,-0.4,-0.6),
        Condition = c("B", "A", "B", "B", "A", "A", "B", "A")
      ),
      .Names = c("Temperature",
                 "Condition"),
      row.names = c(NA,-8L),
      class = "data.frame"
    )
  )
})

test_that(" a random design for continuous and catgoric (etc and htc) factors is created.",{
  set.seed(5)  
  expect_equal(
    GenerateRandomDesign(splitplot.categoric),
    structure(list(X3 = c(-1, -1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 
                          1, 1, 1, 1, 1), X4 = c("C", "C", "C", "C", "C", "C", "C", "C", 
                                                 "C", "C", "C", "C", "C", "C", "C", "C"), X1 = c(-0.4, -0.8, 0.4, 
                                                                                                 0, 0.6, 1, -0.8, -0.4, -0.6, -0.6, -0.8, 0, -0.2, 1, -0.8, 1), 
                   X2 = c("A", "A", "B", "A", "A", "A", "B", "B", "A", "A", 
                          "A", "A", "A", "A", "B", "B")), .Names = c("X3", "X4", "X1", 
                                                                     "X2"), row.names = c(NA, -16L), class = "data.frame")
  )
  }
)
