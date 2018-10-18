context("Generate Iteration Matrix")

test_that( "Generate Iteration Matrix works as intended" , {
  
  expect_equal(GenerateIterationMatrix(splitplot2),
  
               data.frame(
                 W2 = c(1,1,1,1,12,12,12,12),
                 W1 = c(2,2,3,3,13,13,14,14),
                 X1 = c(4,5,6,7, 15, 16, 17, 18),
                 X2 = c(8,9,10,11, 19, 20, 21, 22)
                 ),
               check.attributes = FALSE
               )
})

test_that(" Generate Iteration Matrix works for overlapping whole plots as well.", {
  
  expect_equal(
    GenerateIterationMatrix(splitplot.overlapping),
    
      data.frame(
        X1 = c(1,1,1,1, 8,8,8,8, 15, 15, 15, 15),
        X2 = c(2,2,2,3, 3,3,10, 10,  10, 17,17,17),
        X3 = c(4,5,6,7, 11,12,13,14, 18, 19, 20, 21)
      ),
    check.attributes = FALSE
  )
  
})