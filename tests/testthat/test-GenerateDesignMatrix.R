context("GenerateDesignMatrix")

test_that("function returns expected results for simple linear models.",
          {
            expect_equal(
              GenerateDesignMatrix(bivariate)$X,
              matrix(
                c(1, 1, 1, 1, 1, 1,-1,-1),
                ncol = 2,
                dimnames = list(c("1", "2", "3", "4"), colnames = c("(Intercept)", "X"))
              ),
              check.attributes = FALSE
            )
            
            expect_equal(GenerateDesignMatrix(bivariate)$Z,
                         matrix(),
                         check.attributes = FALSE)
            
            expect_equal(
              GenerateDesignMatrix(multivariate)$X,
              matrix(
                c(1, 1, 1, 1, 1, 1,-1,-1, 1,-1, 1,-1),
                ncol = 3,
                dimnames = list(c("1", "2", "3", "4"), colnames = c("(Intercept)", "X1", "X2"))
              ),
              check.attributes = FALSE
            )
            
            expect_equal(GenerateDesignMatrix(multivariate)$Z,
                         matrix(),
                         check.attributes = FALSE)
            
          })

test_that("function returns expected results for models with interactions",
          {
            expect_equal(
              GenerateDesignMatrix(multivariate.interactions)$X,
              matrix(
                c(1, 1, 1, 1, 1, 1,-1,-1, 1,-1, 1,-1, 1, -1, -1, 1),
                ncol = 4,
                dimnames = list(
                  c("1", "2", "3", "4"),
                  colnames = c("(Intercept)", "X1", "X2", "I(X1 * X2)")
                )
              ),
              check.attributes = FALSE
            )
            
            expect_equal(GenerateDesignMatrix(multivariate.interactions)$Z,
                         matrix(),
                         check.attributes = FALSE)
            
          })

test_that("function returns expected results for polynomial models", {
  expect_equal(
    GenerateDesignMatrix(multivariate.polynomial)$X,
    matrix(
      c(1, 1, 1, 1, 1,
        1, 1,-1,-1, 0,
        1,-1, 1,-1, 0,
        1,-1,-1, 1, 0,
        1, 1, 1, 1, 0),
      ncol = 5,
      dimnames = list(
        c("1", "2", "3", "4", "5"),
        c("(Intercept)", "X1", "X2", "I(X1 * X2)", "I(X1 * X1)")
      )
    ),
    check.attributes = FALSE
  )
  
  expect_equal(GenerateDesignMatrix(multivariate.polynomial)$Z,
               matrix(),
               check.attributes = FALSE)
  
})

test_that("function returns expected results for htc factors", {
  
  expect_equal(
    GenerateDesignMatrix(splitplot)$X,
    structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, 1, 1, -1, -1, 
                1, -1, 1, -1, 1, -1, 1, -1, -1, -1, 1, 1, -1, -1, -1, -1), 
              .Dim = c(8L, 4L), 
              .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", "8"), 
                                c("(Intercept)", "X1", "X2", "W")), 
              assign = 0:3),
    check.attributes = FALSE
  )
  
  expect_equal(GenerateDesignMatrix(splitplot)$Z,
               structure(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 
                           0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1), 
                         .Dim = c(8L, 4L), 
                         .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", "8"),
                                          c("as.factor(W)1", "as.factor(W)2", "as.factor(W)3", "as.factor(W)4")), 
                         assign = c(1L, 1L, 1L, 1L), 
                         contrasts = structure(list(`as.factor(W)` = "contr.treatment"), .Names = "as.factor(W)")),
               check.attributes = FALSE)
  
})