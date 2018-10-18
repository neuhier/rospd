context("Calculation of D-Optimality")

test_that("D-optmiality is calculated correctly for a partially polynomial model",
          {
            
            expect_equal(
              DEfficiency(nist.example),
              68.26,
              tolerance=0.01
            )

            expect_equal(
              DEfficiency(multivariate.interactions),
              100
            )
            
          })


test_that("DEfficiency is consistent with JMP",
          {

            expect_equal(round(DEfficiency(definitive.screening), 5), 85.06141) 
            
          })

test_that("D-optimality is calculated correctly for a split plot design", 
          {
          
            # DEfficiency(splitplot.jones.goos.Doptim)
            # 
            # DEfficiency(splitplot.jones.goos.Ioptim)
            # 
            # DEfficiency.compare(splitplot.jones.goos.Ioptim, splitplot.jones.goos.Doptim)
            
          })