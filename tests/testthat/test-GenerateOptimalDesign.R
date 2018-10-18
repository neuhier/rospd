#----------------------------------------------+
# Test cases for generate optimal design
#----------------------------------------------+
context("Generate Optimal Design")

test_that(" no design is returned if optimality is 0.",
          {
           expect_message(GenerateOptimalDesign(notworking),"Algorithm did not converge. Optimality is 0.")
          
           expect_equal(nrow(GenerateOptimalDesign(notworking)[[1]]@design.table), 0)
          }
          )

test_that(" a D-optimal design can be generated.",
          {

          # Simple 1 etc factors
          bivariate@optimality.function <- DOptimality
          expect_error(GenerateOptimalDesign(bivariate, random.start=1, max.iter=2), NA)

          # 2 etc with one interaction
          multivariate.interactions@optimality.function <- DOptimality
          expect_error(GenerateOptimalDesign(multivariate.interactions, random.start = 1, max.iter = 2), NA)

          # Using categorical factors
          multivariate.categoric@optimality.function <- DOptimality
          expect_error(GenerateOptimalDesign(multivariate.categoric, random.start = 1, max.iter = 2), NA)

          # Splitplot
          splitplot.categoric@optimality.function <- DOptimality
          expect_error(GenerateOptimalDesign(splitplot.categoric, random.start = 1, max.iter = 1), NA)

          })


test_that(" a I-optimal design can be generated.",
          {

            # # Simple 1 etc factors
            # bivariate@optimality.function <- IOptimality
            # expect_error(GenerateOptimalDesign(bivariate, random.start=1, max.iter=2), NA)
            # 
            # # 2 etc with one interaction
            # multivariate.interactions@optimality.function <- IOptimality
            # multivariate.interactions@number.runs <- as.integer(8) # Numeric problems with few runs (?!)
            # expect_error(GenerateOptimalDesign(multivariate.interactions, random.start = 1, max.iter = 2), NA)
            # multivariate.interactions@number.runs <- as.integer(4) # Numeric problems with few runs (?!)
            # 
            # # Using categorical factors
            # multivariate.categoric@optimality.function <- IOptimality
            # GenerateOptimalDesign(multivariate.categoric, random.start = 1, max.iter = 2)
            # expect_error(GenerateOptimalDesign(multivariate.categoric, random.start = 1, max.iter = 2), NA)
            # 
            # # Splitplot
            # splitplot.categoric@optimality.function <- IOptimality
            # expect_error(GenerateOptimalDesign(splitplot.categoric, random.start = 1, max.iter = 1), NA)

          })



test_that(" a A-optimal design can be generated.",
          {

            # # Simple 1 etc factors
            # bivariate@optimality.function <- AOptimality
            # expect_error(GenerateOptimalDesign(bivariate, random.start=1, max.iter=2), NA)
            # 
            # # 2 etc with one interaction
            # multivariate.interactions@optimality.function <- AOptimality
            # multivariate.interactions@number.runs <- as.integer(20)
            # expect_error(GenerateOptimalDesign(multivariate.interactions, random.start = 1, max.iter = 2), NA)
            # 
            # # Using categorical factors
            # multivariate.categoric@optimality.function <- AOptimality
            # expect_error(GenerateOptimalDesign(multivariate.categoric, random.start = 1, max.iter = 2), NA)
            # 
            # # Splitplot
            # splitplot.categoric@optimality.function <- AOptimality
            # expect_error(GenerateOptimalDesign(splitplot.categoric, random.start = 1, max.iter = 1), NA)

          })
