#----------------------------------------------+
# Test cases for doeDesign class.
#----------------------------------------------+
context("DOE Design Class")

test_that("the default constructor works as intended.",
          # doeDesign is not supposed to be created manually. It is returned from the main function.
          {
            
            emptyDeclaration = new("doeDesign")
            
            expect_equal(length(emptyDeclaration@factors), 1)
            expect_is(emptyDeclaration@factors[[1]], "doeFactor")
            
          })

test_that("the constructor using arguments works as intended",
          {

            etc = new("doeFactor", name="temperature", levels=c(180, 200), changes="easy")
            htc = new("doeFactor", name="pH", levels=c(2.7, 12), changes="hard")
            
            namedDeclaration = new("doeDesign", 
                                     factors = list(htc, etc), 
                                     replicate.type="all", 
                                     replicate.n = as.integer(2), 
                                     whole.plot.structure = data.frame(w1 = c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)),
                                     number.runs = as.integer(8)
                                      
                                   )
            
          })

test_that("passing inconsistent arguments produces errors",
          {
            
            
          })


context("Helper Functions for doeDesign")

test_that("getHtcFactors returns the right results.", 
          {
            etc = new("doeFactor", name="temperature", levels=c(180, 200), changes="easy")
            htc = new("doeFactor", name="pH", levels=c(2.7, 12), changes="hard")
            htc2 = new("doeFactor", name="pressure", levels=c(3, 100), changes="hard")
            
            twoHTC = new("doeDesign", 
                         factors = list(htc2, htc, etc),
                         whole.plot.structure=data.frame(w1 = c(1,1,1,1)), number.runs=as.integer(4)
                      )
            
            expect_equal(length(getHtcFactors(twoHTC)), 2) # DOE with two HTC-factors returns a list of two elements
            
            oneHTC = new("doeDesign", 
              factors = list(htc, etc),
              whole.plot.structure=data.frame(w1 = c(1,1,1,1)), number.runs=as.integer(4)
            )
            expect_equal(length(getHtcFactors(oneHTC)), 1) # DOE with one HTC-factor does return list with one element
            expect_is(getHtcFactors(oneHTC), "list") 
                        
            noHTC = new("doeDesign")
            expect_equal(length(getHtcFactors(noHTC)), 0) # DOE with no HTC-factor does return empty list
            
            expect_error(getHtcFactors("something")) # only doeDesign is accepted
          })