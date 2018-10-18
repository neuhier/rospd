#----------------------------------------------+
# Test cases for doeFactor class.
#----------------------------------------------+
context("DOE Factor Class")

test_that("the default constructor works as intended.",
         {
           
           emptyDeclaration = new("doeFactor")
           
           
           expect_is(emptyDeclaration, "doeFactor")
           expect_identical(emptyDeclaration@name, "X1")
           expect_identical(emptyDeclaration@type, "continuous")
           expect_identical(emptyDeclaration@levels, c(-1, 1))
           expect_identical(emptyDeclaration@changes, "easy")
           expect_identical(emptyDeclaration@semi.htc.group.size, NA_integer_)

         })

test_that("the constructor using arguments works for continuous factors",
        {
          namedDeclaration = new("doeFactor", name="temperature", type="continuous", levels=c(180, 200), changes="easy")
          
          expect_is(namedDeclaration, "doeFactor")
          expect_identical(namedDeclaration@name, "temperature")
          expect_identical(namedDeclaration@type, "continuous")
          expect_identical(namedDeclaration@levels, c(180, 200))
          expect_identical(namedDeclaration@changes, "easy")
          expect_identical(namedDeclaration@semi.htc.group.size, NA_integer_)
        })

test_that("passing inconsistent arguments produces errors",
          {
           
            expect_error(new("doeFactor", type="something"))
            expect_error(new("doeFactor", type="continuous", levels=c("-1", 1)))
            expect_error(new("doeFactor", type="continuous", levels=c(-1, 1), number.levels=as.integer(3), changes="semi.hard", semi.htc.group.size=as.integer(100)))
          })

test_that("function getPossibleLevels works as intended",
          {
            
            etc.categoric.factor <- new("doeFactor", type="categorical", levels=c("A", "B", "C"))
            etc.continuous.factor <- new("doeFactor", type="continuous", levels=c(-1, 1), number.levels=as.integer(21))
            etc.continuous.factor.3 <- new("doeFactor", type="continuous", levels=c(-1, 1), number.levels=as.integer(3))
            
            expect_equal(getPossibleLevels(etc.categoric.factor), c("A", "B", "C"))
            expect_equal(getPossibleLevels(etc.continuous.factor), seq(-1,1, by=0.1))
            expect_equal(getPossibleLevels(etc.continuous.factor.3), seq(-1,1, by=1))

          })


test_that("function getRandomLevel works as intended",
          {
            # Categoric factor
            etc.categoric.factor <- new("doeFactor", type="categorical", levels=c("A", "B", "C"))
            set.seed(5)
            expect_equal(getRandomLevel(etc.categoric.factor), "A")

            # Continuous factor
            etc.continuous.factor <- new("doeFactor", type="continuous", levels=c(-1, 1))
            set.seed(5)
            expect_equal(getRandomLevel(etc.continuous.factor), -1)
            
            # Continuous factor different number of characters after decimal sign
            etc.continuous.factor.3 <- new("doeFactor", type="continuous", levels=c(-1, 1), number.levels=as.integer(21))
            set.seed(5)
            expect_equal(getRandomLevel(etc.continuous.factor.3), -0.6)
            
            # continuous htc factor
            htc.continuous.factor <- new("doeFactor", type="continuous", levels=c(-1,1), number.levels = as.integer(101), changes="hard")
            set.seed(5)
            expect_equal(getRandomLevel(htc.continuous.factor, 5), -0.6)
            
            # continuous semi.htc.factor
            semi.htc.continuous.factor <- new("doeFactor", type="continuous", levels=c(-1, 1), number.levels=as.integer(101), changes="semi.hard", semi.htc.group.size=as.integer(3))
            set.seed(5)
            expect_equal(getRandomLevel(semi.htc.continuous.factor, 5), c(-.6, .36, .80))
            
            semi.htc.categoric.factor <- new("doeFactor", type="categorical", levels=c("A", "B", "C"), changes="semi.hard", semi.htc.group.size=as.integer(2))
            set.seed(5)
            expect_equal(getRandomLevel(semi.htc.categoric.factor, 6), c("A", "B"))
            
            # Test skipping
            manyTimes = sapply(1:1000, function(x){getRandomLevel(etc.continuous.factor, skip=seq(0.1, 0.9, by=0.1))})
            expect_equal(any(manyTimes %in% seq(0.1, 0.9, by=0.1)), FALSE)
            
            manyTimesAsWell = sapply(1:1000, function(x){getRandomLevel(etc.categoric.factor, skip=c("A"))})
            expect_equal(any(manyTimesAsWell %in% "A"), FALSE)            
            })
