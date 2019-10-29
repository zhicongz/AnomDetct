context("Bisection")
library(AnomDetct)

test_that("numeric input",{
  arr <- c(0,1,1,3,4,4,5,5,7,10)

  expect_identical(bisect(arr,4), 5)
  expect_identical(bisect(arr,6,right = F), 8)
  expect_identical(bisect(arr,8,right = F,index = F), 7)
})

test_that("character input",{
  arr <- c("apple","banana","coconut","coconut","cranberry","grapes", "mango")

  expect_identical(bisect(arr,"coconut"), 3)
  expect_identical(bisect(arr,"avocados",right = F), 1)
  expect_match(bisect(arr,"lime",right = F,index = F), "grapes")
})

test_that("logical input",{
  arr <- c(rep(F,100),rep(T,1000))

  expect_identical(bisect(arr,T), 101)
  expect_identical(bisect(arr,F,right = F), 100)
})
