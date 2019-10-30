context("Bisection")
library(AnomDetct)

test_that("numeric input",{
  arr <- c(0,1,1,3,4,4,5,5,7,10)
  fn <- function(x) arr[x]

  expect_identical(bisect(fn = fn, lower = 1, upper = 10,
                          discrete = T, target = 4), 5)
  expect_identical(bisect(fn = fn, lower = 1, upper = 10,
                          discrete = T, target = 6,right = F), 8)
  expect_identical(bisect(fn = fn, lower = 1, upper = 10,
                          discrete = T, target = 8,right = F,index = F), 7)
})

test_that("character input",{
  arr <- c("apple","banana","coconut","coconut","cranberry","grapes", "mango")
  fn <- function(x) arr[x]

  expect_identical(bisect(fn = fn, lower = 1, upper = 7,
                          discrete = T, target = "coconut"), 3)
  expect_identical(bisect(fn = fn, lower = 1, upper = 7,
                          discrete = T, target = "avocados",right = F), 1)
  expect_match(bisect(fn = fn, lower = 1, upper = 7,
                      discrete = T, target = "lime", right = F, index = F), "grapes")
})

test_that("logical input",{
  arr <- c(rep(F,100),rep(T,1000))
  fn <- function(x) arr[x]

  expect_identical(bisect(fn = fn, lower = 1, upper = 1100,
                          discrete = T, target = T), 101)
  expect_identical(bisect(fn = fn, lower = 1, upper = 1100,
                          discrete = T, target = F,right = F), 100)
})

test_that("continous domain",{
  fn <- function(x) if(x>3 && x<5) 4 else x

  expect_equal(bisect(fn = fn, lower = 0, upper = 10,
                      discrete = F,target = 4,right = T), 3, tolerance = 10^(-7))
  expect_equal(bisect(fn = fn, lower = 0, upper = 10,
                      discrete = F,target = 4,right = F), 5, tolerance = 10^(-7))
})
