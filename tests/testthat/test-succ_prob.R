context("success_probability")
library(AnomDetct)

test_that("match with Haiman's Table 1", {
  expect_equal(qL_function(256,8,30,0.1), 0.0794, tolerance = 10^(-4))
  expect_equal(qL_function(512,10,30,0.1), 0.73888, tolerance = 10^(-5))
  expect_equal(qL_function(1024,11,30,0.1), 0.8872712, tolerance = 10^(-7))
})

test_that("interpolation",{
  expect_equal(prob_fun(256*30,8,30,0.1), qL_function(256,8,30,0.1))
  expect_equal(prob_fun(24576*30,11,30,0.1), 0.05654, tolerance = 10^(-5))
})
