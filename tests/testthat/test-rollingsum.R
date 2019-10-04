context("rolling_sum")
library(AnomDetct)

test_that("missing values and mixed character strings", {
  sp_data_NA <- rep(c(1,0,NA,0,1), 10)
  sp_data_st <- rep(c(1,0,"a",0,1), 10)
  sp_data    <- rep(c(1,0,0,0,1), 10)

  expect_equal(rolling_sum(sp_data_NA, 3), rolling_sum(sp_data, 3))
  expect_equal(rolling_sum(sp_data_st, 3), rolling_sum(sp_data, 3))
})

test_that("window length great than input size", {
  sp_data <- rep(1, 10)

  expect_error(rolling_sum(sp_data, 30), "Too large `window_l`")
})
