context("Uniformization")
library(AnomDetct)

test_that("shuffled order uniformization", {
  sp_data <- rep(c(seq(1,9,2), seq(2,10,2)), 10:1)
  table_unif_x <- function(x){
    temp <- unif(x, unit = 1, rd = T)
    table(cut(temp,breaks = c(1:11),right = F))
  }
  table_x <- function(x) table(cut(x,breaks = c(1:11),right = F))

  expect_equal(table_unif_x(sp_data), table_x(sp_data))
})

test_that("no observations in middle bins", {
  sp_data <- rep(c(-5,-2,5,7,10), 10)
  table_unif_x <- function(x){
    temp <- unif(x, unit = 1, rd = T)
    table(cut(temp,breaks = c(-5:11),right = F))
  }
  table_x <- function(x) table(cut(x,breaks = c(-5:11),right = F))

  expect_equal(table_unif_x(sp_data), table_x(sp_data))
})

test_that("multiple values in one bin", {
  sp_data <- rep(c(1, 1.2, 1.7, 2, 2.2, 2.5, 3), 10)
  table_unif_x <- function(x){
    temp <- unif(x, unit = 1, rd = T)
    table(cut(temp,breaks = c(1:4),right = F))
  }
  table_x <- function(x) table(cut(x,breaks = c(1:4),right = F))

  expect_equal(table_unif_x(sp_data), table_x(sp_data))
})
