library(testthat)

context("check forward function")
source("/Users/mac/final-project/forward_function.R")

test_that("error thrown when input is number", {
  expect_error(selection_function(0, 0, 0))
})

test_that("error thrown when full data is missing", {
  expect_error(selection_function(respons_e, begin_data))
})

test_that("error thrown when respons_e is not vector", {
  expect_error((selection_function(data.frame("a" = 1, "b" = 2), data.frame(x), data.frame(y))))
})
