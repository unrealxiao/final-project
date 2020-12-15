library(testthat)

context("check forward function")
source("forward_function.R")

test_that("error thrown when input is number", {
  expect_error(selection_function(0, 0, 0))
})
