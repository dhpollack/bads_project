if(!require("testthat")) install.packages("testthat"); library("testthat")
source("helpfuncs/helpFuncs_staging.R")
test_that("Train Num Rows", {
  expect_equal(dim(helpFuncs.load.dataset("data/train.csv"))[1], 51884)
})
test_that("Test Num Rows", {
  expect_equal(dim(helpFuncs.load.dataset("data/test.csv"))[1], 12971)
})