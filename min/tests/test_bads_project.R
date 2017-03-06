if(!require("testthat")) install.packages("testthat"); library("testthat")
source("../helpFuncs.R")
test_that("Load Libraries", {
  reqLibs = c("caret", "klaR", "stringr")
  helpFuncs.installLibs(reqLibs)
})
test_that("Train Num Rows", {
  expect_equal(dim(helpFuncs.load.dataset("../data/train.csv", "../output/"))[1], 51884)
})
test_that("Test Num Rows", {
  expect_equal(dim(helpFuncs.load.dataset("../data/test.csv", "../output/"))[1], 12971)
})