if(!require("testthat")) install.packages("testthat"); library("testthat")
source("helpfuncs/helpFuncs_staging.R")
test_results <- test_dir("tests", reporter="summary")
test_file()
