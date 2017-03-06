if(!require("testthat")) install.packages("testthat"); library("testthat")

outputTestsPath = "output/tests/"
test_results <- test_dir("tests", reporter="summary")
write.csv(test_results, paste0(outputTestsPath, "test_results-", as.numeric(as.POSIXct(Sys.time())), ".csv"))