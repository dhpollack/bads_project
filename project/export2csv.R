#####################################################
#
# This code is inspired by Tim's export code
# - as in the original code, I've used write.csv2
#
#####################################################

source("helpFuncs.R")
reqLibs = c("caret", "klaR", "stringr")
helpFuncs.installLibs(reqLibs)
train = helpFuncs.load.dataset("data/train.csv")
test  = helpFuncs.load.dataset("data/test.csv")
write.csv2(train, "output/train_cleaned_woe.csv", row.names=FALSE)
write.csv2(test, "output/test_cleaned_woe.csv", row.names=FALSE)
