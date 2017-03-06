# This funciton will help us to match up the datasets class and known so that 
# our algorithms do not break later on!

#rm(list = ls())

#setwd("C:/Users/PC/Desktop/BA_DS/code")
#source('helpFuncs_Tim.R')
#traintest <- helpFuncs.load.dataset(PredictSet = FALSE)
#handin <- helpFuncs.load.dataset(PredictSet = TRUE)

#colnames(traintest)

FactDiffLevRemove = function(traintest = NA, handin = NA){

  FactorVars_traintest = sapply(traintest[,-which(colnames(traintest) == "return_customer")], is.factor)
  FactorVars_handin = sapply(handin, is.factor)
  (length(FactorVars_traintest) == length(FactorVars_traintest)) 
  # check if class dataset has accidently one variable more
  
  
  traintest_factors <- traintest[,FactorVars_traintest]
  handin_factors <- handin[,FactorVars_handin]
  
  LevelIdenticalVec = vector(length = ncol(traintest_factors))
  
  i = 1
  while(i < (ncol(traintest_factors) + 1)){
    LevelContained_traintest = (levels(handin_factors[,i]) %in% levels(traintest_factors[,i]))
    NumLevel_traintest_notContained = (length(levels(handin_factors[,i])) - sum(LevelContained_traintest))
    LevelIdenticalVec[i] = (NumLevel_traintest_notContained != 0)
    i = i + 1
  }
  
  LevelIdenticalVec_Names = colnames(traintest_factors)[LevelIdenticalVec]
  return(LevelIdenticalVec_Names)
}



