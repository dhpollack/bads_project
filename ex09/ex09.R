source("../helpfuncs/helpfuncs.R")
loans = get.loan.dataset("../data/Loan_Data.csv")
require(caret)
require(randomForest)
require(foreach)

idx.train_test_ = createDataPartition(loans$BAD, p = 0.8, list = F)
train_test_ = loans[idx.train_test_, ]
valid_ = loans[-idx.train_test_, ]

model.control = trainControl(method = "cv", number = 5, 
                             classProbs = T,
                             returnData = F)

rf.params = expand.grid(mtry = seq(1, 7, 2))
model.rf = train(BAD ~ ., data=train_test_, 
                 method = "rf", 
                 tuneGrid = rf.params, 
                 trControl = model.control, 
                 metric = "Kappa", 
                 importance = T)

print(model.rf)

yhat = predict(model.rf, newdata = valid_)
table(yhat)
yhat_probs = predict(model.rf, newdata = valid_, type = "prob")
hist(yhat_probs[,1])

tot_correct = foreach(p = seq(1, 0.0, -0.01)) %do% {
  yhat_search = factor(ifelse(yhat_probs[,1] <= p, "GOOD", "BAD"), levels=levels(valid_$BAD))
  t = table(valid_$BAD, yhat_search)
  sum(ifelse(yhat_search == valid_$BAD,1,0))
}
plot(1:length(tot_correct), tot_correct)
abline(h=table(valid_$BAD)[1], col = "red")

varImp(model.rf, scale = F)
importance(model.rf$finalModel)
varImpPlot(model.rf$finalModel, type=2)
partialPlot(model.rf$finalModel, valid_[,1:8], x.var = "YOB")
