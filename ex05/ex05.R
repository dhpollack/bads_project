source('helperFunctions.R')

loans = get.loan.dataset()

library(rpart)
#install.packages('rpart.plot')
#library(rpart.plot)

print('Brier Scores for Decision Trees (rpart)')

fit <- rpart(BAD ~ ., data=loans, method='class')
predictions <- ifelse(predict(fit, loans)[,2] > 0.5,1,0)
paste('default2:',brier.score(predictions,loans$BAD))

#rpart.plot(fit) # this doesn't work at the school
plot(fit)

dt.full = rpart(BAD ~ ., data=loans, method='class', control=rpart.control(minsplit=3,cp=0.0))
predictions.full <- ifelse(predict(dt.full, loans)[,2] > 0.5,1,0)
paste('full:',brier.score(predictions.full,loans$BAD))
plot(dt.full)

dt.prunedLess = rpart(BAD ~ ., data=loans, method='class', control=rpart.control(minsplit=4,cp=0.005))
dt.prunedMore = rpart(BAD ~ ., data=loans, method='class', control=rpart.control(minsplit=8,cp=0.02))

predictions.prunedLess <- ifelse(predict(dt.prunedLess, loans)[,2] > 0.5,1,0)
paste('prunedLess:',brier.score(predictions.prunedLess,loans$BAD))
plot(dt.prunedLess)

predictions.prunedMore <- ifelse(predict(dt.prunedMore, loans)[,2] > 0.5,1,0)
paste('prunedMore:',brier.score(predictions.prunedMore,loans$BAD))
plot(dt.prunedMore)
