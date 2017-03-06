# Implement WOE on yearweek

WoEvidence.Generator(handin = NA, traintest = NA){
  # lump all orders in 2014 together because there are only a few per week
  levels(handin$x_order_date_yearweek)[41:length(levels(handin$x_order_date_yearweek))] <- "2014"
  levels(traintest$x_order_date_yearweek)[41:length(levels(traintest$x_order_date_yearweek))] <- "2014"
  
  # Draw a random, stratified sample including p percent of the data
  set.seed(123)
  idx.test.woe <- createDataPartition(y = traintest$return_customer, p = 0.6, list = FALSE) 
  train.woe <- traintest[idx.test.woe,]
  test.woe <- traintest[-idx.test.woe,]
  
  # prepare the zero-adjustment factor corresponding to ln(odds-ratio) in the train sample
  adjust = (sum(traintest$return_customer == "yes")/nrow(traintest))/(sum(traintest$return_customer == "no")/nrow(traintest))
  adjust = log(adjust)
  
  # set up the woe object
  woe.object <- woe(return_customer ~ x_order_date_yearweek + postcode_invoice, data = train.woe, zeroadj = adjust)

  # replace original variable in train.woe with woe numbers
  train.woe$woe.x_order_date_yearweek = woe.object$xnew
  train.woe$x_order_date_yearweek = NULL
  
  # replace original variable in test.woe with woe numbers
  test.woe = predict(woe.object, newdata = test.woe, replace = TRUE)
  
  # merge train.woe and test.woe
  traintest2 = rbind(train.woe, test.woe)

}

# Call predictions for test data via new function!!!
# Or via an elseif condition

# handin.woe = predict(woe.object, newdata = handin, replace = TRUE)