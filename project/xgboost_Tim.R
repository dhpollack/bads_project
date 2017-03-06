dev.off()
rm(list = ls())

setwd("C:/Users/PC/Dropbox/BA_DS/code") # WATCH OUT: SET YA WORKING DIRECTORY HERE ;)
libraries <- c("caret", "randomForest", "xgboost", "caretEnsemble", 
                  "hmeasure", "doParallel", "doParallel", "parallel", 
               "microbenchmark", "plyr", "lattice", "ggplot2", "ModelMetrics", "klaR")

# This command applies the following function to the list:
# install the packages mentioned in the vector "libraries" 
# except if they have already been installed
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# might be needed: "foreach", "lattice", "nnet", "devtools", "reshape"

source('helpFuncs_Tim.R')

traintest = helpFuncs.load.dataset(handin = FALSE)
handin = helpFuncs.load.dataset(handin = TRUE)

# We agreed on removing the advertising_code because the variable is already binned
traintest$advertising_code = NULL
handin$advertising_code = NULL

# We could remove buyer human because that is derived from d in form of adress
traintest$x_buyer_human = NULL
handin$x_buyer_human = NULL

# I am also fine with dropping postcode delivery because it messes up my predictions
# plus: there are so few customer per postcode bin, that applying WOE seems pointless
traintest$postcode_delivery = NULL
handin$postcode_delivery = NULL

traintest_copy = traintest
handin_copy = handin

#source('WoEGenerator_Tim.R')
# I failed to progam that in a function which we could 'outsource' to another RScript or the
# helperfunctions - remaining task

levels(handin$x_order_date_yearweek)[41:length(levels(handin$x_order_date_yearweek))] <- "2014"
levels(traintest$x_order_date_yearweek)[41:length(levels(traintest$x_order_date_yearweek))] <- "2014"

# Draw a random, stratified sample including p percent of the data
set.seed(123)
idx.test.woe <- createDataPartition(y = traintest$return_customer, p = 0.6, list = FALSE) 
train.woe <- traintest[idx.test.woe,]
test.woe <- traintest[-idx.test.woe,]

# prepare the zero-adjustment factor corresponding to ln(odds-ratio) in the train sample
adjust = (sum(traintest$return_customer == "yes")/nrow(traintest))/(sum(traintest$return_customer == "no")/nrow(traintest))


# set up the woe object
woe.object <- woe(return_customer ~ x_order_date_yearweek + postcode_invoice + 
                    email_domain + payment + x_order_Date_weekday, data = train.woe, zeroadj = adjust)

# chk x_order_date_yearweek
length(woe.object$xnew$woe.x_order_date_yearweek) == length(train.woe$x_order_date_yearweek)
sum(is.na(woe.object$xnew$woe.x_order_date_yearweek))
train.woe$woe.x_order_date_yearweek = woe.object$xnew$woe.x_order_date_yearweek
train.woe$x_order_date_yearweek = NULL
# chk postcode_invoice
length(woe.object$xnew$woe.postcode_invoice) == length(train.woe$postcode_invoice)
sum(is.na(woe.object$xnew$woe.postcode_invoice))
woe.object$xnew$woe.postcode_invoice[is.na(woe.object$xnew$woe.postcode_invoice)] = adjust
sum(is.na(woe.object$xnew$woe.postcode_invoice))
train.woe$woe.postcode_invoice = woe.object$xnew$woe.postcode_invoice
train.woe$postcode_invoice = NULL
# chk email_domain

train.woe$woe.email_domain = woe.object$xnew$woe.email_domain
train.woe$email_domain = NULL

length(woe.object$xnew$woe.payment) == length(train.woe$payment)
sum(is.na(woe.object$xnew$woe.payment)) # no missings ok!
train.woe$woe.payment = woe.object$xnew$woe.payment
train.woe$payment = NULL

length(woe.object$xnew$woe.x_order_Date_weekday) == length(train.woe$x_order_Date_weekday)
sum(is.na(woe.object$xnew$woe.x_order_Date_weekday)) # no missings ok!
train.woe$woe.x_order_Date_weekday = woe.object$xnew$woe.x_order_Date_weekday
train.woe$x_order_Date_weekday = NULL

# replace original variable in test.woe with woe numbers - impute missings if required
test.woe = predict(woe.object, newdata = test.woe, replace = TRUE)
sum(is.na(test.woe$woe.postcode_invoice))
test.woe$woe.postcode_invoice[is.na(test.woe$woe.postcode_invoice)] = adjust
sum(is.na(test.woe$woe.postcode_invoice))

sum(is.na(test.woe$woe.x_order_date_yearweek)) # no missings, nothing to do here!
sum(is.na(test.woe$woe.email_domain)) # no missings, nothing to do here!
sum(is.na(test.woe$woe.payment)) # no missings, nothing to do here!
sum(is.na(test.woe$woe.x_order_Date_weekday)) # no missings, nothing to do here!

# merge train.woe and test.woe
test.woe = test.woe[,colnames(train.woe)]
traintest2 <- rbind(train.woe, test.woe)
traintest = traintest2
rm(traintest2)

# insert woe into handin data
handin2 = predict(woe.object, newdata = handin, replace = TRUE)
nrow(handin) == nrow(handin2) # ok
ncol(handin) == ncol(handin2) # ok
# check for NAs generated
# postcode_invoice
sum(is.na(handin2$woe.postcode_invoice)) # ok
# woe.x_order_date_yearweek
sum(is.na(handin2$woe.x_order_date_yearweek)) # ok!
sum(is.na(handin2$woe.email_domain)) # ok!
sum(is.na(handin2$woe.payment)) # ok!
sum(is.na(handin2$woe.x_order_Date_weekday)) # ok!

# so remove handin2
handin = handin2
rm(handin2)
rm(list = c('test.woe', 'train.woe', 'idx.test.woe'))

# Since I found that NA values are still a problem
na.check <- sapply(traintest, FUN = anyNA)
sum(na.check) # we have two variables still featuring missing values
#ColNA = which(na.check == TRUE) # store the column number of the vars with mssings
#colnames(traintest)[ColNA]

# Problem before applying WOE: Factor variables among train, test and handin did not match for some variables
# I detected that issue running DiffLevels_cope.R 
# Problem vanished when I applied WOE to postcode_invoice and x_oder_date_yearweek
# note that I excluded some of the variables in the beginning of this script which were also 
# 'problematic' wrt their (supposed) factor levels

#source('DiffLevels_Cope.R')
#FactVarsRemove_Names <- FactDiffLevRemove(traintest = traintest, handin = handin)
#print(FactVarsRemove_Names)
#traintest = traintest[,-which(colnames(traintest) %in% FactVarsRemove_Names)]
#handin = handin[,-which(colnames(handin) %in% FactVarsRemove_Names)]

# These factors have the same levels, but still the model 
# produced hardly any predictions for the test and the handin data
# Note that this problem vanished as well when I set up Weight of Evidence
#excludeList <- c("order_date", "deliverydate_estimated", "deliverydate_actual", "postcode_delivery")
# I decided to remove postcode delivery bc there are few obs for each postcode present ~30
# most obs show a missing here

# Beware of excluding the "ID" variable later on!
#traintest <- traintest[,-which(colnames(traintest) %in% excludeList)]
#handin <- handin[,-which(colnames(handin) %in% excludeList)]

# remove NA columns
#traintest = traintest[,-ColNA]



# Set up a stratified sample
idx.train <- createDataPartition(y = traintest$return_customer, p = 0.8, list = FALSE)
train <- traintest[idx.train, ]
test <-  traintest[-idx.train, ]


# Set the model.control, i.e. the cross-validation parameters
k <- 3 # No folds
model.control <- trainControl(
  method = "cv", # 'cv' for cross validation
  number = k, # number of folds in cross validation
  classProbs = TRUE, # Return class probabilities
  summaryFunction = twoClassSummary, # twoClassSummary returns AUC
  allowParallel = FALSE # Enable parallelization if available
)

# set the xtboost tune parameters to select from 
xTremeBoost.parms <- expand.grid(
  eta = c(0.06,0.12), # play with this
  max_depth = 5, # play with that, typically between 1 and 5
  nrounds = c(10),
  gamma = 0,               #default=0
  colsample_bytree = 1,    #default=1; setting differently alows for randomness in colums selection
  min_child_weight = 1,     #default=1; might be helpful to avoid overfitting
  subsample = 0.8 #,  # default = 1; helps to avoid overfitting
  #weight = (as.numeric(traintest$return_customer)-1)/nrow(traintest)
)

# early.stop.round = 6

# http://stackoverflow.com/questions/38250440/error-in-na-fail-default-missing-values-in-object-but-no-missing-values
# Train the XGB


# set -ID to exclude the ID column from the train model, but keep it in the dataframe ;)

TrainXGB <- train(return_customer~. -ID, data = train,  
                  method = "xgbTree",
                  tuneGrid = xTremeBoost.parms, 
                  metric = "ROC", trControl = model.control, 
                  na.action = na.pass) 


#, http://stats.stackexchange.com/questions/144922/r-caret-and-nas/195067
                  #early_stopping_rounds = Stop2)

# Extract the predictions
xgb.pred <- predict(TrainXGB, newdata = test, type = "prob")[,2]
length(xgb.pred) == nrow(test)

# Estimate performance on unseen data based on test set
auc(test$return_customer, xgb.pred)
# fit not yet satisfactory, but that can be improved by fine-tuning

# Retrieve information on variable importance
VarImportance <- varImp(TrainXGB)
VarImportanceFrame = VarImportance$importance
VarImportanceFrame$Varname = rownames(VarImportanceFrame)
rownames(VarImportanceFrame) = NULL
VarImportanceFrame = VarImportanceFrame[,c(2,1)]
VarImportanceFrame # all varImportance ordered
VarImportanceFrame[1:20,]

#handin <- helpFuncs.load.dataset(PredictSet = TRUE)

# excludeList <- c("ID", "order_date", "deliverydate_estimated", "deliverydate_actual", "postcode_delivery", "postcode_invoice")
#handin <- handin[,-which(colnames(handin) %in% excludeList)]

#na.check <- sapply(handin, FUN = anyNA)
#sum(na.check)
#ColNA = which(na.check == TRUE)

# remove NA columns
#handin = handin[,-ColNA]

PredClassData <- predict(TrainXGB, newdata = handin, type = "prob")[,2]
length(PredClassData)
nrow(handin)

length(PredClassData) == nrow(handin)

