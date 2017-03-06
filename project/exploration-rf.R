library(randomForest)

source("helpFuncs.R")

train = helpFuncs.load.dataset("data/train.csv")

#features_to_use = c('return_customer', 'order_date','form_of_address','email_domain','newsletter','payment','delivery','postcode_invoice','coupon','goods_value','referrer','cost_shipping','weight','remitted_items','canceled_items','used_items','book_count','paperback_count','schoolbook_count','ebook_count','audiobook_count','audiobook_download_count','film_count','musical_count','imported_count','other_count','x_delivery_time','x_delivery_time_est_bin','x_created_account','x_advertising_code_bin')
#Error in randomForest.default(m, y, ...) : 
#  Can not handle categorical predictors with more than 53 categories.
features_to_use = c('return_customer', 'x_order_date_num','form_of_address','email_domain','newsletter','payment','delivery','coupon','goods_value','referrer','cost_shipping','weight','remitted_items','canceled_items','used_items','book_count','paperback_count','schoolbook_count','ebook_count','audiobook_count','audiobook_download_count','film_count','musical_count','imported_count','other_count','x_delivery_time','x_delivery_time_est_bin','x_created_account','x_advertising_code_bin')

# train with 5000 samples
train_red_idx = sample(nrow(train), size = 5000)
train_red = train[train_red_idx, features_to_use]
# Start the clock!
ptm = proc.time()
model.rf = randomForest(return_customer ~ ., data = train_red)
proc.time() - ptm
# 23.309s on laptop

# train with 10000 samples
train_red_idx = sample(nrow(train), size = 10000)
train_red = train[train_red_idx, features_to_use]
# Start the clock!
ptm = proc.time()
model.rf = randomForest(return_customer ~ ., data = train_red)
proc.time() - ptm
# 66.918s on laptop

# train with 20000 samples
train_red_idx = sample(nrow(train), size = 20000)
train_red = train[train_red_idx, features_to_use]
# Start the clock!
ptm = proc.time()
model.rf = randomForest(return_customer ~ ., data = train_red)
proc.time() - ptm
# 181.745s on laptop

# train with full training set
train_full = train[, features_to_use]
# Start the clock!
ptm = proc.time()
model.rf = randomForest(return_customer ~ ., data = train_full)
proc.time() - ptm
# 509.356s on laptop

test = helpFuncs.load.dataset("data/test.csv")
test = test[,features_to_use[-1]]

yhat_rf = predict(model.rf, test)
table(yhat_rf)
#     0     1 
# 12833   138