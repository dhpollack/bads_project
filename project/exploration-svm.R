source("helpFuncs.R")
library(e1071)

train = helpFuncs.load.dataset("data/train.csv")

features_to_use = c('return_customer', 'order_date','form_of_address','email_domain','newsletter','payment','delivery','postcode_invoice','coupon','goods_value','referrer','cost_shipping','weight','remitted_items','canceled_items','used_items','book_count','paperback_count','schoolbook_count','ebook_count','audiobook_count','audiobook_download_count','film_count','musical_count','imported_count','other_count','x_delivery_time','x_delivery_time_est_bin','x_created_account','x_advertising_code_bin')
train_red = train[sample(nrow(train), size = 5000), features_to_use]
# Start the clock!
ptm = proc.time()
model.svm = svm(return_customer ~ ., data = train_red) # cost = 10, gamma = 0.05
proc.time() - ptm
# 11.578s on cloud9 server

train_red = train[sample(nrow(train), size = 10000), features_to_use]
# Start the clock!
ptm = proc.time()
model.svm = svm(return_customer ~ ., data = train_red)
proc.time() - ptm
# 71.707s on cloud9 server

train_red = train[sample(nrow(train), size = 20000), features_to_use]
# Start the clock!
ptm = proc.time()
model.svm = svm(return_customer ~ ., data = train_red)
proc.time() - ptm
# 601.971s on cloud9 server


