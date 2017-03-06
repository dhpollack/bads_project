setwd("C:/Users/PC/Dropbox/BA_DS/code")
source('helpFuncs.R')

helpFuncs.installLibs(c("caret", "randomForest", "caretEnsemble", 
                        "hmeasure", "doParallel", "parallel", 
                        "microbenchmark", "plyr", "lattice", "ggplot2", "ModelMetrics", 
                        "klaR", "stringr", "assertthat"))


load = helpFuncs.load()
train = as.data.frame(load[1])
test = as.data.frame(load[2])

write.csv(train, file = "./output/train_full.csv")
write.csv(test, file = "./output/test_full.csv")

save(train, file = "./output/train_full.RData")
save(test, file = "./output/test_full.RData")


# removing variables for second datset asked for
Varlist_WoE <- c("email_domain","payment", "x_order_date_yearweek", "postcode_invoice", "model")

train1 = train[,-(which(colnames(train) %in% Varlist_WoE))]
test1 = test[,-(which(colnames(train) %in% Varlist_WoE))]

write.csv(train1, file = "./output/train_norm_bins_woe.csv")
write.csv(test1, file = "./output/test_norm_bins_woe.csv")

save(train1, file = "./output/train_norm_bins_woe.RData")
save(test1, file = "./output/test_norm_bins_woe.RData")

rm(train1, test1)

# Generating the first (more basic) dataset david asked for
RemWoE <- c("x_woe_email_domain", "x_woe_payment", "x_woe_x_order_date_yearweek", "x_woe_postcode_invoice", "x_woe_model")
train1 = train[,-(which(colnames(train) %in% RemWoE))]
test1 = test[,-(which(colnames(train) %in% RemWoE))]

write.csv(train1, file = "./output/train_norm_bins.csv")
write.csv(test1, file = "./output/test_norm_bins.csv")

save(train1, file = "./output/train_norm_bins.RData")
save(test1, file = "./output/test_norm_bins.RData")

rm(train1, test1)

# Tim's 'diet dataset'
# (1) used binned counts and delete original ones
count_cols = c("book_count", "paperback_count", "schoolbook_count", "ebook_count", "audiobook_count", "audiobook_download_count", 
               "film_count", "musical_count", "hardware_count", "used_items", "imported_count", "other_count")
# (2) Resort to WoE versions of variables and delete original ones
Varlist_WoE <- c("email_domain","payment", "x_order_date_yearweek", "postcode_invoice", "model")
# (3) Delete the numeric deliverydates
Delivery_remove = c("deliverydate_actual_num", "x_deliverydate_estimated_num")
# (4) Other variables I would remove
Other_remove = c("postcode_delivery", "advertising_code", "item_count", 
                 "title", "giftwrapping", "x_item_multiple", "x_delivery_time_est_bin", 
                 "x_remitted", "x_canceled", "x_christmas.order")

Tot_remove = c(count_cols, Varlist_WoE, Delivery_remove, Other_remove)

train1 = train[,-(which(colnames(train) %in% Tot_remove))]
test1 = test[,-(which(colnames(train) %in% Tot_remove ))]

write.csv(train1, file = "./output/train_Tim_Diet.csv")
write.csv(test1, file = "./output/test_Tim_Diet.csv")

save(train1, file = "./output/train_Tim_Diet.RData")
save(test1, file = "./output/test_Tim_Diet.RData")

rm(train1, test1)

# Generate a dataset only containing the original yet formatted variables
train1 = train[,1:31]
test1 = test[,1:30]

write.csv(train1, file = "./output/train_original_Vars.csv")
write.csv(test1, file = "./output/test_original_Vars.csv")

save(train1, file = "./output/train_original_Vars.RData")
save(test1, file = "./output/test_original_Vars.RData")