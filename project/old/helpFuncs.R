helpFuncs.clearEnv <- 
function() {
  # General preparation ----
  # === Clear the screen ===
  cat("\014")
  
  # === Clearing the Environment ===
  # remove variables
  # doesn't work within a function
  #rm(list = ls(all = TRUE))
  
  # reset graphics
  graphics.off()
  
  # R will display numbers exceeding 1000 via e^...
  options(scipen=999)
  
}

# Tim: I added 'x' in the function() and c() bracket bc we need to pass the 
# specific library list as an argument when calling this function
helpFuncs.installLibs <- 
function(x) {
  # === Packages ===
  # Install packages if not installed
  libraries = c(x)
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
  })
  
  # Load packages
  lapply(libraries, library, quietly = TRUE, character.only = TRUE)
  rm(libraries)
}

helpFuncs.load.dataset <- 
function(fps = "./data/train.csv", outputPath = "./output/", epochDate = "2013/1/1") {
  
  # USAGE
  # ========================
  # This function returns a dataframe. By default, we are loading
  # the training set.
  # Sample Usage:
  #source('helpFuncs.R')
  
  
  # Loading data and crude overvies ----
  # Read in the data
  df1 = read.csv(fps, header = TRUE, sep = ",", dec = ".")
  
  ##########
  # Dates
  ##########
  
  df1$order_date = as.Date(df1$order_date, format = "%Y/%m/%d")
  
  df1$account_creation_date = as.Date(df1$account_creation_date, format = "%Y/%m/%d")
  
  # changing the "0000/00/00" dates to order_date + 1
  # these are primarily electronic items and other_count
  default_gap = 1
  tmp_deliv_0_idx = which(df1$deliverydate_actual == "0000/00/00")
  df1$deliverydate_actual[tmp_deliv_0_idx] = NA
  df1$deliverydate_actual = as.Date(df1$deliverydate_actual, format = "%Y/%m/%d")
  df1$deliverydate_actual[tmp_deliv_0_idx] = df1$order_date[tmp_deliv_0_idx] + default_gap

  # substituting the year 4746 with 2013
  # replacing 2010/ with 2014/, because all purchases made on late in 2013
  df1$deliverydate_estimated = sapply(df1$deliverydate_estimated, function(x){gsub(x, pattern = '4746', replacement = '2013')})
  df1$deliverydate_estimated = sapply(df1$deliverydate_estimated, function(x){gsub(x, pattern = '2010/', replacement = '2014/')})
  df1$deliverydate_estimated = as.Date(df1$deliverydate_estimated, format = "%Y/%m/%d")
  
  # this is a relatively aggressive change.  It looks like certain actual delivery times have been 
  # keyed-in incorrectly.
  tmp_devord_diff = as.numeric(difftime(df1$deliverydate_estimated ,df1$order_date , units = c("days")))
  tmp_delivery_time = as.numeric(difftime(df1$deliverydate_actual, df1$order_date , units = c("days")))
  tmp_date_input_error_idx = which(tmp_delivery_time > 365 & tmp_devord_diff < 50)
  tmp_date_input_error = df1$deliverydate_actual[tmp_date_input_error_idx]
  # Changing the deliverydate_actual of those entries by -365 days...
  df1$deliverydate_actual[tmp_date_input_error_idx] = tmp_date_input_error - 365

  ##########
  # Factors
  ##########
  
  df1$title = factor(df1$title)
  
  df1$newsletter = factor(df1$newsletter)
  
  df1$model = factor(df1$model)
  
  df1$delivery = factor(df1$delivery)
  
  df1$coupon = factor(df1$coupon)
  
  df1$goods_value = factor(df1$goods_value)
  
  df1$giftwrapping = factor(df1$giftwrapping)
  
  df1$referrer = factor(df1$referrer)
  
  df1$cost_shipping = factor(df1$cost_shipping)
  
  if("return_customer" %in% colnames(df1)) {
    df1$return_customer = factor(df1$return_customer)
  }
  
  ##########
  # Misc
  ##########

  levels(df1$form_of_address) = c(levels(df1$form_of_address), "Other")
  df1$form_of_address[is.na(df1$form_of_address)] = "Other"
    
  # Changing the blank "" factor to an NA
  tmp_ad_levs = levels(df1$advertising_code)
  tmp_ad_levs[which(tmp_ad_levs=="")] = "Other"
  levels(df1$advertising_code) = tmp_ad_levs

  # clean up postal_delivery and postcode_invoice codes
  # combining 00 and 0, and combining the non-number codes
  tmp_pcode_levels = levels(df1$postcode_delivery)
  tmp_pcode_levels[which(tmp_pcode_levels=="")] = -1
  tmp_pcode_levels[which(tmp_pcode_levels=="00")] = tmp_pcode_levels[which(tmp_pcode_levels=="0")]
  tmp_pcode_levels[!grepl("[0-9]+",tmp_pcode_levels)] = -2
  levels(df1$postcode_delivery) = as.numeric(tmp_pcode_levels)
  
  # Making the postcode_delivery and postcode_invoice the same type
  df1$postcode_invoice = factor(df1$postcode_invoice)
  levels(df1$postcode_invoice)[!grepl("[0-9]+",levels(df1$postcode_invoice))] = -2
  levels(df1$postcode_invoice) = as.numeric(levels(df1$postcode_invoice))

  # use a simple linear regression with no intercept to fill in NA values in weight
  tmp_weight_regressors = c('book_count', 'paperback_count', 'schoolbook_count', 'film_count',
                            'musical_count', "hardware_count", 'imported_count', 'other_count')
  tmp_weight_test.idx = which(is.na(df1$weight))
  tmp_weight_test = df1[tmp_weight_test.idx,c(tmp_weight_regressors)]
  if("return_customer" %in% colnames(df1)) {
    tmp_weight_train = df1[-tmp_weight_test.idx,c("weight", tmp_weight_regressors)]
    tmp_model.weight.glm = glm(weight~0+., data=tmp_weight_train)
    save(tmp_model.weight.glm, file=paste0(outputPath, "weight_model.RData"))
    df1$weight[tmp_weight_test.idx] = as.integer(predict(tmp_model.weight.glm, tmp_weight_test))
  } else {
    load(paste0(outputPath, "weight_model.RData"))
    df1$weight[tmp_weight_test.idx] = as.integer(predict(tmp_model.weight.glm, tmp_weight_test))
  }

  ##########
  # New Vars
  ##########
  
  df1$x_timelapse_creat_order = difftime(df1$order_date, df1$account_creation_date, units = "days")
  df1$x_timelapse_creat_order = as.numeric(df1$x_timelapse_creat_order)
  df1$x_timelapse_creat_order = as.integer(df1$x_timelapse_creat_order)
  
  df1$x_item_multiple = (df1$item_count > 1)
  df1$x_item_multiple = factor(df1$x_item_multiple)
  levels(df1$x_item_multiple) = c(0,1)
  
  df1$x_deliverydate_gap = as.numeric(difftime(df1$deliverydate_actual, df1$deliverydate_estimated, units = c("days")))

  df1$x_delivery_time = as.numeric(difftime(df1$deliverydate_actual, df1$order_date, units = c("days")))

  df1$x_delivery_time_est = as.numeric(difftime(df1$deliverydate_estimated, df1$order_date, units = c("days")))
  
  df1$x_delivery_time_est_bin = cut(df1$x_delivery_time_est, c(-Inf, 0, 1, 7, 15, 35, 200, Inf), labels=FALSE) - 2
  
  df1$x_order_date_num = as.numeric(difftime(df1$order_date, as.Date(epochDate), units = c("days")))

  df1$x_created_account = ifelse(is.na(df1$account_creation_date), 0, 1)
  
  df1$x_remitted = (df1$remitted_items > 0)
  df1$x_remitted = factor(df1$x_remitted)
  levels(df1$x_remitted) = c(0,1)
  
  df1$x_remitted_all = (df1$remitted_items == df1$item_count)
  df1$x_remitted_all = factor(df1$x_remitted_all)
  levels(df1$x_remitted_all) = c(0,1)
  
  df1$x_canceled = (df1$canceled_items > 0)
  df1$x_canceled = factor(df1$x_canceled)
  levels(df1$x_canceled) = c(0,1)
  
  df1$x_tot_canceled_remitted = (df1$remitted_items + df1$canceled_items)
  
  df1$x_buyer_human = (df1$form_of_address == "Mr" | df1$form_of_address == "Mrs")
  df1$x_buyer_human = factor(df1$x_buyer_human)
  levels(df1$x_buyer_human) = c(0,1)
  
  #### Bin Item Counts
  # create a vector of item count column names
  count_cols = c("book_count", "paperback_count", "schoolbook_count", "ebook_count", "audiobook_count", "audiobook_download_count", "film_count", "musical_count", "hardware_count")
  # get column indices
  item_count_cols_idx = which(colnames(df1) %in% count_cols)
  # set a temporary variable for the bounds
  bounds = helpFuncs.get.bounds(df1, item_count_cols_idx, paste0(outputPath, "bounds.csv"))
  # create an offset in the bounds to 
  eta = 0.0001
  # use the cut function on each column to bin them
  for(item_count_col_idx in item_count_cols_idx) {
    x_col_name = paste0("x_", colnames(df1)[item_count_col_idx], "_bin")
    hi_bound = bounds[1, colnames(df1)[item_count_col_idx]]
    bins = cut(df1[, item_count_col_idx], c(0, 1, hi_bound+eta, Inf), labels = F, right = FALSE) - 1
    bins[is.na(bins)] = -1
    df1[,x_col_name] = bins
  }
  
  # Section by Tim: Binning order dates by week
  #any(is.na(df1$order_date)) # my procedure below only works IF there are no missing values
  # Note that, if we have 'weird values' in the order date variable, we have to deal with them
  # before generating this variable. Else, data errors propagate
  order_Date_YM_df1 <- strftime(df1$order_date, format = "%Y-%V")
  order_Date_YM_df1 <- factor(order_Date_YM_df1)
  df1$x_order_date_yearweek <- order_Date_YM_df1

  # Generating the Christmas Dummy
  # Note that it might be 'colinear' with the binning by weeks dummy, depending on
  # the model we eventually pick.
  # Beginning and end date subject to discussion in our group ;)
  order.christmas.begin <- as.Date("2013-11-17", "%Y-%m-%d")
  order.christmas.end <- as.Date("2013-12-24", "%Y-%m-%d")
  christmas.order <- (df1$order_date > order.christmas.begin & df1$order_date < order.christmas.end)
  # Did R get it right? 
  christmas.order <- factor(christmas.order)
  levels(christmas.order) = c(0,1)
  df1$x_christmas.order <- christmas.order

  df1$x_advertising_code_bin = df1$advertising_code
  levels(df1$x_advertising_code_bin) = gsub(levels(df1$x_advertising_code_bin), pattern = "A.", replacement = "A")
  levels(df1$x_advertising_code_bin) = gsub(levels(df1$x_advertising_code_bin), pattern = "B.", replacement = "B")
  levels(df1$x_advertising_code_bin) = gsub(levels(df1$x_advertising_code_bin), pattern = "C.", replacement = "C")

  # Recalculate Item Count
  count_cols_minus = c("remitted_items")
  df1$x_item_count = apply(df1[, c(count_cols, count_cols_minus)],
                           1,
                           function(x) {
                             sum(x[1:length(count_cols)]) - sum(x[(length(count_cols)+1):length(x)])
                           })
    
  ##########
  # Purge
  ##########
  
  # although there are not a lot of items, I think we should keep this column
  #df1$hardware_count = NULL
  
  df1$points_redeemed = NULL
  
  return(df1)
  
}

helpFuncs.load <- 
function(fps = c("./data/train.csv", "./data/test.csv")) {
  # items can be accessed as l[1], l[2]
  train = helpFuncs.load.dataset(fps[1])
  test = helpFuncs.load.dataset(fps[2])
  return(list(train,test))
}

helpFuncs.transform.to.numeric <- 
function(df1) {
  # http://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r
  is.date <- function(x) {inherits(x, 'Date')}
  factorCols = sapply(df1, is.factor)
  dateCols = sapply(df1, is.date)
  df1_numeric = df1
  df1_numeric[,factorCols] = sapply(df1[,factorCols], as.numeric)
  min_date = min(df1[,dateCols][,1])
  df1_numeric[,dateCols] = sapply(df1[,dateCols], function(x){difftime(x, min_date, units=c("days"))})
  return(df1_numeric)
}

helpFuncs.get.bounds <- 
function(df1, to_bin_cols_idx, boundsPath = "output/bounds.csv") {
  if("return_customer" %in% colnames(df1)) {
    bounds = list()
    for(to_bin_col_idx in to_bin_cols_idx) {
      bounds[colnames(df1)[to_bin_col_idx]] = summary(df1[which(df1[, to_bin_col_idx] != 0), to_bin_col_idx])[5]
    }
    bounds = as.data.frame(bounds)
    rownames(bounds) = NULL
    write.csv2(bounds, boundsPath, row.names=FALSE)
  } else {
    bounds = read.csv2(boundsPath)
  }
  return(bounds)  
}

