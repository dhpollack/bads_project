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

  #noaddVars = NA, binCounts = NA, rmCountsOrigin = NA, genWoE = NA, 
  #rmWoEOrigin = NA, datesNum = NA
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
  
  # Format order data as date
  df1$order_date = as.Date(df1$order_date, format = "%Y/%m/%d")
  
  # Format account creation date as date
  df1$account_creation_date = as.Date(df1$account_creation_date, format = "%Y/%m/%d")
  # Impute the missing values via the order_date value
  creationNA = which(is.na(df1$account_creation_date))
  df1$account_creation_date[creationNA] = df1$order_date[creationNA]
  
  # Format deliverydate actual
  # Yet before that, clean up wrongly entered data
  default_gap = 1 # changing the "0000/00/00" dates to order_date + 1
  # Why? These mistakes are primarily electronic items and other_count
  tmp_deliv_0_idx = which(df1$deliverydate_actual == "0000/00/00") # Identify the observations with the mistakes
  df1$deliverydate_actual[tmp_deliv_0_idx] = NA # As a first pass, set the deliverydate_actual value missing
  df1$deliverydate_actual = as.Date(df1$deliverydate_actual, format = "%Y/%m/%d") # Format the variable as date
  df1$deliverydate_actual[tmp_deliv_0_idx] = df1$order_date[tmp_deliv_0_idx] + default_gap # Replace the missing values by order_date + 1 day
  df1$deliverydate_actual = as.Date(df1$deliverydate_actual, format = "%Y/%m/%d")
  
    # Format deliverydate estimated
  # Yet the dates were keyed in incorreclty in 2 ways
  # (1) replace year 4746 with 2013 --> most orders for these observations take place in 2013
  df1$deliverydate_estimated = sapply(df1$deliverydate_estimated, function(x){gsub(x, pattern = '4746', replacement = '2013')})
  # (2) replacing 2010/ with 2014/, because all purchases made in late in 2013
  df1$deliverydate_estimated = sapply(df1$deliverydate_estimated, function(x){gsub(x, pattern = '2010/', replacement = '2014/')})
  df1$deliverydate_estimated = as.Date(df1$deliverydate_estimated, format = "%Y/%m/%d") # output as date
  
  # (3) We try to det rid of entirely implausible deiverydate_estimaded values and deliverydate_actual values
  # This is a relatively aggressive change.  It looks like certain actual delivery times have been 
  # keyed-in incorrectly.
  # Compute the day difference between deliverydate_estimated and order_date
  tmp_devord_diff = as.numeric(difftime(df1$deliverydate_estimated ,df1$order_date , units = c("days")))
  # Compute the day difference between deliverydate_actual and order_date
  tmp_delivery_time = as.numeric(difftime(df1$deliverydate_actual, df1$order_date , units = c("days")))
  # Identify observationsregarding which something was really wrong
    # delivery time exceeded one year AND the explected delivery time was less than 50 days
  tmp_date_input_error_idx = which(tmp_delivery_time > 365 & tmp_devord_diff < 50)
  # retrieve all incorrectly specified deliverydate actual
  tmp_date_input_error = df1$deliverydate_actual[tmp_date_input_error_idx]
  # Changing the deliverydate_actual of those entries by -365 days...
  df1$deliverydate_actual[tmp_date_input_error_idx] = tmp_date_input_error - 365

  ##########
  # Factors
  ##########
  
  # Variables which have to be converted to factor
  # i.e. since we use Python for further analysis, leaving the factor levels as numbers ins not problematic
  # We are aware of that we would have to change that proceeding with R
  factorVars = c("title", "newsletter", "model", "delivery", "coupon", 
                 "goods_value", "giftwrapping", "referrer", "cost_shipping", 
                 "postcode_delivery", "postcode_invoice")
  
  if("return_customer" %in% colnames(df1)) {
    factorVars = c(factorVars, "return_customer")
  }
  
  df1[,factorVars] = as.data.frame(sapply(df1[,factorVars], factor))
  
  ##########
  # Misc
  ##########
  # Form_of_address
  # Correctly read in as a factor
  # Add the factor level other and assign all values featurung a missing value to that category
  levels(df1$form_of_address) = c(levels(df1$form_of_address), "Other")
  df1$form_of_address[is.na(df1$form_of_address)] = "Other"
    
  # Advertising_code
  # Correctly read in as factor. Yet the variable needs some cleaning
  # Changing the blank "" factor to an category 'other'
  levels(df1$advertising_code) = c(levels(df1$advertising_code), "Other")
  df1$advertising_code[which(df1$advertising_code=="")] = "Other"
 
  # Postcode_delivery
  # Correctly read in as factor by now. Yet about 95% of the observations in both datasets are left blankl
  # Convert the variable into a dummy stating whether anything was specified. 
  # Step 1: Make sure that blanks have been converted into NAs in both datasets
  if("return_customer" %in% colnames(df1)){
    blanks = which(df1$postcode_delivery == "")
    df1$postcode_delivery[blanks] = NA
    # F�llt weg, da schon zuvor in Faktor umgewandelt
  }else if(("return_customer" %in% colnames(df1)) == FALSE){
    df1$postcode_delivery = factor(df1$postcode_delivery)
  }
  # "argumentum e contrario" of is.na() is that a postcode_delivery has been specified
  df1$postcode_delivery = !(is.na(df1$postcode_delivery))
  df1$postcode_delivery = factor(df1$postcode_delivery)
  levels(df1$postcode_delivery) = c(0,1)

  
  # Postcode_invoice
  # in the test data, we observe a few non-numeric postcode_invoice levels
  # assign those randomly to the other levels
  levels(df1$postcode_invoice)[which(levels(df1$postcode_invoice)=="0")] = floor(runif(1, min = 1, max = 100))
  levels(df1$postcode_invoice)[!grepl("[0-9]+",levels(df1$postcode_invoice))] = floor(runif(1, min = 1, max = 100))
  levels(df1$postcode_invoice) = as.numeric(levels(df1$postcode_invoice))
  # We still have few postcode_invoice levels featuring a zero in the test data
  # Randomly assign these oservations to other levels
  #levels(df1$postcode_invoice)[levels(df1$postcode_invoice) == -2] = sample(levels(df1$postcode_invoice)[which(levels(df1$postcode_invoice) != -2)],1)
  # Weight
  # Variable features missing values
  # Since the weight of the order is determined by the number and type of the items ordered, 
  # we use a simple linear regression with no intercept to fill in NA values
  tmp_weight_regressors = c('book_count', 'paperback_count', 'schoolbook_count', 'film_count',
                            'musical_count', "hardware_count", 'imported_count', 'other_count')
  tmp_weight_test.idx = which(is.na(df1$weight)) # identify observations with weight value missing
  # store observations with missing weight values apart in an auxiliary data frame
  tmp_weight_test = df1[tmp_weight_test.idx,c(tmp_weight_regressors)] 
  # If we deal with the train data, set up the regression model
  if("return_customer" %in% colnames(df1)) {
    # store observations with weight values apart in an auxiliary data frame to train the OLS regression
    tmp_weight_train = df1[-tmp_weight_test.idx,c("weight", tmp_weight_regressors)] 
    tmp_model.weight.glm = glm(weight~0+., data=tmp_weight_train) # Train the OLS regression
    # Store the OLS regression as we use it when imputing missings in the test dataset
    save(tmp_model.weight.glm, file=paste0(outputPath, "weight_model.RData")) 
    # Substitute the missing weight values in the train data by the predictions of the OLS model
    df1$weight[tmp_weight_test.idx] = as.integer(predict(tmp_model.weight.glm, tmp_weight_test))
  } else {
    load(paste0(outputPath, "weight_model.RData")) # load the OLS regression model to predict the missing weight values
    df1$weight[tmp_weight_test.idx] = as.integer(predict(tmp_model.weight.glm, tmp_weight_test)) # replace the missing values by the predictions
  }
  
  
  ##########
  # New Vars
  ##########
  
  # Date differences
  
  # Timlapse between account creation and order
  df1$x_timelapse_creat_order = as.numeric(difftime(df1$order_date, df1$account_creation_date, units = "days"))
  # Time difference betreen estimated and actual delivery date
  df1$x_deliverydate_gap = as.numeric(difftime(df1$deliverydate_actual, df1$deliverydate_estimated, units = c("days")))
  # Actual delivery time in days
  df1$x_delivery_time = as.numeric(difftime(df1$deliverydate_actual, df1$order_date, units = c("days")))
  # Estimated delivery time
  df1$x_delivery_time_est = as.numeric(difftime(df1$deliverydate_estimated, df1$order_date, units = c("days")))
  # binned version of estimated delivery time
  df1$x_delivery_time_est_bin = cut(df1$x_delivery_time_est, c(-Inf, 0, 1, 7, 15, 35, 200, Inf), labels=FALSE) - 2

  # In fact, this variable is of no use as we imputed the account creation date before
  # df1$x_created_account = ifelse(is.na(df1$account_creation_date), 0, 1)
  
  
  # Dummies derived from count variables
  
  # Recalculate Item Count
  count_cols = c("book_count", "paperback_count", "schoolbook_count", "ebook_count", "audiobook_count", "audiobook_download_count", 
                 "film_count", "musical_count", "hardware_count", "used_items", "imported_count", "other_count")
  count_cols_minus = c("remitted_items")
  df1$x_item_count = apply(df1[, c(count_cols, count_cols_minus)],
                           1,
                           function(x){
                             sum(x[1:length(count_cols)]) - sum(x[(length(count_cols)+1):length(x)])
                           })
  
  df1$x_item_multiple = factor(df1$item_count > 1) # dummy multiple items ordered
  levels(df1$x_item_multiple) = c(0,1)
  
  df1$x_remitted = factor(df1$remitted_items > 0) # dummy any items remitted
  levels(df1$x_remitted) = c(0,1)
  
  df1$x_remitted_all = factor(df1$remitted_items == df1$item_count) # dummy all items remitted
  levels(df1$x_remitted_all) = c(0,1)
  
  df1$x_canceled = factor(df1$canceled_items > 0) # dummy any items canceled
  levels(df1$x_canceled) = c(0,1)
  
  df1$x_tot_canceled_remitted = (df1$remitted_items + df1$canceled_items) # total number of canceled and remitted items
  
  # Identify human buyers
  df1$x_buyer_human = factor(df1$form_of_address == "Mr" | df1$form_of_address == "Mrs")
  levels(df1$x_buyer_human) = c(0,1)
  
  # Binning order dates by week
  order_Date_YM_df1 <- strftime(df1$order_date, format = "%Y-%V")
  order_Date_YM_df1 <- factor(order_Date_YM_df1)
  df1$x_order_date_yearweek <- order_Date_YM_df1
  levels(df1$x_order_date_yearweek)[41:length(levels(df1$x_order_date_yearweek))] <- "2014"
  
  # Generating the Christmas Dummy
  order.christmas.begin <- as.Date("2013-11-17", "%Y-%m-%d")
  order.christmas.end <- as.Date("2013-12-24", "%Y-%m-%d")
  df1$x_christmas_order <- factor(df1$order_date > order.christmas.begin & df1$order_date < order.christmas.end)
  levels(df1$x_christmas_order) = c(0,1)
  
  # generate a binned version of the advertising_code
  df1$x_advertising_code_bin = df1$advertising_code
  levels(df1$x_advertising_code_bin) = gsub(levels(df1$x_advertising_code_bin), pattern = "A.", replacement = "A")
  levels(df1$x_advertising_code_bin) = gsub(levels(df1$x_advertising_code_bin), pattern = "B.", replacement = "B")
  levels(df1$x_advertising_code_bin) = gsub(levels(df1$x_advertising_code_bin), pattern = "C.", replacement = "C")
  

  #### Bin Item Counts
  # create a vector of item count column names
  count_cols = c("book_count", "paperback_count", "schoolbook_count", "ebook_count", "audiobook_count", "audiobook_download_count", 
                 "film_count", "musical_count", "hardware_count", "used_items", "imported_count", "other_count")
  # get column indices
  item_count_cols_idx = which(colnames(df1) %in% count_cols)
  # set a temporary variable for the bounds
  # note that the bounds are calculated for each variable based on the train dataset 
  # and stored in the 'bounds.csv' file to be applicable to the test data  
  # take a look at the function for more details
  bounds = helpFuncs.get.bounds(df1, item_count_cols_idx, paste0(outputPath, "bounds.csv"))
  # create an offset in the bounds to 
  offset = 0.0001
  # use the cut function on each column to bin them
  for(item_count_col_idx in item_count_cols_idx) {
      # generate the column names for the new variables
      x_col_name = paste0("x_", colnames(df1)[item_count_col_idx], "_bin")
      hi_bound = bounds[1, colnames(df1)[item_count_col_idx]] # generate the hi_bound
      # apply the binning procedure to the count variable(s)
      bins = cut(df1[, item_count_col_idx], c(0, 1, hi_bound+offset, Inf), labels = F, right = FALSE) - 1 
      bins[is.na(bins)] = -1 
      df1[,x_col_name] = bins
  }

  

  ##########
  # Numeric version of date variables
  ##########

  
  # identify date variables
  dateVars = sapply(df1, is.date)
  date_num_name = paste0("x_", colnames(df1)[dateVars], "_num")
  DateNumConvert = function(data = NA){as.numeric(difftime(data, as.Date(epochDate), units = c("days")))}
  HelpDatesNum = as.data.frame(apply(df1[,dateVars], MARGIN = 2, DateNumConvert))
  colnames(HelpDatesNum) = date_num_name
  
  # attach generated variables to data frame
  df1 = cbind(df1, HelpDatesNum)

  #}

  ########################
  # WoE generating section
  ########################
  
  # Set variables for which to generate WoE
  # weekday not included - done in python code
  Varlist_WoE <- c(
                   "email_domain",
                   "payment", 
                   "x_order_date_yearweek", 
                   #"postcode_delivery", 
                   #"model",
                   "postcode_invoice" 
                   )

  if("return_customer" %in% colnames(df1)){
    Varlist_WoE <- c(Varlist_WoE)
    VarSet <- df1
    
    # Draw a random, stratified sample including p percent of the data
    set.seed(123)
    idx_test_woe <- createDataPartition(y = df1$return_customer, p = 0.8, list = FALSE) 
    train_woe <- df1[idx_test_woe,]
    test_woe <- df1[-idx_test_woe,]
    
    adjust = (sum(df1$return_customer == 1)/nrow(df1))/(sum(df1$return_customer == 0)/nrow(df1))
    
    woe_object = woe(return_customer ~. , data = train_woe[,c(Varlist_WoE, "return_customer")], zeroadj = adjust)
    save(woe_object, file=paste0(outputPath, "WoE_Model.RData"))

    train_woe_NewVars = woe_object$xnew
    colnames(train_woe_NewVars) = gsub("woe.", "x_woe_", colnames(train_woe_NewVars))
    train_woe = cbind(train_woe, train_woe_NewVars)
    
    i = 1
    chkvecFullObs = vector(length = length(Varlist_WoE))
    chkvecNoMiss = vector(length = length(Varlist_WoE))
    while(i < (length(Varlist_WoE)+1)){
      chkvecFullObs[i] = length(train_woe[,Varlist_WoE[i]]) == length(train_woe_NewVars[,i])
      chkvecNoMiss[i] = anyNA(train_woe_NewVars[,i])
      i = i + 1
    }
    if(!all(chkvecFullObs == TRUE)){print("WoE generates less predictions than observations!")}
    if(any(chkvecNoMiss == TRUE)){print("WoE generates missing values!")}
    
    test_woe = predict(woe_object, newdata = test_woe, replace = FALSE)
    colnames(test_woe) = gsub("woe.", "x_woe_", colnames(test_woe))
    
    i = 1
    chkvecNoMiss = vector(length = length(Varlist_WoE))
    WoEVarNo = which(str_detect(colnames(test_woe), "woe") == TRUE)
    while(i < (length(Varlist_WoE)+1)){
      chkvecNoMiss[i] = anyNA(test_woe[,WoEVarNo[i]])
      i = i + 1
    }
    if(any(chkvecNoMiss == TRUE)){print("WoE generates missing values!")}
    
    
    
    df1 <- rbind(train_woe, test_woe)
  } else {
    load(file =paste0(outputPath, "WoE_Model.RData"))
    df1 <- predict(woe_object, newdata = df1, replace = FALSE)
    colnames(df1) = gsub("woe.", "x_woe_", colnames(df1))
    
    i = 1
    chkvecNoMiss = vector(length = length(Varlist_WoE))
    WoEVarNo = which(str_detect(colnames(df1), "woe") == TRUE)
    while(i < (length(Varlist_WoE)+1)){
      chkvecNoMiss[i] = anyNA(df1[,WoEVarNo[i]])
      i = i + 1
    }
    if(any(chkvecNoMiss == TRUE)){print("WoE generates missing values!")}

  }
  

  ##########
  # Purge
  ##########
  
  # this category is empty so delete it
  df1$points_redeemed = NULL
  
  # initial item count can be removed: recalculated item count (x_item_count) will be used
  df1$item_count = NULL
  
  return(df1)
  
}

helpFuncs.load <- 
function(fps = c("./data/train.csv", "./data/test.csv")){
  # items can be accessed as l[1], l[2]
  train = helpFuncs.load.dataset(fps[1])
  test = helpFuncs.load.dataset(fps[2])
  return(list(train,test))
}

helpFuncs.transform.to.numeric <- 
function(df1) {
  # http://stackoverflow.com/questions/18178451/is-there-a-way-to-check-if-a-column-is-a-date-in-r
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
  # if we deal with the train data, i.e. the variable 'return_customer' is in it
  if("return_customer" %in% colnames(df1)) {
    bounds = list() # set up a bounds list
    for(to_bin_col_idx in to_bin_cols_idx) {
      # generate the bounds vie the quantiles which are given by the summary function
      bounds[colnames(df1)[to_bin_col_idx]] = summary(df1[which(df1[, to_bin_col_idx] != 0), to_bin_col_idx])[5]
    }
    bounds = as.data.frame(bounds) # convert into a data frame
    rownames(bounds) = NULL
    # save the data frame to a csv file, which can be used to obtain the binned versions in the test data
    write.csv2(bounds, boundsPath, row.names=FALSE)  
  } else {
    # obtain the binned count variables in the test data
    bounds = read.csv2(boundsPath)
  }
  return(bounds)  
}

is.date <- function(x) {inherits(x, 'Date')}

