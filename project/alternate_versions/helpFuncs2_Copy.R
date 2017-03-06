helpFuncs.clearEnv <- function() {
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
helpFuncs.installLibs <- function(x) {
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

helpFuncs.load.dataset <- function(fps = "./Initial_2_Datasets/assignment_BADS_WS1617_known.csv")
  {
  
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

  # changing the "0000/00/00" dates to NA
  tmp_deliv_act_levs = levels(df1$deliverydate_actual)
  tmp_deliv_act_levs[which(levels(df1$deliverydate_actual)=="0000/00/00")] = NA
  levels(df1$deliverydate_actual) = tmp_deliv_act_levs
  df1$deliverydate_actual = as.Date(df1$deliverydate_actual, format = "%Y/%m/%d")
  rm(tmp_deliv_act_levs)

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
  rm(tmp_devord_diff)
  rm(tmp_delivery_time)
  rm(tmp_date_input_error_idx)
  rm(tmp_date_input_error)
  
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
  
  df1$return_customer = factor(df1$return_customer)

  ##########
  # Misc
  ##########

  # Changing the blank "" factor to an NA
  tmp_ad_levs = levels(df1$advertising_code)
  tmp_ad_levs[which(tmp_ad_levs=="")] = NA
  levels(df1$advertising_code) = tmp_ad_levs
  rm(tmp_ad_levs)
  
  # clean up postal_delivery and postcode_invoice codes
  # combining 00 and 0, and combining the non-number codes
  tmp_pcode_levels = levels(df1$postcode_delivery)
  tmp_pcode_levels[which(tmp_pcode_levels=="")] = NA
  tmp_pcode_levels[which(tmp_pcode_levels=="00")] = tmp_pcode_levels[which(tmp_pcode_levels=="0")]
  tmp_pcode_levels[grepl("[A-Z]+",tmp_pcode_levels)] = -2
  levels(df1$postcode_delivery) = as.numeric(tmp_pcode_levels)
  rm(tmp_pcode_levels)
  # Making the postcode_delivery and postcode_invoice the same type
  df1$postcode_invoice = factor(df1$postcode_invoice)
  
  # use a simple linear regression with no intercept to fill in NA values in weight
  tmp_weight_regressors = c('used_items', 'book_count', 'paperback_count', 'schoolbook_count', 
                            'film_count', 'musical_count', 'imported_count', 'other_count')
  tmp_weight_test.idx = which(is.na(df1$weight))
  tmp_weight_train = df1[-tmp_weight_test.idx,c("weight", tmp_weight_regressors)]
  tmp_weight_test = df1[tmp_weight_test.idx,c(tmp_weight_regressors)]
  tmp_model.weight.glm = glm(weight~0+., data=tmp_weight_train)
  df1$weight[tmp_weight_test.idx] = as.integer(predict(tmp_model.weight.glm, tmp_weight_test))
  rm(tmp_weight_regressors, tmp_weight_test.idx, tmp_weight_train, tmp_weight_test, tmp_model.weight.glm)


  ##########
  # New Vars
  ##########
  
  df1$x_timelapse_creat_order = difftime(df1$order_date, df1$account_creation_date, units = "days")
  df1$x_timelapse_creat_order = as.numeric(df1$x_timelapse_creat_order)
  df1$x_timelapse_creat_order = as.integer(df1$x_timelapse_creat_order)

  df1$x_item_multiple = (df1$item_count > 1)
  df1$x_item_multiple = factor(df1$x_item_multiple)
  levels(df1$x_item_multiple) = c(0,1)

  df1$x_deliverydate_gap = as.numeric(difftime(df1$deliverydate_actual ,df1$deliverydate_estimated , units = c("days")))

  delivery_time = as.numeric(difftime(df1$deliverydate_actual, df1$order_date, units = c("days")))
  df1$x_delivery_time = delivery_time

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

  # Section by Tim: Binning order dates by week
  any(is.na(df1$order_date)) # my procedure below only works IF there are no missing values
  # Note that, if we have 'weird values' in the order date variable, we have to deal with them
  # before generating this variable. Else, data errors propagate
  order_Date_YM_df1 <- strftime(df1$order_date, format = "%Y-%V")
  order_Date_YM_df1 <- factor(order_Date_YM_df1)
  levels(order_Date_YM_df1)
  # just to briefly see if it worked
  table(order_Date_YM_df1)
  df1$x_order_date_yearweek <- order_Date_YM_df1
  head(df1[,c("order_date", "x_order_date_yearweek")]) # in order so that you can
  # briefly verify what I did
  
  # Generating the Christmas Dummy
  # Note that it might be 'colinear' with the binning by weeks dummy, depending on
  # the model we eventually pick.
  # Beginning and end date subject to discussion in our group ;)
  order.christmas.begin <- as.Date("2013-11-17", "%Y-%m-%d")
  order.christmas.end <- as.Date("2013-12-24", "%Y-%m-%d")
  christmas.order <- (df1$order_date > order.christmas.begin & df1$order_date < order.christmas.end)
  # Did R get it right? 
  head(df1[christmas.order, "order_date"])
  christmas.order <- factor(christmas.order)
  levels(christmas.order) = c(0,1)
  df1$x_christmas.order <- christmas.order
  
  
  # I started binning the cound variables. Yet how I did this is open for discussion
  
  # Set the quantile generator for later on
  
  Quantile.Generator.Pre <-  function(x){
    quantile(x, probs = c(seq(0,0.8,0.2), seq(0.8, 1, 0.05)), na.rm = TRUE)
  }
  
  
  
  # seperate count variables into zero and above zero
  
  # generate auxiliary dummy for whether the customer did not order any item of 
  # the respective type
  col.count.df1 <- ncol(df1[,c(16,23:35)])
  col.count.df1.names <- colnames(df1[,c(16,23:35)])
  df1[,paste("x_", col.count.df1.names, "_0", sep = "")] <- NA
  df1[,paste("x_", col.count.df1.names, "_not_0", sep = "")] <- NA
  i = 1
  while(i < (col.count.df1 + 1)){
    df1[,paste("x_", col.count.df1.names[i], "_0", sep = "")] <- ((df1[,col.count.df1.names[i]] == 0))
    df1[,paste("x_", col.count.df1.names[i], "_0", sep = "")] <- factor(df1[,paste("x_", col.count.df1.names[i], "_0", sep = "")])
    levels(df1[,paste("x_", col.count.df1.names[i], "_0", sep = "")]) <- c(0,1)
    i = i + 1
  }
  
  a <- paste("x_", col.count.df1.names, "_0", sep = "")
  head(df1[,a])
  
  b <- paste("x_", col.count.df1.names, "_not_0", sep = "")
  df1[,b] <- NA
  
  help.count.not_0.Mat <- as.data.frame(matrix(NA, nrow = nrow(df1), ncol = col.count.df1))
  
  i = 1
  while(i < (col.count.df1 + 1)){
    Help_Vec <- df1[,a[i]]
    j = 1
    while(j < (nrow(df1) + 1)){
      if(Help_Vec[j] == 0){
        help.count.not_0.Mat[j,i] <- df1[j,col.count.df1.names[i]]
      }
      j = j + 1
    }
    i = i + 1
  }
  
  colnames(help.count.not_0.Mat) <- b
  
  # sanity check: Number of nonzero values in original variable and 
  # number of non-NAs in help.count.not_0.Mat have to coincide 
  
  sum((df1$remitted_items) != 0)
  sum((is.na(help.count.not_0.Mat[,2])) == 0)
  
  ncol(help.count.not_0.Mat)
  nrow(help.count.not_0.Mat)
  
  df1[,b] <- help.count.not_0.Mat
  
  table(df1$x_audiobook_count_not_0)
  
  Q.Cutoffs.Percent <- c(0,0.6,0.9,1)
  
  Quantile.Generator.Final <-  function(x){
    quantile(x, probs = Q.Cutoffs.Percent, na.rm = TRUE)
  }
  
  Quant.Mat.Final <- apply(df1[,b], MARGIN = 2, FUN = Quantile.Generator.Final)
  Quant.Mat.Final[4,] <- Inf
  
  test.df <- as.data.frame(matrix(NA, nrow = nrow(df1), ncol = col.count.df1))
  i = 1
  while(i < (col.count.df1 +1)){
    cutoffs <- Quant.Mat.Final[,i]
    if(cutoffs[2] == cutoffs[1]){
      cutoffs = cutoffs[2:length(cutoffs)]
    }
    test.df[,i] <- cut(df1[,col.count.df1.names[i]], unique(cutoffs), right = FALSE)
    i = i + 1
  }
  colnames(test.df) <- b
  head(df1$item_count)
  head(test.df$x_item_count_not_0)
  
  df1[,b] <- test.df
  df1[,b] <- apply(df1[,b], MARGIN = 2, FUN = as.character)
  
  
  
  
  dummyVec <- is.na(df1[,b[1]])
  head(df1[,b[1]])
  head(dummyVec)
  test <- (df1[,b[1]])
  test[(dummyVec == 1)] = "0"
  head(test)
  test <- factor(test)
  df1[(dummyVec == 1), b[1]] = "0" # necessary to code like this bc R does 
  # not accept factor level of "0" here- strange ....
  df1[,b[1]] <-factor(df1[,b[1]])
  class(df1[,b[1]])
  levels(df1[,b[1]])
  
  dummyVec <- is.na(df1[,b[2]])
  head(df1[,b[2]])
  head(dummyVec)
  df1[(dummyVec == 1), b[2]] = "0"
  df1[,b[2]] <-factor(df1[,b[2]])
  levels(test)
  
  dummyVec <- is.na(df1[,b[3]])
  df1[(dummyVec == 1), b[3]] = "0"
  df1[,b[3]] <-factor(df1[,b[3]])
  
  dummyVec <- is.na(df1[,b[4]])
  df1[(dummyVec == 1), b[4]] = "0"
  df1[,b[4]] <-factor(df1[,b[4]])
  
  dummyVec <- is.na(df1[,b[5]])
  df1[(dummyVec == 1), b[5]] = "0"
  df1[,b[5]] <-factor(df1[,b[5]])
  
  dummyVec <- is.na(df1[,b[6]])
  df1[(dummyVec == 1), b[6]] = "0"
  df1[,b[6]] <-factor(df1[,b[6]])
  
  dummyVec <- is.na(df1[,b[7]])
  df1[(dummyVec == 1), b[7]] = "0"
  df1[,b[7]] <-factor(df1[,b[7]])
  
  dummyVec <- is.na(df1[,b[8]])
  df1[(dummyVec == 1), b[8]] = "0"
  df1[,b[8]] <- factor(df1[,b[8]])
  
  dummyVec <- is.na(df1[,b[9]])
  df1[(dummyVec == 1), b[9]] = "0"
  df1[,b[9]] <-factor(df1[,b[9]])
  
  dummyVec <- is.na(df1[,b[10]])
  df1[(dummyVec == 1), b[10]] = "0"
  df1[,b[10]] <-factor(df1[,b[10]])
  
  dummyVec <- is.na(df1[,b[11]])
  df1[(dummyVec == 1), b[11]] = "0"
  df1[,b[11]] <-factor(df1[,b[11]])
  
  dummyVec <- is.na(df1[,b[12]])
  df1[(dummyVec == 1), b[12]] = "0"
  df1[,b[12]] <-factor(df1[,b[12]])
  
  dummyVec <- is.na(df1[,b[13]])
  df1[(dummyVec == 1), b[13]] = "0"
  df1[,b[13]] <-factor(df1[,b[13]])
  
  dummyVec <- is.na(df1[,b[14]])
  df1[(dummyVec == 1), b[14]] = "0"
  df1[,b[14]] <-factor(df1[,b[14]])
  
  
  ##########
  # Purge
  ##########
  
  df1$hardware_count = NULL
  
  df1$points_redeemed = NULL
  
  return(df1)
  
}
         
helpFuncs.load <- function(fps = c("./Initial_2_Datasets/assignment_BADS_WS1617_known.csv",
                                   "./Initial_2_Datasets/assignment_BADS_WS1617_class.csv")) {
  # items can be accessed as l[1], l[2]
  train = helpFuncs.load.dataset(fps[1])
  test = helpFuncs.load.dataset(fps[2])
  return(list(train,test))
}

helpFuncs.transform.to.numeric = function(df1) {
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

