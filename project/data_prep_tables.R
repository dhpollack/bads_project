# clear environment
rm(list = ls())

# Set your working directory
#setwd("C:/Users/PC/Dropbox/BA_DS/code")

# Load the helpFuncs
source("helpFuncs.R")

# Required libraries; They are loaded (and installed if necessary)
libs = c("assertthat", "caret", "klaR", "stringr", "xtable")
helpFuncs.installLibs(libs)
rm(libs)


# load the original train (called traintest from now on) data
fps = "./data/train.csv"
traintest = read.csv2(fps, header = TRUE, sep = ",", dec = ".")
# load the original test data (referred to as handin from now on)
fps = "./data/test.csv"
handin = read.csv2(fps, header = TRUE, sep = ",", dec = ".")
rm(fps)


# Summarize in a table the dimensions of the traintest and the handin data
# Prepare the data frame
DimensionMat <- as.data.frame(matrix(data = NA, ncol = 2, nrow = 2))
colnames(DimensionMat) <- c("NObs", "NVar")
rownames(DimensionMat) <- c("traintest", "handin")

# Fill in dimension values for both datasets
DimensionMat[1,] = dim(traintest)
DimensionMat[2,] = dim(handin)

# Export a tex table of that 
print.xtable(xtable(DimensionMat), file = "./output/Graphs_Tables/DimensionMat.txt")
rm(DimensionMat) # remove data frame so it does not flow around in environment
# comment upper line out if you want to keep the variable


# Prepare a table providig a nice overview of the groups of variables in the given data

# State the groups
persInfo <- c("ID", "form_of_address", "title", "email_domain", "postcode_invoice", 
              "postcode_delivery", "return_customer")
orderInfo <- c("order_date", "account_creation_date", "deliverydate_estimated", "deliverydate_actual", 
               "goods_value", "weight","item_count",  "remitted_items", "canceled_items", "used_items",
               "book_count", "paperback_count", "schoolbook_count", "ebook_count", "audiobook_count", 
               "audiobook_download_count", "film_count", "musical_count", "hardware_count", 
               "imported_count", "other_count")
marketingInfo <- c("newsletter", "coupon", "advertising_code", "referrer", "points_redeemed", "model")
serviceInfo <- c("payment", "delivery", "giftwrapping", "cost_shipping")

# generate a data frame containing variable nases and respective groups in table form
nrow = max(length(persInfo), length(orderInfo), length(marketingInfo), length(serviceInfo))
# Stack the variable names in a dataframe
VarTable = as.data.frame(matrix(data = NA, nrow = nrow, ncol = 4))
VarTable[1:length(persInfo),1] = persInfo
VarTable[1:length(orderInfo),2] = orderInfo
VarTable[1:length(marketingInfo),3] = marketingInfo
VarTable[1:length(serviceInfo),4] = serviceInfo
colnames(VarTable) = c("persInfo", "orderInfo", "marketingInfo", "serviceInfo")

# generate a latex table  out of that
print.xtable(xtable(VarTable), file = "./output/Graphs_Tables/VarTable.txt")
write.csv(VarTable, file = "./output/Graphs_Tables/VarTable.csv")

rm(persInfo, orderInfo, marketingInfo, serviceInfo, VarTable, nrow)
# comment upper line out if you want to keep the variables


# Initial formatting overview ignored so far. 




# Get an overview regarding how we transformed the traintest data
# for this purpose, I load the transformed traintest data (traintest_trans)
traintest_trans = helpFuncs.load.dataset("data/train.csv")

traintestClass <- sapply(traintest, class)
traintest_transClass <- sapply(traintest_trans[1:38], class)

# Identify Variables whose format was not transformed 
VarClassEqual = (traintestClass == traintest_transClass)
# Store names of variables with not format change
VarOK = colnames(traintest)[VarClassEqual == TRUE]
# Store names of variables with format change
VarAdjust = colnames(traintest)[VarClassEqual == FALSE]
# Prepare a nice data frame for table export
nrow = max(length(VarOK), length(VarAdjust))
VarFormatAdjustdf = as.data.frame(matrix(data = NA, nrow = nrow, ncol = 2))
VarFormatAdjustdf[1:length(VarOK),1] = VarOK
VarFormatAdjustdf[1:length(VarAdjust),2] = VarAdjust
colnames(VarFormatAdjustdf) = c("VarOK", "VarAdjust")
# Perform export to Latex
#VarFormatAdjustdfTex = xtable(VarFormatAdjustdf)
print.xtable(xtable(VarFormatAdjustdf), file = "./output/Graphs_Tables/train_VarFormat_adjust.txt")
write.csv(VarFormatAdjustdf, file = "./output/Graphs_Tables/train_VarFormat_adjust.csv")

rm(VarClassEqual, VarOK, VarAdjust, nrow, VarFormatAdjustdf, traintestClass, traintest_transClass)
# Remove these variables as similar procedure used for test data



# Get an overview regarding how we transformed the handin data
# for this purpose, I load the transformed handin data (handin_trans)
handin_trans = helpFuncs.load.dataset("data/test.csv")

handinClass <- sapply(traintest, class)
handin_transClass <- sapply(handin_trans[1:38], class)

# Identify Variables whose format was not transformed 
VarClassEqual = (handinClass == handin_transClass)
# Store names of variables with not format change
VarOK = colnames(handin)[VarClassEqual == TRUE]
# Store names of variables with format change
VarAdjust = colnames(handin)[VarClassEqual == FALSE]
# Prepare a nice data frame for table export
nrow = max(length(VarOK), length(VarAdjust))
VarFormatAdjustdf = as.data.frame(matrix(data = NA, nrow = nrow, ncol = 2))
VarFormatAdjustdf[1:length(VarOK),1] = VarOK
VarFormatAdjustdf[1:length(VarAdjust),2] = VarAdjust
colnames(VarFormatAdjustdf) = c("VarOK", "VarAdjust")
# Perform export to Latex
print.xtable(xtable(VarFormatAdjustdf), file = "./output/Graphs_Tables/test_VarFormat_adjust.txt")
write.csv(VarFormatAdjustdf, file = "./output/Graphs_Tables/test_VarFormat_adjust.csv")
rm(VarClassEqual, VarOK, VarAdjust, nrow, VarFormatAdjustdf, handin_transClass, handinClass)
# Remove these variables as similar procedure used for test data




# Missing Value overview and table

# Identify variables with missing values in traintest and handin data
VarMissing <- sapply(traintest, anyNA)
VarMissing2 <- sapply(handin, anyNA)
# Generate a table containing the variables with missings 
# and the N? of observations with a missing for these variables
ncol = max(sum(VarMissing), sum(VarMissing2))
VarMissingMat <- matrix(data = NA, ncol = ncol, nrow = 2)
VarMissingMat = as.data.frame(VarMissingMat)
rownames(VarMissingMat) = c("traintest", "handin")
colnames(VarMissingMat) <- colnames(traintest)[VarMissing2] # We can do it like that as we know that
# Variables with missings mostly coincide

# Function to deliver the sum of NAs in each column
NASum = function(data = NA){
  A = sum(is.na(data))
  return(A)
}

# retrieve information for traintest data
VarMissingNObs <- apply(traintest[,VarMissing], MARGIN = 2, NASum)
VarMissingMat[1,c(1,2,4)] <- VarMissingNObs
VarMissingNObs <- apply(handin[,VarMissing2], MARGIN = 2, NASum)
VarMissingMat[2,1:length(VarMissingNObs)] <- VarMissingNObs

# Notice, however, that in the traintest data 49608 observations feature a blank ""
# which essentially a missing value, too!
sum(traintest$postcode_delivery == "")

# Export the able
print.xtable(xtable(VarMissingMat), file = "./output/Graphs_Tables/VarMissingMat.txt")
write.csv(VarMissingMat, file = "./output/Graphs_Tables/VarMissingMat.csv")
rm(VarMissingMat, VarMissingNObs, VarMissing, VarMissing2, ncol)


# Generate frequency tables for bi- and multinomial variables

# prepare function
FreqTableMultinom = function(data = NA, nobs = NA){
  nobs = length(data)
  useNA = ifelse((anyNA(data) == TRUE), "always", "no")
  freqTable = table(data, useNA = useNA)
  freqTable = as.data.frame.table(freqTable)
  # rownames(freqTable) = freqTable$data
  freqTable$percent = (freqTable$Freq / nobs) * 100
  freqTable = freqTable[order(freqTable$Freq, decreasing = TRUE),]
}

# Tables and analysis of initially given data
binVar = c("title", "newsletter", "delivery", "coupon", "giftwrapping")

# Extract frequency tables for dummy variables
#binVarFreqTables_handin = lapply(handin[, binVar], FUN = FreqTableMultinom)
#binVarFreqTables_traintest = lapply(traintest[, binVar], FUN = FreqTableMultinom)

# Extract frequency tables for multinomial variables
multinomVar = c("model", "goods_value", "referrer", "cost_shipping", "payment", 
  "form_of_address","email_domain", "advertising_code", 
  "postcode_invoice", "postcode_delivery")
#multinomVarFreqTables_handin = lapply(handin[, multinomVar], FUN = FreqTableMultinom)
#multinomVarFreqTables_traintest = lapply(traintest[, multinomVar], FUN = FreqTableMultinom)

# Extract frequency tables for count variables
countVar = c(colnames(traintest[,c(16,24:36)]), "remitted_items", "canceled_items")
#countVarFreqTables_handin = lapply(handin[, countVar], FUN = FreqTableMultinom)
#countVarFreqTables_traintest = lapply(traintest[, countVar], FUN = FreqTableMultinom)


iniVarFull = c(binVar, multinomVar, countVar)
iniVarFullFreqTables_handin = lapply(handin[, iniVarFull], FUN = FreqTableMultinom)
iniVarFullFreqTables_traintest = lapply(traintest[, iniVarFull], FUN = FreqTableMultinom)


FreqTableSum_List = as.list(rep(NA, length(iniVarFull)))
names(FreqTableSum_List) <- iniVarFull

i = 1
while(i < (length(iniVarFull) + 1)){
  tt = as.data.frame(iniVarFullFreqTables_traintest[iniVarFull[i]])
  hi = as.data.frame(iniVarFullFreqTables_handin[iniVarFull[i]])
  
  # Generate a list containing all frequency tables of interest
  full = merge(tt, hi, by=colnames(tt)[1], all.x = TRUE, all.y = TRUE)
  rownames(full) = NULL
  # full = full[,-1]
  colnames(full) = c("Observed Value", "traintest NObs", "traintest %", "handin NObs", "handin %")
  FreqTableSum_List[i] = list(full)
  
  # Generate and save all frequency tables in LaTeX format
  NameFile = paste0("./output/Graphs_Tables/", iniVarFull[i], "_FreqTab_PreTransform")
  print.xtable(xtable(full), file = paste0(NameFile, ".txt"))
  write.csv(full, file = paste0(NameFile, ".csv"))
  
  i = i + 1
}

# Zoom in: extract and export the sparse advertising codes
Advert_Sparse = as.data.frame(FreqTableSum_List$advertising_code)
Advert_Sparse = Advert_Sparse[(Advert_Sparse$`traintest NObs` < 6 | Advert_Sparse$`handin NObs` < 6),c(1,2,4)]
print.xtable(xtable(Advert_Sparse), file = "./output/Graphs_Tables/AdvertFreqMat_sparse.txt")
write.csv(Advert_Sparse, file = "./output/Graphs_Tables/AdvertFreqMat_sparse.csv")

# Points redeemed is an irrelevant variable
paste("In the traintest dataset,", sum(traintest$points_redeemed), "customers redeemed points", sep = " ")
paste("In the handin dataset,", sum(handin$points_redeemed), "customers redeemed points", sep = " ")

# Post Variable Removal
rm(full, hi, tt, binVar, countVar, i, iniVarFullFreqTables_handin, iniVarFullFreqTables_traintest,
   multinomVar, NameFile, FreqTableSum_List, Advert_Sparse)
# uncomment to keep information

# Obtain the post-transform frequency tables of those variables
iniVarFull = iniVarFull[-which(iniVarFull == "item_count")]

iniVarFullFreqTables_handin_trans = lapply(handin_trans[, iniVarFull], FUN = FreqTableMultinom)
iniVarFullFreqTables_traintest_trans = lapply(traintest_trans[, iniVarFull], FUN = FreqTableMultinom)

FreqTableSum_List = as.list(rep(NA, length(iniVarFull)))
names(FreqTableSum_List) <- iniVarFull

i = 1
while(i < (length(iniVarFull) + 1)){
  tt = as.data.frame(iniVarFullFreqTables_traintest_trans[iniVarFull[i]])
  hi = as.data.frame(iniVarFullFreqTables_handin_trans[iniVarFull[i]])
  
  # Generate a list containing all frequency tables of interest
  full = merge(tt, hi, by=colnames(tt)[1], all.x = TRUE, all.y = TRUE)
  rownames(full) = NULL
  # full = full[,-1]
  colnames(full) = c("Observed Value", "traintest NObs", "traintest %", "handin NObs", "handin %")
  FreqTableSum_List[i] = list(full)
  
  # Generate and save all frequency tables in LaTeX format
  NameFile = paste0("./output/Graphs_Tables/", iniVarFull[i], "_FreqTab_PostTransform")
  print.xtable(xtable(full), file = paste0(NameFile, ".txt"))
  write.csv(full, file = paste0(NameFile, ".csv"))
  
  i = i + 1
}

# Post-transformation frequency tables of newly generated variables
NewVars1 = c(colnames(traintest_trans[37:63]), "postcode_delivery")

NewVars1FreqTables_handin = lapply(handin_trans[, NewVars1], FUN = FreqTableMultinom)
NewVars1FreqTables_traintest = lapply(traintest_trans[, NewVars1], FUN = FreqTableMultinom)

FreqTableSum_NewVars1 = as.list(rep(NA, length(NewVars1)))
names(FreqTableSum_NewVars1) <- NewVars1

i = 1
while(i < (length(NewVars1) + 1)){
  tt = as.data.frame(NewVars1FreqTables_traintest[NewVars1[i]])
  hi = as.data.frame(NewVars1FreqTables_handin[NewVars1[i]])
  
  # Generate a list containing all frequency tables of interest
  full = merge(tt, hi, by=colnames(tt)[1], all.x = TRUE, all.y = TRUE)
  rownames(full) = NULL
  # full = full[,-1]
  colnames(full) = c("Observed_Value", "traintest NObs", "traintest %", "handin NObs", "handin %")
  FreqTableSum_NewVars1[i] = list(full)
  
  # Generate and save all frequency tables in LaTeX format
  NameFile = paste0("./output/Graphs_Tables/", NewVars1[i], "_FreqTab_NewVar")
  print.xtable(xtable(full), file = paste0(NameFile, ".txt"))
  write.csv(full, file= paste0(NameFile, ".csv"))
  
  i = i + 1
}

# Variable removal
rm(full, hi, tt, i, FreqTableSum_NewVars1, NewVars1, NewVars1FreqTables_traintest, 
   NewVars1FreqTables_handin,  NameFile)
# uncomment to keep variables

# Summary stats for Weight of Evidence
# (1) Identify WoE variables
WoEVars = colnames(traintest_trans)[str_detect(colnames(traintest_trans), "woe")]

# (2) Set up list for summary stats
SmartSummary = function(data = NA){
  A = (as.matrix(summary(data)))
  return(A)
}
WoESum_List <- t(sapply(traintest_trans[,WoEVars], FUN = SmartSummary))
colnames(WoESum_List) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
WoESum_List = as.data.frame(WoESum_List)

print.xtable(xtable(WoESum_List), file = "./output/Graphs_Tables/WoEVars_SumTab.txt")
write.csv(WoESum_List, file= "./output/Graphs_Tables/WoEVars_SumTab.csv")
rm(WoESum_List, WoEVars)



#########################################################################################
# The subsequent code was used to generate other interesting tables we used in our report
# for the BADS assignment 
#########################################################################################

# Transformation of 'advertising_code'
traintest$advertising_code = factor(traintest$advertising_code)
handin$advertising_code = factor(handin$advertising_code)

tmp_ad_levs = levels(traintest$advertising_code)
tmp_ad_levs[which(tmp_ad_levs=="")] = "Other"
tmp_ad_levs[which(tmp_ad_levs=="")] = "Other" # Implement this to avoid code to break if 
# other dataset (class) contains levels which are not in the known(train) dataset
levels(traintest$advertising_code) = tmp_ad_levs
rm(tmp_ad_levs)

tmp_ad_levs = levels(handin$advertising_code)
tmp_ad_levs[which(tmp_ad_levs=="")] = "Other"
tmp_ad_levs[which(tmp_ad_levs=="")] = "Other" # Implement this to avoid code to break if 
# other dataset (class) contains levels which are not in the known(train) dataset
levels(handin$advertising_code) = tmp_ad_levs
rm(tmp_ad_levs)

advert_Freq_traintest = FreqTableMultinom(data = traintest$advertising_code)
advert_Freq_handin = FreqTableMultinom(data = handin$advertising_code)

full = merge(advert_Freq_traintest, advert_Freq_handin, 
             by=colnames(advert_Freq_traintest)[1], all.x = TRUE, all.y = TRUE)
colnames(full) = c("Observed Value", "traintest NObs", "traintest %", "handin NObs", "handin %")
print.xtable(xtable(full), file = "./output/Graphs_Tables/AdvertFreqMat.txt")
write.csv(full, file = "./output/Graphs_Tables/AdvertFreqMat.csv")


# Export table of spares advertising codes
nrow(full[(na.omit(full$`traintest NObs`) < 6),])

print.xtable(xtable(full[(na.omit(full$`traintest NObs`) < 6),]), 
             file = "./output/Graphs_Tables/AdvertFreqMat_sparse.txt")
write.csv(full[(na.omit(full$`traintest NObs`) < 6),], 
          file = "./output/Graphs_Tables/AdvertFreqMat_sparse.csv")
