# clear environment
rm(list = ls())

# Set your working directory
#setwd("C:/Users/PC/Dropbox/BA_DS/code")

# Since we need the transformed data for the plots
# We need to: 
# (1) Load the helpFuncs
source("helpFuncs.R")

# (2) Load the required libraries for those
reqLibs = c("caret", "klaR", "stringr")
helpFuncs.installLibs(reqLibs)
rm(reqLibs)

# (3) load the transformed data of the train and the test dataset
train = helpFuncs.load.dataset("data/train.csv")
test  = helpFuncs.load.dataset("data/test.csv")

# Plot the return customer fraction
# retrieve N� of return and non-return customers and put into matrix
Resume_Mat = t(as.matrix(table(train$return_customer)))
rownames(Resume_Mat) = c("nObs") # label matrix nicely
colnames(Resume_Mat) = c("No", "Yes")

# Plot configurations
jpeg(filename = "./output/Graphs_Tables/RetCustDistr.jpg") # start the JPEG recorder
par(mar = c(rep(3,4))) # set plot margins
# Generate a pie chart
slices = Resume_Mat[1,] # Define the slices of the pie chart
lbls =  c("No", "Yes")
lbls = paste(Resume_Mat[1,], lbls) # Define nice label for the slices
pie(slices, labels = lbls, col = rainbow(length(lbls)), 
                 main = "Did the customer return?", cex = 2, cex.lab = 6, cex.main = 2.5, lwd = 2)
# the cex values allow to set the font sizes in general, the slice labels and the header of the plot
# lwd controls the thickness of the lines drawn
# rainbow(length(lbls)) extracts two different colors from rainbow spectrum
dev.off() # finish recording - save the plot
rm(Resume_Mat, slices, lbls)



# Density plot order_date
# I resort to the numeric version of the date variables as that seems easier to extract the density function
dens_train_order_date = density(train$x_order_date_num)
dens_test_order_date = density(test$x_order_date_num)
# note that you need to further extract the x and y values
# of the density estimates to get an adequate plot

jpeg(filename = "./output/Graphs_Tables/order_date_density.jpg", 
     width = 480, height = 360)
par(mar = c(5,5,5,1)) # set plot margins
plot(dens_train_order_date$x, dens_train_order_date$y, col = "cadetblue4", 
     type = "l", main = "Order Date", 
     xaxt = "n", yaxt = "n", xlab = "Days since Jan 1st 2013", ylab = "Density",
     cex = 1, cex.lab = 1.5, cex.main = 2.5, lwd = 4
     )
axis(1, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
axis(2, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
lines(dens_train_order_date$x, dens_test_order_date$y, col = "green", lwd = 4) 
legend("right", title = "Dataset", legend = c("train", "test"), 
       pch = c(16,16),col = c("cadetblue4", "green"), bty = "n", cex = 2)
# notice that cex.axis controls thickness of axis line
# lwd.ticks allows me to set the ticks less thick than other lines. 
dev.off()
rm(dens_train_order_date, dens_test_order_date)

# Similar Exercise for account_creation_date
# extract the density function
dens_train_account_creation_date = density(train$x_account_creation_date_num)
dens_test_account_creation_date = density(test$x_account_creation_date_num)
# note that you need to further extract the x and y values
# of the density estimates to get an adequate plot

jpeg(filename = "./output/Graphs_Tables/account_creation_date_density.jpg", 
     width = 480, height = 360)
par(mar = c(5,5,5,1))
plot(dens_train_account_creation_date$x, dens_train_account_creation_date$y, col = "cadetblue4", 
     type = "l", main = "Account Creation Date", 
     xaxt = "n", yaxt = "n", xlab = "Days since Jan 1st 2013", ylab = "Density",
     cex = 1, cex.lab = 1.5, cex.main = 2.5, lwd = 4, 
)
axis(1, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
axis(2, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
lines(dens_train_account_creation_date$x, dens_test_account_creation_date$y, col = "green", lwd = 4)
legend("right", title = "Dataset", legend = c("train", "test"), 
       pch = c(16,16),col = c("cadetblue4", "green"), bty = "n", cex = 2)
dev.off()
rm(dens_train_account_creation_date, dens_test_account_creation_date)


# Same exercise for deliverydate estimated

# extract the density function
dens_train_deliverydate_estimated = density(train$x_deliverydate_estimated_num)
dens_test_deliverydate_estimated = density(test$x_deliverydate_estimated_num)
# note that you need to further extract the x and y values
# of the density estimates to get an adequate plot

jpeg(filename = "./output/Graphs_Tables/deliverydate_estimated_density.jpg", 
     width = 480, height = 360)
par(mar = c(5,5,5,1))
plot(dens_train_deliverydate_estimated$x, dens_train_deliverydate_estimated$y, 
     type = "l", main = "Delivery Date Estimated",
     xaxt = "n", yaxt = "n", xlab = "Days since Jan 1st 2013", ylab = "Density",
     cex = 1, cex.lab = 1.5, cex.main = 2.5, lwd = 4, 
     col = "cadetblue4")
axis(1, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
axis(2, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
lines(dens_test_deliverydate_estimated$x, dens_test_deliverydate_estimated$y, col = "green", lwd = 4)
legend("right", title = "Dataset", legend = c("train", "test"), 
       pch = c(16,16),col = c("cadetblue4", "green"), bty = "n", cex = 2)
dev.off()
rm(dens_train_deliverydate_estimated, dens_test_deliverydate_estimated)

# Same exercise for deliverydate actual
# extract the density function
dens_train_deliverydate_actual = density(train$x_deliverydate_actual_num)
dens_test_deliverydate_actual = density(test$x_deliverydate_actual_num)
# note that you need to further extract the x and y values
# of the density estimates to get an adequate plot
jpeg(filename = "./output/Graphs_Tables/deliverydate_actual_density.jpg", 
     width = 480, height = 360)
par(mar = c(5,5,5,1))
plot(dens_train_deliverydate_actual$x, dens_train_deliverydate_actual$y, 
     type = "l", main = "Delivery Date Actual",
     xaxt = "n", yaxt = "n", xlab = "Days since Jan 1st 2013", ylab = "Density",
     cex = 1, cex.lab = 1.5, cex.main = 2.5, lwd = 4, 
     col = "cadetblue4")
axis(1, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
axis(2, cex.axis = 1, lwd = 2.5, lwd.ticks = 1)
lines(dens_test_deliverydate_actual$x, dens_test_deliverydate_actual$y, col = "green", lwd = 4)
legend("right", title = "Dataset", legend = c("train", "test"), 
      pch = c(16,16),col = c("cadetblue4", "green"), bty = "n", cex = 2)
dev.off()
rm(dens_train_deliverydate_actual, dens_test_deliverydate_actual)


# Display a barplox stating the distribution of the original item count variable
# Since the distribution is similar in the test data, I refrain from plotting it here. 
train_origin = read.csv(file = "./data/train.csv")
# Histogram of Item Count

item_count_to_20 = train_origin[(train_origin$item_count < 20), "item_count"]
# Since we have quite some outliers, I refrain from considering the 25 customers who ordered
nrow(train_origin) - length(item_count_to_20) # more than 20 items

# Generate a frequency table and get it into a data frame
item_count_table <- table(item_count_to_20, useNA = "no")
item_count_table <- as.data.frame.table(item_count_table)
colnames(item_count_table) = c("Items", "Freq")

# Based on the frequency table, generate a plot
jpeg(filename = "./output/Graphs_Tables/item_count_original_barplot.jpg", 
     width = 480, height = 360)
barplot(item_count_table$Freq, names.arg = item_count_table$Items, 
        ylim = c(0,30000), cex.lab = 3, cex.main = 2.5,
        cex.names = 2, cex.axis = 2,
        main = "Freq item count train data")
dev.off()
rm(train_origin, item_count_to_20, item_count_table)


# Histogram of recomputed item count
item_count_to_20 = train[(train$x_item_count < 20), "x_item_count"]
nrow(train) - length(item_count_to_20) # more than 20 items

# Generate a frequency table and get it into a data frame
item_count_table <- table(item_count_to_20, useNA = "no")
item_count_table <- as.data.frame.table(item_count_table)
colnames(item_count_table) = c("Items", "Freq")

# Based on the frequency table, generate a plot
jpeg(filename = "./output/Graphs_Tables/item_count_modified_barplot.jpg", 
     width = 480, height = 360)
barplot(item_count_table$Freq, names.arg = item_count_table$Items, 
        ylim = c(0,30000), cex.lab = 3, cex.main = 2.5,
        cex.names = 2, cex.axis = 2,
        main = "Freq item count train data")
dev.off()
rm(item_count_to_20, item_count_table)

