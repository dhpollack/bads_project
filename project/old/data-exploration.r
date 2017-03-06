
source("helpfuncs/helpFuncs_staging.R")
#library(hexbin)
#options(repr.plot.width=5, repr.plot.height=5)
#options(jupyter.plot_mimetypes = "image/png")

train = helpFuncs.load.dataset("data/train.csv")
test = helpFuncs.load.dataset("data/test.csv")
#str(train)

na_dates.idx = which((is.na(train$x_deliverydate_gap) | is.na(train$x_delivery_time) | train$x_delivery_time == 0))
na_dates.data = train[-na_dates.idx, c("x_delivery_time", "x_delivery_time_est", "x_deliverydate_gap", "return_customer")]
Y = ifelse(na_dates.data$return_customer == 0, "green", "red")
min.dgap = min(na_dates.data$x_deliverydate_gap)
plot(na_dates.data$x_delivery_time, na_dates.data$x_deliverydate_gap, col=Y, pch=".")
plot(na_dates.data$x_delivery_time, na_dates.data$x_delivery_time_est, col=Y)


options(repr.plot.width=8, repr.plot.height=8) # plot larger for a moment
df1_numeric = helpFuncs.transform.to.numeric(train)
df1_numeric = na.omit(df1_numeric)
Y = ifelse(df1_numeric$return_customer == 0, "green", "red")
df1_numeric = df1_numeric[,2:35]
df1_numeric = scale(df1_numeric)
model.pca = prcomp(df1_numeric)
#plot(model.pca, type = "l")

PCs = model.pca$x
colors = c("red", "green")
idx.Y_green = which(Y == "green")
par(bg = 'white')
pairs(PCs[,1:5],pch=".", col=Y)
dev.copy(png, "scatterplot.png", width=1500, height=1500)
dev.off()
#bin<-hexbin(PCs[idx.Y_green,1], PCs[idx.Y_green,2], xbins=50) 
#plot(bin, main="Hexagonal Binning PC1 vs PC2 (Good)", colramp=function(n){heat.ob(n,beg=15,end=225)})
#bin<-hexbin(PCs[-idx.Y_green,1], PCs[-idx.Y_green,2], xbins=50) 
#plot(bin, main="Hexagonal Binning PC1 vs PC2 (Bad)", colramp=function(n){BTC(n,beg=15,end=225)})

options(repr.plot.width=8, repr.plot.height=8) # plot larger for a moment

cols_added = c('x_item_multiple', 'x_remitted', 'x_remitted_all', 
               'x_canceled', 'x_tot_canceled_remitted', 'x_buyer_human', 'x_order_date_yearweek',
               'x_deliverydate_gap', 'x_delivery_time', 'x_timelapse_creat_order', 'x_delivery_time_est', 
               'x_christmas.order', 'x_item_count_binned')
cols_count = c('used_items', 'book_count', 'paperback_count', 'schoolbook_count',
               'ebook_count', 'audiobook_count', 'audiobook_download_count', 'film_count',
               'musical_count', 'imported_count', 'other_count')
cols_targets = c("return_customer")
cols_dates   = c('order_date', 'account_creation_date', 'deliverydate_estimated', 'deliverydate_actual')
cols_to_hide = c('ID', 'title', 'model', 'giftwrapping', 'weight', 'postcode_invoice', 'postcode_delivery',
                 'email_domain', 'referrer', 
                 cols_added, cols_count, cols_targets)
cols_to_hide.idx = which(cols_to_hide %in% colnames(train))


df1_numeric = helpFuncs.transform.to.numeric(train)
df1_numeric = na.omit(df1_numeric)
Y = ifelse(df1_numeric$return_customer == 0, "green", "red")
df1_numeric = df1_numeric[,-cols_to_hide.idx]
df1_numeric = scale(df1_numeric)
model.pca = prcomp(df1_numeric)
plot(model.pca, type = "l")

PCs = model.pca$x
par(bg = 'white')
pairs(PCs[,1:5],pch=".", col=Y)
idx.Y_green = which(Y == "green")
par(bg = 'white')
pairs(PCs[idx.Y_green,1:5],pch=".", col=Y[idx.Y_green])
par(bg = 'white')
pairs(PCs[-idx.Y_green,1:5],pch=".", col=Y[-idx.Y_green])
#dev.copy(png, "scatterplot_reduced.png", width=1500, height=1500)
#dev.off()

#PCX = 4
#PCY = 5
#bin<-hexbin(PCs[idx.Y_green,PCX], PCs[idx.Y_green,PCY], xbins=50) 
#plot(bin, main="Hexagonal Binning PCX vs PCY (Good)", colramp=function(n){heat.ob(n,beg=15,end=225)})
#bin<-hexbin(PCs[-idx.Y_green,PCX], PCs[-idx.Y_green,PCY], xbins=50) 
#plot(bin, main="Hexagonal Binning PCX vs PCY (Bad)", colramp=function(n){BTC(n,beg=15,end=225)})


#options(repr.plot.width=5, repr.plot.height=5)


colnames(df1)
cols_added = c('x_item_multiple', 'x_deliverydate_gap', 'x_delivery_time', 'x_remitted', 'x_remitted_all', 
               'x_canceled', 'x_tot_canceled_remitted', 'x_buyer_human', 'x_order_date_yearweek', 
               'x_christmas.order', 'x_item_count_binned', 'x_timelapse_creat_order')
cols_count = c('used_items', 'book_count', 'paperback_count', 'schoolbook_count',
               'ebook_count', 'audiobook_count', 'audiobook_download_count', 'film_count',
               'musical_count', 'imported_count', 'other_count')
cols_to_hide = c('title', 'model', cols_added, cols_count)


df1_numeric = helpFuncs.transform.to.numeric(df1)
df1_numeric = na.omit(df1_numeric)
res = expand.grid(coupon = levels(factor(df1_numeric$coupon)), return_customer = levels(factor(df1_numeric$return_customer)))
res$colors = c("blue", "orange", "red", "green")
coup_ret = apply(df1_numeric[,c("coupon", "return_customer")],1,function(x){which(res$coupon == x[1] & res$return_customer == x[2])})
head(coup_ret)


table(df1$coupon, df1$return_customer)

test = helpFuncs.load.dataset("data/assignment_BADS_WS1617_class.csv")
table(test$coupon)

#which((na_dates.data$x_deliverydate_gap + abs(min(na_dates.data$x_deliverydate_gap)))/na_dates.data$x_delivery_time < 2)
train[1:50, "x_delivery_min"]


