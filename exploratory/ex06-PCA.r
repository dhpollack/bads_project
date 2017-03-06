
source('../helpfuncs/BADS-HelperFunctions.R')

loans = get.loan.dataset.nofact(fpath='../data/Loan_Data.csv')
head(loans)

numcols = sapply(loans,is.numeric)
loans_scaled = scale(loans[,numcols])

model.pca = prcomp(loans_scaled[,-(ncol(loans_scaled)-1)]) # removing BAD column
plot(model.pca, type = "l")
str(model.pca)

PCs = model.pca$x
colors = c("red", "green")
Y = ifelse(loans[,"BAD"] == 0, "green", "red")
table(loans[,"BAD"])
pairs(PCs[,1:4],col=Y)

model.pca$rotation

# note the PCs are all orthagonal and are unit length
t(model.pca$rotation[,1]) %*% model.pca$rotation[,2] # approximately 0
sqrt(t(model.pca$rotation[,1])%*%model.pca$rotation[,1]) # 1


