setwd("C:/Users/PC/Desktop/Dropbox BADS Work Copy")

helpFuncs.clearEnv()

source('helpFuncs2.R')
source('helpFuncs.R')

# No libraries needed to perform this script
# helpFuncs.installLibs("reshape")

df1 <- helpFuncs.load.dataset()

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

Quant.Mat.Pre <- apply(df1[,b], MARGIN = 2, Quantile.Generator)

# looking at this quantile matrix, I would suggest to set the following 
# quantile cutoffs: 0%-60%, 60%-90%, >90%

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
  test.df[,i] <- cut(df1[,col.count.df1.names[i]], cutoffs, right = FALSE)
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

class(df1[,b[10]])
levels(df1[,b[10]])

