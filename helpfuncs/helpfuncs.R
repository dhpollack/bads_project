######################################################################################
#
#   Library with a set of helper functions for the BADS Tutorial
#
######################################################################################

# Function to load the  loan data set (simple version)
get.loan.dataset <-function(fpath = "Loan_Data.csv") {
  # Assume the name of the data set is Loan_Data.csv
  data <- read.csv(fpath, sep = ";")
  # Convert categorical variables into factors
  data$PHON <- factor(data$PHON, labels=c("no", "yes"))
  data$BAD <- factor(data$BAD, labels=c("GOOD", "BAD"))
  # replace missing values
  data$YOB[data$YOB == 99] <- NA
  data$YOB_missing <- factor(ifelse(is.na(data$YOB), 1, 0))
  data$YOB[is.na(data$YOB)] <- as.numeric(names(table(data$YOB))[table(data$YOB) == max(table(data$YOB))])
  return(data)
}
get.loan.dataset.nofact <- function(fpath = "Loan_Data.csv") {
  # Assume the name of the data set is Loan_Data.csv
  data <- read.csv(fpath, sep = ";")
  # Convert categorical variables into factors
  #data$PHON <- factor(data$PHON, labels=c("no", "yes"))
  #data$BAD <- factor(data$BAD, labels=c("GOOD", "BAD"))
  # replace missing values
  data$YOB[data$YOB == 99] <- NA
  data$YOB_missing <- ifelse(is.na(data$YOB), 1, 0)
  data$YOB[is.na(data$YOB)] <- as.numeric(names(table(data$YOB))[table(data$YOB) == max(table(data$YOB))])
  return(data)
}

get.loan.dataset.advanced <- function(folderLocation = NULL, # Character vector of folder names
                                     fileName = "Loan_Data.csv",
                                     phonLabels = c("no", "yes"),
                                     badLabels = c("good", "bad")) {
  
  # Is the data expected in the working directory or elsewhere?
  if (!is.null(folderLocation)) {
    # To properly create the variable that points to the data set,
    # we need to find out how directory paths are seperated on this
    # computer. Note that different operating systems use different separators.
    # Function file.path create a path from a vector 
    file <- do.call(file.path, as.list(c(folderLocation, fileName)))
    # Note that file.path searches for the separator in a global variable starting
    # with a "." that is called .Platform$path.sep
  } 
  # Note that we don't need an else part, because if folderLocation is NULL
  # then we simply expect the file to be available in the working directory
  
  # Check that the data exists in the expected location
  if (file.exists(file)==TRUE) {
    data <- read.csv(file, sep=";") # note that we could easily make the seperator an input parameter using the approach illustrated with fileName above
  } else {
    print(paste("The data set", file, "does not exist.", sep=" "))
    return(NULL)
  }
  
  # Convert categorical variables into factors using the labels
  # specified as input parameters
  data$PHON <- factor(data$PHON, labels = phonLabels)
  data$BAD <- factor(data$BAD, labels = badLabels)
  # replace missing values
  data$YOB[data$YOB == 99] <- NA
  data$YOB_missing <- ifelse(is.na(data$YOB), 1, 0)
  data$YOB[is.na(data$YOB)] <- names(countYOB)[countYOB == max(countYOB)]
  return(data)
}

# Helper funciton to compute measures of predictive accuracy
PredictivePerformance <- function(y=NULL, yhat=NULL, cutoff = .5) {
  
  if (length(y) != length(yhat)) {
    stop("Labels and prediction must have same length")
  }
  
  # Assumption:
  # y is a vector of factors
  # yhat is a vector of probability predictions
  
  # Brier Score
  bs <- sum(((as.numeric(y) - 1) - yhat)^2) / length(y)
  
  # Classification error
  c <- factor(as.numeric(yhat >= cutoff), labels=c("good", "bad")) 
  ce <- 1 - sum(y==c) / length(y)
  
  return( list(BS = bs, CE = ce)  )
}