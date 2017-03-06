get.loan.dataset <- function(){
  df = read.csv('Loan_Data.csv', sep=";")
  df$YOB = as.factor(df$YOB)
  df$PHON = as.factor(df$PHON)
  df$BAD = as.factor(df$BAD)
  df$YOB[df$YOB == 99] <- NA
  df$YOB_missing <- ifelse(is.na(df$YOB), 1, 0)
  df$YOB[is.na(df$YOB)] <- 64
  return(df)
}
brier.score <- function(y_x, t) {
    # the brier score is a error measure so lower is better
    y_x = as.numeric(y_x) + 1
    t = as.numeric(t)
    return(sum((t-y_x)^2) / length(t))
}