##################### MIT LICENSE #########################################
#
# Copyright 2017 
#   David Pollack <david.pollack@cms.hu-berlin.de>
#   
#   Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#   
#   The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
##########################################################################

# badSummary - business Analytics and Data Science Summary function for caret
#
# This function can be used in lieu of the twoClassSummary summary function 
# that we've been using to calculate the AUC in the trainControl function of 
# caret.  This function calculates the expected value of your prediction per 
# customer and finds the best threshold at which this value was found based on 
# the value matrix of in the project instructions.
#
# INPUT:
#   data: a dataframe with the following columns:
#     1) binary predictions
#     2) true values
#     3) probabilities of first factor (non-returning customer)
#     4) probabilities of second factor (returning customer)
#   lev: factor levels of true values ("nonreturning", "returning")
#   model: ??? I don't use it
# OUTPUT:
#   a vector with names "BADS" and "threshold" corresponding to the best expected value per customer 
#   and the threshold at which this value was calculated

badSummary <- function(data, lev = NULL, model = NULL) {
  # load required packages
  require(foreach)
  
  # Fixed Vars
  cm <- matrix(c(3, -10, 0, 0), nrow = 2) # this is the cost matrix as specified in the instructions
  n  <- nrow(data) # number of predictions to make.  Used to get a per customer expected value
  
  # This foreach loop finds the best threshold level of non-returning customers to maximize
  # the cost function.  You might want to think why this is done.  First, I get the binary 
  # predictions (yhat_thr) @ a particular threshold level.  Then I do a element-wise multiplication 
  # on the confusion matrix with the cost matrix and divide by the number of predictions.
  # Note this is the threshold of non-returning customers.  At thr = 0, everyone would be classified 
  # as a non-returning customer and vice-versa at thr = 1.
  x <- foreach(thr = seq(0.01, 0.99, 0.01)) %do% {
    yhat_thr <- factor(ifelse(data[,lev[1]] > thr, lev[1], lev[2]), levels = lev) # yhat @ threshold thr
    
    sum(table(data[,"obs"], yhat_thr)*cm)/n # table(...) creates a confusion matrix 
  }
  
  # return the best expected value per customer and corresponding threshold 
  BADS       <- x[[which.max(x)]]
  threshold  <- seq(0.01, 0.99, 0.01)[which.max(x)]
  out        <- c(BADS, threshold)
  names(out) <- c("BADS", "threshold")
  
  out
}
