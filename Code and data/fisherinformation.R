fisher.selection <- function(features,labels,n,threshold){
  if(!require("assertthat")) install.packages("assertthat"); library(assertthat)
  # Test the inputs
  not_empty(features); not_empty(labels);
  assert_that(nrow(features) == length(labels))
  is.count(n); assert_that(n <= ncol(features));
  is.count(threshold);
  # Convert response to binary
  y <- rep(0,length(labels))
  y[labels > threshold] <- 1
  
  # Function to caluclate the variable score
  fisher.score <- function(x,y){
    num <- (mean(x[y == 0]) - mean(x[y == 1]))^2
    denom <- var(x[y == 0]) + var(x[y == 1])
    score <- round(num/denom,4)
    return(score)
  }
  
  fisher.score = apply(features,2,function(x)fisher.score(x,y))
  names(fisher.score) = colnames(features)
  
  top.ranks = fisher.score[order(fisher.score,decreasing = T)]
  top.vars = names(top.ranks[1:n])
  # subset the data with the top variables
  result.frame <- features[,top.vars]
  return(result.frame)
}