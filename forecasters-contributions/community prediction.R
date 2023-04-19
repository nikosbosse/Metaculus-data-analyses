## This is an R version of the community prediction function 
# that Metaculus uses, which is originally written in Python. 

# functions seem to work. part that is missing is the part about 
# filtering only for the latest forecast from a particular user. 
weighted_median <- function(x, weights, q = 0.5) {
  wmid <- sum(weights) * q
  
  if (wmid <= 0 || any(weights < 0)) {
    return(NaN)
  }
  
  cmf <- cumsum(weights)
  print(paste("sum cumsum", sum(cmf)))
  
  ilow <- match(TRUE, cmf >= wmid) # first value for which cmf is higher than or equal to wmid
  ihigh <- match(TRUE, cmf > wmid) # first value for which cmf is strictly higher than wmid
  # index here is one different from python as R indices start with 1 instead of 0
  
  return(0.5 * (x[ilow] + x[ihigh]))
}

get_cp_python <- function(predictions) {
  n <- seq_along(predictions)
  w <- exp(sqrt(n) - sqrt(length(n)))
  
  if (length(w) <= 2) {
    w <- rep(1, length(w))
  }
  
  ## Make a weighted histogram to get the community prediction
  # 100 breaks. Intervals are closed on the left - intervals are [0.005, 0.015)
  # there are 99 resulting intervals
  bins <- seq(0.005, 1.0, by = 0.01)
  
  # x gives the mid points for each interval
  x <- round(0.5 * (bins[2:length(bins)] + bins[1:(length(bins) - 1)]), 3)
  
  # Calculate the weighted histogram - looks correct
  bin_indices <- cut(predictions, breaks = bins, include.lowest = FALSE, right = FALSE)
  h2 <- tapply(w, bin_indices, sum, simplify = TRUE, default = 0)
  
  round(weighted_median(x = x, weights = h2, 0.5), 3)
}