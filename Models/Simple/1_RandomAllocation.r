# Simple random selection
# This function simple allocates n patients one of T treatments in a completely 
# random way. The results are a vector of size n with the treatment allocation 
# for each patient, i.e. an integer between 1 and T

# n > 0, T > 1
randomAllocation <- function(n, T){
  results = sample(1:T, n, replace = TRUE)
  return(results)
}