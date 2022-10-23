successTreatmentCount <- function(Allocation, Results){
  n = length(Allocation)
  T = dim(Results)[2]
  Successes = matrix(rep(0,n*T),ncol =T, nrow = n)
  for (i in 1:n) {
    Successes[i, Allocation[i]] = Results[i, Allocation[i]]
    }
  return(colSums(Successes))
}
