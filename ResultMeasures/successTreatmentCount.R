successTreatmentCount <- function(Allocation, Results){
  n = length(Allocation)
  Successes = matrix(rep(0,n*T),ncol =T, nrow = n)
  T = dim(Results)[2]
  for (i in 1:n) {
    Successes[i, Allocation[i]] = Results[i, Allocation[i]]
    }
  return(colSums(Successes))
}
