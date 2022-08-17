InitialiseWCovariates <- function(n, n0, T, Results, Z){
  
  Allocation = rep(0,n)
  counter = 0
  
  for (t in 1:T) {
    treatmentCounter = n0
    success = 0
    Allocation[(counter+1):(counter+n0)] = t
    counter = counter + n0
    Res = Results[1:counter, ]
    All = Allocation[1:counter]
    Z_sample = Z[1:counter]
    success_Z0 = sum(Res[(All == t) & (Z_sample == 0), t])
    Failure_Z0 = sum(1- Res[(All == t) & (Z_sample == 0), t])
    success_Z1 = sum(Res[(All == t) & (Z_sample == 1), t])
    Failure_Z0 = sum(1- Res[(All == t) & (Z_sample == 1), t])
    while(success_Z0*Failure_Z0*success_Z1*Failure_Z0 == 0){
      counter = counter + 1
      treatmentCounter = treatmentCounter + 1
      Allocation[counter] = t
      Res = Results[1:counter, ]
      All = Allocation[1:counter]
      Z_sample = Z[1:counter]
      success_Z0 = sum(Res[(All == t) & (Z_sample == 0), t])
      Failure_Z0 = sum(1- Res[(All == t) & (Z_sample == 0), t])
      success_Z1 = sum(Res[(All == t) & (Z_sample == 1), t])
      Failure_Z0 = sum(1- Res[(All == t) & (Z_sample == 1), t])
    }
  }
  return(Allocation)
}
