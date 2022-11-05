successTreatmentCount <- function(Allocation, Results){
  n = length(Allocation)
  Tr = dim(Results)[2]
  if(n==0){return(rep(0,Tr))}
  if(n==1){Tr = length(Results)
    Successes = rep(0, Tr)
    Successes[Allocation] = Results[Allocation]
    return(Successes)
  }else{
    Successes = matrix(rep(0,n*Tr),ncol =Tr, nrow = n)
    for (i in 1:n) {
      Successes[i, Allocation[i]] = Results[i, Allocation[i]]
    }
  
  return(colSums(Successes))
  }
}
