successCount <- function(Allocation, Results){
  n = length(Allocation)
  Successes = rep(0,n)
  
  if(n ==1){
  Success = Results[Allocation]
  return(Success) 
  }else{
  for (i in 1:n) {
    Successes[i] = Results[i, Allocation[i]]
  }
  return(sum(Successes))
  }
}