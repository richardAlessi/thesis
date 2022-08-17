doublyBiasedCoinAllocation <- function(n, n0, T, Results){
  Allocation = rep(0,n) 
  q = rep(0.5,n) 
  #First initialise by assigning no to each treatment
  for (t in 1:T) {
    Allocation[n0*(t-1)+1:t*n0] = t
  }
  
  for (t in t*no+1:n) {
    q[i] = 
    Allocation[i] = 2 - (runif(1)<= q[i])
    
    return(Allocation)
  }
}