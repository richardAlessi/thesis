RSIHR_binary <- function(n, n0, Results){
  Allocation = rep(0,n)
  T = 2
  #First initialise by assigning no to each treatment
  
  for (t in 1:T) {
    Allocation[(n0*(t-1)+1):(t*n0)] = t
  }
  
  for (i in (T*n0+1):n) {
    i
    pA = mean(Results[Allocation[1:(i-1)] == 1 , 1])
    pB = mean(Results[Allocation[1:(i-1)] == 2 , 2])
    R = sqrt((pA)/(pB))
    prob = R/(1+R)
    Allocation[i] = 2 - (runif(1) <= prob)
    
  }
  return(Allocation)
}
