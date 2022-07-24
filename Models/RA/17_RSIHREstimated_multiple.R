RSIHR_multiple <- function(n, n0, T, Results){
  Allocation = rep(0,n)
  p = rep(0,T)
  
  #First initialise by assigning no to each treatment
  
  for (t in 1:T) {
    Allocation[(n0*(t-1)+1):(t*n0)] = t
  }
  
  for (i in (T*n0+1):n) {
    for (j in 1:T) {
      p[j] = mean(Results[Allocation[1:(i-1)] == 1 ,j])
    }
    denom = sum(sqrt(p))
    p_calc = sqrt(p)/denom
    Allocation[i] = sample(T,1, replace = TRUE, prob = p_calc)
  }
  return(Allocation)
}
