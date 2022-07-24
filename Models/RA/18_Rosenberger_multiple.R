Rosenberger_multiple <- function(n, n0, T, Results){
  Allocation = rep(0,n)
  p = rep(0,T)
  q = rep(0,T)
  
  for (t in 1:T) {
    Allocation[(n0*(t-1)+1):(t*n0)] = t
  }
  
  for (i in (T*n0+1):n) {
    for (j in 1:T) {
      p[j] = mean(Results[Allocation[1:(i-1)] == 1 ,j])
      q[j] = 1 - p[j]
    }
    denom = sum(p/q)
    p_calc = p/(q*denom)
    Allocation = sample(T, n, replace = TRUE, prob = p_calc)
  }
  return(Allocation)
}
