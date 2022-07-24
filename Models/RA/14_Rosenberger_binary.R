Rosenberger_binary <- function(n, n0, Results){
  T = 2
  Allocation = rep(0,n)
  for (t in 1:T) {
    Allocation[(n0*(t-1)+1):(t*n0)] = t
  }
  
  for (i in (T*n0+1):n) {
    i
    pA = mean(Results[Allocation[1:(i-1)] == 1 ,1])
    qA = 1 - pA
    pB = mean(Results[Allocation[1:(i-1)] == 2 ,2])
    qB = 1 - pB
    prob = ((pA/qA))/((pA/qA)+(pB/qB))
    Allocation[i] = 2 - (runif(1) <= prob)
    
  }
  return(Allocation)
}
