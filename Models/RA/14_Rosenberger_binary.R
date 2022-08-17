Rosenberger_binary <- function(n, n0, Results){
  T = 2
  
  Allocation = Initialise(n, n0, T, Results)
  counter = sum(Allocation != 0) 
  
  for (i in (counter+1):n) {
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
