RSIHR_binary <- function(n, n0, Results){
  T = 2
  #First initialise by assigning no to each treatment
  
  Allocation = Initialise(n, n0, T, Results)
  counter = sum(Allocation != 0)  
  
  for (i in (counter+1):n) {
    i
    pA = mean(Results[Allocation[1:(i-1)] == 1 , 1])
    pB = mean(Results[Allocation[1:(i-1)] == 2 , 2])
    R = sqrt((pA)/(pB))
    prob = R/(1+R)
    Allocation[i] = 2 - (runif(1) <= prob)
    
  }
  return(Allocation)
}
