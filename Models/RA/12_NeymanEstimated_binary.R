neymanEstimated_binary <- function(n, n0, Results){
  T = 2
  
  # First initialise by assigning no to each treatment
  # We keep on going though until we see at least one success and one failure for
  # each treatment

  Allocation = Initialise(n, n0, T, Results)
  counter = sum(Allocation != 0)  
    
  for (i in (counter+1):n) {
    Res = Results[1:(i-1), ]
    All = Allocation[1:(i-1)]
    pA = mean(Res[All == 1 ,1])
    qA = 1 - pA
    pB = mean(Res[All == 2 ,2])
    qB = 1 - pB
    R = sqrt((pA*qA)/(pB*qB))
    prob = R/(1+R)
    Allocation[i] = 2 - (runif(1) <= prob)
    
  }
  return(Allocation)
}
