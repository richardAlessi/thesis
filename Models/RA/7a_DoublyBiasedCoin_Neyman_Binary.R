doublyBiasedCoinAllocation_Neyman_binary <- function(n, n0, gam, Results){
  Allocation = Initialise(n, n0, T, Results)
  q = rep(0.5,n) 
  counter = sum(Allocation != 0)  
  
  
  for (i in (counter+1):n) {
    Res = Results[1:(i-1), ]
    All = Allocation[1:(i-1)]
    pA = mean(Res[All == 1 ,1])
    qA = 1 - pA
    pB = mean(Res[All == 2 ,2])
    qB = 1 - pB
    R = sqrt((pA*qA)/(pB*qB))
    p_hat = R/(1+R)
    Pi_allocated = 1-mean(All-1)
    q[i] = DBCD_G(Pi_allocated,p_hat,gam)
    Allocation[i] = 2 - (runif(1)<= q[i])
  }
  return(Allocation)
}