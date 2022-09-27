ResponseAdaptive <- function(n, n0 = 5, Results, AllocationMethod, EstimationMethod = NULL,  p_known = NULL, gam = NULL){
  T = dim(Results)[2]
  
  if(AllocationMethod == "Random"){Allocation = sample(T, n, replace = TRUE)
  return(Allocation)
  }
  
  if(EstimationMethod == "Known"){
    p = p_known
    q = 1-p
    if (AllocationMethod == "Neyman") {
      denom = sum(sqrt(p*q))
      p_calc = sqrt(p*q)/denom
    }else if (AllocationMethod == "RSIHR") {
      denom = sum(sqrt(p))
      p_calc = sqrt(p)/denom
    }else if (AllocationMethod == "Rosenberger") {
      denom = sum(p/q)
      p_calc = p/(q*denom)
    }else if (AllocationMethod == "OptimallyAdjusted") {
      denom = sum(sqrt(p)*q)
      p_calc = sqrt(p)*q/denom
    }
    Allocation = sample(T, n, replace = TRUE, prob = p_calc)
    return(Allocation)
  }
  
  
  p = rep(0,T)
  q = rep(0,T)
  
  #First initialise by assigning no to each treatment
  Allocation = Initialise_RR(n, n0, T, Results)
  counter = sum(Allocation != 0)
  if(counter >= n){return(Allocation[1:n])}
  qf = rep(0,n)
  
  if(EstimationMethod == "Estimated" || EstimationMethod == "DBCD"){
    for (i in (counter+1):n) {
      # Estimate or specify p and then calculate q
        for (j in 1:T) {
          Res = Results[1:(i-1), ]
          All = Allocation[1:(i-1)]
          p[j] = mean(Res[All == j ,j])
          q[j] = 1 - p[j]
        }
      
      if (AllocationMethod == "Neyman") {
        denom = sum(sqrt(p*q))
        p_calc = sqrt(p*q)/denom
      }else if (AllocationMethod == "RSIHR") {
        denom = sum(sqrt(p))
        p_calc = sqrt(p)/denom
      }else if (AllocationMethod == "Rosenberger") {
        denom = sum(p/q)
        p_calc = p/(q*denom)
      }else if (AllocationMethod == "OptimallyAdjusted") {
        denom = sum(sqrt(p)*q)
        p_calc = sqrt(p)*q/denom
      }
      
      if(EstimationMethod == "Estimated"){
      Allocation[i] = sample(T,1, replace = TRUE, prob = p_calc)
      } else if(EstimationMethod == "DBCD"){
        Pi_allocated = rep(0,T)
        for (t in 1:T) {
          Pi_allocated[t] = sum(All==t)  
        }
        Pi_allocated = Pi_allocated/sum(Pi_allocated)
        p_sample = DBCD_G_Multi(Pi_allocated,p_calc,gam)
        Allocation[i] = sample(T,1, replace = TRUE, prob = p_sample)
      }
    }
  }
  return(Allocation)
  
  
  
  
}
