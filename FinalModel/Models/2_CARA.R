CARA <- function(Results, n0=2, Z, AllocationMethod, EstimationMethod,  p_known = NULL, gam = NULL){
  n = dim(Results)[1]
  Tr = dim(Results)[2]
  
  Allocation = rep(0,n)
  
  if(EstimationMethod == "Known"){
    for (i in 1:n) {
      p = p_known[Z[i]+1, ]  
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
    Allocation[i] = sample(Tr, 1, replace = TRUE, prob = p_calc)
    }
    return(Allocation)
  }
  
  
  p = rep(0,Tr)
  q = rep(0,Tr)
  
  #First initialise by assigning no to each treatment
  Allocation = InitialiseWCovariates_RR(n0, Results, Z)
  counter = sum(Allocation != 0)
  if(counter >= n){return(Allocation[1:n])}
  qf = rep(0,n)
  
  if(EstimationMethod == "Estimated" || EstimationMethod == "DBCD"){
    for (i in (counter+1):n) {
      # Estimate or specify p and then calculate q
      All = as.factor(Allocation[1:(i-1)])
      Res = rep(0,(i-1))
      for (j in 1:(i-1)){
        Res[j] = Results[j, All[j]] 
      }
      Z_part = Z[1:(i-1)]
      #All_dummy = dummy_cols(All)
      #All_dummy = All_dummy[,3:(dim(All_dummy)[2])] # Remove first column of original data and first treatment due to multicollinearity
      glm_res = glm(formula = Res~All*Z_part, family = binomial(link="logit"))
      for (j in 1:Tr) {
        # Treatment = rep(0,Tr)
        # Treatment[j] = 1
        data = data.frame(All = as.character(j), Z_part = Z[i])
        p[j] = predict(glm_res, newdata = data, type = "response")
        q[j] = 1 - p[j]
      }
      
      if (AllocationMethod == "Neyman") {
        denom = sum(sqrt(p*q))
        p_calc = sqrt(p*q)/denom
      }
      if (AllocationMethod == "RSIHR") {
        denom = sum(sqrt(p))
        p_calc = sqrt(p)/denom
      }
      if (AllocationMethod == "Rosenberger") {
        denom = sum(p/q)
        p_calc = p/(q*denom)
      }
      if (AllocationMethod == "OptimallyAdjusted") {
        denom = sum(sqrt(p)*q)
        p_calc = sqrt(p)*q/denom
      }
      
 
      if(EstimationMethod == "DBCD"){
        Pi_allocated = rep(0,Tr)
        for (w in 1:Tr) {
          Pi_allocated[w] = sum(All[Z_part == Z[i]]==w)  
        }
        Pi_allocated_perc = Pi_allocated/sum(Pi_allocated)
        p_calc = DBCD_G_Multi(Pi_allocated_perc,p_calc,gam)
      }
      Allocation[i] = sample(Tr,1, replace = TRUE, prob = p_calc)
    }
  }
  return(Allocation)
  
  
  
  
}
