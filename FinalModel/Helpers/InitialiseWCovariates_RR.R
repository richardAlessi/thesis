InitialiseWCovariates_RR <- function(n0, Results, Z){
  
  n = dim(Results)[1]
  Tr = dim(Results)[2]
  
  # Initialise variables we will use
  Allocation = rep(0,n)
  counter = 1
  minSuccess = 0
  minTreatment = 0
  n_z0 = rep(0,Tr)
  n_z1 = rep(0,Tr)
  success_z0 = rep(0,Tr)
  success_z1 = rep(0,Tr)
  
  
  while((minSuccess == 0 || minTreatment < n0 || any(success_z0 == n_z0)|| any(success_z1 == n_z1) ) & counter <= n){
    n_t = if(Z[counter] == 0){n_z0}else{n_z1}
    js =  sort.list(n_t) 
    pi_t = 2*(Tr+1-js)/(Tr^2 + Tr)
    
    pi_t_new = pi_t
    
    for(k in 1:Tr){
      if(sum(n_t[k] == n_t)>1){
        pi_t_new[k] = mean(pi_t[n_t[k] == n_t])
      }
    }
    
    x = sample(Tr,1, replace = TRUE, prob = pi_t_new)
    Allocation[counter] = x
    if(Z[counter] == 0){
      n_z0[x] =  n_z0[x] + 1  
      success_z0[x] = success_z0[x] + Results[counter,x]
      }
    if(Z[counter] == 1){
      n_z1[x] =  n_z1[x] + 1
      success_z1[x] = success_z1[x] + Results[counter,x]
      }
    
    minSuccess = min(success_z0, success_z1)
    minTreatment = min(n_z0, n_z1)
    
    counter = counter + 1
  }
  return(Allocation)
}
