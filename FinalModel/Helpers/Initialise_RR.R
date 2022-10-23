Initialise_RR <- function(n, n0, Tr, Results){
  
  # Initialise variables we will use
  Allocation = rep(0,n)
  counter = 1
  minSuccess = 0
  minTreatment = 0
  n_t = rep(0,Tr)
  success_t = rep(0,Tr)
  order = 1:T
  
  
  while((minSuccess == 0 || minTreatment < n0 || any(success_t == n_t) ) & counter <= n){
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
    n_t[x] =  n_t[x] + 1
    success_t[x] = success_t[x] + Results[counter,x]
      
    minSuccess = min(success_t)
    minTreatment = min(n_t)
    
    counter = counter + 1
    }
  return(Allocation)
}
