GittinsIndex <- function(n0, Results, GI, Random = FALSE){
  library(gittins)
  n = dim(Results)[1]
  Tr = dim(Results)[2]
  GIs = rep(0,Tr)

  Sig_start = 1
  n_start = 2 
  
  Allocation = Initialise_RR(n, n0, Tr, Results)
  Allocation_alt = Initialise_RR(n, n0, Tr, Results)
  counter = sum(Allocation != 0)
  if(counter >= n){return(Allocation[1:n])}
  
  
  for(i in (counter+1):n){
    Res = Results[1:(i-1), ]
    All = Allocation[1:(i-1)]
    Tr_success = successTreatmentCount(All, Res)
    
    for(k in 1:Tr){
      Tr_allocation = sum(All == k)
      
      GIs[k] = GI[ Tr_allocation - n_start+1, Tr_success[k] - Sig_start +1 ]
      if(Random){
        GIs[k] = GIs[k] + rexp(1, rate = Tr+1)*(Tr+1)/Tr
      }
    }
  Allocation[i] = which.max(GIs)
  }
  return(Allocation)  
}


# devtools::install_github("jedwards24/gittins")
