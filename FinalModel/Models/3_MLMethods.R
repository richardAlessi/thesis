ML_Methods <- function(n0 = 3, Results, Method = "Random", eps = NULL, temp = NULL){
  n = dim(Results)[1]
  Tr = dim(Results)[2]
  
####################### Random ################################################
  
  if(Method == "Random"){Allocation = sample(Tr, n, replace = TRUE)
  }
  
####################### Play the winner #######################################
  if(Method == "PlayTheWinner"){
    Allocation = rep(0,n)
    winner = 0
    for (i in 1:n) {
      if (winner == 1) {
        Allocation[i] = Allocation[i-1]
      } else{
        Allocation[i] = sample(Tr,1)
      }
      winner = Results[i, Allocation[i]]
    }
  }

####################### Play the winner randomised ############################
    
  if(Method == "PlayTheWinnerRandomised"){
    Allocation = rep(0,n)
    B = rep(2, Tr)
    for (i in 1:n) {
      p = B/sum(B)
      X = sample(Tr,1, replace = TRUE, prob = p)
      Allocation[i] = X
      if(Results[i, Tr] == 1){B[X] = B[X]+1 }else{
        B = B + 1
        B[X] = B[X]-1
      }
    }
    }
  
  
####################### Epsilon Greedy ########################################
  if(Method == "EpsilonGreedy"){
    p = rep(0,Tr)
    #First initialise by assigning no to each treatment
    Allocation = Initialise_RR(n, n0, Tr, Results)
    counter = sum(Allocation != 0)
    if(counter >= n){return(Allocation[1:n])}
    
    for (i in (counter+1):n) {
        # Estimate or specify p
        for (j in 1:Tr) {
          Res = Results[1:(i-1), ]
          All = Allocation[1:(i-1)]
          p[j] = mean(Res[All == j ,j])
        }
        maxP = which(p==max(p))
        if(runif(1)>1-eps){Allocation[i] = maxP }else{Allocation[i] = sample(Tr,1, replace = TRUE)}
  }
  }

###################### SoftMax ################################################
  if(Method == "SoftMax" ){
    p = rep(0,Tr)

    #First initialise by assigning no to each treatment
    Allocation = Initialise_RR(n, n0, Tr, Results)
    counter = sum(Allocation != 0)
    if(counter >= n){return(Allocation[1:n])}
    
    for (i in (counter+1):n) {
      # Estimate or specify p
      for (j in 1:Tr) {
        Res = Results[1:(i-1), ]
        All = Allocation[1:(i-1)]
        p[j] = mean(Res[All == j ,j])
      }
      p_calc = exp(p/temp)/sum(exp(p/temp))
      Allocation[i] = sample(Tr,1, replace = TRUE, prob = p_calc)
    }
  }
  
  
  return(Allocation)
}
