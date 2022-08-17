RSIHR_multiple <- function(n, n0, T, Results){
  p = rep(0,T)
  
  #First initialise by assigning no to each treatment
  Allocation = Initialise(n, n0, T, Results)
  counter = sum(Allocation != 0)  
  
  for (i in (counter+1):n) {
    for (j in 1:T) {
      Res = Results[1:(i-1), ]
      All = Allocation[1:(i-1)]
      p[j] = mean(Res[All == j ,j])
    }
    denom = sum(sqrt(p))
    p_calc = sqrt(p)/denom
    Allocation[i] = sample(T,1, replace = TRUE, prob = p_calc)
  }
  return(Allocation)
}
