# Efron biased coin design with alpha < 0.5 (alpha = 0.5 is simple random selection)
# Only works when T = 2
biasedCoin <- function(n, alpha){
  Allocation = rep(0,n) # biased coin design
  Si = rep(0,n)
  q = rep(0.5,n)
  for (i in seq(from = 1, to = n, by = 1)) {
    Allocation[i] = 2*(runif(1)<= q[i]) - 1
    if(i < n){
      Si[i+1] = sum(Allocation[1:i])/(i)
      if(Si[i+1] == 0){q[i+1] = 0.5}
      if(Si[i+1] < 0){q[i+1] = 1- alpha}
      if(Si[i+1] > 0){q[i+1] = alpha}
    }
  }
  Allocation = (Allocation + 3)/2
  CombinedResult = data.frame(Allocation = Allocation, Si = Si, q = q)
  return(CombinedResult)
}