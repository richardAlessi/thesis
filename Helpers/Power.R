PowerCalc <- function(KnownProbs, Allocation, alpha = 0.05, alternative = "two.sided"){
  library(pwr)
  
  T = length(KnownProbs)
  
  powers = rep(0,T*(T-1)/2)
  counter = 0
  
  for(i in 1:T){
    for(j in 1:T){
      if(i < j){
        counter = counter +1
        n1 = sum(Allocation == i)
        n2 = sum(Allocation == j)
        h = abs(KnownProbs[i] - KnownProbs[j])
        if(min(n1,n2)<2){powers[counter] = 0}else{
          p= pwr.2p2n.test(h, n1 = n1, n2 = n2, sig.level = alpha, alternative  =  alternative )
          powers[counter] = p$power[1]
        }
      }
    }
  }
  
  pow = mean(powers)
  
  return(pow)
}
