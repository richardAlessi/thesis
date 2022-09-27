PowerWCovariates <- function(KnownProbs, Allocation, alpha = 0.05, Z){
  library(pwr)
  
  T = dim(KnownProbs)[2]
  
  powers = rep(0,T*(T-1))
  counter = 0
  
for(k in 1:2){
  for(i in 1:T){
    for(j in 1:T){
      if(i < j){
        counter = counter +1
        n1 = sum(Allocation == i && Z == k)
        n2 = sum(Allocation == j && Z == k)
        h = abs(KnownProbs[k, i] - KnownProbs[k, j])
        if((n1-1)+(n2-1) < 1){powers[counter] = 0}else{
        p= pwr.f2.test(u = 1, v = (n1-1)+(n2-1), f2 = h, sig.level = alpha )
        powers[counter] = p$power[1]}
      }
    }
  }
}
  
  pow = mean(powers)
  
  return(pow)
}
