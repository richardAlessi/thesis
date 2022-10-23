HypothesisTests <- function(Tr, Results, Allocation, alpha = 0.05, alternative = "two.sided"){
  n = length(Allocation)
  ResultsVector = rep(0,n)
  
  for (i in 1:n){
    ResultsVector[i] = Results[i, Allocation[i]]
  }
  
  ChisqTest = chisq.test(Allocation, ResultsVector)
  ChiSq_p = ChisqTest$p.value
  
  pairs = rep(0,Tr*(Tr-1)/2)
  counter = 0
   # Need to have a think about what we want to do here
  
  for(i in 1:Tr){
    for(j in i+1:Tr){
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
  
  pow = mean(powers)
  
  return(pow)
}
