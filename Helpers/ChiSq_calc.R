ChiSq_calc <- function(Allocation, Results){
  Tr = dim(Results)[2]
  n = dim(Results)[1]
  ResultsVector = rep(0,n)
  
  for (i in 1:n){
    ResultsVector[i] = Results[i, Allocation[i]]
  }

  contingency = table(ResultsVector, Allocation)
  if(dim(contingency)[1] == 1 | dim(contingency)[2] ==1){return(NA)}
  if(Tr ==2){ChisqTest = stats::fisher.test(contingency)}else{ChisqTest = chisq.test(Allocation, as.factor(ResultsVector), correct = TRUE)}
  
  
  ChiSq_p = ChisqTest$p.value
  return(ChiSq_p)  
}