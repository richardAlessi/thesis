LR_test_calc <- function(Allocation, Results, Z){
  Tr = dim(Results)[2]
  n = dim(Results)[1]
  
  ResultsVector = rep(0,n)
  
  for (i in 1:n){
    ResultsVector[i] = Results[i, Allocation[i]]
  }
  
  All = as.factor(Allocation)
  model = glm(formula = ResultsVector~1+All*Z, family = binomial(link="logit"))
  null_model = glm(formula = ResultsVector~1+Z, family = binomial(link="logit"))
  
  test_results = lmtest::lrtest(model, null_model)
  
  pvalue = test_results$`Pr(>Chisq)`[2]
  
  return(pvalue)
  }