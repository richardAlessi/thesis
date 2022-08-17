RosenbergerCara <- function(n, n0,Z, Results){
  library(fastDummies)
  Allocation = rep(0,n)
  
  T = dim(Results)[2]
  
  p = rep(0,T)
  q = rep(0,T)
  
  #First initialise by assigning no to each treatment
  Allocation = InitialiseWCovariates(n, n0, T, Results, Z)
  counter = sum(Allocation != 0)  
  
  for (i in (counter+1):n) {
    Res = Results[1:(i-1), ]
    All = Allocation[1:(i-1)]
    Z_part = Z[1:(i-1)]
    All_dummy = dummy_cols(All)
    All_dummy = All_dummy[,3:dim(All_dummy)[2]] # Remove first column of original data and first treatment due to multicollinearity
    glm_res = glm(formula = Res~1+All_dummy+Z_part, family = binomial(link="logit"))
    for (j in 1:T) {
      Treatment = rep(0,T)
      Treatment[j] = 1
      data = data.frame(All_dummy = t(Treatment[2:T]), Z_part = Z[i])
      p[j] = predict(glm_res, newdata = data, type = "response")
      q[j] = 1 - p[j]
    }
    denom = sum(p/q)
    p_calc = p/(q*denom)
    Allocation[i] = sample(T,1, replace = TRUE, prob = p_calc)
  }
  
  return(Allocation)  
}
