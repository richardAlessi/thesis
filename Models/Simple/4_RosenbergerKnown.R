# Simple allocation based off Rosenberger Allocation
RosenbergerKnown <- function(n, T, Results, p){
  q = 1 - p
  denom = sum(p/q)
  p_calc = p/(q*denom)
  Allocation = sample(T, n, replace = TRUE, prob = p_calc)
  return(Allocation)
}
