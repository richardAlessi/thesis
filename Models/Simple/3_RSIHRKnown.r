# Simple allocation based off RSIHR Allocation
RSIHRKnown <- function(n, T, Results, p){
  denom = sum(sqrt(p))
  p_calc = sqrt(p)/denom
  Allocation = sample(T, n, replace = TRUE, prob = p_calc)
  return(Allocation)
}