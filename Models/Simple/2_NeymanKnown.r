# Simple allocation based off Neyman Allocation
neymanKnown <- function(n, T, Results, p){
  q = 1 - p
  denom = sum(sqrt(p*q))
  p_calc = sqrt(p*q)/denom
  Allocation = sample(T, n, replace = TRUE, prob = p_calc)
  return(Allocation)
}