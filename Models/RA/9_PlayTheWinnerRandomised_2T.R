
# Play the winner randomised: Start with number of successes of each treatment (wA. wB). pA = wA / (wA + wB)
# Increment wA if treatment A succeeds or treatment B fails
# Similarly increment wB if treatment B succeeds or treatment A fails
# Two treatments only
playTheWinnerRandomised_2T <- function(n, Results, wA, wB){
  Allocation = rep(0,n)
  q = rep(0,n)
  for (i in 1:n) {
    q[i] = wA/(wA + wB)
    Allocation[i] = 2-(runif(1)<= q[i])
    Outcome = Results[i, Allocation[i]]
    if(( Outcome == 1 && Allocation[i] == 1) || (Outcome == 0 && Allocation[i] == 2) ){
      wA = wA + 1
    } else{ wB = wB + 1}
  }
  CombinedResult = data.frame(Allocation = Allocation, q = q)
  return(CombinedResult)
}
