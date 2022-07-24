# Play the Winner: If it is a success we continue with the same treatment otherwise we randomly select from all treatments
playTheWinner <- function(n, T, Results){
  Allocation = rep(0,n)
  winner = 0
  for (i in 1:n) {
    if (winner == 1) {
      Allocation[i] = Allocation[i-1]
    } else{
      Allocation[i] = sample(T,1)
    }
    winner = Results[i, Allocation[i]]
  }
  return(Allocation)
}
