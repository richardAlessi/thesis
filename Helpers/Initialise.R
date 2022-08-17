Initialise <- function(n, n0, T, Results){

Allocation = rep(0,n)
counter = 0

for (t in 1:T) {
  treatmentCounter = n0
  success = 0
  Allocation[(counter+1):(counter+n0)] = t
  counter = counter + n0
  Res = Results[1:counter, ]
  All = Allocation[1:counter]
  success = sum(Res[All == t, t])
  while(success == 0 || success == treatmentCounter){
    counter = counter + 1
    treatmentCounter = treatmentCounter + 1
    Allocation[counter] = t
    Res = Results[1:counter, ]
    All = Allocation[1:counter]
    success = sum(Res[All == t, t])
  }
}
return(Allocation)
}
