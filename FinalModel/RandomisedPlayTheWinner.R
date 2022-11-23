library(stringr)
library(foreach)
library(parallel)
library(iterators)
library(doParallel)
library(fastDummies)
library(lmtest)
library(zoo)
setwd("C:/Users/richa/Desktop/Thesis/Code/thesis/FinalModel")
source("./Helpers/successCount.R")
source("./Helpers/successTreatmentCount.R")
source("./Helpers/Initialise_RR.R")
source("./Helpers/InitialiseWCovariates_RR.R")
source("./Helpers/ChiSq_calc.R")
source("./Helpers/LR_test_calc.R")
source("./Helpers/DBCD_G_Multi_Function.R")
source("./Models/3_MLMethods.R")



RunModel_RPTW <- function(n_max, n_min, n_step, n0, Z, Results, k, p_known, params){
  
  temp = params$temp
  eps = params$eps
  Allocation = ML_Methods(n0, Results, Method = "PlayTheWinnerRandomised", eps, temp)
  
  
  df_combined = data.frame()
  n_s = n_step*((n_min/n_step):(n_max/n_step))
  
  for(j in n_s){
    TotalSuccesses = successCount(Allocation[1:j], Results[1:j,])
    TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j], Results[1:j,])
    
    if(sum(Z[1:j]==0) ==0){
      Z0_TotalSuccesses = 0
      Z0_TotalTreatmentSuccesses = c(0,0)  
    }else{
      Z0_TotalSuccesses = successCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
      Z0_TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    }
    
    df_update = c(paste("RA", "ML Model", "PlayTheWinnerRandomised", sep = "_"),
                  k, j, 
                  TotalSuccesses,
                  TotalTreatmentSuccesses[1],
                  TotalTreatmentSuccesses[2],
                  sum(Allocation[1:j]==1),
                  sum(Allocation[1:j]==2),
                  Z0_TotalSuccesses,
                  Z0_TotalTreatmentSuccesses[1],
                  Z0_TotalTreatmentSuccesses[2],
                  sum(Allocation[1:j][Z[1:j]==0]==1),
                  sum(Allocation[1:j][Z[1:j]==0]==2),
                  ChiSq_calc(Allocation[1:j], Results[1:j,]),
                  LR_test_calc(Allocation[1:j], Results[1:j,], Z[1:j]),
                  as.character(list(params)),
                  as.character(list(c(p_known)))
    )
    df_combined = rbind(df_combined,data.frame(t(df_update)))
  }
  return(df_combined)
}

runSims_RPTW = function(k,  n_max, n_min, n_step, n0, SuccessProbs, params, GI) {
  
  df = data.frame(
    Model = character(),
    Run = integer(),
    Sample_size = integer(),
    Successes_total = integer(),
    Success_TreatmentA = integer(),
    Success_TreatmentB = integer(),
    Allocation_TreatmentA = integer(),
    Allocation_TreatmentB = integer(),
    Z0_Successes_total = integer(),
    Z0_Successes_TreatmentA = integer(),
    Z0_Successes_TreatmentB = integer(),
    Z0_Allocation_TreatmentA = integer(),
    Z0_Allocation_TreatmentB = integer(),
    ChiSq_pvalue = double(),
    LR_test_pvalue = double(),
    Parameters = character(),
    Success_Probabilities = character()
  )
  
  c_names = colnames(df)
  
  # Results
  Tr = dim(SuccessProbs)[2]
  Results = matrix(0, nrow = n_max, ncol = Tr)
  Z = rbinom(n_max,1,0.5) # Simulate single binary covariate
  
  for (i in 1:Tr) {
    Results[ , i] = rbinom(n_max, 1, SuccessProbs[Z+1, i]) 
  }
  
  SuccessProbs_known = colMeans(SuccessProbs)
  
  df= rbind(df, RunModel_RPTW(n_max, n_min, n_step, n0, Z, Results, k, SuccessProbs_known, params), deparse.level = 2)
  colnames(df) = c_names
  return(df)
}



# Set up global simulation parameters
runSize = 80 # How often we save results to file
runRepeats = 13 # Number of times we repeat runSize
n_max = 1000 # Total sample size calculated
n_min = 10
n_step = 10
n0 = 2


SuccessProbs_List = matrix(data = c(
  0.4, 0.8, 0.3, 0.1,
  0.4, 0.6, 0.35, 0.25,
  0.45, 0.35, 0.3,0.5,
  0.5, 0.5, 0.3, 0.3, 
  0.4, 0.4, 0.4, 0.4), nrow = 5, ncol = 4, byrow = TRUE)

params = data.frame(c_gamma = 3,temp = 5,eps = 0.1)
numCores <- parallel::detectCores() # Requires library(parallel)



cl = makeCluster(numCores)
registerDoParallel(cl)

for (h in 1:(dim(SuccessProbs_List)[1])) {
#  for (h in 1) {
  SuccessProbs = matrix(SuccessProbs_List[h,], nrow = 2, ncol = 2, byrow = FALSE)
  for(g in 1:runRepeats){
    start_time <- Sys.time()
    resultsCombined = foreach(k = 1:runSize, .combine = rbind) %dopar% {
      res = runSims_RPTW(k,  n_max, n_min, n_step, n0, SuccessProbs, params, GI)
      return(res)
    }
    end_time <- Sys.time()
    write.csv(resultsCombined, file = paste("C:/Users/richa/Desktop/Thesis/Results/Scenario",h ,"_T2_Run_RPTW_ONLY_",g,"_",format(Sys.time(), "%Y%m%d"),".csv", sep = ""))
    print(end_time - start_time)
  }
}

stopCluster(cl)

