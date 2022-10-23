Model_allocations = c("Neyman", "RSIHR", "Rosenberger", "OptimallyAdjusted")
Model_estimations = c("Known", "Estimated", "DBCD")

ML_Allocations = c("Random", "PlayTheWinner", "PlayTheWinnerRandomised", "EpsilonGreedy", "SoftMax")

RunModel_RA <- function(AllocationMethod, EstimationMethod, n_max, n_min, n_step, n0, Z, Results, k, p_known, params){
  c_gamma = params$c_gamma
  SuccessProbs_known = colMeans(p_known)
  Allocation = ResponseAdaptive(n_max, n0, Results, AllocationMethod = AllocationMethod, EstimationMethod = EstimationMethod, p_known =  SuccessProbs_known, gam = c_gamma)
  
  df_combined = data.frame()
  n_s = n_step*((n_min/n_step):(n_max/n_step))
  
  for(j in n_s){
    TotalSuccesses = successCount(Allocation[1:j], Results[1:j,])
    TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j], Results[1:j,])
    
    Z0_TotalSuccesses = successCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    Z0_TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    
    
    df_update = c(paste("RA", EstimationMethod, AllocationMethod, sep = "_"),
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

RunModel_CARA <- function(AllocationMethod, EstimationMethod,  n_max, n_min, n_step, n0, Z, Results, k, p_known, params){
  c_gamma = params$c_gamma
  Allocation = CARA(Results, n0, Z, AllocationMethod = AllocationMethod, EstimationMethod = EstimationMethod, p_known =  p_known, gam = c_gamma)
  TotalSuccesses = successCount(Allocation, Results)
  TotalTreatmentSuccesses = successTreatmentCount(Allocation, Results)
  
  df_combined = data.frame()
  n_s = n_step*((n_min/n_step):(n_max/n_step))
  
  for(j in n_s){
    TotalSuccesses = successCount(Allocation[1:j], Results[1:j,])
    TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j], Results[1:j,])
    
    Z0_TotalSuccesses = successCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    Z0_TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    
    
    df_update = c(paste("RA", EstimationMethod, AllocationMethod, sep = "_"),
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


RunModel_ML <- function(AllocationMethod,  n_max, n_min, n_step, n0, Z, Results, k, p_known, params){
  temp = params$temp
  eps = params$eps
  Allocation = ML_Methods(n0, Results, Method = AllocationMethod, eps, temp)
  
  
  df_combined = data.frame()
  n_s = n_step*((n_min/n_step):(n_max/n_step))
  
  for(j in n_s){
    TotalSuccesses = successCount(Allocation[1:j], Results[1:j,])
    TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j], Results[1:j,])
    
    Z0_TotalSuccesses = successCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    Z0_TotalTreatmentSuccesses = successTreatmentCount(Allocation[1:j][Z[1:j]==0], Results[1:j,][Z[1:j]==0,])
    
    
    df_update = c(paste("RA", "ML Model", AllocationMethod, sep = "_"),
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

RunModel_Gittins <- function( n_max, n_min, n_step, n0, Z, Results, k, p_known, Random, GI){
  Allocation = GittinsIndex(n0, Results, GI, Random)
  
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
    
    
    df_update = c(paste("RA", "Gittins", as.character(Random), sep = "_"),
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


runSims = function(k,  n_max, n_min, n_step, n0, SuccessProbs, params, GI) {
  
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
  
  
  # Run models
  for (AM in Model_allocations) {
    for (EM in Model_estimations) {
      df = rbind(df, RunModel_RA(AllocationMethod = AM, EstimationMethod = EM,  n_max, n_min, n_step, n0, Z, Results, k, p_known = SuccessProbs, params), deparse.level = 2)
      df = rbind(df, RunModel_CARA(AllocationMethod = AM, EstimationMethod = EM,  n_max, n_min, n_step, n0, Z, Results, k, p_known = SuccessProbs, params), deparse.level = 1)
    }
    
  }
  
  
  # Run remaining models
  for (M in ML_Allocations) {
    df= rbind(df, RunModel_ML(M,  n_max, n_min, n_step, n0, Z, Results, k, SuccessProbs_known, params), deparse.level = 1)
  }
  
  # Run gittins
  df= rbind(df, RunModel_Gittins( n_max, n_min, n_step, n0, Z, Results, k, SuccessProbs, TRUE, GI), deparse.level = 1)
  df= rbind(df, RunModel_Gittins( n_max, n_min, n_step, n0, Z, Results, k, SuccessProbs, FALSE, GI), deparse.level = 1)
  
  colnames(df) = c_names
  return(df)
}