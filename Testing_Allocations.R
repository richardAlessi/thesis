## Testing file
setwd("C:/Users/richa/Desktop/Thesis/Code/thesis/FinalModel")

# Helper files
source("./Helpers/successCount.R")
source("./Helpers/successTreatmentCount.R")
source("./Helpers/Initialise_RR.R")
source("./Helpers/InitialiseWCovariates_RR.R")
source("./Helpers/ChiSq_calc.R")
source("./Helpers/LR_test_calc.R")
source("./Helpers/DBCD_G_Multi_Function.R")
source("./Models/1_ResponseAdaptive.r")
source("./Models/2_CARA.r")
source("./Models/3_MLMethods.R")
source("./Models/4_GittinsIndex.R")
GI = readRDS("C:/Users/richa/Desktop/Thesis/Code/thesis/FinalModel/Helpers/GittinsMatrix.rds")


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

h = 1

SuccessProbs = matrix(SuccessProbs_List[h,], nrow = 2, ncol = 2, byrow = FALSE)
gam = 3

# Results
Tr = dim(SuccessProbs)[2]
Results = matrix(0, nrow = n_max, ncol = Tr)
Z = rbinom(n_max,1,0.5) # Simulate single binary covariate

for (i in 1:Tr) {
  Results[ , i] = rbinom(n_max, 1, SuccessProbs[Z+1, i]) 
}

SuccessProbs_known = colMeans(SuccessProbs)

AllocationMethod = "Neyman"
EstimationMethod = "DBCD"

p_known = SuccessProbs


n = dim(Results)[1]
Tr = dim(Results)[2]

Allocation = rep(0,n)

p = rep(0,Tr)
q = rep(0,Tr)

#First initialise by assigning no to each treatment
Allocation = InitialiseWCovariates_RR(n0, Results, Z)
counter = sum(Allocation != 0)
if(counter >= n){return(Allocation[1:n])}
qf = rep(0,n)

if(EstimationMethod == "Estimated" || EstimationMethod == "DBCD"){
  for (i in (counter+1):n) {
    # Estimate or specify p and then calculate q
    All = as.factor(Allocation[1:(i-1)])
    Res = rep(0,(i-1))
    for (j in 1:(i-1)){
      Res[j] = Results[j, All[j]] 
    }
    Z_part = Z[1:(i-1)]
    #All_dummy = dummy_cols(All)
    #All_dummy = All_dummy[,3:(dim(All_dummy)[2])] # Remove first column of original data and first treatment due to multicollinearity
    glm_res = glm(formula = Res~All*Z_part, family = binomial(link="logit"))
    for (j in 1:Tr) {
      # Treatment = rep(0,Tr)
      # Treatment[j] = 1
      data = data.frame(All = as.character(j), Z_part = Z[i])
      p[j] = predict(glm_res, newdata = data, type = "response")
      q[j] = 1 - p[j]
    }
    
    if (AllocationMethod == "Neyman") {
      denom = sum(sqrt(p*q))
      p_calc = sqrt(p*q)/denom
    }
    if (AllocationMethod == "RSIHR") {
      denom = sum(sqrt(p))
      p_calc = sqrt(p)/denom
    }
    if (AllocationMethod == "Rosenberger") {
      denom = sum(p/q)
      p_calc = p/(q*denom)
    }
    if (AllocationMethod == "OptimallyAdjusted") {
      denom = sum(sqrt(p)*q)
      p_calc = sqrt(p)*q/denom
    }
    
    
    if(EstimationMethod == "DBCD"){
      Pi_allocated = rep(0,Tr)
      for (w in 1:Tr) {
        Pi_allocated[w] = sum(All==w)  
      }
      Pi_allocated_perc = Pi_allocated/sum(Pi_allocated)
      p_calc = DBCD_G_Multi(Pi_allocated_perc,p_calc,gam)
    }
    Allocation[i] = sample(Tr,1, replace = TRUE, prob = p_calc)
  }
}

S = successCount(Allocation, Results)
S_Z0 = successCount(Allocation[Z==0], Results[Z==0,])
S_Z1 = successCount(Allocation[Z==1], Results[Z==1,])


St = successTreatmentCount(Allocation, Results)
St_Z0 = successTreatmentCount(Allocation[Z==0], Results[Z==0,])
St_Z1 = successTreatmentCount(Allocation[Z==1], Results[Z==1,])

table(Allocation)

table(Allocation[Z==0])

table(Allocation[Z==1])

c(S, S_Z1, St, St_Z1)
