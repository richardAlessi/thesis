library(gittins)
start_time <- Sys.time()
Sig_start = 1
n_start = 2
n = 1000
GI = bmab_gi_multiple(Sigma_start = Sig_start, n_start = n_start, gamma = 0.95 , N = 1000 , num_actions = n, tol = 5e-3)

saveRDS(GI, file = "C:/Users/richa/Desktop/Thesis/Code/thesis/FinalModel/Helpers/GittinsMatrix_rerun.rds")
end_time <- Sys.time()
print(end_time - start_time)

