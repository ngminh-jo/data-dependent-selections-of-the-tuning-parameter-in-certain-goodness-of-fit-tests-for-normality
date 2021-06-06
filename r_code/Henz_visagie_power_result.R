# used function
test_decision_Henz_Visage <- function(n,g, cri_wert, no_cores, no_simu){
  # power result of Henz_Visagie 
  test_erg <- numeric(26) # number of distribution samples
  for (i in 1: 26) {
    registerDoParallel(no_cores)  # use multicore, set to the number of our cores
    temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
      Henz_Visagie_test(n,g, distribution_samples(i,n)) > cri_wert
    }
    test_erg[i] = sum(temp)*100 / no_simu
    stopImplicitCluster()
  }
  return(test_erg)
}
test_decision_Henz_Visage_tab <- function(n,g, cri_wert_d1, no_cores, no_simu){
  test_decision_Henz_Visage_table = matrix(0, ncol = length(g), nrow =  26) # nrow is the number of distribution samples
  for (i in 1: length(g)){
    test_decision_Henz_Visage_table[ ,i] = test_decision_Henz_Visage(n,g[i], cri_wert_d1[i], no_cores, no_simu) # cause it returns a vector
  }
  colnames(test_decision_Henz_Visage_table) <- g
  #c(2.5, 3, 4, 5, 7, 10) 
  return(test_decision_Henz_Visage_table)
  #(print(xtable(as.data.frame(test_decision_Henz_Visage_table)), include.colnames = TRUE,include.rownames=FALSE) )
}

# result present

g_hv = c(2.5, 3, 4, 5, 7, 10)
g_zghoul = c( 1.5, 2, 3, 5, 9, 15)
n = c(10,20,50)
no_cores = 8
no_simulation = 10000
#Henz_Visagie_critical_value = read.csv("Henz_Visagie_critical_value.csv ")
# create the power result table for henz_visagie test
start.time <- Sys.time()
#test_decision_Henz_Visage(n[1], g_hv[1], Henz_Visagie_critical_value[1,1] , 1, 10000)
Henz_visagie_power_result_50 = test_decision_Henz_Visage_tab(n[3], g_hv, Henz_Visagie_critical_value[3,] , no_cores, no_simulation)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
colnames(Henz_visagie_power_result_50) = g_hv
write.csv(Henz_visagie_power_result_50, file = "Henz_visagie_power_result_50.csv " , row.names = FALSE)



# tuning para test

#Bonferoni
Henz_Visagie_Benferoni_critical_value <- HV_Zghoul_benferoni_critical_value[1,]

test_decision_Henz_Visage_bonferroni <- function(n, cri_wert, no_cores, no_simu){
  # power result of Henz_Visagie 
  test_erg <- numeric(26) # number of distribution samples
  for (i in 1: 26) {
    registerDoParallel(no_cores)  # use multicore, set to the number of our cores
    temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
      #(n, distribution_samples(i,n)) > cri_wert
      HV_bonferroni_test(n, distribution_samples(i,n))> cri_wert
    }
    test_erg[i] = sum(temp)*100 / no_simu
    stopImplicitCluster()
  }
  return(test_erg)
}

#test_decision_Henz_Visage_bonferroni(n[1], Henz_Visagie_Benferoni_critical_value[1], 1, 1000)

n = c(10,20,50)
no_cores = 8
no_simulation = 10000

start.time <- Sys.time()
test_decision_Henz_Visage_bonferroni_table <- matrix(0, nrow = 26, ncol = 3)
for (i in 1:3){
  test_decision_Henz_Visage_bonferroni_table[,i] = test_decision_Henz_Visage_bonferroni(n[i], Henz_Visagie_Benferoni_critical_value[i], no_cores, no_simulation)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
colnames(test_decision_Henz_Visage_bonferroni_table) = n
write.csv(test_decision_Henz_Visage_bonferroni_table, file = "Henz_Visage_bonferroni_power_result_table.csv " , row.names = FALSE)

# Bootstrap based method

Henz_Visagie_Bootstrap_critical_value <- HV_Zghoul_Bootstrap_critical_value[1,]

test_decision_Henz_Visage_bootstrap <- function(n, cri_wert, no_cores, no_simu){
  test_erg <- numeric(26) # number of distribution samples
  for (i in 1: 26) {
    registerDoParallel(no_cores)  # use multicore, set to the number of our cores
    temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
      HV_bootstrap_test(n, distribution_samples(i,n))> cri_wert
    }
    test_erg[i] = sum(temp)*100 / no_simu
    stopImplicitCluster()
  }
  return(test_erg)
}
start.time <- Sys.time()
test_decision_Henz_Visage_bootstrap_table <- matrix(0, nrow = 26, ncol = 3)
for (i in 1:3){
  test_decision_Henz_Visage_bootstrap_table[,i] = test_decision_Henz_Visage_bootstrap(n[i], Henz_Visagie_Bootstrap_critical_value[i], no_cores, no_simulation)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
colnames( test_decision_Henz_Visage_bootstrap_table) = n
write.csv( test_decision_Henz_Visage_bootstrap_table, file = "Henz_Visage_bootstrap_power_result_table.csv " , row.names = FALSE)









