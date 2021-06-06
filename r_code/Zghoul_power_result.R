# used function
test_decision_Zghoul <- function(n,g, cri_wert, no_cores, no_simu){
  # power result of Zghoul 
  test_erg <- numeric(26) # number of distribution samples
  for (i in 1: 26) {
    registerDoParallel(no_cores)  # use multicore, set to the number of our cores
    temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
      Zghoul_test(n,g, distribution_samples(i,n)) > cri_wert
    }
    test_erg[i] = sum(temp)*100 / no_simu
    stopImplicitCluster()
  }
  return(test_erg)
}

test_decision_Zghoul_tab <- function(n,g, cri_wert_d1, no_cores, no_simu){
  test_decision_Zghoul_table = matrix(0, ncol = length(g), nrow =  26) # nrow is the number of distribution samples
  for (i in 1: length(g)){
    test_decision_Zghoul_table[ ,i] = test_decision_Zghoul(n,g[i], cri_wert_d1[i], no_cores, no_simu) # cause it returns a vector
  }
  colnames(test_decision_Zghoul_table) <- g
  #c(2.5, 3, 4, 5, 7, 10) 
  return(test_decision_Zghoul_table)
  #(print(xtable(as.data.frame(test_decision_Henz_Visage_table)), include.colnames = TRUE,include.rownames=FALSE) )
}

# result present
g_zghoul = c( 1.5, 2, 3, 5, 9, 15)
n = c(10,20,50)
no_cores = 8
no_simulation = 100
Zghoul_critical_value = read.csv("Zghoul_critical_value.csv ")
# create the power result table for henz_visagie test

start.time <- Sys.time()
Zghoul_power_result_10 = test_decision_Zghoul_tab(n[1], g_zghoul, Zghoul_critical_value[1,] , no_cores, no_simulation)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

colnames(Zghoul_power_result_10) = g_zghoul
write.csv(Zghoul_power_result_10, file = "Zghoul_power_result_10.csv " , row.names = FALSE)



# tuning para test

#Bonferoni

test_decision_Zghoul_bonferroni <- function(n, cri_wert, no_cores, no_simu){
  # power result of Zghoul 
  test_erg <- numeric(26) # number of distribution samples
  for (i in 1: 26) {
    registerDoParallel(no_cores)  # use multicore, set to the number of our cores
    temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
      #(n, distribution_samples(i,n)) > cri_wert
      Z_bonferroni_test(n, distribution_samples(i,n))> cri_wert
    }
    test_erg[i] = sum(temp)*100 / no_simu
    stopImplicitCluster()
  }
  return(test_erg)
}

Zghoul_Benferoni_critical_value <- data.matrix(HV_Zghoul_benferoni_critical_value)[2,]

start.time <- Sys.time()
test_decision_Zghoul_bonferroni_table <- matrix(0, nrow = 26, ncol = 3)
for (i in 1:3){
  test_decision_Zghoul_bonferroni_table[,i] = test_decision_Zghoul_bonferroni(n[i], Zghoul_Benferoni_critical_value[i], no_cores, no_simulation)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
colnames(test_decision_Zghoul_bonferroni_table) = n
write.csv(test_decision_Zghoul_bonferroni_table, file = "Zghoul_bonferroni_power_result_table.csv " , row.names = FALSE)

# Bootstrap based method

test_decision_Zghoul_bootstrap <- function(n, cri_wert, no_cores, no_simu){
  test_erg <- numeric(26) # number of distribution samples
  for (i in 1: 26) {
    registerDoParallel(no_cores)  # use multicore, set to the number of our cores
    temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
      Z_bootstrap_test(n, distribution_samples(i,n))> cri_wert
    }
    test_erg[i] = sum(temp)*100 / no_simu
    stopImplicitCluster()
  }
  return(test_erg)
}

Zghoul_Bootstrap_critical_value <- data.matrix(HV_Zghoul_Bootstrap_critical_value)[2,]

start.time <- Sys.time()
test_decision_Zghoul_bootstrap_table <- matrix(0, nrow = 26, ncol = 3)
for (i in 1:3){
  test_decision_Zghoul_bootstrap_table[,i] = test_decision_Zghoul_bootstrap(n[i], Zghoul_Bootstrap_critical_value[i], no_cores, no_simulation)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
colnames( test_decision_Zghoul_bootstrap_table) = n
write.csv( test_decision_Zghoul_bootstrap_table, file = "Zghoul_bootstrap_power_result_table.csv " , row.names = FALSE)









