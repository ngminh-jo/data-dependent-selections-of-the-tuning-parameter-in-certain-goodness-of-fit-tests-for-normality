library(foreach)
library(doParallel)
Montecarlo_critical_value_test = function( FUN, n, g, alpha, no_cores, no_simu){
  registerDoParallel(no_cores)  # use multicore, set to the number of our cores
  temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
    FUN(n,g, matrix(rnorm(n), nrow = n, ncol = 1) )
  }
  stopImplicitCluster()
  return( quantile(temp, probs= 1- alpha ) ) # u = alpha/ cardility von g
}

Montecarlo_critical_value_table = function(FUN, n, g, alpha ,no_cores , no_simu){
  temp_critical_value = matrix(0, nrow = length(n), ncol = length(g))
  for (j in 1 : length(n)){
    for (k in 1: length(g)){
      temp_critical_value [j,k] = Montecarlo_critical_value_test(FUN, n[j],g[k],alpha, no_cores, no_simu)
    }
  }
  return(temp_critical_value)
}

# for Benferroni, Bootstrap based,  test 
Montecarlo_critical_value_data_dependent_test = function( FUN, n, alpha, no_cores, no_simu){
  registerDoParallel(no_cores)  # use multicore, set to the number of our cores
  temp = foreach(j = 1 : no_simu, .combine = c) %dopar%{
    FUN(n, matrix(rnorm(n), nrow = n, ncol = 1) )
  }
  stopImplicitCluster()
  return( quantile(temp, probs= 1- alpha ) ) # u = alpha/ cardility von g
}

Montecarlo_critical_value_data_dependent_table = function(FUN, n, alpha ,no_cores , no_simu){
  temp_critical_value = numeric(length(n))
  for (j in 1 : length(n)){
      temp_critical_value [j] = Montecarlo_critical_value_Benferroni_test(FUN, n[j],alpha, no_cores, no_simu)
  }
  return(temp_critical_value)
}
