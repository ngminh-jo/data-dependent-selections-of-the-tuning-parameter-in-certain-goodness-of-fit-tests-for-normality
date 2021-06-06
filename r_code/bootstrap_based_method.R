# scaled_residuals
scaled_residuals <- function(n,X){
  emp_mean = 1/n * (sum(X))
  emp_covar = 0
  for (i in 1: n) {
    emp_covar = emp_covar + (1/n) * (X[i,] - emp_mean)^2
  }
  Y = numeric(n)
  for (i in 1:n) {
    Y[i] = (X[i,] - emp_mean)/ sqrt(emp_covar)
  }
  return(Y)
}
# test with scaled residual Y als input  
HV_test <- function(n,g,Y){
  temp_test_statistic = matrix( 0 , nrow = n, ncol = n) 
  for (j in 1:n ){
    for (k in 1:n){
      temp_test_statistic[j,k] =  exp((Y[j] + Y[k])^2/(4*g)) * ( (Y[j]*Y[k]) - (Y[j] + Y[k])^2/(2*g) + 1/(2*g) + (Y[j] + Y[k])^2/(4*(g^2)))
    }
  }
  return( 16 * ( g^(2+(1/2)))/ (pi^(1/2)) * (1/n) * ((pi/g)^(1/2)) * sum(temp_test_statistic))
} 
Z_test <- function(n,g,Y){
  A = matrix(0, ncol = n, nrow = n)
  for (i in 1:n){
    for (j in 1:n) {
      A[i,j] = ( 1 / (n*sqrt(g)) ) * exp( (Y[i] + Y[j])^2 / (4*g) )
    }
  }
  B = c(0, rep(n))
  for (i in 1:n) {
    B[i] = 2/(sqrt(g - 0.5)) * exp( (Y[i]^2) / (4*g-2) )
  }
  return( 1000* sqrt(pi) * ( ( n/sqrt(g-1) ) - sum(B) + sum(A) ) )
}

# Bootstrap based method for scaled residual 
bootstrap_for_Y <- function(n,Y){
  Y_bootstrap = numeric(n)
  for (j in 1: n ){ #sample size
    Y_bootstrap[j] = Y[sample(n, 1)]
  }
  return(Y_bootstrap)
}
# bootstrap based 
bootstrap_based_para <- function(FUN,critical_value,n,g,X) {
  tuning_para = numeric(length(g))
  Y = scaled_residuals(n,X)
  for (i in 1: length(g)){
    I = 0 # the indicatot function
    for (k in 1: 100) { #Bootstrap number 
      Y_bootstrap = bootstrap_for_Y(n,Y)
      I = I + (1/100)*( FUN(n,g[i], Y_bootstrap) > critical_value[i] )
    }
    tuning_para[i] = I
  }
  erg = g[which.max(tuning_para)]
  return(erg) # tuning para meter and the position in the set
}
HV_bootstrap_test <- function(n,X){
  Hv_test_cri_10_20_50 = matrix(c(59.850,58.220, 53.591, 50.083, 47.745, 44.979,
                                  119.115, 104.389, 88.294, 79.485, 70.131, 65.184,
                                  219.382, 173.079, 131.732, 110.610, 92.291, 82.927), 
                                  byrow = T , nrow = 3, ncol = 6)
  g_hv = c(2.5, 3, 4, 5, 7, 10)
  if(n ==10){ Hv_cri = Hv_test_cri_10_20_50[1,]}
  if(n ==20){ Hv_cri = Hv_test_cri_10_20_50[2,]}
  if(n ==50){ Hv_cri = Hv_test_cri_10_20_50[3,]}
  tunning_para = bootstrap_based_para(HV_test, Hv_cri, n, g_hv, X)
  return(Henz_Visagie_test(n, tunning_para, X))
}
Z_bootstrap_test <- function(n,X){
  Z_test_cri_10_20_50 = matrix(c(1885.307, 406.606, 62.165, 7.165, 0.739, 0.112,
                                 4882.133, 895.159, 110.043, 11.207, 1.051, 0.155,
                                 12134.918, 1675.366, 170.144, 14.987, 1.335, 0.189), 
                               byrow = T , nrow = 3, ncol = 6)
  g_Z = c(1.5, 2, 3, 5, 9, 15)
  if(n ==10){ Z_cri = Z_test_cri_10_20_50[1,]}
  if(n ==20){ Z_cri = Z_test_cri_10_20_50[2,]}
  if(n ==50){ Z_cri = Z_test_cri_10_20_50[3,]}
  tunning_para = bootstrap_based_para(Z_test, Z_cri, n, g_Z, X)
  return(Zghoul_test(n, tunning_para, X))
}
# test with normal distribution 
X = matrix(rnorm(10), nrow = 10, ncol = 1)
HV_bootstrap_test(20, matrix(rnorm(20), nrow = 20, ncol = 1) )
Z_bootstrap_test(20, matrix(rnorm(20), nrow = 20, ncol = 1) )



