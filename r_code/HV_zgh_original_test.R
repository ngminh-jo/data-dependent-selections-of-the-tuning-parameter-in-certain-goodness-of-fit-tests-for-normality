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
# Henz-Visagie test
Henz_Visagie_test = function(n,g,X){
  Y = scaled_residuals(n,X)
  temp_test_statistic = matrix( 0 , nrow = n, ncol = n) 
  for (j in 1:n ){
    for (k in 1:n){
      temp_test_statistic[j,k] =  exp((Y[j] + Y[k])^2/(4*g)) * ( (Y[j]*Y[k]) - (Y[j] + Y[k])^2/(2*g) + 1/(2*g) + (Y[j] + Y[k])^2/(4*(g^2)))
    }
  }
  return( 16 * ( g^(2+(1/2)))/ (pi^(1/2)) * (1/n) * ((pi/g)^(1/2)) * sum(temp_test_statistic))
}

# Zghoul test  
Zghoul_test = function(n,g,X){
  Y = scaled_residuals(n,X)
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
