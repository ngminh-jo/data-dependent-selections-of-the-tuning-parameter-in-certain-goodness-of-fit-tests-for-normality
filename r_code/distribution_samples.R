# verteilung 
distribution_samples <- function(i, n){
  if (i==1) { 
    return(matrix(rnorm(n), nrow = n, ncol = 1)) #normal verteilung
  }
  if (i==2) { 
    components_1 <- sample(1:2,prob=c(0.5,0.5),size=n,replace=TRUE)
    mus <- c(0,0)
    sds <- sqrt(c(1,4))
    NMIX1 <- matrix(rnorm(n= n,mean=mus[components_1],sd=sds[components_1]), nrow = n, ncol = 1) # mixtures of the normal distribution N(0,1) and N(0,4)
    return(NMIX1)
  }
  if (i==3) { 
    components_2 <- sample(1:2,prob=c(0.75,0.25),size=n,replace=TRUE)
    mus <- c(0,0)
    sds <- sqrt(c(1,4))
    NMIX2 <- matrix(rnorm(n= n,mean=mus[components_2],sd=sds[components_2]), nrow = n, ncol = 1)
    return(NMIX2)
  }
  if (i==4) { 
    return(matrix(rt(n,3), nrow = n, ncol = 1)) #  t-distributions with 3, 5 and 10 degress of freedom
  }
  if (i==5) { 
    return(matrix(rt(n,5), nrow = n, ncol = 1))
  }
  if (i==6) { 
    return(matrix(rt(n,10), nrow = n, ncol = 1))
  }
  if (i==7) { 
    return(matrix(rlnorm(n, meanlog = 0, sdlog = 1/2),nrow = n, ncol = 1 )) # lognormal distributions with parameters (0,1/2) and (0,1/4)
  }
  if (i==8) { 
    return(matrix(rlnorm(n, meanlog = 0, sdlog = 1/4),nrow = n, ncol = 1 ))
  }
  if (i==9) { 
    return(matrix(rchisq(n, 5, ncp = 0), nrow = n, ncol = 1)) #The (non-central) Chi-Squared Distribution 5,15
  }
  if (i==10) { 
    return(matrix(rchisq(n, 15, ncp = 0), nrow = n, ncol = 1))
  }
  if (i==11) { 
    return(matrix(rlogis(n, location = 0, scale = 1), nrow = n, ncol = 1)) # the logistic distribution
  }
  if (i==12) { 
    return(matrix(rweibull(n, shape = 10, scale = 1), nrow = n, ncol = 1)) # the weibull distribution 10, 20
  }
  if (i==13) { 
    return(matrix(rweibull(n, shape = 20, scale = 1), nrow = n, ncol = 1))
  }
  if (i==14) { 
    return(matrix(rpearsonVII(n,df=5, location=0, scale=1), nrow = n, ncol = 1)) #the Pearson type VII distributions with 5 and 10 degrees of freedom
  }
  if (i==15) { 
    return(matrix(rpearsonVII(n,df=10, location=0, scale=1), nrow = n, ncol = 1))
  }
  if (i==16) { 
    return(matrix(rsn(n= n, xi=0, omega=1, alpha=3, tau=0, dp=NULL), nrow = n, ncol = 1)) #the skew-normal law with skewness parameters 3 and 5 
  }
  if (i==17) { 
    return(matrix(rsn(n= n, xi=0, omega=1, alpha=5, tau=0, dp=NULL), nrow = n, ncol = 1))
  }
  if (i==18) { 
    return(matrix(runif(n, min = -sqrt(3), max = sqrt(3)), nrow = n, ncol = 1)) # unit distribution
  }
  if (i==19) { 
    return(matrix(rbeta(n,1,4), nrow = n, ncol = 1)) # beta  distribution
  }
  if (i==20) { 
    return(matrix(rbeta(n,2,5), nrow = n, ncol = 1)) 
  }
  if (i==21) { 
    return(matrix(rgamma(n,shape= 1, scale = 5), nrow = n, ncol = 1))  # gamma distribution
  }
  if (i==22) { 
    return(matrix(rgamma(n,shape= 5, scale = 1), nrow = n, ncol = 1))  
  }
  if (i==23) { 
    return(matrix(rgumbel(n, 0, 1), nrow = n, ncol = 1))  # gumbel distribution
  }
  if (i==24) { 
    return(matrix(rcauchy(n), nrow = n, ncol = 1))  # cauchy(0,1) distribution
  }
  if (i==25) { 
    return(matrix(rexp(n), nrow = n, ncol = 1))  # exp(1) distribution
  }
  if (i==26) { 
    return(matrix(rexp(n)-rexp(n), nrow = n, ncol = 1))  # Laplace(0,1) distribution
  }
}








